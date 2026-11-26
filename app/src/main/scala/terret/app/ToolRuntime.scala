package terret.app

import heddle.ChildCommand
import heddle.client.Client
import heddle.http.header.Headers
import heddle.mcp.client.{McpClient, McpSession, McpStdio}
import heddle.mcp.protocol.{CallToolResult, ContentBlock, Implementation, Tool, ToolName}
import terret.find.*
import terret.grok.{McpCatalog, McpDocumentError, McpGrant, McpPolicy, McpServer}
import terret.mcp.{TerretMcp, ToolSurface}
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, Exit, IO, Promise, Ref, Scope, ZIO, ZLayer}

/** Search and call against the workspace `.mcp.json`.
  *
  * The ONNX session is opened on the first find that has both a grant and a server, and it stays in the process scope.
  * Each find reconnects and rebuilds the index. A call connects for that one tool. Nothing here runs at bind.
  */
object ToolRuntime:
  def make(
      config: ProcessConfig,
      open: ZIO[Scope, FindError, Embedder] = ZIO.service[Embedder].provide(ModelHome.live >>> BgeSmall.layer),
  ): ZIO[Scope, Nothing, ToolSurface] =
    for
      scope <- ZIO.scope
      slot  <- Ref.make[Option[Promise[FindError, Embedder]]](None)
    yield Live(config, scope, slot, open)

  private final class Live(
      config: ProcessConfig,
      scope: Scope,
      slot: Ref[Option[Promise[FindError, Embedder]]],
      open: ZIO[Scope, FindError, Embedder],
  ) extends ToolSurface:
    private val settings = McpClient.Settings(Implementation(TerretMcp.name, config.version))

    def find(query: String, k: Int): IO[FindError, FindResult] =
      if config.mcp.grants.isEmpty then
        Find.screen(query, k, Access.empty).map {
          case Find.Screened.Done(result) => result
          case Find.Screened.Query(_)     => FindResult.noGrants
        }
      else
        load(config.root).mapError(error => FindError.Remote(document(error))).flatMap { servers =>
          val access = Access(grants(config.mcp), defined(servers))
          Find.screen(query, k, access).flatMap {
            case Find.Screened.Done(result)                => ZIO.succeed(result)
            case Find.Screened.Query(_) if servers.isEmpty => ZIO.succeed(FindResult.empty)
            case Find.Screened.Query(_)                    => search(query, k, access, servers)
          }
        }

    def call(server: Option[String], tool: String, arguments: String): IO[CallError, String] =
      if config.mcp.grants.isEmpty then refused(server, tool)
      else
        load(config.root).mapError(error => CallError.Unavailable(document(error))).flatMap { servers =>
          val access = Access(grants(config.mcp), defined(servers))
          ZIO.fromEither(Calls.decide(server, tool, access)).flatMap { (name, id) =>
            ZIO.fromEither(located(servers, name)).flatMap { target =>
              ZIO.scoped {
                connect(target, CallError.Unavailable(_)).flatMap { connected =>
                  invoke(connected, id, arguments)
                }
              }
            }
          }
        }

    private def search(
        query: String,
        k: Int,
        access: Access,
        servers: Chunk[McpServer],
    ): IO[FindError, FindResult] =
      ZIO.scoped {
        for
          embedder <- model
          found    <- (Indexer.load *> Find.find(query, k, access)).provideSome[Scope](
            (ZLayer.succeed(embedder) ++ Catalog.live(listed(servers)) ++ SqliteIndex.temporary) >+> Find.layer
          )
        yield found
      }

    /** One opener. A failure clears the slot so the next find can try again. */
    private def model: IO[FindError, Embedder] =
      Promise.make[FindError, Embedder].flatMap { created =>
        slot
          .modify {
            case Some(existing) => (Left(existing), Some(existing))
            case None           => (Right(created), Some(created))
          }
          .flatMap {
            case Left(existing) => existing.await
            case Right(fresh)   =>
              scope.extend(open).onExit {
                case Exit.Success(value) => fresh.succeed(value).unit
                case Exit.Failure(cause) =>
                  fresh.failCause(cause) *> slot.update {
                    case Some(current) if current eq fresh => None
                    case other                             => other
                  }
              }
          }
      }

    private def listed(servers: Chunk[McpServer]): IO[FindError, Chunk[ToolRef]] =
      ZIO.foreach(servers)(listOne).map(_.flatten)

    private def listOne(server: McpServer): IO[FindError, Chunk[ToolRef]] =
      ServerName.from(server.server).toOption match
        case None       => ZIO.succeed(Chunk.empty)
        case Some(name) =>
          ZIO.scoped {
            connect(server, FindError.Remote(_)).flatMap { connected =>
              connected.listTools.mapBoth(
                error => FindError.Remote(error.message),
                tools => tools.flatMap(tool => Chunk.fromIterable(referenced(name, tool))),
              )
            }
          }

    /** SSE has no client. The detail is diagnostic. */
    private def connect[E](server: McpServer, fail: String => E): ZIO[Scope, E, McpSession] =
      server match
        case McpServer.Sse(_, _, _)                 => ZIO.fail(fail("sse"))
        case McpServer.Stdio(_, command, args, env) =>
          McpStdio
            .spawn(ChildCommand(command, args, Map.from(env)), settings)
            .mapError(error => fail(error.message))
        case McpServer.Http(_, url, headers) =>
          McpClient
            .http(url, settings, headerOf(headers))
            .mapError(error => fail(error.message))
            .provideSomeLayer[Scope](Client.live.mapError(error => fail(error.message)))
  end Live

  private def refused(server: Option[String], tool: String): IO[CallError, String] =
    ZIO.fromEither(Calls.decide(server, tool, Access.empty)).flatMap { (name, id) =>
      ZIO.fail(CallError.NotGranted(name, id))
    }

  private def referenced(server: ServerName, tool: Tool): Option[ToolRef] =
    ToolId.from(tool.name.value).toOption.map { id =>
      val description = tool.description.map(_.trim).filter(_.nonEmpty).getOrElse(id.value)
      ToolRef(Some(server), id, description)
    }

  private def invoke(connected: McpSession, tool: ToolId, arguments: String): IO[CallError, String] =
    for
      args   <- parsed(arguments)
      name   <- ZIO.fromEither(ToolName.from(tool.value)).mapError(_ => CallError.BadTool)
      result <- connected.callTool(name, args).mapError(error => CallError.Unavailable(error.message))
      text   <- rendered(result)
    yield text

  private def parsed(raw: String): IO[CallError, Json.Obj] =
    raw.fromJson[Json] match
      case Right(obj: Json.Obj) => ZIO.succeed(obj)
      case _                    => ZIO.fail(CallError.BadArguments)

  private def rendered(result: CallToolResult): IO[CallError, String] =
    val texts = result.content.collect { case ContentBlock.Text(text, _) => text }
    if result.failed then
      val detail = if texts.isEmpty then "tool error" else texts.mkString
      ZIO.fail(CallError.Unavailable(detail))
    else if texts.nonEmpty then ZIO.succeed(texts.mkString)
    else
      result.structuredContent match
        case Some(obj) => ZIO.succeed(obj.toJson)
        case None      => ZIO.succeed("")

  private def headerOf(pairs: Chunk[(String, String)]): Headers =
    pairs.foldLeft(Headers.empty) { (headers, pair) =>
      val (name, value) = pair
      headers.add(name, value)
    }

  private def load(root: Option[Workspace]): IO[McpDocumentError, Chunk[McpServer]] =
    root match
      case None            => ZIO.succeed(Chunk.empty)
      case Some(workspace) => McpCatalog.live.load(workspace)

  private def grants(policy: McpPolicy): Chunk[Grant] =
    policy.grants.flatMap { row =>
      Chunk.fromIterable(grant(row))
    }

  private def grant(row: McpGrant): Option[Grant] =
    ServerName.from(row.server).toOption.map { server =>
      Grant(server, row.tools.map(kept => kept.flatMap(raw => ToolId.from(raw).toOption)))
    }

  private def defined(servers: Chunk[McpServer]): Set[ServerName] =
    servers.flatMap(server => Chunk.fromIterable(ServerName.from(server.server).toOption)).toSet

  private def located(servers: Chunk[McpServer], name: ServerName): Either[CallError, McpServer] =
    servers.find(server => ServerName.from(server.server).toOption.contains(name)) match
      case Some(server) => Right(server)
      case None         => Left(CallError.UnknownServer(name))

  private def document(error: McpDocumentError): String =
    error match
      case McpDocumentError.Malformed             => "malformed"
      case McpDocumentError.NotObject             => "not an object"
      case McpDocumentError.ServersMissing        => "mcpServers missing"
      case McpDocumentError.NameEmpty             => "empty server name"
      case McpDocumentError.ServerNotObject(name) => s"$name is not an object"
      case McpDocumentError.CommandMissing(name)  => s"$name has no command"
      case McpDocumentError.UrlMissing(name)      => s"$name has no url"
      case McpDocumentError.Transport(name, kind) => s"$name transport $kind"
      case McpDocumentError.Args(name)            => s"$name args"
      case McpDocumentError.Pair(name)            => s"$name pairs"
      case McpDocumentError.Unreadable            => "unreadable"
end ToolRuntime
