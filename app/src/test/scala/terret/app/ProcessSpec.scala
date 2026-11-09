package terret.app

import heddle.mcp.protocol.{ClientRequest, Message, Methods, ProtocolVersion, RequestId}
import heddle.mcp.transport.Http
import terret.memory.{InMemory, Script}
import terret.permission.PermissionPolicy
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.json.*
import zio.test.*
import zio.durationInt
import zio.{ConfigProvider, ZIO}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

object ProcessSpec extends ZIOSpecDefault:
  def spec = suite("process")(
    test("an empty config is the local Warp default") {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map.empty))
        .map { config =>
          assertTrue(
            config == ProcessConfig.local,
            config.listen match
              case http: Listen.Http =>
                WarpLocal.url(http.host, http.port) == "http://127.0.0.1:8080/mcp"
              case Listen.Stdio => false,
          )
        }
    },
    test("stdio, a clamp, and a socket bind decode") {
      val parsed = ProcessConfig.load(
        ProcessConfig.Raw(
          listen = "stdio",
          host = WarpLocal.Host,
          port = WarpLocal.Port,
          policy = "Clamp",
          allow = "ReadWorkspace, Network",
          root = Some("/work"),
          version = "dev",
          command = "grok",
          transport = "AcpWebSocket",
          approval = "Always",
          bindHost = Some("127.0.0.1"),
          bindPort = Some(6767),
          bindSecret = Some("secret"),
        )
      )
      assertTrue(
        parsed.map(_.listen) == Right(Listen.Stdio),
        parsed.map(_.policy) == Right(
          PermissionPolicy.Clamp(
            terret.permission.Ceiling(Set(terret.permission.Permission.ReadWorkspace, terret.permission.Permission.Network))
          )
        ),
        parsed.map(_.root) == Workspace.from("/work").map(Some(_)),
        parsed.map(_.grok.approval) == Right(terret.grok.GrokApproval.Always),
        parsed.map(_.grok.transport) == Right(terret.grok.GrokTransport.AcpWebSocket),
      )
    },
    test("an empty version and a relative root are refused") {
      val empty = ProcessConfig.load(defaults.copy(version = ""))
      val relative = ProcessConfig.load(defaults.copy(root = Some("work")))
      assertTrue(
        empty == Left(ProcessError.Version),
        relative == Left(ProcessError.Workspace(terret.workspace.WorkspaceError.Relative("work"))),
      )
    },
    test("the Warp file names the local HTTP endpoint") {
      val doc = WarpLocal.http(WarpLocal.url("0.0.0.0", 8080))
      assertTrue(
        WarpLocal.url("0.0.0.0", 8080) == "http://127.0.0.1:8080/mcp",
        WarpLocal.url("::1", 9) == "http://[::1]:9/mcp",
        endpoint(doc) == Some(Json.Str("http://127.0.0.1:8080/mcp")),
      )
    },
    test("HTTP serves the jobs on the bound port") {
      ZIO.scoped {
        for
          memory <- InMemory.make(Script.echo)
          config = ProcessConfig.local.copy(listen = Listen.Http("127.0.0.1", 0))
          server <- Process.bind(config, memory)
          port   <- server.port
          status <- listed(port)
        yield assertTrue(status == Right(Set("open", "run", "resume", "export", "cancel")))
      }
    },
  ) @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private def endpoint(doc: Json): Option[Json] =
    doc match
      case root: Json.Obj =>
        root.get("mcpServers") match
          case Some(servers: Json.Obj) =>
            servers.get("terret") match
              case Some(entry: Json.Obj) => entry.get("url")
              case _                      => None
          case _ => None
      case _ => None

  private val defaults: ProcessConfig.Raw = ProcessConfig.Raw(
    listen = "http",
    host = WarpLocal.Host,
    port = WarpLocal.Port,
    policy = "Closed",
    allow = "",
    root = None,
    version = "dev",
    command = "grok",
    transport = "Cli",
    approval = "Ask",
    bindHost = None,
    bindPort = None,
    bindSecret = None,
  )

  private def listed(port: Int): ZIO[Any, Throwable, Either[String, Set[String]]] =
    ZIO.scoped:
      ZIO
        .acquireRelease(ZIO.attempt(HttpClient.newHttpClient()))(client => ZIO.attempt(client.close()).ignore)
        .flatMap: client =>
          ZIO.attemptBlocking:
            val tool = Message.stateless(RequestId.Num(1), ClientRequest.ListTools(None))
            val request = HttpRequest
              .newBuilder(URI.create(s"http://127.0.0.1:$port/mcp"))
              .header("Content-Type", "application/json")
              .header("Accept", "application/json")
              .header(Http.ProtocolHeader, ProtocolVersion.Current.value)
              .header(Http.MethodHeader, Methods.ListTools)
              .POST(HttpRequest.BodyPublishers.ofString(tool.json.toJson))
              .build()
            val response = client.send(request, HttpResponse.BodyHandlers.ofString())
            val body     = Option(response.body()).getOrElse("")
            if response.statusCode() != 200 then Left(body)
            else names(body)

  private def names(body: String): Either[String, Set[String]] =
    for
      json    <- body.fromJson[Json]
      message <- Message.decode(json).left.map(_.json.toJson)
      result  <- message match
                   case Message.Result(_, result) => Right(result)
                   case other                     => Left(other.json.toJson)
      tools   <- result.get("tools") match
                   case Some(Json.Arr(values)) => Right(values)
                   case _                      => Left("missing tools")
    yield tools.flatMap {
      case obj: Json.Obj =>
        obj.get("name") match
          case Some(Json.Str(name)) => Set(name)
          case _                     => Set.empty
      case _ => Set.empty
    }.toSet
end ProcessSpec
