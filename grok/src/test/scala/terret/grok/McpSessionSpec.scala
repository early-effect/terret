package terret.grok

import slayer.*
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.model.ModelKey
import terret.permission.{Ceiling, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, Duration, Exit, Ref, Scope, ZIO, durationInt}

object McpSessionSpec extends ZIOSpecDefault:
  private val histos = McpServer.Stdio("histos", "histos", Chunk("mcp"), Chunk.empty)
  private val metals = McpServer.Http("metals", "http://127.0.0.1:1/mcp", Chunk.empty)
  private val policy = McpPolicy(Chunk(McpGrant("histos", Some(Set("version")))))
  private val sent   = McpServer.encodeAll(Chunk(histos))

  private val idle: Spawner = new:
    def spawn(command: String, args: Chunk[String], cwd: Workspace) =
      ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))

  def spec = suite("MCP session")(
    test("session/new and session/load send only the granted server") {
      driven(Duration.Zero)(opened(ZIO.succeed(Chunk(histos, metals))))
    },
    test("a slow catalog is advanced by the test clock") {
      driven(5.seconds)(opened(ZIO.sleep(5.seconds).as(Chunk(histos, metals))))
    },
    test("a broken catalog is malformed and does not open the agent") {
      driven(Duration.Zero) {
        for
          pipe    <- ZIO.service[RpcPipe]
          acp     <- ZIO.service[AcpOpen]
          catalog <- ZIO.service[McpCatalog]
          opened  <- Ref.make(false)
          _       <- stub[McpCatalog](_.load(any[Workspace]))(ZIO.fail(McpDocumentError.Malformed))
          _       <- stub[AcpOpen](
            _.open(
              any[Workspace],
              any[PermissionPolicy],
              any[Ceiling],
              any[McpPolicy],
              any[NetworkPolicy],
              any[Option[ModelKey]],
            )
          ) { (_, _, _, _, _, _) =>
            opened.set(true) *> ZIO.succeed(pipe)
          }
          _       <- stub[RpcPipe](_.read)(ZIO.succeed(None))
          _       <- stub[RpcPipe](_.write(any[Json]))(ZIO.unit)
          _       <- stub[RpcPipe](_.close)(ZIO.unit)
          harness <- GrokHarness.make(GrokConfig.stdio, idle, acp, mcp = policy, catalog = catalog)
          root    <- workspace
          failed  <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed).exit
          called  <- opened.get
        yield assertTrue(
          failed == Exit.fail(HarnessFailure.Protocol(ProtocolFault.Malformed)),
          !called,
        )
      }
    },
  ) @@ TestAspect.sequential @@ TestAspect.timeout(20.seconds)

  private val frames: Chunk[Json] =
    Chunk(
      Wire.ok(Json.Num(BigDecimal(1)), Json.Obj("protocolVersion" -> Json.Num(BigDecimal(1)))),
      Wire.ok(Json.Num(BigDecimal(2)), Json.Obj("sessionId" -> Json.Str("sess-1"))),
      Wire.ok(Json.Num(BigDecimal(3)), Json.Obj()),
    )

  private def opened(loaded: ZIO[Any, McpDocumentError, Chunk[McpServer]]) =
    for
      pipe    <- ZIO.service[RpcPipe]
      acp     <- ZIO.service[AcpOpen]
      catalog <- ZIO.service[McpCatalog]
      pending <- Ref.make(frames)
      written <- Ref.make(Chunk.empty[Json])
      _       <- stub[McpCatalog](_.load(any[Workspace]))(loaded)
      _       <- stub[AcpOpen](
        _.open(
          any[Workspace],
          any[PermissionPolicy],
          any[Ceiling],
          any[McpPolicy],
          any[NetworkPolicy],
          any[Option[ModelKey]],
        )
      ) { (_, _, _, _, _, _) =>
        ZIO.succeed(pipe)
      }
      _       <- stub[RpcPipe](_.read)(pull(pending))
      _       <- stub[RpcPipe](_.write(any[Json]))(json => written.update(_ :+ json))
      _       <- stub[RpcPipe](_.close)(ZIO.unit)
      harness <- GrokHarness.make(GrokConfig.stdio, idle, acp, mcp = policy, catalog = catalog)
      root    <- workspace
      id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
      _       <- harness.resume(id).runDrain
      calls   <- written.get
    yield assertTrue(
      id.value == "sess-1",
      servers(calls, "session/new") == Some(sent),
      servers(calls, "session/load") == Some(sent),
    )

  private def driven[A](waited: Duration)(
      effect: ZIO[
        Scope & RpcPipe & Stubbed[RpcPipe] & AcpOpen & Stubbed[AcpOpen] & McpCatalog & Stubbed[McpCatalog],
        HarnessFailure,
        A,
      ]
  ) =
    ZIO.scoped {
      (for
        fiber <- effect.fork
        _     <- TestClock.adjust(waited)
        value <- fiber.join
      yield value).provideSome[Scope](stubbed[RpcPipe], stubbed[AcpOpen], stubbed[McpCatalog])
    }

  private def pull(pending: Ref[Chunk[Json]]): ZIO[Any, HarnessFailure, Option[Json]] =
    pending.modify { waiting =>
      waiting.headOption match
        case Some(frame) => (ZIO.succeed(Some(frame)), waiting.drop(1))
        case None        => (ZIO.succeed(None), waiting)
    }.flatten

  private def servers(calls: Chunk[Json], method: String): Option[Json] =
    calls.collectFirst {
      Function.unlift { json =>
        Wire.parse(json).toOption.flatMap {
          case Wire.Call(_, name, params) if name == method =>
            JsonFields.obj(params).flatMap(obj => JsonFields.field(obj, "mcpServers"))
          case _ => None
        }
      }
    }

  private def workspace =
    ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))
end McpSessionSpec
