package terret.grok

import heddle.server.Port
import heddle.ws.{WebSocket, WebSocketFrame}
import heddle.{Handler, HeaderName, Method, PathCodec, Route, Routes, Server}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Duration, Ref, Task, ZIO}

object WsHarnessSpec extends ZIOSpecDefault:
  def spec = suite("WebSocket harness")(
    test("an unbound socket fails before connecting") {
      ZIO.scoped {
        for
          harness <- GrokHarness.make(GrokConfig.unbound, idle, AcpOpen.live(GrokConfig.unbound, idle))
          root    <- ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))
          failed  <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed).flip
        yield assertTrue(failed == HarnessFailure.Protocol(ProtocolFault.BindMissing))
      }
    },
    test("open completes initialize and session/new and sends the bearer token") {
      val secret = "test-secret"
      ZIO.scoped {
        for
          auth    <- Ref.make(Option.empty[String])
          server  <- Server.install(routes(auth), listen)
          port    <- server.port
          bind    <- ZIO.fromEither(AcpBind.make("127.0.0.1", port, secret)).orDieWith(_ =>
                       new IllegalArgumentException("bind")
                     )
          config = GrokConfig.socket(bind)
          harness <- GrokHarness.make(config, idle, AcpOpen.live(config, idle))
          root    <- ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          seen    <- auth.get
        yield assertTrue(id.value == "sess-1", seen.contains(s"Bearer $secret"))
      }
    },
  ) @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private val idle: Spawner = new:
    def spawn(command: String, args: zio.Chunk[String], cwd: Workspace) =
      ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))

  private val listen: Server.Config =
    Server.Config.default.copy(
      host = "127.0.0.1",
      port = Port(0),
      http2 = false,
      gracefulShutdownTimeout = Duration.Zero,
    )

  private def routes(auth: Ref[Option[String]]): Routes[Any, Nothing] =
    val socket = Handler.websocket(script)
    Routes(
      Route.from(
        Method.GET,
        PathCodec.empty,
        (_, req) => auth.set(req.header(HeaderName.Authorization)) *> socket(req),
      )
    )

  private def script(ws: WebSocket): Task[Unit] =
    Ref.make(0).flatMap { answered =>
      ws.receive
        .mapZIO {
          case WebSocketFrame.Text(text) =>
            answered.get.flatMap { n =>
              idOf(text) match
                case Some(id) =>
                  ws.send(WebSocketFrame.Text(Wire.ok(id, result(n)).toJson)) *> answered.set(n + 1)
                case None => ZIO.unit
            }
          case _ => ZIO.unit
        }
        .takeUntilZIO(_ => answered.get.map(_ >= 2))
        .runDrain *> ZIO.never
    }

  private def result(n: Int): Json =
    if n == 0 then Json.Obj("protocolVersion" -> Json.Num(BigDecimal(1)))
    else Json.Obj("sessionId" -> Json.Str("sess-1"))

  private def idOf(text: String): Option[Json] =
    text.fromJson[Json] match
      case Right(json) => JsonFields.obj(json).flatMap(obj => JsonFields.field(obj, "id"))
      case Left(_)     => None
end WsHarnessSpec
