package terret.grok

import slayer.*
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.model.ModelKey
import terret.permission.{Ceiling, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, Duration, Ref, Scope, ZIO, durationInt}

object HostSessionSpec extends ZIOSpecDefault:
  private val granted = NetworkPolicy(Set("example.com"))

  private val idle: Spawner = new:
    def spawn(command: String, args: Chunk[String], cwd: Workspace) =
      ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))

  def spec = suite("host allowlist session")(
    test("open waits for the allowlist and passes that policy to the agent") {
      driven(Duration.Zero)(ZIO.succeed(granted))
    },
    test("a slow allowlist is advanced by the test clock") {
      driven(5.seconds)(ZIO.sleep(5.seconds).as(granted))
    },
  ) @@ TestAspect.sequential @@ TestAspect.timeout(20.seconds)

  private val frames: Chunk[Json] =
    Chunk(
      Wire.ok(Json.Num(BigDecimal(1)), Json.Obj("protocolVersion" -> Json.Num(BigDecimal(1)))),
      Wire.ok(Json.Num(BigDecimal(2)), Json.Obj("sessionId" -> Json.Str("sess-1"))),
    )

  private def driven(waited: Duration)(loaded: ZIO[Any, Nothing, NetworkPolicy]) =
    ZIO.scoped {
      (for
        fiber <- opened(loaded).fork
        _     <- TestClock.adjust(waited)
        value <- fiber.join
      yield value).provideSome[Scope](stubbed[RpcPipe], stubbed[AcpOpen], stubbed[HostAllowlist])
    }

  private def opened(loaded: ZIO[Any, Nothing, NetworkPolicy]) =
    for
      pipe    <- ZIO.service[RpcPipe]
      acp     <- ZIO.service[AcpOpen]
      hosts   <- ZIO.service[HostAllowlist]
      pending <- Ref.make(frames)
      seen    <- Ref.make(NetworkPolicy.empty)
      _       <- stub[HostAllowlist](_.load)(loaded)
      _       <- stub[AcpOpen](
        _.open(
          any[Workspace],
          any[PermissionPolicy],
          any[Ceiling],
          any[McpPolicy],
          any[NetworkPolicy],
          any[Option[ModelKey]],
        )
      ) { (_, _, _, _, network, _) =>
        seen.set(network) *> ZIO.succeed(pipe)
      }
      _       <- stub[RpcPipe](_.read)(pull(pending))
      _       <- stub[RpcPipe](_.write(any[Json]))(_ => ZIO.unit)
      _       <- stub[RpcPipe](_.close)(ZIO.unit)
      harness <- GrokHarness.make(GrokConfig.stdio, idle, acp, hosts = hosts, sandbox = ShellSandbox.missing)
      root    <- ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))
      id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
      passed  <- seen.get
    yield assertTrue(id.value == "sess-1", passed == granted)

  private def pull(pending: Ref[Chunk[Json]]): ZIO[Any, HarnessFailure, Option[Json]] =
    pending.modify { waiting =>
      waiting.headOption match
        case Some(frame) => (ZIO.succeed(Some(frame)), waiting.drop(1))
        case None        => (ZIO.succeed(None), waiting)
    }.flatten
end HostSessionSpec
