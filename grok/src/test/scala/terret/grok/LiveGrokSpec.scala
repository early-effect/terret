package terret.grok

import terret.event.{Event, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.workspace.Workspace
import zio.test.*
import zio.durationInt
import zio.{Scope, ZIO}

import java.nio.file.Files

object LiveGrokSpec extends ZIOSpecDefault:
  def spec = suite("live grok")(
    test("a headless prompt finishes a turn") {
      ZIO.scoped {
        for
          dir  <- ZIO.attemptBlocking(Files.createTempDirectory("terret-live"))
          root <- ZIO.fromEither(Workspace.from(dir.toString)).orDieWith(_ => new IllegalArgumentException("workspace"))
          harness <- GrokHarness.make(GrokConfig.cli, Spawner.jdk, dead)
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          events  <- harness.prompt(id, Prompt("Reply with the single word pong. Do not use tools.")).runCollect
        yield assertTrue(events.exists {
          case Event.TurnFinished(TurnOutcome.Completed) => true
          case _                                         => false
        })
      }
    } @@ TestAspect.timeout(2.minutes) @@ TestAspect.withLiveClock,
    test("a granted shell write finishes and the file exists") {
      val ceiling = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      ZIO.scoped {
        for
          dir  <- ZIO.attemptBlocking(Files.createTempDirectory("terret-live-write"))
          root <- ZIO.fromEither(Workspace.from(dir.toString)).orDieWith(_ => new IllegalArgumentException("workspace"))
          log    = dir.resolve("terret-test.log")
          config = GrokConfig.stdio
          harness <- GrokHarness.make(config, Spawner.jdk, AcpOpen.live(config, Spawner.jdk))
          id      <- harness.open(root, ceiling, PermissionPolicy.Clamp(ceiling))
          events  <- harness
            .prompt(id, Prompt(s"Run exactly this command and nothing else: echo hi | tee ${log.toString}"))
            .runCollect
          exists   <- ZIO.attemptBlocking(Files.exists(log))
          text     <- ZIO.attemptBlocking(if exists then Files.readString(log) else "")
          exported <- harness.transcript(id)
        yield assertTrue(
          events.exists {
            case Event.TurnFinished(TurnOutcome.Completed) => true
            case _                                         => false
          },
          exists,
          text.contains("hi"),
          exported.events.exists {
            case Event.ToolUpdated(call) => call.input.toString.contains("hi")
            case _                       => false
          },
        )
      }
    } @@ TestAspect.timeout(3.minutes) @@ TestAspect.withLiveClock,
  ) @@ TestAspect.ifEnvSet("TERRET_LIVE_GROK")

  private val dead: AcpOpen = new:
    def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe] =
      ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))
end LiveGrokSpec
