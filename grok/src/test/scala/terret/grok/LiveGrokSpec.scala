package terret.grok

import terret.event.{Event, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
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
          dir <- ZIO.attemptBlocking(Files.createTempDirectory("terret-live"))
          root <- ZIO.fromEither(Workspace.from(dir.toString)).orDieWith(_ => new IllegalArgumentException("workspace"))
          harness <- GrokHarness.make(GrokConfig.cli, Spawner.jdk, dead)
          id <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          events <- harness.prompt(id, Prompt("Reply with the single word pong. Do not use tools.")).runCollect
        yield assertTrue(events.exists {
          case Event.TurnFinished(TurnOutcome.Completed) => true
          case _                                         => false
        })
      }
    } @@ TestAspect.timeout(2.minutes) @@ TestAspect.withLiveClock,
  ) @@ TestAspect.ifEnvSet("TERRET_LIVE_GROK")

  private val dead: AcpOpen = new:
    def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe] =
      ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))
end LiveGrokSpec
