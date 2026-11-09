package terret.dsh

import terret.event.{Event, TurnOutcome}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.workspace.Workspace
import zio.test.*
import zio.durationInt
import zio.ZIO

import java.nio.file.Files

object LiveDshSpec extends ZIOSpecDefault:
  def spec = suite("live dsh")(
    test("a headless prompt finishes a turn") {
      ZIO.scoped {
        for
          dir <- ZIO.attemptBlocking(Files.createTempDirectory("terret-live-dsh"))
          root <- ZIO.fromEither(Workspace.from(dir.toString)).orDieWith(_ => new IllegalArgumentException("workspace"))
          harness <- DshHarness.make(DshConfig.headless, Spawner.jdk)
          id <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          events <- harness.prompt(id, Prompt("Reply with the single word pong. Do not use tools.")).runCollect
        yield assertTrue(events.exists {
          case Event.TurnFinished(TurnOutcome.Completed) => true
          case _                                         => false
        })
      }
    } @@ TestAspect.timeout(2.minutes) @@ TestAspect.withLiveClock,
  ) @@ TestAspect.ifEnvSet("TERRET_LIVE_DSH")
end LiveDshSpec
