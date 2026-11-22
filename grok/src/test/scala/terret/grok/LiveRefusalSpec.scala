package terret.grok

import zio.test.*

object LiveRefusalSpec extends ZIOSpecDefault:
  def spec =
    suite("live refusal cases")(
      test("a permission store key percent-encodes the workspace path") {
        assertTrue(
          LiveGrokSpec.encodeCwd("/Users/russ/projects/early-effect/terret") == Right(
            "%2FUsers%2Fruss%2Fprojects%2Fearly-effect%2Fterret"
          ),
          LiveGrokSpec.encodeCwd("/work/a b") == Right("%2Fwork%2Fa%20b"),
          LiveGrokSpec.encodeCwd("x" * 256).isLeft,
        )
      },
      suite("live grok refusals")(
        LiveGrokSpec.ungrantedWrite,
        LiveGrokSpec.heredocOutside,
      ).when(LiveGate.current == LiveGate.Decision.Ready)
        @@ TestAspect.beforeAll(LiveGrokSpec.prepare)
        @@ TestAspect.parallelN(LiveGrokSpec.parallelBound)
        @@ TestAspect.withLiveClock,
    ) @@ TestAspect.sequential
