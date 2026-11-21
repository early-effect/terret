package terret.grok

import zio.test.*

object LiveGateSpec extends ZIOSpecDefault:
  private val loggedIn = Some((0, "You are logged in with grok.com.\n"))
  private val ready    = (_: String, arg: String) => if arg == "--version" then Some((0, "ok")) else loggedIn

  def spec = suite("LiveGate")(
    test("CI skips") {
      assertTrue(LiveGate.decide(Map("CI" -> "true"), ready) == LiveGate.Decision.Skip("CI"))
    },
    test("GITHUB_ACTIONS skips") {
      assertTrue(LiveGate.decide(Map("GITHUB_ACTIONS" -> "true"), ready) == LiveGate.Decision.Skip("CI"))
    },
    test("a missing binary skips") {
      assertTrue(LiveGate.decide(Map.empty, (_, _) => None) == LiveGate.Decision.Skip("grok is not on PATH"))
    },
    test("a version check is not an auth check") {
      val found = LiveGate.decide(Map.empty, (_, arg) => if arg == "--version" then Some((0, "ok")) else None)
      assertTrue(found == LiveGate.Decision.Skip("grok is not authenticated"))
    },
    test("a logged-in models listing is ready") {
      assertTrue(LiveGate.decide(Map.empty, ready) == LiveGate.Decision.Ready)
    },
  )
end LiveGateSpec
