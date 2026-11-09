package terret.grok

import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import zio.Chunk
import zio.test.*

object LaunchSpec extends ZIOSpecDefault:
  private val id     = SessionId("11111111-1111-1111-1111-111111111111")
  private val closed = "run_terminal_cmd,search_replace,web_fetch,web_search,write"

  def spec = suite("Launch")(
    test("a closed ask prompt passes the session id, streaming json, and the deny list") {
      val argv = Launch.cli(GrokConfig.cli, id, Prompt("hello"), resume = false, PermissionPolicy.Closed, Ceiling.closed)
      assertTrue(
        argv.rendered == Chunk(
          "grok",
          "-p",
          "hello",
          "-s",
          id.value,
          "--output-format",
          "streaming-json",
          "--permission-mode",
          "default",
          "--disallowed-tools",
          closed,
        ),
      )
    },
    test("a later prompt resumes that session") {
      val argv = Launch.cli(GrokConfig.cli, id, Prompt("next"), resume = true, PermissionPolicy.Closed, Ceiling.closed)
      assertTrue(argv.args.contains("-r"), !argv.args.contains("-s"))
    },
    test("inherit ask passes no permission flags") {
      val argv = Launch.cli(GrokConfig.cli, id, Prompt("hi"), resume = false, PermissionPolicy.Inherit, Ceiling.all)
      assertTrue(
        !argv.args.contains("--permission-mode"),
        !argv.args.contains("--disallowed-tools"),
        !argv.args.contains("--always-approve"),
      )
    },
    test("always-approve is opt-in and still denies from the ceiling") {
      val config = GrokConfig.cli.withApproval(GrokApproval.Always)
      val argv   = Launch.cli(config, id, Prompt("hi"), resume = false, PermissionPolicy.Closed, Ceiling.closed)
      assertTrue(
        argv.args.contains("--always-approve"),
        !argv.args.contains("--permission-mode"),
        argv.args.contains(closed),
      )
    },
    test("a clamp of every permission asks and denies nothing") {
      val argv = Launch.stdio(GrokConfig.stdio, PermissionPolicy.Clamp(Ceiling.all), Ceiling.all)
      assertTrue(
        argv.rendered == Chunk("grok", "agent", "--permission-mode", "default", "stdio"),
      )
    },
    test("stdio flags sit between agent and the mode") {
      val argv = Launch.stdio(GrokConfig.stdio, PermissionPolicy.Closed, Ceiling.closed)
      assertTrue(
        argv.rendered == Chunk(
          "grok",
          "agent",
          "--permission-mode",
          "default",
          "--disallowed-tools",
          closed,
          "stdio",
        ),
      )
    },
    test("denied names stay sorted") {
      val onlyNetwork = Ceiling(Set(Permission.Network))
      assertTrue(
        Launch.denied(onlyNetwork) == Chunk(
          "grep",
          "list_dir",
          "read_file",
          "run_terminal_cmd",
          "search_replace",
          "write",
        ),
      )
    },
  )
end LaunchSpec
