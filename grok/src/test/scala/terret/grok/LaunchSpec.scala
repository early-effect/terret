package terret.grok

import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import zio.Chunk
import zio.test.*

object LaunchSpec extends ZIOSpecDefault:
  private val id     = SessionId("11111111-1111-1111-1111-111111111111")
  private val closed =
    "Agent,WebFetch,WebSearch,get_command_or_subagent_output,get_task_output,image_edit,image_gen,image_to_video,kill_command_or_subagent,kill_task,monitor,reference_to_video,run_terminal_cmd,run_terminal_command,scheduler_create,scheduler_delete,scheduler_list,search_replace,search_tool,spawn_subagent,task,use_tool,wait_commands_or_subagents,wait_tasks,web_fetch,web_search,workflow,write"
  private val commands = Ceiling(
    Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands)
  )

  def spec = suite("Launch")(
    test("a closed ask prompt passes default mode and the deny list") {
      val argv =
        Launch.cli(GrokConfig.cli, id, Prompt("hello"), resume = false, PermissionPolicy.Closed, Ceiling.closed)
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
          "--disable-web-search",
        ),
        !argv.args.contains("--allow"),
        !argv.args.contains("dontAsk"),
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
        !argv.args.contains("--disable-web-search"),
        !argv.args.contains("--allow"),
      )
    },
    test("always-approve is opt-in and still denies the shell by both names") {
      val config = GrokConfig.cli.withApproval(GrokApproval.Always)
      val argv   = Launch.cli(config, id, Prompt("hi"), resume = false, PermissionPolicy.Closed, Ceiling.closed)
      assertTrue(
        argv.args.contains("--always-approve"),
        argv.args.contains("--disable-web-search"),
        !argv.args.contains("--permission-mode"),
        !argv.args.contains("--allow"),
        argv.args.contains(closed),
        deny(argv).exists(names => names.contains("run_terminal_command") && names.contains("run_terminal_cmd")),
      )
    },
    test("a command ceiling keeps the shell and does not pre-approve it") {
      val argv =
        Launch.cli(GrokConfig.cli, id, Prompt("go"), resume = false, PermissionPolicy.Clamp(commands), commands)
      val names = deny(argv).getOrElse("")
      assertTrue(
        argv.rendered == Chunk(
          "grok",
          "-p",
          "go",
          "-s",
          id.value,
          "--output-format",
          "streaming-json",
          "--permission-mode",
          "default",
          "--disallowed-tools",
          "WebFetch,WebSearch,image_edit,image_gen,image_to_video,reference_to_video,search_tool,use_tool,web_fetch,web_search",
          "--disable-web-search",
        ),
        !argv.args.contains("--allow"),
        !argv.args.contains("dontAsk"),
        !names.contains("run_terminal_command"),
        !names.contains("search_replace"),
        !names.contains("write"),
      )
    },
    test("a read-only always session denies the real shell name") {
      val config = GrokConfig.cli.withApproval(GrokApproval.Always)
      val argv   = Launch.cli(
        config,
        id,
        Prompt("hi"),
        resume = false,
        PermissionPolicy.Clamp(Ceiling.closed),
        Ceiling.closed,
      )
      assertTrue(
        argv.args.contains("--always-approve"),
        deny(argv).exists(_.split(',').contains("run_terminal_command")),
      )
    },
    test("stdio ask keeps default mode and does not pre-approve") {
      val argv = Launch.stdio(GrokConfig.stdio, PermissionPolicy.Clamp(commands), commands)
      assertTrue(
        argv.args.contains("default"),
        !argv.args.contains("dontAsk"),
        !argv.args.contains("--allow"),
        deny(argv).exists(names => !names.contains("run_terminal_command") && names.contains("web_fetch")),
        argv.args.contains("--disable-web-search"),
      )
    },
    test("a clamp of every permission asks and denies only the ungoverned tools") {
      val argv = Launch.stdio(GrokConfig.stdio, PermissionPolicy.Clamp(Ceiling.all), Ceiling.all)
      assertTrue(
        argv.rendered == Chunk(
          "grok",
          "--permission-mode",
          "default",
          "--disallowed-tools",
          "search_tool,use_tool",
          "agent",
          "stdio",
        )
      )
    },
    test("stdio flags sit before the agent subcommand") {
      val argv = Launch.stdio(GrokConfig.stdio, PermissionPolicy.Closed, Ceiling.closed)
      assertTrue(
        argv.rendered == Chunk(
          "grok",
          "--permission-mode",
          "default",
          "--disallowed-tools",
          closed,
          "--disable-web-search",
          "agent",
          "stdio",
        )
      )
    },
    test("denied names stay sorted and name the shell the model sees") {
      val onlyNetwork = Ceiling(Set(Permission.Network))
      assertTrue(
        Launch.denied(onlyNetwork) == Chunk(
          "Agent",
          "get_command_or_subagent_output",
          "get_task_output",
          "grep",
          "kill_command_or_subagent",
          "kill_task",
          "list_dir",
          "monitor",
          "read_file",
          "run_terminal_cmd",
          "run_terminal_command",
          "scheduler_create",
          "scheduler_delete",
          "scheduler_list",
          "search_replace",
          "search_tool",
          "spawn_subagent",
          "task",
          "use_tool",
          "wait_commands_or_subagents",
          "wait_tasks",
          "workflow",
          "write",
        )
      )
    },
  )

  private def deny(argv: Argv): Option[String] =
    val rest = argv.args.dropWhile(_ != "--disallowed-tools").drop(1)
    rest.headOption
end LaunchSpec
