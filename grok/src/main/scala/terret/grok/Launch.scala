package terret.grok

import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import zio.Chunk

final case class Argv(command: String, args: Chunk[String]):
  def rendered: Chunk[String] = command +: args

object Launch:
  def cli(
      config: GrokConfig,
      id: SessionId,
      prompt: Prompt,
      resume: Boolean,
      policy: PermissionPolicy,
      ceiling: Ceiling,
  ): Argv =
    val session = if resume then Chunk("-r", id.value) else Chunk("-s", id.value)
    Argv(
      config.command,
      Chunk("-p", prompt.text) ++ session ++ Chunk("--output-format", "streaming-json") ++ flags(
        config,
        policy,
        ceiling,
      ),
    )

  def stdio(config: GrokConfig, policy: PermissionPolicy, ceiling: Ceiling): Argv =
    Argv(config.command, Chunk("agent") ++ flags(config, policy, ceiling) ++ Chunk("stdio"))

  def flags(config: GrokConfig, policy: PermissionPolicy, ceiling: Ceiling): Chunk[String] =
    val always =
      if config.approval == GrokApproval.Always then Chunk("--always-approve") else Chunk.empty
    policy match
      case PermissionPolicy.Inherit => always
      case _                        =>
        // The CLI has no prompter. dontAsk refuses what was not granted instead of cancelling the turn.
        val mode =
          if config.approval != GrokApproval.Ask then Chunk.empty
          else if config.transport == GrokTransport.Cli then Chunk("--permission-mode", "dontAsk")
          else Chunk("--permission-mode", "default")
        val grants = allowFlags(config, ceiling)
        val tools  = denied(ceiling)
        val deny   =
          if tools.isEmpty then Chunk.empty else Chunk("--disallowed-tools", tools.mkString(","))
        always ++ grants ++ mode ++ deny

  def denied(ceiling: Ceiling): Chunk[String] =
    val names = gates.filterNot(gate => ceiling.allow.contains(gate.permission)).flatMap(_.deny) ++ ungoverned
    Chunk.fromIterable(names.distinct.sorted)

  private def allowFlags(config: GrokConfig, ceiling: Ceiling): Chunk[String] =
    if config.transport != GrokTransport.Cli || config.approval != GrokApproval.Ask then Chunk.empty
    else allows(ceiling).flatMap(rule => Chunk("--allow", rule))

  private def allows(ceiling: Ceiling): Chunk[String] =
    gates.filter(gate => ceiling.allow.contains(gate.permission)).flatMap(_.allow)

  // Ids and the names the model sees both have to be listed. Grok removes a tool by either spelling.
  private val commandTools = Chunk(
    "Agent",
    "get_command_or_subagent_output",
    "get_task_output",
    "kill_command_or_subagent",
    "kill_task",
    "monitor",
    "run_terminal_cmd",
    "run_terminal_command",
    "scheduler_create",
    "scheduler_delete",
    "scheduler_list",
    "spawn_subagent",
    "task",
    "wait_commands_or_subagents",
    "wait_tasks",
    "workflow",
  )

  private val networkTools = Chunk(
    "image_edit",
    "image_gen",
    "image_to_video",
    "reference_to_video",
    "web_fetch",
    "web_search",
  )

  // No permission covers MCP calls. Closed and Clamp remove them so always-approve cannot reach them.
  private val ungoverned = Chunk("search_tool", "use_tool")

  private val gates = Chunk(
    Gate(Permission.ReadWorkspace, Chunk("grep", "list_dir", "read_file"), Chunk.empty),
    Gate(Permission.WriteWorkspace, Chunk("search_replace", "write"), Chunk("Edit", "Write")),
    Gate(Permission.RunCommands, commandTools, Chunk("Bash")),
    Gate(Permission.Network, networkTools, Chunk("WebFetch", "WebSearch")),
  )

  private final case class Gate(permission: Permission, deny: Chunk[String], allow: Chunk[String])
end Launch
