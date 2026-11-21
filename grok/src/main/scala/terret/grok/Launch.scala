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
    Argv(config.command, flags(config, policy, ceiling) ++ Chunk("agent", "stdio"))

  def flags(config: GrokConfig, policy: PermissionPolicy, ceiling: Ceiling): Chunk[String] =
    val always =
      if config.approval == GrokApproval.Always then Chunk("--always-approve") else Chunk.empty
    policy match
      case PermissionPolicy.Inherit => always
      case _                        =>
        val mode =
          if config.approval == GrokApproval.Ask then Chunk("--permission-mode", "default") else Chunk.empty
        val tools = denied(ceiling)
        val deny  =
          if tools.isEmpty then Chunk.empty else Chunk("--disallowed-tools", tools.mkString(","))
        always ++ mode ++ deny

  def denied(ceiling: Ceiling): Chunk[String] =
    val names = gates.filterNot(gate => ceiling.allow.contains(gate.permission)).flatMap(_.deny) ++ ungoverned
    Chunk.fromIterable(names.distinct.sorted)

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
    Gate(Permission.ReadWorkspace, Chunk("grep", "list_dir", "read_file")),
    Gate(Permission.WriteWorkspace, Chunk("search_replace", "write")),
    Gate(Permission.RunCommands, commandTools),
    Gate(Permission.Network, networkTools),
  )

  private final case class Gate(permission: Permission, deny: Chunk[String])
end Launch
