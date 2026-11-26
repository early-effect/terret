package terret.grok

import terret.model.ModelKey
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
      mcp: McpPolicy = McpPolicy.empty,
      network: NetworkPolicy = NetworkPolicy.empty,
      model: Option[ModelKey] = None,
  ): Argv =
    val session = if resume then Chunk("-r", id.value) else Chunk("-s", id.value)
    Argv(
      config.command,
      Chunk("-p", prompt.text) ++ session ++ Chunk("--output-format", "streaming-json") ++ modelArg(model) ++ flags(
        config,
        policy,
        ceiling,
        mcp,
        network,
      ),
    )

  def stdio(
      config: GrokConfig,
      policy: PermissionPolicy,
      ceiling: Ceiling,
      mcp: McpPolicy = McpPolicy.empty,
      network: NetworkPolicy = NetworkPolicy.empty,
      model: Option[ModelKey] = None,
  ): Argv =
    Argv(
      config.command,
      modelArg(model) ++ flags(config, policy, ceiling, mcp, network) ++ Chunk("agent", "stdio"),
    )

  private def modelArg(model: Option[ModelKey]): Chunk[String] =
    model match
      case Some(key) => Chunk("--model", key.value)
      case None      => Chunk.empty

  def flags(
      config: GrokConfig,
      policy: PermissionPolicy,
      ceiling: Ceiling,
      mcp: McpPolicy = McpPolicy.empty,
      network: NetworkPolicy = NetworkPolicy.empty,
  ): Chunk[String] =
    val always =
      if config.approval == GrokApproval.Always then Chunk("--always-approve") else Chunk.empty
    policy match
      case PermissionPolicy.Inherit => always
      case _                        =>
        val mode =
          if config.approval == GrokApproval.Ask then Chunk("--permission-mode", "default") else Chunk.empty
        val tools = denied(ceiling, mcp, network)
        val deny  =
          if tools.isEmpty then Chunk.empty else Chunk("--disallowed-tools", tools.mkString(","))
        val search =
          if networkOpen(ceiling, network) then Chunk.empty else Chunk("--disable-web-search")
        always ++ mode ++ deny ++ search

  // Network tools stay denied until a host is granted, so Always cannot reach them with an empty allowlist.
  def denied(
      ceiling: Ceiling,
      mcp: McpPolicy = McpPolicy.empty,
      network: NetworkPolicy = NetworkPolicy.empty,
  ): Chunk[String] =
    val names = gates.filterNot(gate => open(ceiling, network, gate)).flatMap(_.deny) ++ ungoverned(mcp)
    Chunk.fromIterable(names.distinct.sorted)

  private def open(ceiling: Ceiling, network: NetworkPolicy, gate: Gate): Boolean =
    ceiling.allow.contains(gate.permission) && (gate.permission != Permission.Network || network.hosts.nonEmpty)

  private def networkOpen(ceiling: Ceiling, network: NetworkPolicy): Boolean =
    ceiling.allow.contains(Permission.Network) && network.hosts.nonEmpty

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
    "WebFetch",
    "WebSearch",
    "image_edit",
    "image_gen",
    "image_to_video",
    "reference_to_video",
    "web_fetch",
    "web_search",
  )

  // search_tool and use_tool stay denied until a grant exists, so Always cannot reach MCP with an empty policy.
  private def ungoverned(mcp: McpPolicy): Chunk[String] =
    if mcp.grants.isEmpty then Chunk("search_tool", "use_tool") else Chunk.empty

  private val gates = Chunk(
    Gate(Permission.ReadWorkspace, Chunk("grep", "list_dir", "read_file")),
    Gate(Permission.WriteWorkspace, Chunk("search_replace", "write")),
    Gate(Permission.RunCommands, commandTools),
    Gate(Permission.Network, networkTools),
  )

  private final case class Gate(permission: Permission, deny: Chunk[String])
end Launch
