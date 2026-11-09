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
      Chunk("-p", prompt.text) ++ session ++ Chunk("--output-format", "streaming-json") ++ flags(config, policy, ceiling),
    )

  def stdio(config: GrokConfig, policy: PermissionPolicy, ceiling: Ceiling): Argv =
    Argv(config.command, Chunk("agent") ++ flags(config, policy, ceiling) ++ Chunk("stdio"))

  def flags(config: GrokConfig, policy: PermissionPolicy, ceiling: Ceiling): Chunk[String] =
    val always =
      if config.approval == GrokApproval.Always then Chunk("--always-approve") else Chunk.empty
    policy match
      case PermissionPolicy.Inherit => always
      case _                         =>
        val mode =
          if config.approval == GrokApproval.Ask then Chunk("--permission-mode", "default") else Chunk.empty
        val tools = denied(ceiling)
        val deny  =
          if tools.isEmpty then Chunk.empty else Chunk("--disallowed-tools", tools.mkString(","))
        always ++ mode ++ deny

  def denied(ceiling: Ceiling): Chunk[String] =
    val names =
      (if ceiling.allow.contains(Permission.ReadWorkspace) then Nil else List("grep", "list_dir", "read_file")) ++
        (if ceiling.allow.contains(Permission.WriteWorkspace) then Nil else List("search_replace", "write")) ++
        (if ceiling.allow.contains(Permission.RunCommands) then Nil else List("run_terminal_cmd")) ++
        (if ceiling.allow.contains(Permission.Network) then Nil else List("web_fetch", "web_search"))
    Chunk.fromIterable(names.distinct.sorted)
end Launch
