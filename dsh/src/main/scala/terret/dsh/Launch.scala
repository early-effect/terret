package terret.dsh

import terret.prompt.Prompt
import zio.Chunk

final case class Argv(command: String, args: Chunk[String]):
  def rendered: Chunk[String] = command +: args

object Launch:
  def argv(config: DshConfig, prompt: Prompt, remote: Option[String]): Argv =
    val session = remote match
      case Some(id) => Chunk("--session-id", id)
      case None     => Chunk.empty
    Argv(config.command, Chunk("--profile", config.profile, "--json") ++ session ++ Chunk(prompt.text))
end Launch
