package terret.permission

import zio.Chunk

/** One server a session may call. `tools` empty of a value means every tool. `None` is that. `Some` is only those tools.
  */
final case class ToolGrant(server: String, tools: Option[Set[String]])

/** `Unset` keeps the harness grant. `Chosen` is this session's grant, and an empty chunk refuses every server. */
enum SessionTools:
  case Unset
  case Chosen(grants: Chunk[ToolGrant])

object SessionTools:
  val refused: SessionTools = SessionTools.Chosen(Chunk.empty)
end SessionTools
