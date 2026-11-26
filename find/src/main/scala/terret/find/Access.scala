package terret.find

import zio.Chunk

/** One opted-in server. `tools` empty of a value means every tool on that server. `None` is that. `Some` is only those
  * tools.
  */
final case class Grant(server: ServerName, tools: Option[Set[ToolId]]):
  def allows(tool: ToolId): Boolean =
    tools match
      case None       => true
      case Some(kept) => kept.contains(tool)

/** The grant and the servers `.mcp.json` defines. An empty grant list grants nothing. */
final case class Access(grants: Chunk[Grant], defined: Set[ServerName]):
  def empty: Boolean = grants.isEmpty

  def allows(server: ServerName, tool: ToolId): Boolean =
    grants.find(_.server == server) match
      case Some(grant) => grant.allows(tool)
      case None        => false

  def defines(server: ServerName): Boolean = defined.contains(server)
end Access

object Access:
  val empty: Access = Access(Chunk.empty, Set.empty)
