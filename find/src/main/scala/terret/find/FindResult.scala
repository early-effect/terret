package terret.find

import zio.Chunk

/** What a search returns after the guard.
  *
  * `hits` are the tools a call would be allowed to run. `withheld` names ungranted tools that outrank the last hit,
  * without their descriptions. `hint` is set when the grant list is empty, and then `hits` is empty.
  */
final case class FindResult(hits: Chunk[ToolRef], withheld: Chunk[Withheld], hint: Option[FindHint])

object FindResult:
  val empty: FindResult    = FindResult(Chunk.empty, Chunk.empty, None)
  val noGrants: FindResult = FindResult(Chunk.empty, Chunk.empty, Some(FindHint.NoGrants))

enum FindHint:
  /** `terret.mcp` is empty. Search does not fail. */
  case NoGrants

/** A tool the guard removed, and the one change that would let it through. */
final case class Withheld(server: ServerName, tool: ToolId, enable: Enable)

object Withheld:
  /** How many withheld tools a result keeps. */
  val Max: Int = 3

  def from(ref: ToolRef, error: CallError): Option[Withheld] =
    error match
      case CallError.NotGranted(server, tool) =>
        Some(Withheld(server, tool, Enable.AddGrant(s"${server.value}/${tool.value}")))
      case CallError.UnknownServer(server) =>
        Some(Withheld(server, ref.tool, Enable.DefineServer(server)))
      case CallError.NotCallable | CallError.BadTool | CallError.BadArguments | CallError.Unavailable(_) =>
        None
end Withheld

enum Enable:
  /** The `server/tool` entry to add to `terret.mcp`. */
  case AddGrant(entry: String)

  /** The server is granted. `.mcp.json` does not define it. */
  case DefineServer(server: ServerName)
end Enable

/** Rank, drop what the guard refuses, then keep `k` hits. Withheld tools are those that outrank the last hit. */
object Selection:
  def apply(ranked: Chunk[ToolRef], k: Int, access: Access): FindResult =
    val allowed = ranked.filter(ref => Guard(ref, access).isRight)
    val hits    = allowed.take(k)
    val cutoff  = hits.lastOption.fold(0)(last =>
      val at = ranked.indexWhere(ref => ref.server == last.server && ref.tool == last.tool)
      if at < 0 then 0 else at
    )
    val withheld = ranked
      .take(cutoff)
      .flatMap: ref =>
        Guard(ref, access) match
          case Left(error) => Withheld.from(ref, error)
          case Right(_)    => None
      .take(Withheld.Max)
    FindResult(hits, withheld, None)
