package terret.app.watch

import terret.activity.{Activity, ActivityEvent, ActivityKind, ActivityMemory}
import terret.event.Event
import terret.redact.Redact
import terret.transcript.Entry
import zio.Chunk

import java.nio.charset.StandardCharsets
import java.time.Instant

/** One row the page can place. `parent` is a turn id or a tool call id. Thought is a journal event, not an activity
  * kind.
  */
final case class WatchRow(
    id: String,
    parent: Option[String],
    kind: WatchKind,
    call: Option[String],
    tool: Option[String],
    status: Option[String],
    text: String,
    at: Instant,
    index: Long,
)

/** Shapes a journal page into the rows the watch view shows. This bound is the outbound window, not `journal.limit`. */
object WatchFeed:
  val MaxEntries: Int = 5000
  val MaxBytes: Int   = 8 * 1024 * 1024
  val TailChars: Int  = 64 * 1024
  val TailLines: Int  = 20

  def shape(entries: Chunk[Entry], query: WatchQuery): Chunk[WatchRow] =
    val projected = project(entries)
    val limited   = collapse(projected, query.output)
    val selected  = prune(withTurns(limited).filter(row => accept(query, row)))
    slide(selected)

  private def project(entries: Chunk[Entry]): Chunk[WatchRow] =
    entries
      .foldLeft((Chunk.empty[WatchRow], ActivityMemory.empty)) { case ((acc, memory), entry) =>
        entry.event match
          case Event.Thought(text) =>
            val row = WatchRow(
              id = s"${entry.index}:thought",
              parent = Some(Activity.turnId(entry.turn)),
              kind = WatchKind.Thought,
              call = None,
              tool = None,
              status = None,
              text = Redact.text(text),
              at = entry.at,
              index = entry.index,
            )
            (acc :+ row, memory)
          case other =>
            val (next, notes) = Activity.step(memory, other)
            val placed        = Activity.place(entry.index, entry.turn, entry.at, notes).map(rowOf(entry.index, _))
            (acc ++ placed, next)
      }
      ._1

  private def rowOf(index: Long, event: ActivityEvent): WatchRow =
    WatchRow(
      id = event.id,
      parent = Some(event.parent),
      kind = kindOf(event.kind),
      call = event.call,
      tool = event.name.map(Redact.text),
      status = event.status.map(Redact.text),
      text = Redact.text(event.text),
      at = event.at,
      index = index,
    )

  private def kindOf(kind: ActivityKind): WatchKind =
    kind match
      case ActivityKind.ToolStarted  => WatchKind.ToolStarted
      case ActivityKind.ToolFinished => WatchKind.ToolFinished
      case ActivityKind.Output       => WatchKind.Output
      case ActivityKind.TestLine     => WatchKind.TestLine
      case ActivityKind.Plan         => WatchKind.Plan
      case ActivityKind.Message      => WatchKind.Message
      case ActivityKind.Refused      => WatchKind.Refused

  private def withTurns(rows: Chunk[WatchRow]): Chunk[WatchRow] =
    val ids     = rows.map(_.id).toSet
    val missing = rows.flatMap(_.parent).distinct.filter(parent => parent.startsWith("turn:") && !ids.contains(parent))
    val turns   = missing.flatMap { id =>
      val kids  = rows.filter(_.parent.contains(id))
      val at    = kids.map(_.at).minOption.getOrElse(Instant.EPOCH)
      val index = kids.map(_.index).minOption.getOrElse(0L)
      Chunk(WatchRow(id, None, WatchKind.Turn, None, None, None, id, at, index))
    }
    turns ++ rows

  private def accept(query: WatchQuery, row: WatchRow): Boolean =
    val kindOk   = row.kind == WatchKind.Turn || query.kinds.contains(row.kind)
    val toolOk   = query.tool.forall(name => row.kind == WatchKind.Turn || row.tool.contains(name))
    val statusOk = query.status.forall(name => row.kind == WatchKind.Turn || row.status.contains(name))
    kindOk && toolOk && statusOk

  private def prune(rows: Chunk[WatchRow]): Chunk[WatchRow] =
    val wanted = rows.filter(_.kind != WatchKind.Turn).flatMap(_.parent).toSet
    rows.filter(row => row.kind != WatchKind.Turn || wanted.contains(row.id))

  private def collapse(rows: Chunk[WatchRow], size: OutputSize): Chunk[WatchRow] =
    if size == OutputSize.Full then rows
    else
      val grouped = rows.foldLeft(Map.empty[String, Chunk[WatchRow]]) { (acc, row) =>
        if row.kind != WatchKind.Output then acc
        else
          val key = row.call.getOrElse(row.id)
          acc.updated(key, acc.getOrElse(key, Chunk.empty) :+ row)
      }
      rows.flatMap { row =>
        if row.kind != WatchKind.Output then Chunk(row)
        else
          val key   = row.call.getOrElse(row.id)
          val group = grouped.getOrElse(key, Chunk(row))
          val last  = group.lastOption.fold(row.id)(_.id)
          if row.id != last then Chunk.empty
          else Chunk(row.copy(text = limit(size, group.map(_.text).mkString)))
      }

  private def limit(size: OutputSize, text: String): String =
    size match
      case OutputSize.Full  => text
      case OutputSize.Lines =>
        val lines = text.split("\n", -1)
        if lines.length <= TailLines then text else lines.takeRight(TailLines).mkString("\n")
      case OutputSize.Kb64 =>
        if text.length <= TailChars then text else text.substring(text.length - TailChars)

  /** Keeps the newest rows that fit in [[MaxEntries]] or [[MaxBytes]], whichever comes first. */
  def slide(rows: Chunk[WatchRow]): Chunk[WatchRow] =
    rows.reverse
      .foldLeft((Chunk.empty[WatchRow], 0, 0)) { case ((acc, count, bytes), row) =>
        val nextBytes = bytes + row.text.getBytes(StandardCharsets.UTF_8).length
        val nextCount = count + 1
        if nextCount > MaxEntries || nextBytes > MaxBytes then (acc, count, bytes)
        else (row +: acc, nextCount, nextBytes)
      }
      ._1
end WatchFeed
