package terret.milestone

import terret.event.{Event, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.session.SessionId
import terret.transcript.Entry
import zio.{Chunk, Ref, UIO, ULayer, ZIO, ZLayer}

import java.time.Instant

/** What a progress notification can name. `Commit`, `TestResult`, and `WaitingOnPermission` are filter names. The
  * journal does not record those facts, so the classifier does not emit them.
  */
enum MilestoneKind:
  case TurnStarted
  case TurnFinished
  case ToolCall
  case Commit
  case TestResult
  case Error
  case WaitingOnPermission

/** A classified journal event: the kind and the short text a notification carries. */
final case class Notice(kind: MilestoneKind, summary: String)

/** One published milestone. `cursor` is the journal index, or nothing when the log is empty. */
final case class Milestone(
    session: String,
    kind: MilestoneKind,
    summary: String,
    cursor: Option[Long],
    at: Instant,
)

/** The classification export and the MCP event share. The match is pure. The service is the seam. */
trait Milestones:
  def classify(event: Event): UIO[Option[Notice]]
  def turnStarted: UIO[Notice]
  def failed(failure: HarnessFailure): UIO[Notice]

object Milestones:
  val SummaryLimit: Int = 160

  val live: ULayer[Milestones] = ZLayer.succeed(new:
    def classify(event: Event): UIO[Option[Notice]]  = ZIO.succeed(Milestones.classify(event))
    def turnStarted: UIO[Notice]                     = ZIO.succeed(Milestones.turnStarted)
    def failed(failure: HarnessFailure): UIO[Notice] = ZIO.succeed(Milestones.failed(failure)))

  def classify(event: Event): Option[Notice] =
    event match
      case Event.Assistant(_)                        => None
      case Event.Thought(_)                          => None
      case Event.Plan(_)                             => None
      case Event.ToolUpdated(_)                      => None
      case Event.ToolStarted(call)                   => Some(Notice(MilestoneKind.ToolCall, clip(call.name.value)))
      case Event.TurnFinished(TurnOutcome.Completed) => Some(Notice(MilestoneKind.TurnFinished, "completed"))
      case Event.TurnFinished(TurnOutcome.Cancelled) => Some(Notice(MilestoneKind.TurnFinished, "cancelled"))
      case Event.TurnFinished(TurnOutcome.Failed(failure)) => Some(Notice(MilestoneKind.Error, clip(label(failure))))

  def turnStarted: Notice = Notice(MilestoneKind.TurnStarted, "started")

  def failed(failure: HarnessFailure): Notice = Notice(MilestoneKind.Error, clip(label(failure)))

  def milestone(id: SessionId, notice: Notice, cursor: Option[Long], at: Instant): Milestone =
    Milestone(id.value, notice.kind, notice.summary, cursor, at)

  def label(failure: HarnessFailure): String =
    failure match
      case HarnessFailure.UnknownSession(_) => "unknown session"
      case HarnessFailure.Cancelled         => "cancelled"
      case HarnessFailure.Exited(code)      => s"exited $code"
      case HarnessFailure.Protocol(fault)   =>
        fault match
          case ProtocolFault.Malformed        => "malformed"
          case ProtocolFault.BindMissing      => "bind missing"
          case ProtocolFault.Rejected(code)   => s"rejected $code"
          case ProtocolFault.Incomplete       => "incomplete"
          case ProtocolFault.Refused          => "refused"
          case ProtocolFault.PermissionDenied => "permission denied"

  def clip(text: String): String =
    if text.length <= SummaryLimit then text else text.take(SummaryLimit)

  /** `latest` keeps the newest N of an already filtered selection. `None` keeps the selection. */
  def latest(entries: Chunk[Entry], n: Option[Int]): Chunk[Entry] =
    n.fold(entries)(entries.takeRight)
end Milestones

/** What the journal and `Terret` offer. `discard` drops them. The process binds a publisher later. */
trait MilestoneSink:
  def started(id: SessionId, cursor: Option[Long], epoch: Long): UIO[Unit]
  def appended(id: SessionId, entry: Entry): UIO[Unit]
  def failed(id: SessionId, failure: HarnessFailure, cursor: Option[Long], epoch: Long): UIO[Unit]

object MilestoneSink:
  val discard: MilestoneSink = new:
    def started(id: SessionId, cursor: Option[Long], epoch: Long): UIO[Unit]                         = ZIO.unit
    def appended(id: SessionId, entry: Entry): UIO[Unit]                                             = ZIO.unit
    def failed(id: SessionId, failure: HarnessFailure, cursor: Option[Long], epoch: Long): UIO[Unit] = ZIO.unit

/** One cell so the journal can offer before the MCP ring exists. `bind` swaps in the publisher. */
final class MilestoneGate private (cell: Ref[MilestoneSink]) extends MilestoneSink:
  def started(id: SessionId, cursor: Option[Long], epoch: Long): UIO[Unit] =
    cell.get.flatMap(_.started(id, cursor, epoch))

  def appended(id: SessionId, entry: Entry): UIO[Unit] =
    cell.get.flatMap(_.appended(id, entry))

  def failed(id: SessionId, failure: HarnessFailure, cursor: Option[Long], epoch: Long): UIO[Unit] =
    cell.get.flatMap(_.failed(id, failure, cursor, epoch))

  def bind(sink: MilestoneSink): UIO[Unit] = cell.set(sink)
end MilestoneGate

object MilestoneGate:
  def make: UIO[MilestoneGate] =
    Ref.make(MilestoneSink.discard: MilestoneSink).map(MilestoneGate(_))
