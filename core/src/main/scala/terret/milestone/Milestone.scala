package terret.milestone

import terret.event.{Event, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.session.SessionId
import terret.transcript.Entry
import zio.json.ast.Json
import zio.{Chunk, Ref, UIO, ULayer, ZIO, ZLayer}

import java.math.BigDecimal
import java.time.Instant

/** What Rex is told. A tool start is not a milestone, except `ask_user_question`, which waits on permission. A commit,
  * a push, or a pull-request state comes from finished tool output. A test line is activity, not a milestone. Streamed
  * text is not a milestone. `key` is stable for the same fact, so a repeated frame does not ping again.
  */
enum MilestoneKind:
  case TurnStarted
  case TurnFinished
  case Commit
  case Push
  case PrState
  case Error
  case WaitingOnPermission

/** A classified journal event: the kind, the short text a notification carries, and the dedupe key. */
final case class Notice(kind: MilestoneKind, summary: String, key: String)

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
  def classify(event: Event): UIO[Chunk[Notice]]
  def turnStarted: UIO[Notice]
  def failed(failure: HarnessFailure): UIO[Notice]

object Milestones:
  val SummaryLimit: Int = 160

  private val Ask = "ask_user_question"

  val live: ULayer[Milestones] = ZLayer.succeed(new:
    def classify(event: Event): UIO[Chunk[Notice]]   = ZIO.succeed(Milestones.classify(event))
    def turnStarted: UIO[Notice]                     = ZIO.succeed(Milestones.turnStarted)
    def failed(failure: HarnessFailure): UIO[Notice] = ZIO.succeed(Milestones.failed(failure)))

  def classify(event: Event): Chunk[Notice] =
    event match
      case Event.Assistant(_)                                => Chunk.empty
      case Event.Thought(_)                                  => Chunk.empty
      case Event.Plan(_)                                     => Chunk.empty
      case Event.ToolStarted(call) if call.name.value == Ask =>
        val summary = clip(Finished.question(call.input, call.name.value))
        Chunk(Notice(MilestoneKind.WaitingOnPermission, summary, s"wait:${call.id.value}"))
      case Event.ToolStarted(_)                      => Chunk.empty
      case Event.ToolUpdated(call)                   => Chunk.fromIterable(Finished.notices(call.input))
      case Event.TurnFinished(TurnOutcome.Completed) =>
        Chunk(Notice(MilestoneKind.TurnFinished, "completed", "turn-finished:completed"))
      case Event.TurnFinished(TurnOutcome.Cancelled) =>
        Chunk(Notice(MilestoneKind.TurnFinished, "cancelled", "turn-finished:cancelled"))
      case Event.TurnFinished(TurnOutcome.Failed(failure)) =>
        Chunk(Notice(MilestoneKind.Error, clip(label(failure)), "error"))

  def turnStarted: Notice = Notice(MilestoneKind.TurnStarted, "started", "turn-started")

  def failed(failure: HarnessFailure): Notice = Notice(MilestoneKind.Error, clip(label(failure)), "error")

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

  /** A finished shell result. Commit, push, and pull-request evidence sit on one object: a command and its output. */
  private object Finished:
    private val Pull = """https://github\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+/pull/[0-9]+""".r

    private enum Exit:
      case Missing
      case Code(value: Int)
      case Unusable

    private final case class Terminal(command: String, output: String, exit: Exit)

    def question(json: Json, fallback: String): String =
      json match
        case obj: Json.Obj =>
          string(obj, "questions") match
            case Some(Json.Arr(values)) =>
              values.headOption match
                case Some(item: Json.Obj) =>
                  text(item, "question") match
                    case Some(asked) if asked.nonEmpty => asked
                    case _                             => fallback
                case _ => fallback
            case _ => fallback
        case _ => fallback

    def notices(json: Json): List[Notice] =
      val found = terminals(json)
      found.filter(committed).map(commitOf) ++ found.filter(pushed).map(pushOf) ++ found.flatMap(prOf)

    private def commitOf(terminal: Terminal): Notice =
      val line = firstLine(terminal.output)
      Notice(MilestoneKind.Commit, clip(line), s"commit:${clip(line)}")

    private def pushOf(terminal: Terminal): Notice =
      val line = firstLine(terminal.output)
      Notice(MilestoneKind.Push, clip(line), s"push:${clip(line)}")

    private def prOf(terminal: Terminal): List[Notice] =
      if !succeeded(terminal.exit) || !segments(terminal.command).exists(isGhPr) then Nil
      else
        val state = prState(terminal.command, terminal.output)
        val urls  = pullUrls(terminal.output)
        if urls.nonEmpty then urls.map(url => prNote(state, url))
        else
          val line = firstLine(terminal.output)
          if line.isEmpty then Nil else List(prNote(state, line))

    private def prNote(state: String, body: String): Notice =
      Notice(MilestoneKind.PrState, clip(s"$state $body"), s"pr:${clip(body)}:$state")

    private def pullUrls(output: String): List[String] =
      Pull.findAllMatchIn(output).map(_.matched).toList.distinct

    private def terminals(json: Json): List[Terminal] =
      json match
        case obj: Json.Obj =>
          val nested = obj.fields.foldLeft(List.empty[Terminal]) { case (acc, (_, child)) =>
            acc ++ terminals(child)
          }
          terminal(obj).toList ++ nested
        case Json.Arr(values) =>
          values.foldLeft(List.empty[Terminal])((acc, value) => acc ++ terminals(value))
        case _ => Nil

    private def terminal(obj: Json.Obj): Option[Terminal] =
      val command = text(obj, "command").filter(_.nonEmpty)
      val output  = text(obj, "output").filter(_.nonEmpty).orElse(text(obj, "stdout").filter(_.nonEmpty))
      command.zip(output).map((found, printed) => Terminal(found, printed, exitOf(obj)))

    private def committed(terminal: Terminal): Boolean =
      succeeded(terminal.exit) && segments(terminal.command).exists(isCommit) && firstLine(terminal.output).nonEmpty

    private def pushed(terminal: Terminal): Boolean =
      succeeded(terminal.exit) && segments(terminal.command).exists(isPush) && firstLine(terminal.output).nonEmpty

    private def succeeded(exit: Exit): Boolean =
      exit match
        case Exit.Missing  => true
        case Exit.Code(0)  => true
        case Exit.Code(_)  => false
        case Exit.Unusable => false

    private def isCommit(words: List[String]): Boolean =
      words.dropWhile(assignment) match
        case program :: args if basename(program) == "git" =>
          after(args) match
            case "commit" :: tail => !tail.contains("--dry-run")
            case _                => false
        case _ => false

    private def isPush(words: List[String]): Boolean =
      words.dropWhile(assignment) match
        case program :: args if basename(program) == "git" =>
          after(args) match
            case "push" :: tail => !tail.contains("--dry-run") && !tail.contains("-n")
            case _              => false
        case _ => false

    private def isGhPr(words: List[String]): Boolean =
      words.dropWhile(assignment) match
        case program :: "pr" :: _ if basename(program) == "gh" => true
        case _                                                 => false

    private def prState(command: String, output: String): String =
      val named = segments(command).collectFirst {
        case words if isGhPr(words) =>
          words.dropWhile(assignment) match
            case _ :: "pr" :: "merge" :: _  => "merged"
            case _ :: "pr" :: "close" :: _  => "closed"
            case _ :: "pr" :: "reopen" :: _ => "open"
            case _ :: "pr" :: "ready" :: _  => "open"
            case _ :: "pr" :: "create" :: _ => "open"
            case _                          => viewed(output)
      }
      named.getOrElse(viewed(output))

    private def viewed(output: String): String =
      if output.contains("\"MERGED\"") || output.contains(" MERGED") then "merged"
      else if output.contains("\"CLOSED\"") || output.contains(" CLOSED") then "closed"
      else if output.contains("\"OPEN\"") || output.contains(" OPEN") then "open"
      else "seen"

    private def after(args: List[String]): List[String] =
      args match
        case "--" :: rest                         => rest
        case "-C" :: _ :: rest                    => after(rest)
        case "-c" :: _ :: rest                    => after(rest)
        case flag :: rest if flag.startsWith("-") => after(rest)
        case other                                => other

    private def assignment(token: String): Boolean =
      val split = token.indexOf('=')
      split > 0 && shellName(token.substring(0, split))

    private def shellName(raw: String): Boolean =
      raw.nonEmpty &&
        (raw.charAt(0).isLetter || raw.charAt(0) == '_') &&
        raw.forall(ch => ch.isLetterOrDigit || ch == '_')

    private def basename(program: String): String =
      val slash = program.lastIndexOf('/')
      if slash < 0 || slash == program.length - 1 then program else program.substring(slash + 1)

    private def segments(command: String): List[List[String]] =
      def scan: List[List[String]] =
        val tokens            = scala.collection.mutable.ListBuffer.empty[String]
        val parsed            = scala.collection.mutable.ListBuffer.empty[List[String]]
        val word              = new StringBuilder
        var index             = 0
        var quote             = Option.empty[Char]
        def flushWord(): Unit =
          if word.nonEmpty then
            tokens += word.toString
            word.clear()
        def flushSegment(): Unit =
          flushWord()
          if tokens.nonEmpty then
            parsed += tokens.toList
            tokens.clear()
        while index < command.length do
          val ch = command.charAt(index)
          quote match
            case Some(mark) =>
              if ch == mark then quote = None else word.append(ch)
              index += 1
            case None =>
              ch match
                case ' ' | '\t' | '\r' =>
                  flushWord()
                  index += 1
                case '\n' | ';' =>
                  flushSegment()
                  index += 1
                case '|' =>
                  flushSegment()
                  index += (if index + 1 < command.length && command.charAt(index + 1) == '|' then 2 else 1)
                case '&' =>
                  if index + 1 < command.length && command.charAt(index + 1) == '&' then
                    flushSegment()
                    index += 2
                  else
                    word.append(ch)
                    index += 1
                case '\'' | '"' =>
                  quote = Some(ch)
                  index += 1
                case _ =>
                  word.append(ch)
                  index += 1
        flushSegment()
        parsed.toList
      scan

    private def exitOf(obj: Json.Obj): Exit =
      obj.fields.collectFirst { case ("exit_code", value) => value } match
        case Some(Json.Num(number)) =>
          code(number) match
            case Some(value) => Exit.Code(value)
            case None        => Exit.Unusable
        case Some(_) => Exit.Unusable
        case None    => Exit.Missing

    private def code(number: BigDecimal): Option[Int] =
      val stripped = number.stripTrailingZeros
      if stripped.scale > 0 then None
      else
        val whole = stripped.toBigInteger
        if whole.bitLength < 32 then Some(whole.intValue) else None

    private def lines(output: String): List[String] =
      output.split("\n", -1).toList.map(_.trim).filter(_.nonEmpty)

    private def firstLine(output: String): String =
      lines(output).headOption.getOrElse("")

    private def text(obj: Json.Obj, name: String): Option[String] =
      obj.fields.collectFirst { case (key, Json.Str(value)) if key == name => value }

    private def string(obj: Json.Obj, name: String): Option[Json] =
      obj.fields.collectFirst { case (key, value) if key == name => value }
  end Finished
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
