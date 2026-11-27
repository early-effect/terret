package terret.milestone

import terret.event.{Event, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.session.SessionId
import terret.transcript.Entry
import zio.json.ast.Json
import zio.{Chunk, Ref, UIO, ULayer, ZIO, ZLayer}

import java.math.BigDecimal
import java.time.Instant

/** What a progress notification can name. A tool start is a tool call, except `ask_user_question`, which waits on
  * permission. A commit or a test result comes from finished tool output: `git commit` that exited 0, or a test command
  * whose output reports tests passed or failed. Streamed text is not a milestone.
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

  private val Ask = "ask_user_question"

  val live: ULayer[Milestones] = ZLayer.succeed(new:
    def classify(event: Event): UIO[Option[Notice]]  = ZIO.succeed(Milestones.classify(event))
    def turnStarted: UIO[Notice]                     = ZIO.succeed(Milestones.turnStarted)
    def failed(failure: HarnessFailure): UIO[Notice] = ZIO.succeed(Milestones.failed(failure)))

  def classify(event: Event): Option[Notice] =
    event match
      case Event.Assistant(_)                                => None
      case Event.Thought(_)                                  => None
      case Event.Plan(_)                                     => None
      case Event.ToolStarted(call) if call.name.value == Ask =>
        Some(Notice(MilestoneKind.WaitingOnPermission, clip(Finished.question(call.input, call.name.value))))
      case Event.ToolStarted(call)                   => Some(Notice(MilestoneKind.ToolCall, clip(call.name.value)))
      case Event.ToolUpdated(call)                   => Finished.notice(call.input)
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

  /** A finished shell result. Commit and test evidence have to sit on one object: a command and its output. */
  private object Finished:
    private val Runners = Set("histos", "sbt", "mill", "cargo", "npm", "pnpm", "yarn", "go")

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

    def notice(json: Json): Option[Notice] =
      val found = terminals(json)
      found.find(committed) match
        case Some(terminal) => Some(Notice(MilestoneKind.Commit, clip(firstLine(terminal.output))))
        case None           =>
          val reports = found.flatMap { terminal =>
            if tested(terminal) then report(terminal.output) else None
          }
          reports.lastOption.map(line => Notice(MilestoneKind.TestResult, clip(line)))

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

    private def tested(terminal: Terminal): Boolean =
      segments(terminal.command).exists(isTest)

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

    private def isTest(words: List[String]): Boolean =
      words.dropWhile(assignment) match
        case program :: args =>
          val base = basename(program)
          base == "pytest" || (Runners.contains(base) && positional(args).contains("test"))
        case Nil => false

    private def after(args: List[String]): List[String] =
      args match
        case "--" :: rest                         => rest
        case "-C" :: _ :: rest                    => after(rest)
        case "-c" :: _ :: rest                    => after(rest)
        case flag :: rest if flag.startsWith("-") => after(rest)
        case other                                => other

    private def positional(args: List[String]): Option[String] =
      args match
        case "--" :: name :: _                    => Some(name)
        case flag :: rest if flag.startsWith("-") => positional(rest)
        case name :: _                            => Some(name)
        case Nil                                  => None

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

    private def report(output: String): Option[String] =
      lines(output).filter(line => line.contains("tests passed") || line.contains("tests failed")).lastOption

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
