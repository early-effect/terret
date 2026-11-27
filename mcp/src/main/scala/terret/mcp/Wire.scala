package terret.mcp

import heddle.Schema
import terret.activity.ActivityKind
import terret.event.{Event, TurnOutcome}
import terret.mcp.given
import terret.redact.Redact
import terret.error.TerretError
import terret.find.{CallError, EmbeddingProblem, FetchProblem, FindError}
import terret.github.{GhError, GhProblem}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.model.CatalogError
import terret.permission.Permission
import terret.session.SessionState
import terret.workspace.WorkspaceError
import zio.json.*
import zio.Chunk

given Schema[Permission]    = Schema.derived
given JsonCodec[Permission] = JsonCodec.derived

enum WorkspaceProblem derives Schema, JsonCodec:
  case Empty
  case Relative(raw: String)
  case Invalid(raw: String)

object WorkspaceProblem:
  def from(error: WorkspaceError): WorkspaceProblem =
    error match
      case WorkspaceError.Empty         => Empty
      case WorkspaceError.Relative(raw) => Relative(raw)
      case WorkspaceError.Invalid(raw)  => Invalid(raw)

enum WireFault derives Schema, JsonCodec:
  case Malformed
  case BindMissing
  case Rejected(code: Int)
  case Incomplete
  case Refused
  case PermissionDenied

object WireFault:
  def from(fault: ProtocolFault): WireFault =
    fault match
      case ProtocolFault.Malformed        => Malformed
      case ProtocolFault.BindMissing      => BindMissing
      case ProtocolFault.Rejected(code)   => Rejected(code)
      case ProtocolFault.Incomplete       => Incomplete
      case ProtocolFault.Refused          => Refused
      case ProtocolFault.PermissionDenied => PermissionDenied

enum WireFailure derives Schema, JsonCodec:
  case UnknownSession(id: String)
  case Cancelled
  case Exited(code: Int)
  case Protocol(fault: WireFault)

object WireFailure:
  def from(failure: HarnessFailure): WireFailure =
    failure match
      case HarnessFailure.UnknownSession(id) => UnknownSession(id.value)
      case HarnessFailure.Cancelled          => Cancelled
      case HarnessFailure.Exited(code)       => Exited(code)
      case HarnessFailure.Protocol(fault)    => Protocol(WireFault.from(fault))

enum WireOutcome derives Schema, JsonCodec:
  case Completed
  case Cancelled
  case Failed(failure: WireFailure)

object WireOutcome:
  def from(outcome: TurnOutcome): WireOutcome =
    outcome match
      case TurnOutcome.Completed     => Completed
      case TurnOutcome.Cancelled     => Cancelled
      case TurnOutcome.Failed(error) => Failed(WireFailure.from(error))

enum WireState derives Schema, JsonCodec:
  case Open
  case Running
  case Ended(outcome: WireOutcome)

object WireState:
  def from(state: SessionState): WireState =
    state match
      case SessionState.Open           => Open
      case SessionState.Running        => Running
      case SessionState.Ended(outcome) => Ended(WireOutcome.from(outcome))

enum WireEvent derives Schema, JsonCodec:
  case Assistant(text: String)
  case Thought(text: String)
  case ToolStarted(id: String, name: String, input: String)
  case ToolUpdated(id: String, name: String, input: String)
  case Plan(steps: Chunk[String])
  case Refused(call: String, name: String, reason: String)
  case TurnFinished(outcome: WireOutcome)
  case SteerQueued(actor: String, item: String, text: String, at: String)
  case SteerSent(actor: String, text: String, at: String)
  case TurnCancelled(actor: String, at: String)
  case SessionResumed(actor: String, at: String)
  case SessionOpened(actor: String, workspace: String, model: Option[String], at: String)
  case Activity(
      id: String,
      parent: String,
      kind: ActivityKind,
      call: Option[String],
      name: Option[String],
      status: Option[String],
      text: String,
      at: String,
  )

object WireEvent:
  def from(event: Event): WireEvent =
    event match
      case Event.Assistant(text)   => Assistant(Redact.text(text))
      case Event.Thought(text)     => Thought(Redact.text(text))
      case Event.ToolStarted(call) =>
        ToolStarted(call.id.value, Redact.text(call.name.value), Redact.text(call.input.toJson))
      case Event.ToolUpdated(call) =>
        ToolUpdated(call.id.value, Redact.text(call.name.value), Redact.text(call.input.toJson))
      case Event.Plan(steps)                 => Plan(steps.map(step => Redact.text(step.text)))
      case Event.Refused(call, name, reason) =>
        Refused(call, Redact.text(name), Redact.text(reason))
      case Event.TurnFinished(outcome) => TurnFinished(WireOutcome.from(outcome))
      case Event.SteerQueued(actor, item, text, at) =>
        SteerQueued(actor, item, Redact.text(text), at.toString)
      case Event.SteerSent(actor, text, at) => SteerSent(actor, Redact.text(text), at.toString)
      case Event.TurnCancelled(actor, at)   => TurnCancelled(actor, at.toString)
      case Event.SessionResumed(actor, at)  => SessionResumed(actor, at.toString)
      case Event.SessionOpened(actor, workspace, model, at) =>
        SessionOpened(actor, workspace, model, at.toString)

  def activity(event: terret.activity.ActivityEvent): WireEvent =
    Activity(
      event.id,
      event.parent,
      event.kind,
      event.call,
      event.name.map(Redact.text),
      event.status,
      Redact.text(event.text),
      event.at.toString,
    )
end WireEvent

enum WireGhProblem derives Schema, JsonCodec:
  case Limit(value: Int)
  case Number(value: Int)
  case Run(value: Long)
  case Title
  case Body
  case Branch(raw: String)
  case Detached
end WireGhProblem

enum JobFailure derives Schema, JsonCodec:
  case WorkspaceDenied(workspace: String, root: String)
  case WorkspaceInvalid(problem: WorkspaceProblem)
  case UnknownSession(id: String)
  case Busy(id: String)
  case EmptySession
  case BadCursor
  case BadLatest
  case Harness(failure: WireFailure)
  case UnknownModel(name: String, known: Chunk[String])
  case ModelsUnreadable(path: String)
  case ModelsMalformed(path: String, line: Int)
  case EmptyQuery
  case BadLimit
  case NotCallable
  case NotGranted(server: String, tool: String)
  case UnknownServer(server: String)
  case BadArguments
  case BadTool
  case ToolUnavailable(detail: String)
  case GhDenied(grant: String, op: String, message: String)
  case GhRejected(problem: WireGhProblem)
  case GhFailed(code: Int, detail: String)
  case GhUnavailable
  case GhTooLarge

object JobFailure:
  def from(error: TerretError): JobFailure =
    error match
      case TerretError.WorkspaceDenied(workspace, root) =>
        WorkspaceDenied(workspace.path.toString, root.path.toString)
      case TerretError.WorkspaceInvalid(problem) => WorkspaceInvalid(WorkspaceProblem.from(problem))
      case TerretError.UnknownSession(id)        => UnknownSession(id.value)
      case TerretError.Busy(id)                  => Busy(id.value)
      case TerretError.Harness(failure)          => Harness(WireFailure.from(failure))
      case TerretError.UnknownModel(name, known) => UnknownModel(name, known)
      case TerretError.Models(error)             =>
        error match
          case CatalogError.Unreadable(path)      => ModelsUnreadable(path)
          case CatalogError.Malformed(path, line) => ModelsMalformed(path, line)

  def fromFind(error: FindError): JobFailure =
    error match
      case FindError.EmptyQuery     => EmptyQuery
      case FindError.NonPositive(_) => BadLimit
      case other                    => ToolUnavailable(detail(other))

  def fromCall(error: CallError): JobFailure =
    error match
      case CallError.NotCallable              => NotCallable
      case CallError.NotGranted(server, tool) => NotGranted(server.value, tool.value)
      case CallError.UnknownServer(server)    => UnknownServer(server.value)
      case CallError.BadTool                  => BadTool
      case CallError.BadArguments             => BadArguments
      case CallError.Unavailable(detail)      => ToolUnavailable(detail)

  def fromGh(error: GhError): JobFailure =
    error match
      case GhError.NotGranted(grant, op) =>
        GhDenied(grant.name, op, s"$op needs a gh ${grant.name} grant in terret.gh")
      case GhError.BadRequest(problem) => GhRejected(wireProblem(problem))
      case GhError.Failed(code, detail) => GhFailed(code, detail)
      case GhError.Unavailable          => GhUnavailable
      case GhError.TooLarge             => GhTooLarge

  private def wireProblem(problem: GhProblem): WireGhProblem =
    problem match
      case GhProblem.Limit(value)  => WireGhProblem.Limit(value)
      case GhProblem.Number(value) => WireGhProblem.Number(value)
      case GhProblem.Run(value)    => WireGhProblem.Run(value)
      case GhProblem.Title         => WireGhProblem.Title
      case GhProblem.Body          => WireGhProblem.Body
      case GhProblem.Branch(raw)   => WireGhProblem.Branch(raw)
      case GhProblem.Detached      => WireGhProblem.Detached

  /** Diagnostic text. Callers do not branch on it. */
  private def detail(error: FindError): String =
    error match
      case FindError.EmptyQuery                 => "empty query"
      case FindError.HomeUnavailable            => "model home unavailable"
      case FindError.ModelMissing(paths)        => paths.map(_.toString).mkString(",")
      case FindError.DimMismatch(left, right)   => s"dims $left $right"
      case FindError.ModelMismatch(left, right) => s"models $left $right"
      case FindError.Degenerate                 => "degenerate vector"
      case FindError.NonPositive(limit)         => s"limit $limit"
      case FindError.Storage(text)              => text
      case FindError.Remote(text)               => text
      case FindError.Fetch(url, problem)        => s"$url ${fetch(problem)}"
      case FindError.Embedding(problem)         => embedding(problem)

  private def fetch(problem: FetchProblem): String =
    problem match
      case FetchProblem.Status(code)      => s"status $code"
      case FetchProblem.Unavailable(text) => text

  private def embedding(problem: EmbeddingProblem): String =
    problem match
      case EmbeddingProblem.Shape           => "shape"
      case EmbeddingProblem.Tokenizer(text) => text
      case EmbeddingProblem.Runtime(text)   => text
end JobFailure

final case class OpenJob(
    workspace: String,
    allow: Chunk[Permission],
    model: Option[String] = None,
) derives Schema,
      JsonCodec

final case class RunJob(session: String, prompt: String) derives Schema, JsonCodec

final case class SessionJob(session: String) derives Schema, JsonCodec

final case class ExportJob(
    session: String,
    after: Option[Long] = None,
    milestones: Option[Boolean] = None,
    latest: Option[Int] = None,
) derives Schema,
      JsonCodec

final case class ListedSession(session: String, state: WireState, workspace: String) derives Schema, JsonCodec

final case class Opened(
    session: String,
    effective: Chunk[Permission],
    dropped: Chunk[Permission],
    state: WireState,
    model: Option[String] = None,
) derives Schema,
      JsonCodec

final case class RunAccepted(session: String, state: WireState) derives Schema, JsonCodec

final case class StatusView(session: String, state: WireState) derives Schema, JsonCodec

final case class Log(
    session: String,
    events: Chunk[WireEvent],
    cursor: Option[Long],
    state: WireState,
) derives Schema,
      JsonCodec

final case class Stopped(session: String) derives Schema, JsonCodec

final case class FindJob(query: String, k: Int) derives Schema, JsonCodec

final case class CallJob(
    server: Option[String] = None,
    tool: String,
    arguments: String,
) derives Schema,
      JsonCodec

final case class FoundTool(server: String, tool: String, description: String) derives Schema, JsonCodec

enum WireEnable derives Schema, JsonCodec:
  case AddGrant(entry: String)
  case DefineServer(server: String)

final case class WithheldTool(server: String, tool: String, enable: WireEnable) derives Schema, JsonCodec

enum WireHint derives Schema, JsonCodec:
  case NoGrants

final case class Found(
    hits: Chunk[FoundTool],
    withheld: Chunk[WithheldTool],
    hint: Option[WireHint],
) derives Schema,
      JsonCodec

final case class Called(text: String) derives Schema, JsonCodec
