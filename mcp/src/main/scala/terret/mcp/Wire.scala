package terret.mcp

import heddle.Schema
import terret.event.{Event, TurnOutcome}
import terret.error.TerretError
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
  case TurnFinished(outcome: WireOutcome)

object WireEvent:
  def from(event: Event): WireEvent =
    event match
      case Event.Assistant(text)       => Assistant(text)
      case Event.Thought(text)         => Thought(text)
      case Event.ToolStarted(call)     => ToolStarted(call.id.value, call.name.value, call.input.toJson)
      case Event.ToolUpdated(call)     => ToolUpdated(call.id.value, call.name.value, call.input.toJson)
      case Event.Plan(steps)           => Plan(steps.map(_.text))
      case Event.TurnFinished(outcome) => TurnFinished(WireOutcome.from(outcome))

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
