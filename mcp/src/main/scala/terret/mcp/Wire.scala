package terret.mcp

import heddle.Schema
import terret.event.{Event, TurnOutcome}
import terret.error.TerretError
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.Permission
import terret.session.SessionStatus
import terret.workspace.WorkspaceError
import zio.json.*
import zio.Chunk

given Schema[Permission]       = Schema.derived
given JsonCodec[Permission]    = JsonCodec.derived
given Schema[SessionStatus]    = Schema.derived
given JsonCodec[SessionStatus] = JsonCodec.derived

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
end WorkspaceProblem

enum WireFault derives Schema, JsonCodec:
  case Malformed
  case BindMissing
  case Rejected(code: Int)
  case Incomplete
  case Refused

object WireFault:
  def from(fault: ProtocolFault): WireFault =
    fault match
      case ProtocolFault.Malformed    => Malformed
      case ProtocolFault.BindMissing  => BindMissing
      case ProtocolFault.Rejected(code) => Rejected(code)
      case ProtocolFault.Incomplete   => Incomplete
      case ProtocolFault.Refused      => Refused
end WireFault

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
end WireFailure

enum WireOutcome derives Schema, JsonCodec:
  case Completed
  case Cancelled
  case Failed(failure: WireFailure)

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
      case Event.Assistant(text)     => Assistant(text)
      case Event.Thought(text)       => Thought(text)
      case Event.ToolStarted(call)   => ToolStarted(call.id.value, call.name.value, call.input.toJson)
      case Event.ToolUpdated(call)   => ToolUpdated(call.id.value, call.name.value, call.input.toJson)
      case Event.Plan(steps)         => Plan(steps.map(_.text))
      case Event.TurnFinished(outcome) =>
        outcome match
          case TurnOutcome.Completed     => TurnFinished(WireOutcome.Completed)
          case TurnOutcome.Cancelled     => TurnFinished(WireOutcome.Cancelled)
          case TurnOutcome.Failed(error) => TurnFinished(WireOutcome.Failed(WireFailure.from(error)))
end WireEvent

enum JobFailure derives Schema, JsonCodec:
  case WorkspaceDenied(workspace: String, root: String)
  case WorkspaceInvalid(problem: WorkspaceProblem)
  case UnknownSession(id: String)
  case Busy(id: String)
  case EmptySession
  case Unfinished(session: String)
  case Harness(failure: WireFailure)

object JobFailure:
  def from(error: TerretError): JobFailure =
    error match
      case TerretError.WorkspaceDenied(workspace, root) =>
        WorkspaceDenied(workspace.path.toString, root.path.toString)
      case TerretError.WorkspaceInvalid(problem) => WorkspaceInvalid(WorkspaceProblem.from(problem))
      case TerretError.UnknownSession(id)        => UnknownSession(id.value)
      case TerretError.Busy(id)                  => Busy(id.value)
      case TerretError.Harness(failure)          => Harness(WireFailure.from(failure))
end JobFailure

final case class OpenJob(workspace: String, allow: Chunk[Permission]) derives Schema, JsonCodec

final case class RunJob(session: String, prompt: String) derives Schema, JsonCodec

final case class SessionJob(session: String) derives Schema, JsonCodec

final case class Opened(
    session: String,
    effective: Chunk[Permission],
    dropped: Chunk[Permission],
    status: SessionStatus,
) derives Schema, JsonCodec

final case class RunDone(session: String, outcome: WireOutcome) derives Schema, JsonCodec

final case class Log(session: String, events: Chunk[WireEvent]) derives Schema, JsonCodec

final case class Stopped(session: String) derives Schema, JsonCodec
