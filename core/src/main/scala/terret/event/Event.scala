package terret.event

import zio.Chunk

import java.time.Instant

enum Event:
  case Assistant(text: String)
  case Thought(text: String)
  case ToolStarted(call: ToolCall)
  case ToolUpdated(call: ToolCall)
  case Plan(steps: Chunk[PlanStep])
  case Refused(callId: String, name: String, reason: String)
  case TurnFinished(outcome: TurnOutcome)
  case SteerQueued(actor: String, item: String, text: String, at: Instant)
  case SteerSent(actor: String, text: String, at: Instant)
  case TurnCancelled(actor: String, at: Instant)
  case SessionResumed(actor: String, at: Instant)
  case SessionOpened(actor: String, workspace: String, model: Option[String], at: Instant)
end Event
