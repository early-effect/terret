package terret.event

import zio.Chunk

enum Event:
  case Assistant(text: String)
  case Thought(text: String)
  case ToolStarted(call: ToolCall)
  case ToolUpdated(call: ToolCall)
  case Plan(steps: Chunk[PlanStep])
  case Refused(callId: String, name: String, reason: String)
  case TurnFinished(outcome: TurnOutcome)
end Event
