package terret.event

import zio.Chunk

enum Event:
  case Assistant(text: String)
  case Thought(text: String)
  case ToolStarted(call: ToolCall)
  case ToolUpdated(call: ToolCall)
  case Plan(steps: Chunk[PlanStep])
  case TurnFinished(outcome: TurnOutcome)
end Event
