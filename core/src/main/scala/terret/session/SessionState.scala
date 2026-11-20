package terret.session

import terret.event.TurnOutcome

/** `Ended` always carries the outcome. A turn with no outcome is still `Running`. */
enum SessionState:
  case Open
  case Running
  case Ended(outcome: TurnOutcome)
