package terret.event

import terret.harness.HarnessFailure

enum TurnOutcome:
  case Completed
  case Cancelled
  case Failed(failure: HarnessFailure)
end TurnOutcome
