package terret.grok

import terret.event.TurnOutcome
import terret.harness.{HarnessFailure, ProtocolFault}

object Stop:
  def outcome(reason: String): TurnOutcome =
    reason match
      case "cancelled" => TurnOutcome.Cancelled
      case "refusal"   => TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Refused))
      case _           => TurnOutcome.Completed
end Stop
