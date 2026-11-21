package terret.grok

import terret.event.TurnOutcome
import terret.harness.{HarnessFailure, ProtocolFault}

object Stop:
  def outcome(reason: String, hostCancelled: Boolean = false): TurnOutcome =
    reason match
      case "cancelled" if hostCancelled => TurnOutcome.Cancelled
      case "cancelled"                  => TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.PermissionDenied))
      case "refusal"                    => TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Refused))
      case _                            => TurnOutcome.Completed
