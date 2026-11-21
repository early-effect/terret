package terret.grok

import terret.event.TurnOutcome
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.json.ast.Json

object Stop:
  def outcome(reason: String, category: Option[String] = None): TurnOutcome =
    if permission(category) then TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.PermissionDenied))
    else
      reason match
        case "cancelled" => TurnOutcome.Cancelled
        case "refusal"   => TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Refused))
        case _           => TurnOutcome.Completed

  def category(obj: Json.Obj): Option[String] =
    JsonFields.str(obj, "cancellationCategory").orElse(JsonFields.str(obj, "cancellation_category"))

  private def permission(category: Option[String]): Boolean =
    category.map(normalize) match
      case Some("permissioncancelled") | Some("permissionrejected") => true
      case _                                                        => false

  private def normalize(raw: String): String =
    raw.filter(ch => ch != '_' && ch != '-').toLowerCase
end Stop
