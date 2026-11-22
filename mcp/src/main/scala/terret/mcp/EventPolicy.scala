package terret.mcp

import heddle.mcp.events.{TtlPolicy, VerificationPolicy}
import zio.Duration
import zio.durationInt

import scala.util.Try

final case class EventPolicy(
    verification: VerificationPolicy,
    floor: Duration,
    default: Duration,
    cap: Duration,
):
  def ttl: TtlPolicy =
    TtlPolicy.default.copy(floor = floor, default = default, cap = cap)

object EventPolicy:
  val terret: EventPolicy =
    EventPolicy(VerificationPolicy.Skip, 1.minute, 2.hours, 24.hours)

  def name(policy: VerificationPolicy): String =
    policy match
      case VerificationPolicy.Skip     => "Skip"
      case VerificationPolicy.Required => "Required"

  def from(
      verification: String,
      floor: Duration,
      default: Duration,
      cap: Duration,
  ): Either[EventPolicyError, EventPolicy] =
    for
      chosen <- verificationOf(verification)
      low    <- millisOf(floor, "floor")
      mid    <- millisOf(default, "default")
      high   <- millisOf(cap, "cap")
      _      <- order(low, mid, high)
    yield EventPolicy(chosen, floor, default, cap)

  private def verificationOf(name: String): Either[EventPolicyError, VerificationPolicy] =
    name match
      case "Skip"     => Right(VerificationPolicy.Skip)
      case "Required" => Right(VerificationPolicy.Required)
      case other      => Left(EventPolicyError.Verification(other))

  private def millisOf(duration: Duration, name: String): Either[EventPolicyError, Long] =
    positiveMillis(duration).toRight(EventPolicyError.Duration(name))

  private def positiveMillis(duration: Duration): Option[Long] =
    Try(duration.toMillis).toOption.filter(_ > 0L)

  private def order(floor: Long, default: Long, cap: Long): Either[EventPolicyError, Unit] =
    if floor <= default && default <= cap then Right(())
    else Left(EventPolicyError.Order)
end EventPolicy

enum EventPolicyError:
  case Verification(value: String)
  case Duration(name: String)
  case Order

  def render: String = this match
    case Verification(value) => s"events.verification is Skip or Required, got $value"
    case Duration(name)      => s"events.$name must be greater than zero"
    case Order               => "events.floor, events.default, and events.cap must be in order"
end EventPolicyError
