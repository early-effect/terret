package terret.app

import zio.{Chunk, Clock, Ref, UIO, ULayer, ZLayer}

import java.time.Instant

/** Why a tunnel request was refused. The name is what the log stores. */
enum AuthReason:
  case Rejected
  case Certs
  case Malformed
  case UnknownKid
  case UnknownSession

object AuthReason:
  def from(error: AccessError): AuthReason =
    error match
      case AccessError.Rejected   => Rejected
      case AccessError.Certs      => Certs
      case AccessError.Malformed  => Malformed
      case AccessError.UnknownKid => UnknownKid

/** One rejected request. The reason is the error name. The path is the request path. The token and any email stay out.
  */
final case class Denial(reason: String, path: String, at: Instant)

/** A bounded log of auth denials. Past the cap, the oldest entry drops. */
trait AuthLog:
  def record(reason: AuthReason, path: String): UIO[Unit]
  def recent: UIO[Chunk[Denial]]

object AuthLog:
  val Limit: Int = 256

  val live: ULayer[AuthLog] =
    ZLayer.scoped {
      Ref.make(Chunk.empty[Denial]).map { rows =>
        new:
          def record(reason: AuthReason, path: String): UIO[Unit] =
            Clock.instant.flatMap { at =>
              rows.update { current =>
                val next = current :+ Denial(reason.toString, path, at)
                if next.length > Limit then next.takeRight(Limit) else next
              }
            }

          def recent: UIO[Chunk[Denial]] = rows.get
      }
    }
end AuthLog
