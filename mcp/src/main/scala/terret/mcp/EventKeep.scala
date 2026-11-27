package terret.mcp

import heddle.mcp.events.Events
import zio.{Duration, Schedule, Scope, ZIO}

import scala.util.Try

/** Keeps a live webhook from lapsing while its deliveries are still succeeding. */
object EventKeep:
  /** Half the shortest grant. A floor TTL is renewed before `refreshBefore`, and a non-positive floor stays one
    * millisecond.
    */
  def pace(floor: Duration): Duration =
    val millis = positive(floor)
    val half   = millis / 2L
    Duration.fromMillis(if half > 0L then half else millis)

  /** Move `refreshBefore` out to `by` on `pace`. A URL that has failed a delivery is left to expire. */
  def renewed(by: Duration, pace: Duration): ZIO[Events & Scope, Nothing, Unit] =
    Events.renewHealthy(by).ignore.repeat(Schedule.spaced(pace)).forkScoped.unit

  private def positive(duration: Duration): Long =
    Try(duration.toMillis).toOption.filter(_ > 0L).getOrElse(1L)
end EventKeep
