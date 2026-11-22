package heddle.mcp

import heddle.http.header.Headers
import heddle.mcp.events.{Events, Principal}
import zio.ZIO

import scala.util.NotGiven

/** `Mcp.principal` summons a given that is visible only in this package. The given is chosen for an abstract `R` unless
  * the method bound already says an event gateway exists.
  */
object InstallPrincipal:
  def apply[R, R1](mcp: Mcp[R])(read: Headers => ZIO[R1, Nothing, Option[Principal]])(using
      NotGiven[R <:< Events.Env]
  ): Mcp[R & R1] =
    mcp.principal(read)

  def events[R <: Events.Env, R1](mcp: Mcp[R])(
      read: Headers => ZIO[R1, Nothing, Option[Principal]]
  ): Mcp[R & R1] =
    mcp.principal(read)
end InstallPrincipal
