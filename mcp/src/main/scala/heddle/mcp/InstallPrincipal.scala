package heddle.mcp

import heddle.http.header.Headers
import heddle.mcp.events.Principal
import zio.ZIO

/** `Mcp.principal` summons a given that is visible only in this package. */
object InstallPrincipal:
  def apply[R, R1](mcp: Mcp[R])(read: Headers => ZIO[R1, Nothing, Option[Principal]]): Mcp[R & R1] =
    mcp.principal(read)
