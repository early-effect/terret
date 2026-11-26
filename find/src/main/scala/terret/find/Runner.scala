package terret.find

import zio.{IO, ZIO}

/** Calls one tool on one server. The live layer speaks MCP `tools/call`. Tests stub this. */
trait Runner:
  def call(server: ServerName, tool: ToolId, arguments: String): IO[CallError, String]

object Runner:
  def call(server: ServerName, tool: ToolId, arguments: String): ZIO[Runner, CallError, String] =
    ZIO.serviceWithZIO(_.call(server, tool, arguments))
