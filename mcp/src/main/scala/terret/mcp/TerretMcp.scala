package terret.mcp

import heddle.endpoint.Api
import heddle.http.Response
import heddle.mcp.{Mcp, McpBuildError}
import heddle.route.Routes
import terret.Terret
import zio.NonEmptyChunk

object TerretMcp:
  val name    = "terret"
  val summary = "Sessions against a coding harness."

  def api(terret: Terret, version: String): Either[TerretMcpError, Api[Any]] =
    checked(version).map(Jobs.api(terret, _))

  def mcp(terret: Terret, version: String): Either[TerretMcpError, Mcp[Any]] =
    served(terret, version).map(_.mcp)

  /** MCP on `/mcp`, plus the API routes. HTTP serves both. Stdio serves `mcp` only. */
  def served(terret: Terret, version: String): Either[TerretMcpError, Served] =
    api(terret, version).flatMap { built =>
      Mcp.from(built).left.map(TerretMcpError.Rejected(_)).map { raw =>
        val mcp = raw.instructions(summary)
        Served(mcp, mcp.routes ++ built.routes)
      }
    }

  private def checked(version: String): Either[TerretMcpError, String] =
    if version.isEmpty then Left(TerretMcpError.EmptyVersion) else Right(version)
end TerretMcp

final case class Served(mcp: Mcp[Any], routes: Routes[Any, Response])

enum TerretMcpError:
  case EmptyVersion
  case Rejected(errors: NonEmptyChunk[McpBuildError])
