package terret.mcp

import heddle.endpoint.Api
import heddle.mcp.{Mcp, McpBuildError}
import terret.Terret
import zio.NonEmptyChunk

object TerretMcp:
  val name    = "terret"
  val summary = "Sessions against a coding harness."

  def api(terret: Terret, version: String): Either[TerretMcpError, Api[Any]] =
    checked(version).map(Jobs.api(terret, _))

  def mcp(terret: Terret, version: String): Either[TerretMcpError, Mcp[Any]] =
    api(terret, version).flatMap { built =>
      Mcp.from(built).left.map(TerretMcpError.Rejected(_)).map(_.instructions(summary))
    }

  private def checked(version: String): Either[TerretMcpError, String] =
    if version.isEmpty then Left(TerretMcpError.EmptyVersion) else Right(version)
end TerretMcp

enum TerretMcpError:
  case EmptyVersion
  case Rejected(errors: NonEmptyChunk[McpBuildError])
end TerretMcpError
