package terret.mcp

import heddle.endpoint.Api
import heddle.http.Response
import heddle.mcp.events.Events
import heddle.mcp.{InstallPrincipal, Mcp, McpBuildError}
import heddle.route.Routes
import terret.Terret
import terret.milestone.Milestones
import zio.NonEmptyChunk

object TerretMcp:
  val name    = "terret"
  val summary = "Sessions against a coding harness."

  def api(terret: Terret, version: String): Either[TerretMcpError, Api[Milestones]] =
    checked(version).map(Jobs.api(terret, _))

  def mcp(
      terret: Terret,
      version: String,
      events: EventPolicy = EventPolicy.terret,
  ): Either[TerretMcpError, Mcp[Milestones & Events.Env]] =
    served(terret, version, events).map(_.mcp)

  /** MCP on `/mcp`, plus the API routes. HTTP serves both. Stdio serves `mcp` only. */
  def served(
      terret: Terret,
      version: String,
      events: EventPolicy = EventPolicy.terret,
  ): Either[TerretMcpError, Served] =
    api(terret, version).flatMap { built =>
      Mcp.from(built).left.map(TerretMcpError.Rejected(_)).flatMap { raw =>
        InstallPrincipal(raw.instructions(summary))(MilestoneEvents.principal)
          .event[MilestoneFilter, MilestoneData]("session.milestone", "A session milestone")
          .pollAndWebhook(MilestoneEvents.accept)
          .left
          .map(TerretMcpError.Rejected(_))
          .map { mcp =>
            val tuned = mcp.eventPolicy(events.ttl).verificationPolicy(events.verification)
            val jobs: Routes[Milestones & Events.Env, Response] = built.routes
            Served(tuned, tuned.routes ++ jobs)
          }
      }
    }

  private def checked(version: String): Either[TerretMcpError, String] =
    if version.isEmpty then Left(TerretMcpError.EmptyVersion) else Right(version)
end TerretMcp

final case class Served(mcp: Mcp[Milestones & Events.Env], routes: Routes[Milestones & Events.Env, Response])

enum TerretMcpError:
  case EmptyVersion
  case Rejected(errors: NonEmptyChunk[McpBuildError])
