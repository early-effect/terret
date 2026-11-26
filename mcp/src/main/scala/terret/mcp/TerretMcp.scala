package terret.mcp

import heddle.endpoint.Api
import heddle.http.Response
import heddle.mcp.events.Events
import heddle.mcp.{Mcp, McpBuildError}
import heddle.route.Routes
import terret.Terret
import terret.milestone.Milestones
import zio.NonEmptyChunk

object TerretMcp:
  val name    = "terret"
  val summary = "Sessions against a coding harness."

  def api(
      terret: Terret,
      version: String,
      tools: ToolSurface = ToolSurface.closed,
  ): Either[TerretMcpError, Api[Milestones]] =
    checked(version).map(Jobs.api(terret, _, tools))

  def mcp(
      terret: Terret,
      version: String,
      events: EventPolicy = EventPolicy.terret,
      key: Option[EventsKey] = None,
      tools: ToolSurface = ToolSurface.closed,
  ): Either[TerretMcpError, Mcp[Milestones & Events.Env]] =
    served(terret, version, events, key, tools).map(_.mcp)

  /** MCP on `/mcp`, plus the API routes. HTTP serves both. Stdio serves `mcp` only. */
  def served(
      terret: Terret,
      version: String,
      events: EventPolicy = EventPolicy.terret,
      key: Option[EventsKey] = None,
      tools: ToolSurface = ToolSurface.closed,
  ): Either[TerretMcpError, Served] =
    api(terret, version, tools).flatMap { built =>
      Mcp.from(built).left.map(TerretMcpError.Rejected(_)).flatMap { raw =>
        raw
          .instructions(summary)
          .event[MilestoneFilter, MilestoneData]("session.milestone", "A session milestone")
          .pollAndWebhook(MilestoneEvents.accept)
          .left
          .map(TerretMcpError.Rejected(_))
          .map { declared =>
            val keyed = declared.principal(EventsKey.principal(key))
            val tuned = keyed.eventPolicy(events.ttl).verificationPolicy(events.verification)
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
