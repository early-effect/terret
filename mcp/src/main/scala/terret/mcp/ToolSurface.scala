package terret.mcp

import terret.find.{Access, CallError, Calls, Find, FindError, FindResult, Runner}
import zio.{IO, ZIO, ZLayer}

/** Search and call, as the MCP jobs see them. The grant is already chosen. */
trait ToolSurface:
  def find(query: String, k: Int): IO[FindError, FindResult]
  def call(server: Option[String], tool: String, arguments: String): IO[CallError, String]

object ToolSurface:
  /** Empty grant. Search returns `NoGrants`. Call refuses. Nothing is embedded and nothing is spawned. */
  val closed: ToolSurface = new:
    def find(query: String, k: Int): IO[FindError, FindResult] =
      Find.screen(query, k, Access.empty).flatMap {
        case Find.Screened.Done(result) => ZIO.succeed(result)
        case Find.Screened.Query(_)     => ZIO.succeed(FindResult.empty)
      }

    def call(server: Option[String], tool: String, arguments: String): IO[CallError, String] =
      ZIO.fromEither(Calls.decide(server, tool, Access.empty)).flatMap { (name, id) =>
        ZIO.fail(CallError.NotGranted(name, id))
      }

  def direct(search: Find, runner: Runner, access: Access): ToolSurface = new:
    def find(query: String, k: Int): IO[FindError, FindResult] =
      search.find(query, k, access)

    def call(server: Option[String], tool: String, arguments: String): IO[CallError, String] =
      Calls.invoke(server, tool, arguments, access).provide(ZLayer.succeed(runner))
end ToolSurface
