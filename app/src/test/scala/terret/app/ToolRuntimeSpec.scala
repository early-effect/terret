package terret.app

import terret.find.{CallError, Embedder, FindError, FindResult}
import terret.grok.{McpGrant, McpPolicy}
import terret.workspace.Workspace
import zio.{Chunk, Scope, ZIO}
import zio.test.*

import java.nio.file.Files

object ToolRuntimeSpec extends ZIOSpecDefault:
  def spec = suite("tool runtime")(
    test("an empty grant returns NoGrants and does not open the embedder") {
      ZIO.scoped {
        for
          root <- ZIO.fromEither(Workspace.from("/work"))
          config = ProcessConfig.local.copy(root = Some(root))
          surface <- ToolRuntime.make(config, boom)
          found   <- surface.find("open a pull request", 1)
          denied  <- surface.call(Some("linear"), "list_issues", "{}").either
        yield assertTrue(
          found == FindResult.noGrants,
          notGranted(denied, "linear", "list_issues"),
        )
      }
    },
    test("a grant with no servers returns no hits and does not open the embedder") {
      ZIO.scoped {
        for
          dir <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("terret-tools"))) { path =>
            ZIO.attemptBlocking(Files.deleteIfExists(path)).ignore
          }
          root <- ZIO.fromEither(Workspace.from(dir.toString))
          granted = McpPolicy(Chunk(McpGrant("histos", None)))
          surface <- ToolRuntime.make(ProcessConfig.local.copy(root = Some(root), mcp = granted), boom)
          found   <- surface.find("open a pull request", 1)
        yield assertTrue(found == FindResult.empty)
      }
    },
  )

  /** Dies if a find or a call opens the model. The short-circuit must not reach it. */
  private val boom: ZIO[Scope, FindError, Embedder] =
    ZIO.dieMessage("embedder opened")

  private def notGranted(error: Either[CallError, String], server: String, tool: String): Boolean =
    error match
      case Left(CallError.NotGranted(got, id)) => got.value == server && id.value == tool
      case _                                   => false
end ToolRuntimeSpec
