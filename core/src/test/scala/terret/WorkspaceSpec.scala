package terret

import terret.workspace.{Workspace, WorkspaceError}
import zio.test.*

object WorkspaceSpec extends ZIOSpecDefault:
  def spec = suite("Workspace")(
    test("from rejects a relative path") {
      assertTrue(Workspace.from("relative/path") == Left(WorkspaceError.Relative("relative/path")))
    },
    test("from rejects an empty path") {
      assertTrue(Workspace.from("") == Left(WorkspaceError.Empty))
    },
    test("from accepts an absolute path") {
      assertTrue(Workspace.from("/work").map(_.path.toString) == Right("/work"))
    },
    test("from rejects a path the platform will not parse") {
      assertTrue(Workspace.from("\u0000") == Left(WorkspaceError.Invalid("\u0000")))
    },
  )
end WorkspaceSpec
