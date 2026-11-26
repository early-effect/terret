package terret.error

import terret.harness.HarnessFailure
import terret.model.CatalogError
import terret.session.SessionId
import terret.workspace.{Workspace, WorkspaceError}
import zio.Chunk

enum TerretError:
  case WorkspaceDenied(workspace: Workspace, root: Workspace)
  case WorkspaceInvalid(error: WorkspaceError)
  case UnknownSession(id: SessionId)
  case Busy(id: SessionId)
  case Harness(failure: HarnessFailure)
  case UnknownModel(name: String, known: Chunk[String])
  case Models(error: CatalogError)
