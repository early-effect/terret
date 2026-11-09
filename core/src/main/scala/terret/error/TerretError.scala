package terret.error

import terret.harness.HarnessFailure
import terret.session.SessionId
import terret.workspace.{Workspace, WorkspaceError}

enum TerretError:
  case WorkspaceDenied(workspace: Workspace, root: Workspace)
  case WorkspaceInvalid(error: WorkspaceError)
  case UnknownSession(id: SessionId)
  case Busy(id: SessionId)
  case Harness(failure: HarnessFailure)
end TerretError
