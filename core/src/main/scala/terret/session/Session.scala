package terret.session

import terret.model.ModelKey
import terret.permission.{Ceiling, Permission}
import terret.workspace.Workspace

final case class Session(
    id: SessionId,
    workspace: Workspace,
    effective: Ceiling,
    dropped: Set[Permission],
    state: SessionState,
    model: Option[ModelKey] = None,
)
