package terret.config

import terret.permission.PermissionPolicy
import terret.workspace.Workspace

final case class TerretConfig(policy: PermissionPolicy, root: Option[Workspace])

object TerretConfig:
  val closed: TerretConfig = TerretConfig(PermissionPolicy.Closed, None)
end TerretConfig
