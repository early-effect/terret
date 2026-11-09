package terret.docs

import specular.*
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import zio.test.*

/** The session cap. Closed is read-only. Inherit is the whole set. */
object CeilingPage extends DocSpec:
  def doc = page("Ceiling")(
    md"""
Nothing set is `Closed`. That cap is `ReadWorkspace` only. A request is intersected with the
cap, and the session records what was dropped.

`Clamp` uses the ceiling it was given. `Inherit` uses every permission: `ReadWorkspace`,
`WriteWorkspace`, `RunCommands`, and `Network`. A workspace outside the configured root is
rejected. An unset root means the opened workspace is its own root.
""",
    exampleValue(PermissionPolicy.Closed.ceiling).assert { cap =>
      assertTrue(cap.allow == Set(Permission.ReadWorkspace))
    },
    exampleValue(PermissionPolicy.Inherit.ceiling).assert { cap =>
      assertTrue(cap == Ceiling.all)
    },
    exampleValue(Ceiling.all.clamp(Ceiling.closed)).assert { (kept, dropped) =>
      assertTrue(
        kept.allow == Set(Permission.ReadWorkspace),
        dropped == Set(Permission.WriteWorkspace, Permission.RunCommands, Permission.Network),
      )
    },
  )
end CeilingPage
