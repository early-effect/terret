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

On Grok, `Ask` under `Closed` or `Clamp` is ACP stdio. The ceiling removes every tool it does not
grant, and terret answers `session/request_permission`. A shell write needs `RunCommands` and
`WriteWorkspace`, and the target has to sit in the session workspace or a `writeRoots` path. A
ceiling without `RunCommands` removes the shell, including both names Grok uses for it.
`Network` covers `web_fetch` and `web_search`. A ceiling without `Network` removes both and
passes `--disable-web-search`. A `use_tool` request whose `tool_name` is one of those, or
another network tool, is `Network` too. A refusal names the missing grant. When server policy
removed that grant, the refusal says it was clamped by server policy, and the turn continues.
An edit or a write checks `file_path` and every other path in `rawInput` against the workspace
roots. A running command reports only new output.
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
