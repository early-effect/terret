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
grant, and terret answers `session/request_permission`. A file tool is allowed only when its path,
after `~` and `..` and symlink resolution, sits inside a granted root. With no extra config the only
root is the session workspace. `readRoots` adds read roots and `writeRoots` adds write roots. Both
default to empty, and neither grant implies the other: `ReadWorkspace` never grants a write. The
check runs when the call arrives, against the real path. A path that is already outside every root
is refused without resolving it. A refusal names the missing grant, or `terret.readRoots` /
`terret.writeRoots`, and it is recorded as an activity message. When server policy removed the
grant, the refusal says it was clamped by server policy, and the turn continues.
A ceiling without `RunCommands` removes the shell, including both names Grok uses for it. When the
shell is granted, its cwd is the workspace. The scan refuses a write the command line shows unless
`WriteWorkspace` covers that path. A command terret starts is confined: it may write in the workspace
and `writeRoots` only when `WriteWorkspace` is granted, it may read the workspace and `readRoots`,
and network follows `Network`. The SDKMAN JDK is readable when `current` resolves to a directory
strictly inside `~/.sdkman/candidates/java/`. The link is only a literal, so lookup can lstat and
readlink it, and a symlink at `.sdkman` is not granted. A symlink inside that directory that
points outside it is denied. SDKMAN sbt is the same kind of grant: `candidates/sbt/current` and the
directory it resolves to, when that directory is real and strictly inside the home directory. That
directory may sit under `~/sdks`. Nothing else under `~/sdks` is granted. The shell may read
`.histos`, `.cache/histos`, `.cache/coursier`, `Library/Caches/Coursier`, `.cache/sbt`, `.ivy2`,
and `.m2` when each is a real directory strictly inside the home directory. It may write them only
when `WriteWorkspace` is granted. A missing directory is skipped. A symlink to somewhere else is
not granted, and `Library` and `.cache` themselves are not granted. `~/.m2/settings.xml`,
`~/.m2/settings-security.xml`, and `~/.ivy2/.credentials` stay unreadable. The rest of a granted
cache is readable, so a command with `Network` can still send other files in it. A command with
the cache write can change a cache that a later unsandboxed build trusts, and it can delete a
granted cache directory. Each sandbox also gets one private temp directory, mode 0700, and may
read and write only that directory, even without `WriteWorkspace`. `/tmp`, `/private/tmp`, and
`/var/folders` are not granted. `Network` is on only when the ceiling grants it, which a command
needs when a cache is empty and it has to fetch. A host that cannot apply that profile refuses
the command. Grok's own process is not sandboxed.
`Network` covers `web_fetch` and `web_search`. A ceiling without `Network` removes both and
passes `--disable-web-search`. A `use_tool` request whose `tool_name` is one of those, or
another network tool, is `Network` too. A running command reports only new output.
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
