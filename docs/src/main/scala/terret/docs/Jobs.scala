package terret.docs

import specular.*
import terret.Terret
import terret.app.ProcessConfig
import terret.config.TerretConfig
import terret.hooks.Hooks
import terret.mcp.{TerretMcp, TerretMcpError}
import terret.memory.{InMemory, Script}
import terret.permission.PermissionPolicy
import zio.test.*

/** The host catalog. The names are the jobs, not a harness. */
object Jobs extends DocSpec:
  def doc = page("Jobs")(
    md"""
A host attaches to five jobs: `open`, `run`, `resume`, `export`, and `cancel`.

`open` starts a session on a workspace. `run` sends one prompt and publishes each event while
the tool is in flight. `resume` replays the retained log and does not send a prompt. `export`
returns that log. The Scala method is `transcript`, because `export` is a keyword. `cancel`
interrupts the open turn.

An empty server version is rejected before the catalog is built.
""",
    exampleZIO {
      for
        memory  <- InMemory.make(Script.echo)
        session <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, None), Hooks.identity)
      yield TerretMcp.api(session, ProcessConfig.local.version).map(_.ops.map(_.endpoint.doc.toolName).toSet)
    }.assert { built =>
      assertTrue(built == Right(Set("open", "run", "resume", "export", "cancel")))
    },
    exampleZIO {
      for
        memory  <- InMemory.make(Script.echo)
        session <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, None), Hooks.identity)
      yield TerretMcp.mcp(session, "")
    }.assert { built =>
      assertTrue(built == Left(TerretMcpError.EmptyVersion))
    },
  )
end Jobs
