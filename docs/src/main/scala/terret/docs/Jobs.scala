package terret.docs

import specular.*
import terret.Terret
import terret.app.ProcessConfig
import terret.config.TerretConfig
import terret.hooks.Hooks
import terret.mcp.{TerretMcp, TerretMcpError}
import terret.memory.{InMemory, Script}
import terret.permission.PermissionPolicy
import zio.ZIO
import zio.test.*

/** The host catalog. The names are the jobs, not a harness. */
object Jobs extends DocSpec:
  def doc = page("Jobs")(
    md"""
A host attaches to six jobs: `open`, `run`, `status`, `resume`, `export`, and `cancel`.

`open` starts a session on a workspace. `run` accepts one prompt and returns while the turn is
running. The turn keeps going if that call is cancelled. `status` is the poll: it stays `Running`
until the turn ends, and then it carries the outcome. `resume` replays the retained log and does
not send a prompt. `export` returns that log, including whatever has been retained while the turn
is still running. The Scala method is `transcript`, because `export` is a keyword. `cancel` is the
only stop.

An empty server version is rejected before the catalog is built.
""",
    exampleZIO {
      ZIO.scoped {
        for
          memory  <- InMemory.make(Script.echo)
          session <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, None), Hooks.identity)
        yield TerretMcp
          .api(session, ProcessConfig.local.version)
          .map(_.ops.map(_.endpoint.doc.toolName).toSet)
      }
    }.assert { built =>
      assertTrue(built == Right(Set("open", "run", "status", "resume", "export", "cancel")))
    },
    exampleZIO {
      ZIO.scoped {
        for
          memory  <- InMemory.make(Script.echo)
          session <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, None), Hooks.identity)
        yield TerretMcp.mcp(session, "")
      }
    }.assert { built =>
      assertTrue(built == Left(TerretMcpError.EmptyVersion))
    },
  )
