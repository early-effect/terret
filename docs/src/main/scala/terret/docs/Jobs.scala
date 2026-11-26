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
A host attaches to `open`, `run`, `status`, `resume`, `export`, `cancel`, `find`, and `call`.

`open` starts a session on a workspace. It takes an optional `model`. When set, the name must be a
`[model.<name>]` key in the grok config, and a grok process terret spawns is started with `--model`.
An unknown name fails and lists the known keys. The result reports the chosen model. Omit `model`
to keep grok's default. `run` accepts one prompt and returns while the turn is
running. The turn keeps going if that call is cancelled. `status` is the poll: it stays `Running`
until the turn ends, and then it carries the outcome. `resume` replays the retained log and does
not send a prompt. `export` returns that log, including whatever has been retained while the turn
is still running. The Scala method is `transcript`, because `export` is a keyword. `export` takes
optional `after`, `milestones`, and `latest`. `after` is exclusive and is applied first.
`milestones` keeps only milestone events. `latest` keeps the newest N of that selection. `cursor`
is the tail of the whole log, even when `latest` returns a shorter window, and `state` is the
session state. One call shows whether the caller is caught up. `cancel` is the only stop.

`find` searches the MCP catalog `.mcp.json` defines. Hits are tools the `terret.mcp` grant allows.
`withheld` names a few stronger matches that were refused, and how to enable them. An empty grant
returns no hits and hint `NoGrants`. `call` runs one tool. `arguments` is a JSON object as text. A
tool with no server is `NotCallable`. A missing grant is `NotGranted`. A granted server that
`.mcp.json` does not define is `UnknownServer`.

`follow` is a subscription, not a job. A host listens with MCP `subscriptions/listen`, or
opens `GET /sessions/{session}/events` as server-sent events. The event id is the log index. Send
that id back as `after` to continue strictly after it. Closing the stream does not cancel the turn.

An empty server version is rejected before the catalog is built.
""",
    exampleZIO {
      ZIO.scoped {
        for
          memory  <- InMemory.make(Script.echo)
          session <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, None), Hooks.identity)
        yield TerretMcp
          .api(session, ProcessConfig.local.version)
          .map { api =>
            (
              api.ops.map(_.endpoint.doc.toolName).toSet,
              api.subscriptions.map(_.subscription.doc.toolName).toSet,
            )
          }
      }
    }.assert { built =>
      assertTrue(
        built == Right(
          (
            Set("open", "run", "status", "resume", "export", "cancel", "find", "call"),
            Set("follow"),
          )
        )
      )
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
