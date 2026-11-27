package terret.docs

import specular.*
import terret.app.{Command, Listen, ProcessConfig, ProcessHarness, WarpLocal}
import terret.dsh.DshConfig
import terret.grok.GrokConfig
import zio.Chunk
import zio.test.*

/** Why Terret exists, how to run it, and the latest stable coordinate. */
object Run extends DocSpec:
  def doc = page("Terret")(
    md"""
Terret is named for the ring a rein passes through. It is the guide between a driver and a horse,
the way a heddle is a guide on a loom. It is not the host, and it is not the harness.

A coding host speaks MCP. Cursor, Claude Code, Grok Bot, and Warp are hosts. A coding harness does
not speak MCP. Grok Build and dsh are harnesses. Terret takes MCP in and talks to the harness
through an adapter. The core never learns a harness's name. The jobs a host calls stay `open`,
`run`, `status`, `resume`, `export`, `cancel`, `find`, and `call`. `find` searches the MCP catalog.
`call` runs one tool when `terret.mcp` grants that server or tool. `open` takes an optional `model`
key from the grok config. A grok process terret spawns is started with `--model`, and the result
reports the chosen model. `follow` is a subscription on that same API, not a job. A host listens
with MCP `subscriptions/listen`, or opens
`GET /sessions/{session}/events` as server-sent events. The event id is the log index. Send it back
as `after` to continue strictly after that event. Closing the stream does not cancel the turn.
heddle already owns HTTP and MCP, so Terret does not grow a second tool DSL. It is the outbound
edge: one adapter per harness, then those same jobs promoted through heddle.

From this repository, `./histos test` proves the suites. `./histos run app` listens on
`http://127.0.0.1:8080/mcp`. `./histos run app -- restart` stops the supervised process, waits
until that process has exited and the port is free, starts it, and returns when the new server is
listening. That command stays in the caller's session. `scripts/terret-restart` is the one a
session under the launchd job can run: it detaches, unloads the job, polls until the port is free,
starts it, and polls until it is listening. The supervisor is `TERRET_SUPERVISOR`: `launchd`,
`foreground`, or `none`. `none` is the default and does not call a service manager.

The default harness is Grok: command `grok`, transport `AcpStdio`,
approval `Ask`. Nothing else is required. `Ask` under `Closed` or `Clamp` uses ACP stdio, because
the CLI transport cannot answer a permission prompt. The ceiling removes every tool it does not
grant, and terret answers the prompts that remain. A file tool stays inside the session workspace
unless `readRoots` or `writeRoots` grants that path for that mode. `ReadWorkspace` never grants a
write. The shell stays off unless `RunCommands` is granted. When it runs, its cwd is the workspace,
and it is not confined. A shell write that the command line shows needs `RunCommands` and
`WriteWorkspace`. A stop Grok cancels on its own is not a host cancel.

Warp's local agent reads `.warp/.mcp.json` in the project or the home directory. The file names that
URL. A bind on `0.0.0.0` is written as `127.0.0.1`.

Keys live under `terret`. `harness` `dsh` selects the dsh adapter and the command becomes `dsh`.
`listen` `stdio` is the other pipe. The same jobs are on both.

The coordinates are the latest stable, `<terret ship>`.

```scala
dep"rocks.earlyeffect::terret:<terret ship>"
dep"rocks.earlyeffect::terret-grok:<terret ship>"
dep"rocks.earlyeffect::terret-dsh:<terret ship>"
dep"rocks.earlyeffect::terret-mcp:<terret ship>"
dep"rocks.earlyeffect::terret-app:<terret ship>"
```
""",
    exampleValue(ProcessConfig.local).assert { config =>
      val url = config.listen match
        case http: Listen.Http => WarpLocal.url(http.host, http.port)
        case Listen.Stdio      => ""
      assertTrue(
        url == "http://127.0.0.1:8080/mcp",
        config.harness == ProcessHarness.Grok(GrokConfig.stdio),
        WarpLocal.File == ".warp/.mcp.json",
      )
    },
    exampleValue(ProcessConfig.load(dsh).map(_.harness)).assert { harness =>
      assertTrue(harness == Right(ProcessHarness.Dsh(DshConfig.headless)))
    },
    exampleValue(Command.parse(Chunk.empty)).assert { parsed =>
      assertTrue(parsed == Right(Command.Serve))
    },
    exampleValue(Command.parse(Chunk("restart"))).assert { parsed =>
      assertTrue(parsed == Right(Command.Restart))
    },
  )

  private val dsh: ProcessConfig.Raw = ProcessConfig.Raw(
    listen = "http",
    host = WarpLocal.Host,
    port = WarpLocal.Port,
    policy = "Closed",
    allow = "",
    root = None,
    version = ProcessConfig.local.version,
    harness = "dsh",
    command = None,
    profile = None,
    transport = None,
    approval = None,
    bindHost = None,
    bindPort = None,
    bindSecret = None,
  )
end Run
