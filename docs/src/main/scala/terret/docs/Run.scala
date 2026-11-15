package terret.docs

import specular.*
import terret.app.{Listen, ProcessConfig, ProcessHarness, WarpLocal}
import terret.dsh.DshConfig
import terret.grok.GrokConfig
import zio.test.*

/** Why Terret exists, how to run it, and the coordinate of the next ship. */
object Run extends DocSpec:
  def doc = page("Terret")(
    md"""
Terret is named for the ring a rein passes through. It is the guide between a driver and a horse,
the way a heddle is a guide on a loom. It is not the host, and it is not the harness.

A coding host speaks MCP. Cursor, Claude Code, Grok Bot, and Warp are hosts. A coding harness does
not speak MCP. Grok Build and dsh are harnesses. Terret takes MCP in and talks to the harness
through an adapter. The core never learns a harness's name. The jobs a host calls stay `open`,
`run`, `resume`, `export`, and `cancel`. heddle already owns HTTP and MCP, so Terret does not grow
a second tool DSL. It is the outbound edge: one adapter per harness, then those same jobs promoted
through heddle.

From this repository, `./histos test` proves the suites. `./histos run terret.app.Main` listens on
`http://127.0.0.1:8080/mcp`. The default harness is Grok: command `grok`, transport `Cli`, approval
`Ask`. Nothing else is required.

Warp's local agent reads `.warp/.mcp.json` in the project or the home directory. The file names that
URL. A bind on `0.0.0.0` is written as `127.0.0.1`.

Keys live under `terret`. `harness` `dsh` selects the dsh adapter and the command becomes `dsh`.
`listen` `stdio` is the other pipe. The same five jobs are on both.

The coordinates are the next ship, `<terret ship>`. The jars publish with that release. This site
uses that number now.

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
        config.harness == ProcessHarness.Grok(GrokConfig.cli),
        WarpLocal.File == ".warp/.mcp.json",
      )
    },
    exampleValue(ProcessConfig.load(dsh).map(_.harness)).assert { harness =>
      assertTrue(harness == Right(ProcessHarness.Dsh(DshConfig.headless)))
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
