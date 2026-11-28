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
through an adapter. The core never learns a harness's name.

```mermaid
flowchart LR
  host["MCP host"] --> mcp["MCP inbound"]
  mcp --> core["terret core"]
  core --> adapter["adapter"]
  adapter --> harness["harness"]
  core -.-> sandbox["command sandbox"]
```

From this repository, `./histos run app` listens on `http://127.0.0.1:8080/mcp`. Point a host at
that URL with the `x-terret-key` header. The [Attach](attach.html) page covers how a host connects.
Jobs, events, the permission ceiling, config, and operations are in the pages that follow.

The default harness is Grok: command `grok`, transport `AcpStdio`, approval `Ask`. `harness` `dsh`
selects the dsh adapter. `listen` `stdio` is the other pipe. The same jobs are on both.

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
