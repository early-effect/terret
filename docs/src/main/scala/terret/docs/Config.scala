package terret.docs

import specular.*
import terret.app.{Listen, ProcessConfig, ProcessError, WarpLocal}
import terret.grok.{AcpBind, GrokApproval, GrokConfig, GrokTransport}
import terret.workspace.WorkspaceError
import zio.test.*

/** Process config. Listener, policy, and the Grok adapter are separate facts. */
object Config extends DocSpec:
  def doc = page("Config")(
    md"""
Keys live under `terret`. `listen` is `http` or `stdio`. `policy` is `Closed`, `Inherit`, or
`Clamp`. `Clamp` reads `allow` as permission names. `Closed` and `Inherit` ignore `allow`.

The adapter has its own keys: `command`, `transport` (`Cli`, `AcpStdio`, or `AcpWebSocket`),
and `approval` (`Ask` or `Always`). `Ask` is the default. A socket transport needs `bindHost`,
`bindPort`, and `bindSecret`. Bind fields on any other transport are rejected.

An empty version is rejected. A relative root is rejected. The packaged process does not spawn
`grok agent serve`. It connects to a serve the user already started.
""",
    exampleValue(ProcessConfig.local.grok).assert { grok =>
      assertTrue(grok == GrokConfig.cli, grok.approval == GrokApproval.Ask, grok.transport == GrokTransport.Cli)
    },
    exampleValue(GrokConfig.stdio.transport).assert { transport =>
      assertTrue(transport == GrokTransport.AcpStdio)
    },
    exampleValue(GrokConfig.unbound).assert { grok =>
      assertTrue(grok.transport == GrokTransport.AcpWebSocket, grok.bind.isEmpty)
    },
    exampleValue(AcpBind.make("127.0.0.1", 6767, "secret").map(GrokConfig.socket)).assert { built =>
      assertTrue(built.map(_.transport) == Right(GrokTransport.AcpWebSocket))
    },
    exampleValue(ProcessConfig.load(base.copy(listen = "stdio")).map(_.listen)).assert { listen =>
      assertTrue(listen == Right(Listen.Stdio))
    },
    exampleValue(ProcessConfig.load(base.copy(version = ""))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Version))
    },
    exampleValue(ProcessConfig.load(base.copy(root = Some("work")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Workspace(WorkspaceError.Relative("work"))))
    },
    exampleValue(ProcessConfig.load(base.copy(transport = "AcpWebSocket"))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.BindIncomplete))
    },
  )

  private val base: ProcessConfig.Raw = ProcessConfig.Raw(
    listen = "http",
    host = WarpLocal.Host,
    port = WarpLocal.Port,
    policy = "Closed",
    allow = "",
    root = None,
    version = ProcessConfig.local.version,
    command = "grok",
    transport = "Cli",
    approval = "Ask",
    bindHost = None,
    bindPort = None,
    bindSecret = None,
  )
end Config
