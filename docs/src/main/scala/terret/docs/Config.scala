package terret.docs

import specular.*
import terret.app.{Listen, ProcessConfig, ProcessError, ProcessHarness, WarpLocal}
import terret.dsh.DshConfig
import terret.grok.{AcpBind, GrokConfig, GrokTransport}
import terret.workspace.WorkspaceError
import zio.test.*

/** Process config. Listener, policy, and the selected harness are separate facts. */
object Config extends DocSpec:
  def doc = page("Config")(
    md"""
Keys live under `terret`. `listen` is `http` or `stdio`. `policy` is `Closed`, `Inherit`, or
`Clamp`. `Clamp` reads `allow` as permission names. `Closed` and `Inherit` ignore `allow`.

`harness` is `grok` or `dsh`. The default is `grok`. An unset `command` follows the harness.
`profile` belongs to dsh and defaults to `headless`. A profile on grok is rejected. `transport`
(`Cli`, `AcpStdio`, or `AcpWebSocket`), `approval` (`Ask` or `Always`), and the bind triple belong
to grok. Setting them on dsh is rejected. `Ask` is the default. A socket transport needs `bindHost`,
`bindPort`, and `bindSecret`. Bind fields on any other transport are rejected.

An empty version is rejected. A relative root is rejected. The packaged process does not spawn
`grok agent serve`. It connects to a serve the user already started.
""",
    exampleValue(ProcessConfig.local.harness).assert { harness =>
      assertTrue(harness == ProcessHarness.Grok(GrokConfig.cli))
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
    exampleValue(ProcessConfig.load(base.copy(transport = Some("AcpWebSocket")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.BindIncomplete))
    },
    exampleValue(
      ProcessConfig
        .load(base.copy(harness = "dsh", command = None, profile = None, transport = None, approval = None))
        .map(_.harness),
    ).assert { harness =>
      assertTrue(harness == Right(ProcessHarness.Dsh(DshConfig.headless)))
    },
    exampleValue(ProcessConfig.load(base.copy(profile = Some("headless")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.ProfileUnexpected))
    },
    exampleValue(ProcessConfig.load(base.copy(harness = "dsh"))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.GrokKeys))
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
    harness = "grok",
    command = Some("grok"),
    profile = None,
    transport = Some("Cli"),
    approval = Some("Ask"),
    bindHost = None,
    bindPort = None,
    bindSecret = None,
  )
end Config
