package terret.docs

import heddle.mcp.events.VerificationPolicy
import specular.*
import terret.app.{
  Listen,
  ProcessConfig,
  ProcessError,
  ProcessHarness,
  ServiceConfig,
  ServiceError,
  SupervisorChoice,
  WarpLocal,
}
import terret.dsh.DshConfig
import terret.mcp.{EventPolicy, EventPolicyError}
import terret.grok.{
  AcpBind,
  GrokConfig,
  GrokTransport,
  McpGrant,
  McpGrantError,
  McpPolicy,
  NetworkGrantError,
  NetworkPolicy,
}
import terret.workspace.WorkspaceError
import zio.{Chunk, ConfigProvider, Duration, ZIO}
import zio.durationInt
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
to grok. Setting them on dsh is rejected. `Ask` is the default. Under `Closed` or `Clamp`, an omitted
transport is `AcpStdio`, and an explicit `Cli` is rejected. `Inherit` keeps `Cli`. `writeRoots` is an
optional comma-separated list of absolute paths a shell write may use besides the session workspace.
A socket transport needs `bindHost`, `bindPort`, and `bindSecret`. Bind fields on any other transport
are rejected. `mcp` is an opt-in grant for harness MCP calls. Empty denies them. `histos` grants
every tool on that server. `histos:version` grants only `version`. dsh stores the value and does
not pass servers. An allowed server comes from the workspace `.mcp.json`. `network` is a host
allowlist. Empty denies every host. `example.com` grants that host. dsh stores the value and does
not apply it. `events.verification` is `Skip` or `Required`. The default is `Skip`, because the
edge already checks the caller. `events.floor`, `events.default`, and `events.cap` are the webhook
TTL. They default to 1 minute, 2 hours, and 24 hours. A requested ttl below the floor is raised,
an omitted ttl uses the default, and a longer one, including an unbounded ttl, is the cap.

An empty version is rejected. A relative root is rejected. The packaged process does not spawn
`grok agent serve`. It connects to a serve the user already started.

`supervisor` is `launchd`, `foreground`, or `none`. `none` is the default: restart does not call a
service manager. `launchd` reads `launchd.labels` and `launchd.plistDir`. There is no built-in job
label. `foreground` reads `foreground.command`, one executable path. The server writes `pidFile`
when it starts. Restart waits use `restart.pace`, `restart.exit`, `restart.port`, and
`restart.listen`.
""",
    exampleValue(ProcessConfig.local.harness).assert { harness =>
      assertTrue(harness == ProcessHarness.Grok(GrokConfig.stdio))
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
        .map(_.harness)
    ).assert { harness =>
      assertTrue(harness == Right(ProcessHarness.Dsh(DshConfig.headless)))
    },
    exampleValue(ProcessConfig.load(base.copy(profile = Some("headless")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.ProfileUnexpected))
    },
    exampleValue(ProcessConfig.load(base.copy(harness = "dsh"))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.GrokKeys))
    },
    exampleValue(ProcessConfig.load(base.copy(transport = Some("Cli")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.CliAsk))
    },
    exampleValue(ProcessConfig.load(base.copy(writeRoots = Some("work")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Workspace(WorkspaceError.Relative("work"))))
    },
    exampleValue(ProcessConfig.load(base.copy(mcp = Some("histos:version"))).map(_.mcp)).assert { policy =>
      assertTrue(policy == Right(McpPolicy(Chunk(McpGrant("histos", Some(Set("version")))))))
    },
    exampleValue(ProcessConfig.load(base.copy(mcp = Some("histos:")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.McpGrant(McpGrantError.EmptyTool("histos"))))
    },
    exampleValue(ProcessConfig.load(base.copy(network = Some("example.com"))).map(_.network)).assert { policy =>
      assertTrue(policy == Right(NetworkPolicy(Set("example.com"))))
    },
    exampleValue(ProcessConfig.load(base.copy(network = Some("example.com,")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.NetworkGrant(NetworkGrantError.BadToken(""))))
    },
    exampleValue(ProcessConfig.load(base).map(_.events)).assert { events =>
      assertTrue(
        events == Right(EventPolicy.terret),
        EventPolicy.terret.verification == VerificationPolicy.Skip,
        EventPolicy.terret.floor == 1.minute,
        EventPolicy.terret.default == 2.hours,
        EventPolicy.terret.cap == 24.hours,
      )
    },
    exampleZIO {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(
          ConfigProvider.fromMap(
            Map(
              "terret.events.verification" -> "Required",
              "terret.events.floor"        -> "2 minutes",
              "terret.events.default"      -> "10 minutes",
              "terret.events.cap"          -> "45 minutes",
            )
          )
        )
    }.assert { config =>
      assertTrue(
        config.events == EventPolicy(VerificationPolicy.Required, 2.minutes, 10.minutes, 45.minutes)
      )
    },
    exampleValue(
      ProcessConfig
        .load(
          base.copy(
            eventVerification = "Required",
            eventFloor = 2.minutes,
            eventDefault = 10.minutes,
            eventCap = 45.minutes,
          )
        )
        .map(_.events)
    ).assert { events =>
      assertTrue(events == Right(EventPolicy(VerificationPolicy.Required, 2.minutes, 10.minutes, 45.minutes)))
    },
    exampleValue(ProcessConfig.load(base.copy(eventVerification = "Ask"))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Events(EventPolicyError.Verification("Ask"))))
    },
    exampleValue(ProcessConfig.load(base.copy(eventFloor = 3.hours))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Events(EventPolicyError.Order)))
    },
    exampleValue(ProcessConfig.load(base.copy(eventCap = Duration.Zero))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Events(EventPolicyError.Duration("cap"))))
    },
    exampleValue(ServiceConfig.load(ServiceConfig.defaultRaw).map(_.supervisor)).assert { supervisor =>
      assertTrue(supervisor == Right(SupervisorChoice.None))
    },
    exampleValue(ServiceConfig.load(ServiceConfig.defaultRaw.copy(supervisor = "launchd"))).assert { loaded =>
      assertTrue(loaded == Left(ServiceError.LabelsMissing))
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
    transport = Some("AcpStdio"),
    approval = Some("Ask"),
    bindHost = None,
    bindPort = None,
    bindSecret = None,
  )
end Config
