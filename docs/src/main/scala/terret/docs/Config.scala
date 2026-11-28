package terret.docs

import heddle.mcp.events.VerificationPolicy
import specular.*
import terret.app.{
  AccessConfig,
  AccessConfigError,
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
import terret.github.{GhAccess, GhGrant, GhGrantError}
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
transport is `AcpStdio`, and an explicit `Cli` is rejected. `Inherit` keeps `Cli`. `readRoots` and
`writeRoots` are optional comma-separated lists of absolute paths. `readRoots` grants reads besides
the session workspace. `writeRoots` grants writes besides the session workspace. Both default to
empty. A read root does not grant writes, and a write root does not grant reads. The shell is not
confined to those roots. When `RunCommands` is granted, the shell cwd is the workspace.
A socket transport needs `bindHost`, `bindPort`, and `bindSecret`. Bind fields on any other transport
are rejected. `mcp` is an opt-in grant for harness MCP calls. Empty denies them. `histos` grants
every tool on that server. `histos:version` grants only `version`. dsh stores the value and does
not pass servers. An allowed server comes from the workspace `.mcp.json`. `find` and `call` use
that same grant. Empty returns no hits and hint `NoGrants`. A call also needs the server defined
in `.mcp.json`. `network` is a host
allowlist. Empty denies every host. `example.com` grants that host. dsh stores the value and does
not apply it. `gh` grants `gh` operations on the session workspace. Empty denies them. `read`
lists and views pull requests, issues, checks, and workflow runs. `write` creates and comments, and
it merges when the caller names a method. `write` does not grant read. `read,write` grants both.
A missing grant names itself. dsh does not apply it. `events.verification` is `Skip` or `Required`.
The default is `Skip`, because the
edge already rejects a request with no `x-terret-key`. `events.key` (`TERRET_EVENTS_KEY`) is optional.
When it is set, that header must match it or the caller is not a principal. When it is unset, or
empty, a presented non-empty `x-terret-key` is not a principal either. An absent or blank header stays
unauthenticated, which is how local MCP keeps working. The id is `key:` plus the first 8 hex characters
of the SHA-256 of the key. The key is not logged. `events.floor`, `events.default`, and `events.cap`
are the webhook TTL. They default to 1 minute, 2 hours, and 24 hours. A requested ttl below the floor
is raised, an omitted ttl uses the default, and a longer one, including an unbounded ttl, is the cap.
`journal.limit` (`TERRET_JOURNAL_LIMIT`) is how many events one session log keeps. The default is 4096.
Past that, the oldest entries drop and the indexes that remain do not move. Zero or a negative limit
is rejected. `access.team` and `access.audience` (`TERRET_ACCESS_TEAM`, `TERRET_ACCESS_AUDIENCE`) turn
on Cloudflare Access. Set both or neither. A port without both is rejected. The team value is a hostname
such as `example.cloudflareaccess.com`, with no scheme and no path. The audience is the application's tag.
When both are set, the local listener (default `127.0.0.1:8080`) has no Access check and does not serve
`GET /watch`. The tunnel listener on `127.0.0.1` and `access.port` (default 8081, `TERRET_ACCESS_PORT`)
keeps the events-key check on `/mcp`. Every other tunnel path, including `GET /watch`, requires
`Cf-Access-Jwt-Assertion`. The token must verify as RS256 against `https://<team>/cdn-cgi/access/certs`,
for that audience, with issuer `https://<team>`, and not be expired. A missing token is rejected. There
is no exemption for loopback or a Cloudflare header: cloudflared connects from 127.0.0.1, so point it at
the tunnel port, not 8080. `GET /watch` reads the feed. Steering is POST on that tunnel listener
(`run`, `cancel`, `resume`, and `open`). The same
port as the local listener is rejected. Nothing in this config is a secret.

`grok.config` (`TERRET_GROK_CONFIG`) is the file whose `[model.<name>]` tables name the models
`open` may request. Absent, that file is `~/.grok/config.toml`. An empty path is rejected. Nested
tables and the `[models]` settings table are not names. The file is read when `open` names a model.

An empty version is rejected. A relative root is rejected. The packaged process does not spawn
`grok agent serve`. It connects to a serve the user already started.

`supervisor` is `launchd`, `foreground`, or `none`. `none` is the default: restart does not call a
service manager. `launchd` reads `launchd.labels` and `launchd.plistDir`. There is no built-in job
label. `foreground` reads `foreground.command`, one executable path. The server writes `pidFile`
when it starts. Webhook subscriptions and ring heads are stored in `subscriptions.json` beside
that file. A restart keeps each subscription that still restores, with the same id and cursor,
and the next event is one past the saved head. A subscription that names a session this process
does not hold, or that cannot be restored, is skipped and logged, and startup continues.
A subscription whose deliveries are failing is left to expire. Restart waits use `restart.pace`,
`restart.exit`, `restart.port`, and `restart.listen`.
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
    exampleValue(ProcessConfig.load(base.copy(readRoots = Some("work")))).assert { loaded =>
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
    exampleValue(ProcessConfig.load(base.copy(gh = Some("read"))).map(_.gh)).assert { access =>
      assertTrue(access == Right(GhAccess(Set(GhGrant.Read))))
    },
    exampleValue(ProcessConfig.load(base.copy(gh = Some("merge")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.GhGrant(GhGrantError.BadToken("merge"))))
    },
    exampleValue(ProcessConfig.load(base.copy(grokConfig = Some("/var/terret/grok.toml"))).map(_.grokConfig)).assert {
      path =>
        assertTrue(path == Right(java.nio.file.Path.of("/var/terret/grok.toml")))
    },
    exampleValue(ProcessConfig.load(base.copy(grokConfig = Some("")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.GrokConfigPath))
    },
    exampleZIO {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map("terret.grok.config" -> "/var/terret/grok.toml")))
    }.assert { config =>
      assertTrue(config.grokConfig == java.nio.file.Path.of("/var/terret/grok.toml"))
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
    exampleValue(ProcessConfig.load(base).map(_.eventsKey)).assert { key =>
      assertTrue(key == Right(None))
    },
    exampleZIO {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map("terret.events.key" -> "example-events-key")))
    }.assert { config =>
      assertTrue(config.eventsKey.isDefined, !config.toString.contains("example-events-key"))
    },
    exampleZIO {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map("terret.events.key" -> "")))
    }.assert { config =>
      assertTrue(config.eventsKey.isEmpty)
    },
    exampleValue(ProcessConfig.load(base).map(_.journal.limit)).assert { limit =>
      assertTrue(limit == Right(4096))
    },
    exampleValue(ProcessConfig.load(base.copy(journalLimit = 0))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Journal(terret.transcript.JournalBoundError.NonPositive(0))))
    },
    exampleValue(ProcessConfig.load(base.copy(accessTeam = Some("example.cloudflareaccess.com")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Access(AccessConfigError.Incomplete)))
    },
    exampleValue(ProcessConfig.load(base.copy(accessPort = Some(8081)))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Access(AccessConfigError.Incomplete)))
    },
    exampleValue(
      ProcessConfig.load(
        base.copy(accessTeam = Some("example.cloudflareaccess.com"), accessAudience = Some("aud"))
      )
    ).assert { loaded =>
      assertTrue(
        loaded.map(_.access.map(_.audience.value)) == Right(Some("aud")),
        loaded.map(_.access.map(_.port)) == Right(Some(AccessConfig.TunnelPort)),
      )
    },
    exampleValue(
      ProcessConfig.load(
        base.copy(
          accessTeam = Some("example.cloudflareaccess.com"),
          accessAudience = Some("aud"),
          accessPort = Some(8080),
        )
      )
    ).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.Access(AccessConfigError.SamePort(8080))))
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
