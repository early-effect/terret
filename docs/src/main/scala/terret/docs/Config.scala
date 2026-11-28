package terret.docs

import heddle.mcp.events.VerificationPolicy
import specular.*
import terret.app.{
  AccessConfig,
  AccessConfigError,
  KeyGate,
  KeySource,
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

import java.nio.file.Path

/** Process config. Listener, policy, and the selected harness are separate facts. */
object Config extends DocSpec:
  def doc = page("Config")(
    md"""
Keys live under `terret`. A dotted key is also an environment variable: `terret.journal.limit`
is `TERRET_JOURNAL_LIMIT`. Nothing in this config is a secret. The secret itself is not a
config value and is not an environment variable of the start script.

## Listen and harness

| Key | Env | Default | Meaning |
| --- | --- | --- | --- |
| `listen` | `TERRET_LISTEN` | `http` | `http` or `stdio`. |
| `host` | `TERRET_HOST` | `127.0.0.1` | HTTP bind host. |
| `port` | `TERRET_PORT` | `8080` | HTTP bind port. |
| `harness` | `TERRET_HARNESS` | `grok` | `grok` or `dsh`. |
| `command` | `TERRET_COMMAND` | the harness name | An unset command follows the harness. |
| `profile` | `TERRET_PROFILE` | `headless` on dsh | Belongs to dsh. A profile on grok is rejected. |
| `transport` | `TERRET_TRANSPORT` | see below | `Cli`, `AcpStdio`, or `AcpWebSocket`. Belongs to grok. Setting it on dsh is rejected. |
| `approval` | `TERRET_APPROVAL` | `Ask` | `Ask` or `Always`. Belongs to grok. |
| `bindHost`, `bindPort`, `bindSecret` | | unset | A socket transport needs all three. Bind fields on any other transport are rejected. The packaged process does not spawn `grok agent serve`. It connects to a serve the user already started. |

Under `Closed` or `Clamp`, an omitted transport is `AcpStdio`, and an explicit `Cli` is rejected.
`Inherit` keeps `Cli`. `Ask` is the default.

An empty version is rejected. A relative root is rejected.

## Policy and roots

| Key | Env | Default | Meaning |
| --- | --- | --- | --- |
| `policy` | `TERRET_POLICY` | `Closed` | `Closed`, `Inherit`, or `Clamp`. |
| `allow` | `TERRET_ALLOW` | empty | Permission names for `Clamp`. `Closed` and `Inherit` ignore `allow`. |
| `root` | `TERRET_ROOT` | unset | Optional absolute workspace root. A relative root is rejected. |
| `readRoots` | | empty | Comma-separated absolute paths granted for reads besides the session workspace. |
| `writeRoots` | | empty | Comma-separated absolute paths granted for writes besides the session workspace. |

A read root does not grant writes, and a write root does not grant reads. The shell is not
confined to those roots. When `RunCommands` is granted, the shell cwd is the workspace.

## Grants

| Key | Env | Default | Meaning |
| --- | --- | --- | --- |
| `mcp` | `TERRET_MCP` | empty | Opt-in grant for harness MCP calls. Empty denies them. `histos` grants every tool on that server. `histos:version` grants only `version`. An allowed server comes from the workspace `.mcp.json` and is passed on ACP `session/new`. `find` and `call` use that same grant. Empty returns no hits and hint `NoGrants`. A call also needs the server defined in `.mcp.json`. dsh stores the value and does not pass servers. |
| `network` | `TERRET_NETWORK` | empty | Host allowlist. Empty denies every host. `example.com` grants that host. dsh stores the value and does not apply it. |
| `gh` | `TERRET_GH` | empty | Grants `gh` operations on the session workspace. Empty denies them. `read` lists and views pull requests, issues, checks, and workflow runs. `write` creates and comments, and it merges when the caller names a method. `write` does not grant read. `read,write` grants both. A missing grant names itself. dsh does not apply it. |

## Key

There is one key. Set one source, not both.

| Key | Env | Meaning |
| --- | --- | --- |
| `key.file` | `TERRET_KEY_FILE` | Absolute path of a mode 600 env file that sets `TERRET_KEY`. A relative path is rejected. |
| `key.service` | `TERRET_KEY_SERVICE` | Login keychain service name, used with `key.account`. Read with `security`. |
| `key.account` | `TERRET_KEY_ACCOUNT` | Login keychain account name, used with `key.service`. |

HTTP, including the local listener, refuses `/mcp` unless `x-terret-key` matches that key. The
same match is the event subscriber's principal: `key:` plus the first 8 hex characters of the
SHA-256. A missing source on HTTP logs the error and exits non-zero. Stdio has no headers: tool
calls are not checked, and `events/subscribe` on that pipe stays forbidden. The key is not
logged. `TERRET_KEY` in the process environment is not a source.

## Events, journal, and Access

| Key | Env | Default | Meaning |
| --- | --- | --- | --- |
| `events.verification` | `TERRET_EVENTS_VERIFICATION` | `Skip` | `Skip` or `Required`. The caller is authenticated by `x-terret-key`. |
| `events.floor` | `TERRET_EVENTS_FLOOR` | 1 minute | Webhook TTL floor. A requested ttl below the floor is raised. |
| `events.default` | `TERRET_EVENTS_DEFAULT` | 2 hours | Used when a ttl is omitted. |
| `events.cap` | `TERRET_EVENTS_CAP` | 24 hours | A longer ttl, including an unbounded ttl, is the cap. |
| `journal.limit` | `TERRET_JOURNAL_LIMIT` | 4096 | How many events one session log keeps. Past that, the oldest entries drop and the indexes that remain do not move. Zero or a negative limit is rejected. |
| `access.team` | `TERRET_ACCESS_TEAM` | unset | Cloudflare Access team hostname such as `example.cloudflareaccess.com`, with no scheme and no path. Set both team and audience, or neither. A port without both is rejected. |
| `access.audience` | `TERRET_ACCESS_AUDIENCE` | unset | The Access application's tag. |
| `access.port` | `TERRET_ACCESS_PORT` | 8081 | Tunnel listener port on `127.0.0.1`. The same port as the local listener is rejected. |

When both Access keys are set, the local listener (default `127.0.0.1:8080`) has no Access check
and does not serve `GET /watch`. `/mcp` on that listener still requires `x-terret-key`. The
tunnel listener requires the same key on `/mcp`. Every other tunnel path, including `GET /watch`,
requires `Cf-Access-Jwt-Assertion`. The token must verify as RS256 against
`https://<team>/cdn-cgi/access/certs`, for that audience, with issuer `https://<team>`, and not
be expired. A missing token is rejected. There is no exemption for loopback or a Cloudflare
header: cloudflared connects from 127.0.0.1, so point it at the tunnel port, not 8080.
`GET /watch` reads the feed. Steering is POST on that tunnel listener (`run`, `cancel`, `resume`,
and `open`).

## Models and supervisor

| Key | Env | Default | Meaning |
| --- | --- | --- | --- |
| `grok.config` | `TERRET_GROK_CONFIG` | `~/.grok/config.toml` | The file whose `[model.<name>]` tables name the models `open` may request. An empty path is rejected. Nested tables and the `[models]` settings table are not names. The file is read when `open` names a model. |
| `supervisor` | `TERRET_SUPERVISOR` | `none` | `launchd`, `foreground`, or `none`. `none` does not call a service manager. |
| `launchd.labels` | `TERRET_LAUNCHD_LABELS` | unset | Comma-separated launchd job labels. There is no built-in job label. `launchd` also needs `launchd.plistDir`. |
| `foreground.command` | | unset | One executable path for the `foreground` supervisor. |
| `pidFile` | | the default pid path | Written when the server starts. `subscriptions.json` is stored beside it. |
| `restart.pace`, `restart.exit`, `restart.port`, `restart.listen` | | 250 ms, 20 s, 20 s, 2 min | Restart waits. |

Restart and launchd install steps are on the [Operations](operations.html) page. Milestone and
activity events are on the [Events](events.html) page.
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
    exampleValue(ProcessConfig.load(base).map(_.key)).assert { key =>
      assertTrue(key == Right(None))
    },
    exampleZIO {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map("terret.key.file" -> "/var/terret/key.env")))
    }.assert { config =>
      assertTrue(
        config.key == Some(KeySource.File(Path.of("/var/terret/key.env"))),
        !config.toString.contains("example-events-key"),
      )
    },
    exampleZIO {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map("terret.key.file" -> "")))
    }.assert { config =>
      assertTrue(config.key.isEmpty)
    },
    exampleZIO {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map("TERRET_KEY" -> "example-events-key")))
    }.assert { config =>
      assertTrue(config.key.isEmpty, !config.toString.contains("example-events-key"))
    },
    exampleValue(ProcessConfig.load(base.copy(keyFile = Some("key.env")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.KeyPath))
    },
    exampleValue(
      ProcessConfig.load(
        base.copy(keyFile = Some("/var/terret/key.env"), keyService = Some("terret"), keyAccount = Some("mcp"))
      )
    ).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.KeyBoth))
    },
    exampleValue(ProcessConfig.load(base.copy(keyService = Some("terret")))).assert { loaded =>
      assertTrue(loaded == Left(ProcessError.KeyPair))
    },
    exampleValue(KeyGate.ready(Listen.Http(WarpLocal.Host, WarpLocal.Port), None)).assert { ready =>
      assertTrue(ready == Left(ProcessError.KeyMissing))
    },
    exampleValue(KeyGate.ready(Listen.Stdio, None)).assert { ready =>
      assertTrue(ready == Right(None))
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
