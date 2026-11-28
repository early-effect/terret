package terret.docs

import specular.*
import terret.app.{KeyGate, Listen, ProcessError, WarpLocal}
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import zio.test.*

/** The key, Access, redaction, and the deny-by-default ceiling. */
object Security extends DocSpec:
  def doc = page("Security")(
    md"""
Nothing set is `Closed`. That cap is `ReadWorkspace` only. The permissions are `ReadWorkspace`,
`WriteWorkspace`, `RunCommands`, and `Network`. A request is intersected with the cap, and the
session records what was dropped. The [Ceiling](ceiling.html) page covers how the cap is applied.

File tools must resolve inside a granted root. `terret.readRoots` and `terret.writeRoots` add
roots, separately, and both default to empty. `ReadWorkspace` never grants a write. The shell
stays off unless `RunCommands` is granted. When it is, its cwd is the workspace. A command terret
starts is confined to the workspace and the configured roots, and a host that cannot sandbox
refuses it. Grok's own process is not sandboxed.

There is one key. HTTP, including the local listener, refuses `/mcp` unless header `x-terret-key`
matches that key. The same match is the event subscriber's principal: `key:` plus the first 8 hex
characters of the SHA-256. A missing source on HTTP logs the error and exits non-zero. Stdio has
no headers: tool calls are not checked, and `events/subscribe` on that pipe stays forbidden. The
key is not logged, and it is not an environment variable of the start script. Set
`terret.key.file` or the keychain pair `terret.key.service` and `terret.key.account`, not both.
The [Config](config.html) page lists those keys.

`terret.access.team`, `terret.access.audience`, and `terret.access.owner` (`TERRET_ACCESS_TEAM`,
`TERRET_ACCESS_AUDIENCE`, `TERRET_ACCESS_OWNER`) are all set or all omitted. When set, the local
listener (default `127.0.0.1:8080`) has no Access check and does not serve `GET /watch`. `/mcp` on
that listener still requires `x-terret-key`. The tunnel listener (`127.0.0.1:8081`,
`terret.access.port`, `TERRET_ACCESS_PORT`) requires the same key on `/mcp`. Every other tunnel
path, including `GET /watch`, requires a `Cf-Access-Jwt-Assertion` (RS256, the team certs at
`https://<team>/cdn-cgi/access/certs`, the audience, issuer `https://<team>`, and not expired).
When `nbf` or `iat` is present it must be an epoch second at most 60 seconds ahead of now. A
missing `nbf` or `iat` is allowed. The `email` claim must match `terret.access.owner` ignoring
case, on the whole address. A service-token JWT (`common_name` and no email) is rejected. There
is no exemption for loopback or a Cloudflare header.
Point cloudflared at the tunnel port, not 8080.

`GET /watch` reads the feed. Steering is POST on the tunnel listener: `POST /watch/{id}/run`,
`/cancel`, `/resume`, and `POST /watch/open`. The local listener does not serve them.

Tokens, secret-shaped keys, and email addresses are replaced with `[redacted]` before an event
leaves the process.
""",
    exampleValue(PermissionPolicy.Closed.ceiling).assert { cap =>
      assertTrue(cap.allow == Set(Permission.ReadWorkspace))
    },
    exampleValue(
      KeyGate.ready(Listen.Http(WarpLocal.Host, WarpLocal.Port), None)
    ).assert { ready =>
      assertTrue(ready == Left(ProcessError.KeyMissing))
    },
    exampleValue(KeyGate.ready(Listen.Stdio, None)).assert { ready =>
      assertTrue(ready == Right(None))
    },
    exampleValue(Ceiling.all.clamp(Ceiling.closed)).assert { (kept, dropped) =>
      assertTrue(
        kept.allow == Set(Permission.ReadWorkspace),
        dropped == Set(
          Permission.WriteWorkspace,
          Permission.RunCommands,
          Permission.Network,
        ),
      )
    },
  )
