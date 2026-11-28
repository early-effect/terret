package terret.docs

import specular.*
import terret.app.{Command, ServiceConfig, ServiceError, SupervisorChoice}
import zio.Chunk
import zio.test.*

/** Restart, the supervisor, launchd, and how the docs site is published. */
object Operations extends DocSpec:
  def doc = page("Operations")(
    md"""
`terret.app.Main` listens on HTTP by default. From this repository, `./histos run app` starts it
on `http://127.0.0.1:8080/mcp`.

`./histos run app -- restart` stops the supervised process, waits until it has exited and the
port is free, starts it, and returns when the new server is listening. That command stays in the
caller's session, so a session under the launchd job dies with the job before the start.

`scripts/terret-restart` detaches first, unloads the job, polls until the port is free, starts
it, and polls until it is listening. Install it with
`install -m 755 scripts/terret-restart ~/.local/bin/terret-restart`.

`TERRET_SUPERVISOR` selects `launchd`, `foreground`, or `none`. `none` is the default and does
not call a service manager. `launchd` reads `terret.launchd.labels` (`TERRET_LAUNCHD_LABELS`) and
`terret.launchd.plistDir`. There is no built-in job label. `foreground` reads
`terret.foreground.command`, one executable path. The server writes `pidFile` when it starts.
Webhook subscriptions and ring heads are stored in `subscriptions.json` beside that file. A
restart keeps each subscription that still restores, with the same id and cursor, and the next
event is one past the saved head. A subscription that names a session this process does not
hold, or that cannot be restored, is skipped and logged, and startup continues. A subscription
whose deliveries are failing is left to expire. Restart waits use `terret.restart.pace`,
`terret.restart.exit`, `terret.restart.port`, and `terret.restart.listen`.

`./histos test` proves the docs. Each page is a Scala program that asserts under the test run
and then renders to the site, so a snippet that does not compile or does not hold fails CI. The
catalog names specular-histos, and the build turns that site service on. A push to `main`
publishes the site at [earlyeffect.rocks/terret](https://www.earlyeffect.rocks/terret/). Pull
request CI builds the site and runs those doc tests so a stale or broken page fails review.

See [DESIGN.md](https://github.com/early-effect/terret/blob/main/DESIGN.md) for the decisions
and [ROADMAP.md](https://github.com/early-effect/terret/blob/main/ROADMAP.md) for which phase
is open.
""",
    exampleValue(Command.parse(Chunk("restart"))).assert { parsed =>
      assertTrue(parsed == Right(Command.Restart))
    },
    exampleValue(ServiceConfig.load(ServiceConfig.defaultRaw).map(_.supervisor)).assert { supervisor =>
      assertTrue(supervisor == Right(SupervisorChoice.None))
    },
    exampleValue(ServiceConfig.load(ServiceConfig.defaultRaw.copy(supervisor = "launchd"))).assert { loaded =>
      assertTrue(loaded == Left(ServiceError.LabelsMissing))
    },
  )
end Operations
