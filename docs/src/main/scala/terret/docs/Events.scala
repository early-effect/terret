package terret.docs

import heddle.mcp.events.VerificationPolicy
import specular.*
import terret.app.ProcessConfig
import terret.mcp.EventPolicy
import zio.durationInt
import zio.test.*

/** Milestones, activity, follow, the journal, and webhook TTL. */
object Events extends DocSpec:
  def doc = page("Events")(
    md"""
`session.milestone` is the short list: a turn, a commit, a push, a pull-request state, an error,
or a wait. A tool call and a test line are not milestones.

`session.activity` is the finer stream. Each fact has a stable id, a parent (the turn id
`turn:<epoch>` or the tool call id), a kind, and the journal time. A tool update on that stream
is the new output, not the whole output again. The journal keeps the cumulative frame. Each
journal entry also records the turn epoch and the time it was appended.

`follow` is a subscription, not a job. A host listens with MCP `subscriptions/listen`, or opens
`GET /sessions/{session}/events` as server-sent events. The SSE id is the journal index.
`after` and `Last-Event-ID` continue strictly after that index, and `Last-Event-ID` wins when both
are set. Closing the stream does not cancel the turn.

`export` returns that same log. Optional `after` is exclusive and is applied first. `milestones`
keeps only milestone events. `latest` keeps the newest N of that selection. `cursor` is the tail
of the whole log, even when `latest` returns a shorter window, and `state` is the session state.
One call shows whether the caller is caught up.

`terret.journal.limit` (`TERRET_JOURNAL_LIMIT`) is how many events one session log keeps. The
default is 4096. Past that, the oldest entries drop and the indexes that remain do not move.
Zero or a negative limit is rejected.

`terret.events.verification` is `Skip` or `Required` and defaults to `Skip`. `terret.events.floor`,
`terret.events.default`, and `terret.events.cap` bound a webhook's TTL and default to 1 minute,
2 hours, and 24 hours. A requested ttl below the floor is raised, an omitted ttl uses the default,
and a longer one, including an unbounded ttl, is the cap. The event subscriber's principal is
`key:` plus the first 8 hex characters of the SHA-256 of the configured key.

Tokens, secret-shaped keys, and email addresses are replaced with `[redacted]` before an event
leaves the process.
""",
    exampleValue(ProcessConfig.local.events).assert { events =>
      assertTrue(
        events == EventPolicy.terret,
        EventPolicy.terret.verification == VerificationPolicy.Skip,
        EventPolicy.terret.floor == 1.minute,
        EventPolicy.terret.default == 2.hours,
        EventPolicy.terret.cap == 24.hours,
      )
    },
  )
end Events
