package terret.docs

import specular.*
import terret.dsh.{DshConfig, DshLine, DshNotice, Launch}
import terret.event.TurnOutcome
import terret.prompt.Prompt
import zio.Chunk
import zio.test.*

/** The headless dsh adapter. The process still wires Grok. */
object DshAdapter extends DocSpec:
  def doc = page("Dsh")(
    md"""
The dsh adapter speaks the headless `--json` projection. The command is `dsh --profile headless --json`,
and the task is one positional argument. The first prompt omits `--session-id`. The opening `session`
event names the id, and the next prompt passes it back. An id with no stored session fails before the
task runs, so the adapter does not mint one.

`text` is an assistant event. `thinking` is a thought. `tool_call` starts a tool and `tool_result`
updates it. A `turn_end` status carries `reason.kind`. `completed` finishes the turn, `aborted`
cancels it, and `blocked` is a refusal. Any other kind fails the turn. The error payload is not
control. An `error` event fails the turn and ends the stream without `final`. `final` is the runner's
answer and does not add a second assistant event. A `turn_end` or an `error` event decides the turn,
so the process exit code is not a second failure.

Headless has no permission flag. The adapter records the ceiling it was given. A task whose text is a
single `-` is the CLI's stdin marker, and the adapter rejects it.
""",
    exampleValue(Launch.argv(DshConfig.headless, Prompt("run the tests"), None).rendered).assert { argv =>
      assertTrue(argv == Chunk("dsh", "--profile", "headless", "--json", "run the tests"))
    },
    exampleValue(Launch.argv(DshConfig.headless, Prompt("continue"), Some("session-1")).rendered).assert { argv =>
      assertTrue(
        argv == Chunk("dsh", "--profile", "headless", "--json", "--session-id", "session-1", "continue"),
      )
    },
    exampleValue(
      DshLine.parse("""{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"completed"}}"""),
    ).assert { parsed =>
      assertTrue(parsed == Right(DshNotice.Turn(TurnOutcome.Completed)))
    },
  )
end DshAdapter
