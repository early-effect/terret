package terret.activity

import terret.event.{CallId, Event, PlanStep, ToolCall, ToolName}
import zio.json.ast.Json
import zio.test.*
import zio.Chunk

import java.time.Instant

object ActivitySpec extends ZIOSpecDefault:
  private val id   = CallId("c1")
  private val name = ToolName("bash")

  def spec = suite("activity")(
    test("a repeated tool frame emits the suffix, and an equal frame emits nothing") {
      val started                  = Event.ToolStarted(ToolCall(id, name, Json.Str("body")))
      val (afterStart, startNotes) = Activity.step(ActivityMemory.empty, started)
      val (afterBody, bodyNotes)   = Activity.step(afterStart, updated("body"))
      val (afterSame, sameNotes)   = Activity.step(afterBody, updated("body"))
      val (_, moreNotes)           = Activity.step(afterSame, updated("body\nmore"))
      assertTrue(
        startNotes.map(_.kind) == Chunk(ActivityKind.ToolStarted),
        bodyNotes.map(note => (note.kind, note.text)) == Chunk((ActivityKind.Output, "body")),
        sameNotes.isEmpty,
        moreNotes.map(note => (note.kind, note.text)) == Chunk((ActivityKind.Output, "\nmore")),
      )
    },
    test("a test line is emitted once, including from a nested result") {
      val (once, first) = Activity.step(ActivityMemory.empty, updated("tests passed: 1\n"))
      val (_, again)    = Activity.step(once, updated("tests passed: 1\n"))
      val nested        = Json.Obj(
        "Result" -> Json.Obj(
          "exit_code" -> Json.Num(1),
          "output"    -> Json.Str("tests failed: 2\n"),
        )
      )
      val notes = Activity.step(ActivityMemory.empty, Event.ToolUpdated(ToolCall(id, name, nested)))._2
      assertTrue(
        first.collect { case note if note.kind == ActivityKind.TestLine => note.text } == Chunk("tests passed: 1"),
        again.isEmpty,
        notes.collect { case note if note.kind == ActivityKind.TestLine => note.text } == Chunk("tests failed: 2"),
        notes.exists(note => note.kind == ActivityKind.ToolFinished && note.status.contains("failed")),
      )
    },
    test("a plan or message that grows by a prefix emits the suffix") {
      val (planned, firstPlan) =
        Activity.step(ActivityMemory.empty, Event.Plan(Chunk(PlanStep("one"))))
      val (_, nextPlan) = Activity.step(planned, Event.Plan(Chunk(PlanStep("one"), PlanStep("two"))))
      val (said, first) = Activity.step(ActivityMemory.empty, Event.Assistant("hello"))
      val (_, more)     = Activity.step(said, Event.Assistant("world"))
      assertTrue(
        firstPlan.map(_.text) == Chunk("one"),
        nextPlan.map(_.text) == Chunk("\ntwo"),
        first.map(_.text) == Chunk("hello"),
        more.map(_.text) == Chunk("world"),
      )
    },
    test("a refusal is a message and does not join the assistant text") {
      val reason = "grep reads ~/.cursor, outside the workspace; grant it in terret.readRoots"
      val (kept, notes) = Activity.step(ActivityMemory.empty, Event.Refused("c1", "grep", reason))
      val (_, more)    = Activity.step(kept, Event.Assistant("hello"))
      assertTrue(
        notes.map(note => (note.kind, note.call, note.name, note.status, note.text)) ==
          Chunk((ActivityKind.Message, Some("c1"), Some("grep"), Some("refused"), reason)),
        more.map(_.text) == Chunk("hello"),
      )
    },
    test("a tool start hangs under the turn, and a later update sends only the suffix") {
      val when                      = Instant.parse("2026-11-21T15:04:05Z")
      val started                   = Event.ToolStarted(ToolCall(id, name, Json.Str("body")))
      val (afterStart, startNotes)  = Activity.step(ActivityMemory.empty, started)
      val placedStart               = Activity.place(0L, 3L, when, startNotes)
      val grown                     = Event.ToolUpdated(ToolCall(id, name, Json.Obj("stdout" -> Json.Str("abcd"))))
      val (afterGrown, _)           = Activity.step(afterStart, grown)
      val longer: Event.ToolUpdated =
        Event.ToolUpdated(ToolCall(id, name, Json.Obj("stdout" -> Json.Str("abcdefgh"))))
      val (_, longerNotes) = Activity.step(afterGrown, longer)
      val sent             = Activity.wireUpdate(longer, longerNotes)
      assertTrue(
        placedStart.map(event => (event.id, event.parent, event.kind, event.at)) ==
          Chunk((id.value, Activity.turnId(3L), ActivityKind.ToolStarted, when)),
        sent.map(_.call.input) == Some(Json.Obj("stdout" -> Json.Str("efgh"))),
        Activity.place(2L, 3L, when, longerNotes).map(event => (event.id, event.parent)) ==
          Chunk(("2:0", id.value)),
      )
    },
  )

  private def updated(text: String): Event =
    Event.ToolUpdated(ToolCall(id, name, Json.Obj("stdout" -> Json.Str(text))))
end ActivitySpec
