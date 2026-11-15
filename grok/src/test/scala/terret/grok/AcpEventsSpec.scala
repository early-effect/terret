package terret.grok

import terret.event.{CallId, Event, PlanStep, ToolCall, ToolName, TurnOutcome}
import terret.harness.ProtocolFault
import terret.session.SessionId
import zio.json.ast.Json
import zio.Chunk
import zio.test.*

object AcpEventsSpec extends ZIOSpecDefault:
  def spec = suite("AcpEvents")(
    test("a session update maps the chunks terret keeps") {
      val update = Json.Obj(
        "sessionId" -> Json.Str("sess-1"),
        "update"    -> Json.Obj(
          "sessionUpdate" -> Json.Str("agent_message_chunk"),
          "content"       -> Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("hi")),
        ),
      )
      val thought = Json.Obj(
        "sessionUpdate" -> Json.Str("agent_thought_chunk"),
        "content"       -> Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("hmm")),
      )
      val user = Json.Obj(
        "sessionUpdate" -> Json.Str("user_message_chunk"),
        "content"       -> Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("me")),
      )
      val tool = Json.Obj(
        "sessionUpdate" -> Json.Str("tool_call"),
        "toolCallId"    -> Json.Str("call_1"),
        "title"         -> Json.Str("Edit"),
        "kind"          -> Json.Str("edit"),
        "rawInput"      -> Json.Obj("path" -> Json.Str("a")),
      )
      val plan = Json.Obj(
        "sessionUpdate" -> Json.Str("plan"),
        "entries"       -> Json.Arr(Json.Obj("content" -> Json.Str("look"))),
      )
      assertTrue(
        AcpEvents.fromParams(update) == Right(Chunk(Event.Assistant("hi"))),
        AcpEvents.fromUpdate(thought) == Right(Chunk(Event.Thought("hmm"))),
        AcpEvents.fromUpdate(user) == Right(Chunk.empty),
        AcpEvents.fromUpdate(tool) == Right(
          Chunk(Event.ToolStarted(ToolCall(CallId("call_1"), ToolName("Edit"), Json.Obj("path" -> Json.Str("a")))))
        ),
        AcpEvents.fromUpdate(plan) == Right(Chunk(Event.Plan(Chunk(PlanStep("look"))))),
        AcpEvents.fromUpdate(Json.Obj("sessionUpdate" -> Json.Str("brand_new"))) == Right(Chunk.empty),
      )
    },
    test("a null tool update is dropped and a nameless update keeps the started name") {
      val bytes = Json.Arr(Json.Num(60), Json.Num(119), Json.Num(111), Json.Num(114), Json.Num(107))
      val start = Json.Obj(
        "sessionUpdate" -> Json.Str("tool_call"),
        "toolCallId"    -> Json.Str("call_1"),
        "toolName"      -> Json.Str("grep"),
        "rawInput"      -> Json.Obj(),
      )
      val cleared = Json.Obj(
        "sessionUpdate" -> Json.Str("tool_call_update"),
        "toolCallId"    -> Json.Str("call_1"),
        "rawOutput"     -> Json.Null,
      )
      val update = Json.Obj(
        "sessionUpdate" -> Json.Str("tool_call_update"),
        "toolCallId"    -> Json.Str("call_1"),
        "rawOutput"     -> Json.Obj(
          "type"   -> Json.Str("GrepSearch"),
          "stdout" -> bytes,
          "hits"   -> Json.Arr(Json.Num(1)),
        ),
      )
      val started = ToolCall(CallId("call_1"), ToolName("grep"), Json.Obj())
      val printed = ToolCall(
        CallId("call_1"),
        ToolName("grep"),
        Json.Obj("type" -> Json.Str("GrepSearch"), "stdout" -> Json.Str("<work"), "hits" -> Json.Arr(Json.Num(1))),
      )
      val orphan = Json.Obj(
        "sessionUpdate" -> Json.Str("tool_call_update"),
        "toolCallId"    -> Json.Str("never"),
        "rawOutput"     -> Json.Obj("stdout" -> bytes),
      )
      val unnamed = Json.Obj(
        "sessionUpdate" -> Json.Str("tool_call"),
        "toolCallId"    -> Json.Str("call_2"),
      )
      assertTrue(
        replay(List(start, cleared, update)) == Right(
          Chunk(Event.ToolStarted(started), Event.ToolUpdated(printed))
        ),
        AcpEvents.fromUpdate(orphan) == Right(Chunk.empty),
        AcpEvents.fromUpdate(unnamed) == Right(Chunk.empty),
      )
    },
    test("stop reasons and session ids stay typed") {
      assertTrue(
        AcpEvents.stop(Json.Obj("stopReason" -> Json.Str("cancelled"))) == Right(TurnOutcome.Cancelled),
        AcpEvents.stop(Json.Obj("stopReason" -> Json.Str("end_turn"))) == Right(TurnOutcome.Completed),
        AcpEvents.sessionId(Json.Obj("sessionId" -> Json.Str("sess-1"))) == Right(SessionId("sess-1")),
        AcpEvents.sessionId(Json.Obj("sessionId" -> Json.Str(""))) == Left(ProtocolFault.Incomplete),
        AcpEvents.sessionId(Json.Arr()) == Left(ProtocolFault.Malformed),
      )
    },
  )

  private def replay(lines: List[Json]): Either[ProtocolFault, Chunk[Event]] =
    lines
      .foldLeft[Either[ProtocolFault, (Map[String, ToolName], Chunk[Event])]](Right((Map.empty, Chunk.empty))) {
        case (Right((seen, acc)), line) => AcpEvents.fromUpdate(line, seen).map((next, events) => (next, acc ++ events))
        case (failed, _)                => failed
      }
      .map(_._2)
end AcpEventsSpec
