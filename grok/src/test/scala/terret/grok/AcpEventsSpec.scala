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
          Chunk(Event.ToolStarted(ToolCall(CallId("call_1"), ToolName("Edit"), Json.Obj("path" -> Json.Str("a"))))),
        ),
        AcpEvents.fromUpdate(plan) == Right(Chunk(Event.Plan(Chunk(PlanStep("look"))))),
        AcpEvents.fromUpdate(Json.Obj("sessionUpdate" -> Json.Str("brand_new"))) == Right(Chunk.empty),
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
end AcpEventsSpec
