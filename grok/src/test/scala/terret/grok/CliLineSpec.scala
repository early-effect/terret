package terret.grok

import terret.event.{CallId, Event, PlanStep, ToolCall, ToolName, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.json.ast.Json
import zio.Chunk
import zio.test.*

object CliLineSpec extends ZIOSpecDefault:
  private val call = ToolCall(CallId("call_1"), ToolName("read_file"), Json.Obj("path" -> Json.Str("a")))

  def spec = suite("CliLine")(
    test("text, thought, tool, and plan lines become events") {
      val lines = List(
        """{"type":"thought","data":"thinking"}""",
        """{"type":"text","data":"hi"}""",
        """{"type":"tool_call","toolCallId":"call_1","toolName":"read_file","kind":"read","rawInput":{"path":"a"}}""",
        """{"type":"tool_call_update","toolCallId":"call_1","toolName":"read_file","rawOutput":{"path":"a"}}""",
        """{"type":"plan","entries":[{"content":"look"},{"content":""},{"text":"stop"}]}""",
        """{"type":"usage","stopReason":"end_turn"}""",
        """{"type":"made_up"}""",
        "",
      )
      val events = lines.map(CliLine.parse)
      assertTrue(
        events == List(
          Right(Chunk(Event.Thought("thinking"))),
          Right(Chunk(Event.Assistant("hi"))),
          Right(Chunk(Event.ToolStarted(call))),
          Right(Chunk(Event.ToolUpdated(call))),
          Right(Chunk(Event.Plan(Chunk(PlanStep("look"), PlanStep("stop"))))),
          Right(Chunk.empty),
          Right(Chunk.empty),
          Right(Chunk.empty),
        ),
      )
    },
    test("an empty tool id is skipped and a bad line is malformed") {
      assertTrue(
        CliLine.parse("""{"type":"tool_call","toolCallId":"","toolName":"read_file"}""") == Right(Chunk.empty),
        CliLine.parse("not-json") == Left(ProtocolFault.Malformed),
        CliLine.parse("""{"data":"hi"}""") == Left(ProtocolFault.Malformed),
      )
    },
    test("end and error lines finish the turn") {
      assertTrue(
        CliLine.parse("""{"type":"end","stopReason":"end_turn"}""") == Right(
          Chunk(Event.TurnFinished(TurnOutcome.Completed)),
        ),
        CliLine.parse("""{"type":"end","stopReason":"cancelled"}""") == Right(
          Chunk(Event.TurnFinished(TurnOutcome.Cancelled)),
        ),
        CliLine.parse("""{"type":"end","stopReason":"refusal"}""") == Right(
          Chunk(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Refused)))),
        ),
        CliLine.parse("""{"type":"end","stopReason":"max_tokens"}""") == Right(
          Chunk(Event.TurnFinished(TurnOutcome.Completed)),
        ),
        CliLine.parse("""{"type":"error","message":"boom"}""") == Right(
          Chunk(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Incomplete)))),
        ),
      )
    },
  )
end CliLineSpec
