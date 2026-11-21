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
        )
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
          Chunk(Event.TurnFinished(TurnOutcome.Completed))
        ),
        CliLine.parse("""{"type":"end","stopReason":"cancelled"}""") == Right(
          Chunk(Event.TurnFinished(TurnOutcome.Cancelled))
        ),
        CliLine.parse("""{"type":"end","stopReason":"cancelled","cancellationCategory":"permission_cancelled"}""") ==
          Right(
            Chunk(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.PermissionDenied))))
          ),
        CliLine.parse("""{"type":"end","stopReason":"cancelled","cancellation_category":"PermissionRejected"}""") ==
          Right(
            Chunk(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.PermissionDenied))))
          ),
        CliLine.parse("""{"type":"end","stopReason":"refusal"}""") == Right(
          Chunk(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Refused))))
        ),
        CliLine.parse("""{"type":"end","stopReason":"max_tokens"}""") == Right(
          Chunk(Event.TurnFinished(TurnOutcome.Completed))
        ),
        CliLine.parse("""{"type":"error","message":"boom"}""") == Right(
          Chunk(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Incomplete))))
        ),
      )
    },
    test("a null tool update is dropped and a nameless update keeps the started name") {
      val lines = List(
        """{"type":"tool_call","toolCallId":"call_1","toolName":"grep","rawInput":{}}""",
        """{"type":"tool_call_update","toolCallId":"call_1","rawOutput":null}""",
        """{"type":"tool_call_update","toolCallId":"call_1","rawOutput":""" +
          """{"type":"GrepSearch","stdout":[60,119,111,114,107],"hits":[1,2]}}""",
      )
      val started = ToolCall(CallId("call_1"), ToolName("grep"), Json.Obj())
      val printed = ToolCall(
        CallId("call_1"),
        ToolName("grep"),
        Json.Obj(
          "type"   -> Json.Str("GrepSearch"),
          "stdout" -> Json.Str("<work"),
          "hits"   -> Json.Arr(Json.Num(1), Json.Num(2)),
        ),
      )
      assertTrue(
        replay(lines) == Right(Chunk(Event.ToolStarted(started), Event.ToolUpdated(printed))),
        CliLine.parse("""{"type":"tool_call","toolCallId":"call_2","rawInput":{}}""") == Right(Chunk.empty),
        CliLine.parse("""{"type":"tool_call_update","toolCallId":"never","rawOutput":{"stdout":[60]}}""") == Right(
          Chunk.empty
        ),
      )
    },
    test("a null update records a name and emits nothing") {
      val lines = List(
        """{"type":"tool_call_update","toolCallId":"call_1","toolName":"grep","rawOutput":null}""",
        """{"type":"tool_call_update","toolCallId":"call_1","rawOutput":{"stdout":[60]}}""",
      )
      val printed = ToolCall(CallId("call_1"), ToolName("grep"), Json.Obj("stdout" -> Json.Str("<")))
      assertTrue(replay(lines) == Right(Chunk(Event.ToolUpdated(printed))))
    },
  )

  private def replay(lines: List[String]): Either[ProtocolFault, Chunk[Event]] =
    lines
      .foldLeft[Either[ProtocolFault, (Map[String, ToolName], Chunk[Event])]](Right((Map.empty, Chunk.empty))) {
        case (Right((seen, acc)), line) => CliLine.parse(line, seen).map((next, events) => (next, acc ++ events))
        case (failed, _)                => failed
      }
      .map(_._2)
end CliLineSpec
