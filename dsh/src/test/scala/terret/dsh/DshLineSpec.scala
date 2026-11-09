package terret.dsh

import terret.event.{CallId, ToolCall, ToolName, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.json.ast.Json
import zio.test.*

object DshLineSpec extends ZIOSpecDefault:
  private val call = ToolCall(CallId("call_1"), ToolName("read_file"), Json.Obj("path" -> Json.Str("a")))
  private val failed = TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Incomplete))
  private val refused = TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Refused))

  def spec = suite("DshLine")(
    test("text, thought, tool, and status lines become notices") {
      val lines = List(
        """{"type":"session","sessionId":"session-1","cwd":"/work"}""",
        """{"type":"thinking","text":"thinking","truncated":true}""",
        """{"type":"text","text":"hi"}""",
        """{"type":"text","text":""}""",
        """{"type":"tool_call","callId":"call_1","tool":"read_file","input":{"path":"a"}}""",
        """{"type":"tool_call","callId":"call_2","tool":"bash","input":"1e400"}""",
        """{"type":"tool_call","callId":"call_3","tool":"bash"}""",
        """{"type":"tool_result","callId":"call_1","status":"completed","result":"contents"}""",
        """{"type":"tool_result","callId":"","status":"completed","result":""}""",
        """{"type":"status","phase":"turn_start","turn":1}""",
        """{"type":"status","phase":"step_end","turn":1,"step":1,"usage":{"inputTokens":1,"outputTokens":2}}""",
        """{"type":"status","phase":"later"}""",
        """{"type":"made_up"}""",
        "",
      )
      val notices = lines.map(DshLine.parse)
      assertTrue(
        notices == List(
          Right(DshNotice.Session("session-1")),
          Right(DshNotice.Thought("thinking")),
          Right(DshNotice.Assistant("hi")),
          Right(DshNotice.Skip),
          Right(DshNotice.ToolStarted(call)),
          Right(DshNotice.ToolStarted(ToolCall(CallId("call_2"), ToolName("bash"), Json.Str("1e400")))),
          Right(DshNotice.ToolStarted(ToolCall(CallId("call_3"), ToolName("bash"), Json.Obj()))),
          Right(DshNotice.ToolResult("call_1", "completed", "contents")),
          Right(DshNotice.Skip),
          Right(DshNotice.Skip),
          Right(DshNotice.Skip),
          Right(DshNotice.Skip),
          Right(DshNotice.Skip),
          Right(DshNotice.Skip),
        ),
      )
    },
    test("turn_end kind decides the outcome and the error payload is not control") {
      assertTrue(
        DshLine.parse("""{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"completed"}}""") ==
          Right(DshNotice.Turn(TurnOutcome.Completed)),
        DshLine.parse(
          """{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"aborted","reason":{"kind":"user"}}}""",
        ) == Right(DshNotice.Turn(TurnOutcome.Cancelled)),
        DshLine.parse("""{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"blocked"}}""") ==
          Right(DshNotice.Turn(refused)),
        DshLine.parse(
          """{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"error","error":{"code":"UNKNOWN","message":"boom"}}}""",
        ) == Right(DshNotice.Turn(failed)),
        DshLine.parse("""{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"max-tokens"}}""") ==
          Right(DshNotice.Turn(failed)),
        DshLine.parse("""{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"interrupted"}}""") ==
          Right(DshNotice.Turn(failed)),
        DshLine.parse("""{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"forked"}}""") ==
          Right(DshNotice.Turn(failed)),
        DshLine.parse("""{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"plugin"}}""") ==
          Right(DshNotice.Turn(failed)),
        DshLine.parse("""{"type":"error","message":"boom"}""") == Right(DshNotice.Failed),
        DshLine.parse("""{"type":"error"}""") == Right(DshNotice.Failed),
        DshLine.parse("""{"type":"final","text":""}""") == Right(DshNotice.Final("")),
      )
    },
    test("a bad line is malformed") {
      assertTrue(
        DshLine.parse("not-json") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"sessionId":"session-1"}""") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"session","sessionId":""}""") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"session"}""") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"text"}""") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"tool_call","callId":1,"tool":"bash"}""") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"tool_result","callId":"call_1","result":"x"}""") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"tool_result","callId":"call_1","status":"","result":"x"}""") ==
          Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"status"}""") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"status","phase":"turn_end","turn":1}""") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"status","phase":"turn_end","turn":1,"reason":"completed"}""") ==
          Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"status","phase":"turn_end","turn":1,"reason":{"turn":1}}""") ==
          Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"final"}""") == Left(ProtocolFault.Malformed),
        DshLine.parse("""{"type":"error","message":1}""") == Left(ProtocolFault.Malformed),
      )
    },
  )
end DshLineSpec
