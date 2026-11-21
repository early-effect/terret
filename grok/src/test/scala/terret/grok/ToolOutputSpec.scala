package terret.grok

import terret.event.{CallId, Event, ToolCall, ToolName}
import zio.json.ast.Json
import zio.test.*
import zio.Chunk

object ToolOutputSpec extends ZIOSpecDefault:
  private val name = ToolName("run_terminal_command")

  def spec = suite("ToolOutput")(
    test("a longer prefix keeps only the new suffix") {
      val (seen, once) = ToolOutput.trim(Map.empty, Chunk(updated("hi")))
      val (_, twice)   = ToolOutput.trim(seen, Chunk(updated("hi there")))
      assertTrue(text(once) == "hi", text(twice) == " there")
    },
    test("an equal chunk and a non-prefix are emitted whole") {
      val (seen, _)     = ToolOutput.trim(Map.empty, Chunk(updated("xxxx")))
      val (next, again) = ToolOutput.trim(seen, Chunk(updated("xxxx")))
      val (_, other)    = ToolOutput.trim(next, Chunk(updated("yyyy")))
      assertTrue(text(again) == "xxxx", text(other) == "yyyy")
    },
    test("a refusal string is left unchanged") {
      val call        = ToolCall(CallId("call_1"), name, Json.Str("needs WriteWorkspace"))
      val event       = Event.ToolUpdated(call)
      val (_, events) = ToolOutput.trim(Map.empty, Chunk(event))
      assertTrue(events == Chunk(event))
    },
  )

  private def updated(output: String): Event =
    Event.ToolUpdated(ToolCall(CallId("call_1"), name, Json.Obj("stdout" -> Json.Str(output))))

  private def text(events: Chunk[Event]): String =
    events
      .collectFirst { case Event.ToolUpdated(call) =>
        call.input match
          case obj: Json.Obj =>
            obj.fields.collectFirst { case ("stdout", Json.Str(value)) => value }.getOrElse("")
          case _ => ""
      }
      .getOrElse("")
end ToolOutputSpec
