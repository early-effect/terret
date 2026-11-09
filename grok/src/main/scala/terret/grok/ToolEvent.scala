package terret.grok

import terret.event.{CallId, Event, ToolCall, ToolName}
import zio.json.ast.Json
import zio.Chunk

object ToolEvent:
  def decode(obj: Json.Obj, started: Boolean): Chunk[Event] =
    val idRaw = JsonFields.str(obj, "toolCallId").getOrElse("")
    val nameRaw = JsonFields
      .str(obj, "toolName")
      .orElse(JsonFields.str(obj, "title"))
      .orElse(JsonFields.str(obj, "kind"))
      .filter(_.nonEmpty)
      .getOrElse("tool")
    (CallId.from(idRaw), ToolName.from(nameRaw)) match
      case (Right(id), Right(name)) =>
        val payload =
          if started then JsonFields.field(obj, "rawInput").getOrElse(Json.Obj())
          else JsonFields.field(obj, "rawOutput").orElse(JsonFields.field(obj, "rawInput")).getOrElse(Json.Obj())
        val call = ToolCall(id, name, payload)
        Chunk(if started then Event.ToolStarted(call) else Event.ToolUpdated(call))
      case _ => Chunk.empty
end ToolEvent
