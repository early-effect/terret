package terret.grok

import terret.event.{Event, ToolCall}
import zio.json.ast.Json
import zio.Chunk

object ToolOutput:
  def trim(printed: Map[String, String], events: Chunk[Event]): (Map[String, String], Chunk[Event]) =
    events.foldLeft((printed, Chunk.empty[Event])) { case ((seen, acc), event) =>
      event match
        case Event.ToolUpdated(call) =>
          val (next, updated) = one(seen, call)
          (next, acc :+ updated)
        case other => (seen, acc :+ other)
    }

  private def one(seen: Map[String, String], call: ToolCall): (Map[String, String], Event) =
    call.input match
      case obj: Json.Obj =>
        val previous  = seen.getOrElse(call.id.value, "")
        val current   = textOf(obj)
        val stored    = if current.isEmpty then previous else current
        val rewritten = if previous.isEmpty then obj else suffix(obj, previous)
        (seen.updated(call.id.value, stored), Event.ToolUpdated(call.copy(input = rewritten)))
      case _ => (seen, Event.ToolUpdated(call))

  private def textOf(obj: Json.Obj): String =
    field(obj, "output")
      .orElse(field(obj, "stdout"))
      .orElse(field(obj, "stderr"))
      .orElse(field(obj, "output_for_prompt"))
      .getOrElse("")

  private def field(obj: Json.Obj, name: String): Option[String] =
    obj.fields.collectFirst { case (key, Json.Str(text)) if key == name && text.nonEmpty => text }

  private def suffix(obj: Json.Obj, previous: String): Json =
    Json.Obj(obj.fields.map { (key, value) =>
      value match
        case Json.Str(text) if streamField(key) && text.length > previous.length && text.startsWith(previous) =>
          (key, Json.Str(text.substring(previous.length)))
        case _ => (key, value)
    })

  private def streamField(name: String): Boolean =
    name match
      case "output" | "stdout" | "stderr" | "output_for_prompt" => true
      case _                                                    => false
end ToolOutput
