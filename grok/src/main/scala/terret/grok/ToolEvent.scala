package terret.grok

import terret.event.{CallId, Event, ToolCall, ToolName}
import zio.json.ast.Json
import zio.Chunk

object ToolEvent:
  def decode(obj: Json.Obj, started: Boolean, seen: Map[String, ToolName]): (Map[String, ToolName], Chunk[Event]) =
    val idRaw    = JsonFields.str(obj, "toolCallId").getOrElse("")
    val explicit = JsonFields
      .str(obj, "toolName")
      .orElse(JsonFields.str(obj, "title"))
      .orElse(JsonFields.str(obj, "kind"))
      .filter(_.nonEmpty)
    val remembered = if started then None else seen.get(idRaw)
    (CallId.from(idRaw), choose(explicit, remembered)) match
      case (Right(id), Some(name)) =>
        val next  = if started || explicit.isDefined then seen.updated(id.value, name) else seen
        val raw   = payload(obj, started)
        val input = raw.as[ToolPayload] match
          case Right(value) => value.json
          case Left(_)      => raw
        val call  = ToolCall(id, name, input)
        val event = if started then Event.ToolStarted(call) else Event.ToolUpdated(call)
        (next, Chunk(event))
      case _ => (seen, Chunk.empty)

  private def choose(explicit: Option[String], remembered: Option[ToolName]): Option[ToolName] =
    explicit match
      case Some(raw) =>
        ToolName.from(raw) match
          case Right(name) => Some(name)
          case Left(_)     => remembered
      case None => remembered

  private def payload(obj: Json.Obj, started: Boolean): Json =
    if started then JsonFields.field(obj, "rawInput").getOrElse(Json.Obj())
    else JsonFields.field(obj, "rawOutput").orElse(JsonFields.field(obj, "rawInput")).getOrElse(Json.Obj())
end ToolEvent
