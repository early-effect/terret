package terret.grok

import terret.event.{CallId, Event, ToolCall, ToolName}
import zio.json.ast.Json
import zio.Chunk

object ToolEvent:
  def decode(
      obj: Json.Obj,
      started: Boolean,
      seen: Map[String, ToolName],
      refusals: Map[String, String] = Map.empty,
  ): (Map[String, ToolName], Chunk[Event]) =
    val idRaw    = JsonFields.str(obj, "toolCallId").getOrElse("")
    val explicit = JsonFields
      .str(obj, "toolName")
      .orElse(JsonFields.str(obj, "title"))
      .orElse(JsonFields.str(obj, "kind"))
      .filter(_.nonEmpty)
    val remembered = if started then None else seen.get(idRaw)
    (CallId.from(idRaw), choose(explicit, remembered)) match
      case (Right(id), Some(name)) =>
        val next   = if started || explicit.isDefined then seen.updated(id.value, name) else seen
        val input  = rendered(obj, started, refusals.get(idRaw).filter(_.nonEmpty))
        val call   = ToolCall(id, name, input)
        val events =
          if started then Chunk(Event.ToolStarted(call))
          else if input == Json.Null then Chunk.empty
          else Chunk(Event.ToolUpdated(call))
        (next, events)
      case _ => (seen, Chunk.empty)

  private def choose(explicit: Option[String], remembered: Option[ToolName]): Option[ToolName] =
    explicit match
      case Some(raw) =>
        ToolName.from(raw) match
          case Right(name) => Some(name)
          case Left(_)     => remembered
      case None => remembered

  private def rendered(obj: Json.Obj, started: Boolean, refusal: Option[String]): Json =
    val raw = payload(obj, started)
    if started || raw != Json.Null then decode(raw)
    else
      JsonFields.field(obj, "content").flatMap(JsonFields.text).filter(_.nonEmpty).orElse(refusal) match
        case Some(text) => Json.Str(text)
        case None       =>
          if failed(obj) then Json.Obj("status" -> Json.Str("failed")) else Json.Null

  private def payload(obj: Json.Obj, started: Boolean): Json =
    if started then JsonFields.field(obj, "rawInput").getOrElse(Json.Obj())
    else JsonFields.field(obj, "rawOutput").orElse(JsonFields.field(obj, "rawInput")).getOrElse(Json.Obj())

  private def failed(obj: Json.Obj): Boolean =
    JsonFields.str(obj, "status").contains("failed")

  private def decode(raw: Json): Json =
    raw.as[ToolPayload] match
      case Right(value) => value.json
      case Left(_)      => raw
end ToolEvent
