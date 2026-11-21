package terret.grok

import terret.event.{Event, PlanStep, ToolName, TurnOutcome}
import terret.harness.ProtocolFault
import terret.session.SessionId
import zio.json.ast.Json
import zio.Chunk

object AcpEvents:
  def fromParams(params: Json): Either[ProtocolFault, Chunk[Event]] =
    fromParams(params, Map.empty).map(_._2)

  def fromParams(
      params: Json,
      seen: Map[String, ToolName],
      refusals: Map[String, String] = Map.empty,
  ): Either[ProtocolFault, (Map[String, ToolName], Chunk[Event])] =
    params match
      case obj: Json.Obj => fromUpdate(JsonFields.field(obj, "update").getOrElse(params), seen, refusals)
      case _             => Left(ProtocolFault.Malformed)

  def sessionId(result: Json): Either[ProtocolFault, SessionId] =
    result match
      case obj: Json.Obj =>
        JsonFields.str(obj, "sessionId") match
          case Some(raw) => SessionId.from(raw).left.map(_ => ProtocolFault.Incomplete)
          case None      => Left(ProtocolFault.Incomplete)
      case _ => Left(ProtocolFault.Malformed)

  def stop(result: Json, hostCancelled: Boolean = false): Either[ProtocolFault, TurnOutcome] =
    result match
      case obj: Json.Obj =>
        val reason = JsonFields.str(obj, "stopReason").getOrElse("end_turn")
        Right(Stop.outcome(reason, hostCancelled))
      case _ => Left(ProtocolFault.Malformed)

  def fromUpdate(json: Json): Either[ProtocolFault, Chunk[Event]] =
    fromUpdate(json, Map.empty).map(_._2)

  def fromUpdate(
      json: Json,
      seen: Map[String, ToolName],
      refusals: Map[String, String] = Map.empty,
  ): Either[ProtocolFault, (Map[String, ToolName], Chunk[Event])] =
    json match
      case obj: Json.Obj =>
        JsonFields.str(obj, "sessionUpdate") match
          case Some("agent_message_chunk") => Right((seen, text(obj, Event.Assistant.apply)))
          case Some("agent_thought_chunk") => Right((seen, text(obj, Event.Thought.apply)))
          case Some("user_message_chunk")  => Right((seen, Chunk.empty))
          case Some("tool_call")           => Right(ToolEvent.decode(obj, started = true, seen))
          case Some("tool_call_update")    => Right(ToolEvent.decode(obj, started = false, seen, refusals))
          case Some("plan")                => Right((seen, plan(obj)))
          case Some(_)                     => Right((seen, Chunk.empty))
          case None                        => Right((seen, Chunk.empty))
      case _ => Left(ProtocolFault.Malformed)

  private def text(obj: Json.Obj, event: String => Event): Chunk[Event] =
    JsonFields.field(obj, "content").flatMap(JsonFields.text).fold(Chunk.empty)(value => Chunk(event(value)))

  private def plan(obj: Json.Obj): Chunk[Event] =
    val steps = JsonFields.field(obj, "entries") match
      case Some(Json.Arr(values)) =>
        values.flatMap { item =>
          JsonFields.text(item) match
            case Some(text) => Chunk(PlanStep(text))
            case None       => Chunk.empty
        }
      case _ => Chunk.empty
    if steps.isEmpty then Chunk.empty else Chunk(Event.Plan(steps))
end AcpEvents
