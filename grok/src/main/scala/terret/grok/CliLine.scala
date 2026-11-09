package terret.grok

import terret.event.{Event, PlanStep, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.json.*
import zio.json.ast.Json
import zio.Chunk

object CliLine:
  def parse(line: String): Either[ProtocolFault, Chunk[Event]] =
    if line.isBlank then Right(Chunk.empty)
    else line.fromJson[Json].left.map(_ => ProtocolFault.Malformed).flatMap(fromJson)

  def fromJson(json: Json): Either[ProtocolFault, Chunk[Event]] =
    json match
      case obj: Json.Obj =>
        JsonFields.str(obj, "type") match
          case None            => Left(ProtocolFault.Malformed)
          case Some("text")    => Right(text(obj, Event.Assistant.apply))
          case Some("thought") => Right(text(obj, Event.Thought.apply))
          case Some("tool_call")        => Right(ToolEvent.decode(obj, started = true))
          case Some("tool_call_update") => Right(ToolEvent.decode(obj, started = false))
          case Some("plan")             => Right(plan(obj))
          case Some("end")              =>
            Right(Chunk(Event.TurnFinished(Stop.outcome(JsonFields.str(obj, "stopReason").getOrElse("")))))
          case Some("error") =>
            Right(Chunk(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Incomplete)))))
          case Some(_) => Right(Chunk.empty)
      case _ => Left(ProtocolFault.Malformed)

  private def text(obj: Json.Obj, event: String => Event): Chunk[Event] =
    JsonFields.str(obj, "data").filter(_.nonEmpty).fold(Chunk.empty)(value => Chunk(event(value)))

  private def plan(obj: Json.Obj): Chunk[Event] =
    val steps = JsonFields.field(obj, "entries") match
      case Some(Json.Arr(values)) =>
        values.flatMap { item =>
          item match
            case Json.Str(text) if text.nonEmpty => Chunk(PlanStep(text))
            case entry: Json.Obj                 =>
              JsonFields.str(entry, "content").orElse(JsonFields.str(entry, "text")).filter(_.nonEmpty) match
                case Some(text) => Chunk(PlanStep(text))
                case None       => Chunk.empty
            case _ => Chunk.empty
        }
      case _ => Chunk.empty
    if steps.isEmpty then Chunk.empty else Chunk(Event.Plan(steps))
end CliLine
