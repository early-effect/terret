package terret.dsh

import terret.event.{CallId, ToolCall, ToolName, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.json.*
import zio.json.ast.Json

enum DshNotice:
  case Session(id: String)
  case Thought(text: String)
  case Assistant(text: String)
  case ToolStarted(call: ToolCall)
  case ToolResult(callId: String, status: String, result: String)
  case Turn(outcome: TurnOutcome)
  case Final(text: String)
  case Failed
  case Skip
end DshNotice

object DshLine:
  def parse(line: String): Either[ProtocolFault, DshNotice] =
    if line.isBlank then Right(DshNotice.Skip)
    else line.fromJson[Json].left.map(_ => ProtocolFault.Malformed).flatMap(fromJson)

  def fromJson(json: Json): Either[ProtocolFault, DshNotice] =
    json match
      case obj: Json.Obj =>
        str(obj, "type") match
          case None             => Left(ProtocolFault.Malformed)
          case Some("session")  => session(obj)
          case Some("thinking") => text(obj, DshNotice.Thought.apply)
          case Some("text")     => text(obj, DshNotice.Assistant.apply)
          case Some("tool_call")   => toolCall(obj)
          case Some("tool_result") => toolResult(obj)
          case Some("status")      => status(obj)
          case Some("final")       => finalText(obj)
          case Some("error")       => errorLine(obj)
          case Some(_)             => Right(DshNotice.Skip)
      case _ => Left(ProtocolFault.Malformed)

  private def session(obj: Json.Obj): Either[ProtocolFault, DshNotice] =
    str(obj, "sessionId") match
      case Some(id) if id.nonEmpty => Right(DshNotice.Session(id))
      case _                       => Left(ProtocolFault.Malformed)

  private def text(obj: Json.Obj, build: String => DshNotice): Either[ProtocolFault, DshNotice] =
    field(obj, "text") match
      case Some(Json.Str(value)) if value.nonEmpty => Right(build(value))
      case Some(Json.Str(_))                       => Right(DshNotice.Skip)
      case _                                       => Left(ProtocolFault.Malformed)

  private def toolCall(obj: Json.Obj): Either[ProtocolFault, DshNotice] =
    for
      id   <- optionalStr(obj, "callId")
      name <- optionalStr(obj, "tool")
    yield (id, name) match
      case (Some(rawId), Some(rawName)) =>
        (CallId.from(rawId), ToolName.from(rawName)) match
          case (Right(callId), Right(tool)) =>
            val input = field(obj, "input").getOrElse(Json.Obj())
            DshNotice.ToolStarted(ToolCall(callId, tool, input))
          case _ => DshNotice.Skip
      case _ => DshNotice.Skip

  private def toolResult(obj: Json.Obj): Either[ProtocolFault, DshNotice] =
    for
      id     <- optionalStr(obj, "callId")
      status <- optionalStr(obj, "status")
      result <- optionalStr(obj, "result")
      notice <- (id, status, result) match
        case (Some(rawId), Some(rawStatus), Some(rawResult)) if rawId.nonEmpty && rawStatus.nonEmpty =>
          Right(DshNotice.ToolResult(rawId, rawStatus, rawResult))
        case (Some(""), Some(status), Some(_)) if status.nonEmpty =>
          Right(DshNotice.Skip)
        case _ =>
          Left(ProtocolFault.Malformed)
    yield notice

  private def status(obj: Json.Obj): Either[ProtocolFault, DshNotice] =
    str(obj, "phase") match
      case Some("turn_end") =>
        field(obj, "reason") match
          case Some(reason) => outcome(reason).map(DshNotice.Turn.apply)
          case None         => Left(ProtocolFault.Malformed)
      case Some(_) => Right(DshNotice.Skip)
      case None    => Left(ProtocolFault.Malformed)

  private def outcome(reason: Json): Either[ProtocolFault, TurnOutcome] =
    reason match
      case obj: Json.Obj =>
        str(obj, "kind") match
          case Some("completed") => Right(TurnOutcome.Completed)
          case Some("aborted")   => Right(TurnOutcome.Cancelled)
          case Some("blocked")   => Right(failed(ProtocolFault.Refused))
          case Some(_)           => Right(failed(ProtocolFault.Incomplete))
          case None              => Left(ProtocolFault.Malformed)
      case _ => Left(ProtocolFault.Malformed)

  private def finalText(obj: Json.Obj): Either[ProtocolFault, DshNotice] =
    field(obj, "text") match
      case Some(Json.Str(text)) => Right(DshNotice.Final(text))
      case _                    => Left(ProtocolFault.Malformed)

  private def errorLine(obj: Json.Obj): Either[ProtocolFault, DshNotice] =
    field(obj, "message") match
      case None              => Right(DshNotice.Failed)
      case Some(Json.Str(_)) => Right(DshNotice.Failed)
      case Some(_)           => Left(ProtocolFault.Malformed)

  private def failed(fault: ProtocolFault): TurnOutcome =
    TurnOutcome.Failed(HarnessFailure.Protocol(fault))

  private def optionalStr(obj: Json.Obj, name: String): Either[ProtocolFault, Option[String]] =
    field(obj, name) match
      case None              => Right(None)
      case Some(Json.Str(value)) => Right(Some(value))
      case Some(_)           => Left(ProtocolFault.Malformed)

  private def field(obj: Json.Obj, name: String): Option[Json] =
    obj.fields.collectFirst { case (key, value) if key == name => value }

  private def str(obj: Json.Obj, name: String): Option[String] =
    field(obj, name).collect { case Json.Str(value) => value }
end DshLine
