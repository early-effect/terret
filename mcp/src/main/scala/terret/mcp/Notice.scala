package terret.mcp

import heddle.mcp.Session
import heddle.mcp.protocol.Message
import terret.event.{Event, TurnOutcome}
import zio.json.*
import zio.json.ast.Json
import zio.UIO

object Notice:
  val Method = "notifications/message"
  val Logger = "terret"

  def publish(event: Event): UIO[Unit] =
    val data = WireEvent.from(event).toJsonAST match
      case Right(json) => json
      case Left(_)     => Json.Null
    Session.publish(
      Message.Notification(
        Method,
        Json.Obj(
          "level"  -> Json.Str(level(event)),
          "logger" -> Json.Str(Logger),
          "data"   -> data,
        ),
      )
    )

  private def level(event: Event): String =
    event match
      case Event.TurnFinished(TurnOutcome.Failed(_)) => "error"
      case Event.TurnFinished(TurnOutcome.Cancelled) => "warning"
      case Event.Assistant(_) | Event.Thought(_) | Event.ToolStarted(_) | Event.ToolUpdated(_) | Event.Plan(_) |
          Event.TurnFinished(TurnOutcome.Completed) =>
        "info"
end Notice
