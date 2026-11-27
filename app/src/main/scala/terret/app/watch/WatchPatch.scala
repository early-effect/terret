package terret.app.watch

import heddle.sse.{ServerSentEvent, SseField}
import zio.json.*
import zio.json.ast.Json

/** Datastar SSE frames. `elements` lines rejoin with a newline. `signals` is one JSON object. */
enum WatchFrame:
  case Signals(json: String, onlyIfMissing: Boolean)
  case Elements(selector: String, mode: String, elements: String, useViewTransition: Boolean)

enum WatchPatchError:
  case Event(name: String)
  case Signals
  case Mode(token: String)

object WatchPatch:
  val SignalsEvent: String  = "datastar-patch-signals"
  val ElementsEvent: String = "datastar-patch-elements"
  val Modes: Set[String]    = Set("outer", "inner", "replace", "append", "prepend", "before", "after", "remove")

  def elements(html: String, index: Long): ServerSentEvent =
    ServerSentEvent(
      data = s"selector #feed\nmode outer\nuseViewTransition true\nelements $html",
      event = Some(SseField("datastar-patch-elements")),
      id = Some(SseField.of(index)),
    )

  def signals(follow: String): ServerSentEvent =
    val json = Json.Obj("follow" -> Json.Str(follow)).toJson
    ServerSentEvent(
      data = s"signals $json\nonlyIfMissing true",
      event = Some(SseField("datastar-patch-signals")),
    )

  def parse(event: String, data: String): Either[WatchPatchError, WatchFrame] =
    event match
      case SignalsEvent  => parseSignals(data)
      case ElementsEvent => parseElements(data)
      case other         => Left(WatchPatchError.Event(other))

  private def parseSignals(data: String): Either[WatchPatchError, WatchFrame] =
    val fields        = lines(data)
    val onlyIfMissing = fields.exists((field, value) => field == "onlyIfMissing" && value.trim == "true")
    val json          = fields.collect { case ("signals", value) => value }.mkString("\n")
    if json.isEmpty then Left(WatchPatchError.Signals)
    else
      json.fromJson[Json] match
        case Right(_: Json.Obj) => Right(WatchFrame.Signals(json, onlyIfMissing))
        case _                  => Left(WatchPatchError.Signals)

  private def parseElements(data: String): Either[WatchPatchError, WatchFrame] =
    val fields   = lines(data)
    val selector = fields.collectFirst { case ("selector", value) => value.trim }.filter(_.nonEmpty).getOrElse("#feed")
    val mode     = fields.collectFirst { case ("mode", value) => value.trim }
    val useView  = fields.exists((field, value) => field == "useViewTransition" && value.trim == "true")
    val html     = fields.collect { case ("elements", value) => value }.mkString("\n")
    mode match
      case None        => Right(WatchFrame.Elements(selector, "outer", html, useView))
      case Some(token) =>
        if Modes.contains(token) then Right(WatchFrame.Elements(selector, token, html, useView))
        else Left(WatchPatchError.Mode(token))

  private def lines(data: String): Vector[(String, String)] =
    data.linesIterator
      .filter(_.nonEmpty)
      .map { line =>
        val space = line.indexOf(' ')
        if space < 0 then (line, "") else (line.substring(0, space), line.substring(space + 1))
      }
      .toVector
end WatchPatch
