package terret.activity

import terret.event.{Event, PlanStep, ToolCall}
import zio.json.ast.Json
import zio.Chunk

/** One fact on the live view. A repeated tool frame that adds no output is absent. */
enum ActivityKind:
  case ToolStarted
  case ToolFinished
  case Output
  case TestLine
  case Plan
  case Message

final case class ActivityNote(
    kind: ActivityKind,
    call: Option[String],
    name: Option[String],
    status: Option[String],
    text: String,
)

/** Output already emitted for each tool call, so the next frame can be a suffix. */
final case class ActivityMemory(calls: Map[String, ActivityMemory.Call], plan: String, message: String)

object ActivityMemory:
  final case class Call(name: String, output: String, status: String, finished: Boolean, tests: Set[String])

  val empty: ActivityMemory = ActivityMemory(Map.empty, "", "")

  def call(name: String): Call = Call(name, "", "started", false, Set.empty)
end ActivityMemory

/** Fold ACP tool frames into deltas. A frame that repeats the output so far emits nothing. */
object Activity:
  def observe(events: Chunk[Event]): Chunk[ActivityNote] =
    events
      .foldLeft((ActivityMemory.empty, Chunk.empty[ActivityNote])) { case ((memory, acc), event) =>
        val (next, notes) = step(memory, event)
        (next, acc ++ notes)
      }
      ._2

  def step(memory: ActivityMemory, event: Event): (ActivityMemory, Chunk[ActivityNote]) =
    event match
      case Event.ToolStarted(call) => started(memory, call)
      case Event.ToolUpdated(call) => updated(memory, call)
      case Event.Assistant(text)   => message(memory, text)
      case Event.Thought(text)     => message(memory, text)
      case Event.Plan(steps)       => plan(memory, steps)
      case Event.TurnFinished(_)   => (memory, Chunk.empty)

  private def started(memory: ActivityMemory, call: ToolCall): (ActivityMemory, Chunk[ActivityNote]) =
    val name = call.name.value
    val note = ActivityNote(ActivityKind.ToolStarted, Some(call.id.value), Some(name), Some("started"), name)
    val kept = memory.calls.updated(call.id.value, ActivityMemory.call(name))
    (memory.copy(calls = kept), Chunk(note))

  private def updated(memory: ActivityMemory, call: ToolCall): (ActivityMemory, Chunk[ActivityNote]) =
    val id              = call.id.value
    val previous        = memory.calls.getOrElse(id, ActivityMemory.call(call.name.value))
    val name            = if previous.name.nonEmpty then previous.name else call.name.value
    val status          = statusOf(call.input).getOrElse(previous.status)
    val (delta, stored) = change(previous.output, textOf(call.input))
    val output          =
      if delta.isEmpty then Chunk.empty
      else Chunk(ActivityNote(ActivityKind.Output, Some(id), Some(name), Some(status), delta))
    val lines  = freshTests(previous.tests, delta)
    val tests  = lines.map(line => ActivityNote(ActivityKind.TestLine, Some(id), Some(name), Some(status), line))
    val done   = previous.finished || finishedOf(call.input)
    val finish =
      if done && !previous.finished then
        Chunk(ActivityNote(ActivityKind.ToolFinished, Some(id), Some(name), Some(status), name))
      else Chunk.empty
    val next = previous.copy(
      name = name,
      output = stored,
      status = status,
      finished = done,
      tests = previous.tests ++ lines,
    )
    (memory.copy(calls = memory.calls.updated(id, next)), output ++ tests ++ finish)

  private def message(memory: ActivityMemory, text: String): (ActivityMemory, Chunk[ActivityNote]) =
    val (delta, stored) = prose(memory.message, text)
    if delta.isEmpty then (memory, Chunk.empty)
    else (memory.copy(message = stored), Chunk(ActivityNote(ActivityKind.Message, None, None, None, delta)))

  private def plan(memory: ActivityMemory, steps: Chunk[PlanStep]): (ActivityMemory, Chunk[ActivityNote]) =
    val text            = steps.map(_.text).mkString("\n")
    val (delta, stored) = prose(memory.plan, text)
    if delta.isEmpty then (memory, Chunk.empty)
    else (memory.copy(plan = stored), Chunk(ActivityNote(ActivityKind.Plan, None, None, None, delta)))

  /** A longer frame that starts with the stored output contributes the suffix. An equal frame contributes nothing. */
  private def change(previous: String, current: String): (String, String) =
    if current.isEmpty || current == previous then ("", previous)
    else if previous.nonEmpty && current.startsWith(previous) then (current.substring(previous.length), current)
    else (current, current)

  /** Independent chunks append. A cumulative frame that starts with what we stored contributes the suffix. */
  private def prose(previous: String, current: String): (String, String) =
    if current.isEmpty || current == previous then ("", previous)
    else if previous.nonEmpty && current.startsWith(previous) then (current.substring(previous.length), current)
    else (current, if previous.isEmpty then current else previous + current)

  private def freshTests(seen: Set[String], delta: String): Chunk[String] =
    if delta.isEmpty then Chunk.empty
    else
      Chunk.fromArray(delta.split("\n", -1)).map(_.trim).filter { line =>
        line.nonEmpty && !seen.contains(line) && (line.contains("tests passed") || line.contains("tests failed"))
      }

  private def textOf(json: Json): String =
    json match
      case Json.Str(text) => text
      case obj: Json.Obj  =>
        direct(obj).orElse(nestedText(obj)).getOrElse("")
      case Json.Arr(values) =>
        values.foldLeft("") { (acc, value) =>
          val found = textOf(value)
          if found.nonEmpty then found else acc
        }
      case _ => ""

  private def direct(obj: Json.Obj): Option[String] =
    field(obj, "output")
      .orElse(field(obj, "stdout"))
      .orElse(field(obj, "stderr"))
      .orElse(field(obj, "output_for_prompt"))

  private def nestedText(obj: Json.Obj): Option[String] =
    obj.fields.foldLeft(Option.empty[String]) { case (acc, (_, child)) =>
      val found = textOf(child)
      if found.nonEmpty then Some(found) else acc
    }

  private def field(obj: Json.Obj, name: String): Option[String] =
    obj.fields.collectFirst { case (key, Json.Str(text)) if key == name && text.nonEmpty => text }

  private def statusOf(json: Json): Option[String] =
    json match
      case obj: Json.Obj =>
        field(obj, "status").orElse(exitStatus(obj)).orElse {
          obj.fields.foldLeft(Option.empty[String]) { case (acc, (_, child)) =>
            statusOf(child).orElse(acc)
          }
        }
      case Json.Arr(values) => values.foldLeft(Option.empty[String])((acc, value) => statusOf(value).orElse(acc))
      case _                => None

  private def exitStatus(obj: Json.Obj): Option[String] =
    obj.fields.collectFirst { case ("exit_code", Json.Num(number)) =>
      if number.compareTo(java.math.BigDecimal.ZERO) == 0 then "ok" else "failed"
    }

  private def finishedOf(json: Json): Boolean =
    json match
      case obj: Json.Obj =>
        obj.fields.exists {
          case ("exit_code", Json.Num(_))                     => true
          case ("status", Json.Str(value)) if terminal(value) => true
          case (_, child)                                     => finishedOf(child)
        }
      case Json.Arr(values) => values.exists(finishedOf)
      case _                => false

  private def terminal(status: String): Boolean =
    status.equalsIgnoreCase("completed") ||
      status.equalsIgnoreCase("failed") ||
      status.equalsIgnoreCase("error") ||
      status.equalsIgnoreCase("ok")
end Activity
