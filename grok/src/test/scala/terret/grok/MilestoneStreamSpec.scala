package terret.grok

import terret.activity.{Activity, ActivityKind}
import terret.event.{Event, ToolName}
import terret.harness.ProtocolFault
import terret.milestone.{MilestoneKind, Milestones, Notice}
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, ZIO}

import java.nio.charset.StandardCharsets

object MilestoneStreamSpec extends ZIOSpecDefault:
  private val subject  = "[histos-0.7.1 7077bce] Pin Histos 0.7.1"
  private val rollup   = "tests passed: 7 failed: 0 cached: 10 modules: 7"
  private val question = "How should a restart keep webhook subscriptions?"

  def spec = suite("recorded milestones")(
    test("a recorded commit stream reports the commit from the finished output") {
      load("commit.json").map { updates =>
        assertTrue(
          notices(updates) == Right(
            Chunk(
              None,
              None,
              Some(Notice(MilestoneKind.Commit, subject, s"commit:$subject")),
            )
          )
        )
      }
    },
    test("a recorded test stream is activity, not a milestone") {
      load("test.json").map { updates =>
        val ran   = events(updates)
        val lines = ran.map(found =>
          Activity.observe(found).collect { case note if note.kind == ActivityKind.TestLine => note.text }
        )
        val marked = notices(updates)
        assertTrue(
          marked.map(_.flatten).forall(_.isEmpty),
          lines.map(_.count(_ == rollup)) == Right(1),
        )
      }
    },
    test("a recorded question waits once, when the call starts") {
      val call = "call-503da2a7-1e9f-4d9c-94c6-714c8f23cce4-89"
      load("ask.json").map { updates =>
        assertTrue(
          notices(updates) == Right(
            Chunk(
              Some(Notice(MilestoneKind.WaitingOnPermission, question, s"wait:$call")),
              None,
              None,
            )
          )
        )
      }
    },
  )

  private def notices(updates: Json.Arr): Either[ProtocolFault, Chunk[Option[Notice]]] =
    updates.elements
      .foldLeft[Either[ProtocolFault, (Map[String, ToolName], Chunk[Option[Notice]])]](
        Right((Map.empty, Chunk.empty))
      ) {
        case (Right((seen, acc)), update) =>
          AcpEvents.fromUpdate(update, seen).map { (next, events) =>
            val marks =
              if events.isEmpty then Chunk(None)
              else
                events.flatMap { event =>
                  val found = Milestones.classify(event)
                  if found.isEmpty then Chunk(None) else found.map(Some(_))
                }
            (next, acc ++ marks)
          }
        case (failed, _) => failed
      }
      .map(_._2)

  private def events(updates: Json.Arr): Either[ProtocolFault, Chunk[Event]] =
    updates.elements
      .foldLeft[Either[ProtocolFault, (Map[String, ToolName], Chunk[Event])]](Right((Map.empty, Chunk.empty))) {
        case (Right((seen, acc)), update) =>
          AcpEvents.fromUpdate(update, seen).map { (next, found) =>
            (next, acc ++ found)
          }
        case (failed, _) => failed
      }
      .map(_._2)

  private def load(name: String): ZIO[Any, String, Json.Arr] =
    val path = s"/terret/grok/milestones/$name"
    ZIO
      .attempt {
        Option(getClass.getResourceAsStream(path)) match
          case None     => Left(s"missing $path")
          case Some(in) =>
            try Right(new String(in.readAllBytes(), StandardCharsets.UTF_8))
            finally in.close()
      }
      .mapError(error => Option(error.getMessage).getOrElse(error.toString))
      .flatMap(outcome => ZIO.fromEither(outcome))
      .flatMap { text =>
        ZIO.fromEither(text.fromJson[Json]).flatMap {
          case arr: Json.Arr => ZIO.succeed(arr)
          case other         => ZIO.fail(other.toJson)
        }
      }
end MilestoneStreamSpec
