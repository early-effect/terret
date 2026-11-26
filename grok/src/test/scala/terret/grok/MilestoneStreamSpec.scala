package terret.grok

import terret.event.ToolName
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
              Some(Notice(MilestoneKind.ToolCall, "run_terminal_command")),
              None,
              Some(Notice(MilestoneKind.Commit, subject)),
            )
          )
        )
      }
    },
    test("a recorded test stream reports the last tests passed line") {
      load("test.json").map { updates =>
        assertTrue(
          notices(updates) == Right(
            Chunk(
              Some(Notice(MilestoneKind.ToolCall, "run_terminal_command")),
              None,
              None,
              Some(Notice(MilestoneKind.ToolCall, "get_command_or_subagent_output")),
              None,
              Some(Notice(MilestoneKind.TestResult, rollup)),
            )
          )
        )
      }
    },
    test("a recorded question waits once, when the call starts") {
      load("ask.json").map { updates =>
        assertTrue(
          notices(updates) == Right(
            Chunk(
              Some(Notice(MilestoneKind.WaitingOnPermission, question)),
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
            val marks = if events.isEmpty then Chunk(None) else events.map(Milestones.classify)
            (next, acc ++ marks)
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
