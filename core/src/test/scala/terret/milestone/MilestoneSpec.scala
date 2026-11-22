package terret.milestone

import terret.Terret
import terret.config.TerretConfig
import terret.error.TerretError
import terret.event.{CallId, Event, PlanStep, ToolCall, ToolName, ToolNameError, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.transcript.{Entry, Journal}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*
import zio.{Chunk, Ref, ZIO}

object MilestoneSpec extends ZIOSpecDefault:
  private val id = SessionId("session")

  def spec = suite("milestones")(
    test("streamed updates are not milestones, and a tool call keeps only its name") {
      val started = tool("Edit")
      val renamed = tool("n" * (Milestones.SummaryLimit + 40))
      assertTrue(
        Milestones.classify(Event.Assistant("hello")).isEmpty,
        Milestones.classify(Event.Thought("hmm")).isEmpty,
        Milestones.classify(Event.Plan(Chunk(PlanStep("step")))).isEmpty,
        started.map(Milestones.classify) ==
          Right(Some(Notice(MilestoneKind.ToolCall, "Edit"))),
        renamed.map(Milestones.classify) ==
          Right(Some(Notice(MilestoneKind.ToolCall, "n" * Milestones.SummaryLimit))),
        Milestones.classify(Event.TurnFinished(TurnOutcome.Completed)) ==
          Some(Notice(MilestoneKind.TurnFinished, "completed")),
        Milestones.classify(Event.TurnFinished(TurnOutcome.Cancelled)) ==
          Some(Notice(MilestoneKind.TurnFinished, "cancelled")),
        Milestones.classify(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Exited(7)))) ==
          Some(Notice(MilestoneKind.Error, "exited 7")),
        Milestones.turnStarted == Notice(MilestoneKind.TurnStarted, "started"),
      )
    },
    test("a failure summary is the enum label") {
      assertTrue(
        Milestones.failed(HarnessFailure.UnknownSession(id)) == Notice(MilestoneKind.Error, "unknown session"),
        Milestones.failed(HarnessFailure.Cancelled) == Notice(MilestoneKind.Error, "cancelled"),
        Milestones.failed(HarnessFailure.Exited(3)) == Notice(MilestoneKind.Error, "exited 3"),
        Milestones.failed(HarnessFailure.Protocol(ProtocolFault.Malformed)) == Notice(MilestoneKind.Error, "malformed"),
        Milestones.failed(HarnessFailure.Protocol(ProtocolFault.BindMissing)) ==
          Notice(MilestoneKind.Error, "bind missing"),
        Milestones.failed(HarnessFailure.Protocol(ProtocolFault.Rejected(9))) ==
          Notice(MilestoneKind.Error, "rejected 9"),
        Milestones.failed(HarnessFailure.Protocol(ProtocolFault.Incomplete)) ==
          Notice(MilestoneKind.Error, "incomplete"),
        Milestones.failed(HarnessFailure.Protocol(ProtocolFault.Refused)) == Notice(MilestoneKind.Error, "refused"),
        Milestones.failed(HarnessFailure.Protocol(ProtocolFault.PermissionDenied)) ==
          Notice(MilestoneKind.Error, "permission denied"),
      )
    },
    test("latest keeps the newest N and a missing bound keeps the selection") {
      val entries =
        Chunk(Entry(0, Event.Assistant("a")), Entry(1, Event.Assistant("b")), Entry(2, Event.Assistant("c")))
      assertTrue(
        Milestones.latest(entries, None) == entries,
        Milestones.latest(entries, Some(1)) == entries.takeRight(1),
        Milestones.latest(entries, Some(10)) == entries,
        Milestones.latest(Chunk.empty, Some(1)).isEmpty,
      )
    },
    test("append offers each entry in index order") {
      for
        seen    <- Ref.make(Chunk.empty[Mark])
        journal <- Journal.make(recording(seen))
        _       <- journal.open(id)
        _       <- journal.append(id, Event.Assistant("a"))
        _       <- journal.append(id, Event.Thought("b"))
        got     <- seen.get
      yield assertTrue(got == Chunk(Mark.Appended(id.value, 0), Mark.Appended(id.value, 1)))
    },
    test("a turn offers its start and each append") {
      for
        seen              <- Ref.make(Chunk.empty[Mark])
        (terret, session) <- opened(Script.echo, recording(seen))
        _                 <- terret.run(session.id, Prompt("hi")).runDrain
        got               <- seen.get
      yield assertTrue(
        got == Chunk(
          Mark.Started(session.id.value, 1, None),
          Mark.Appended(session.id.value, 0),
          Mark.Appended(session.id.value, 1),
        )
      )
    },
    test("a stream failure is offered once and is not a journal row") {
      val script = Script((_, _) => ZStream.fail(HarnessFailure.Exited(7)))
      for
        seen              <- Ref.make(Chunk.empty[Mark])
        (terret, session) <- opened(script, recording(seen))
        result            <- terret.run(session.id, Prompt("hi")).runCollect.either
        got               <- seen.get
        page              <- terret.page(session.id, None)
      yield assertTrue(
        result == Left(TerretError.Harness(HarnessFailure.Exited(7))),
        got == Chunk(
          Mark.Started(session.id.value, 1, None),
          Mark.Failed(session.id.value, 1, HarnessFailure.Exited(7), None),
        ),
        page.events.isEmpty,
      )
    },
  )

  private def tool(name: String): Either[ToolNameError, Event] =
    ToolName.from(name).map { found =>
      Event.ToolStarted(ToolCall(CallId("c1"), found, Json.Obj("secret" -> Json.Str("no"))))
    }

  private def recording(seen: Ref[Chunk[Mark]]): MilestoneSink = new:
    def started(id: SessionId, cursor: Option[Long], epoch: Long): zio.UIO[Unit] =
      seen.update(_ :+ Mark.Started(id.value, epoch, cursor))
    def appended(id: SessionId, entry: Entry): zio.UIO[Unit] =
      seen.update(_ :+ Mark.Appended(id.value, entry.index))
    def failed(id: SessionId, failure: HarnessFailure, cursor: Option[Long], epoch: Long): zio.UIO[Unit] =
      seen.update(_ :+ Mark.Failed(id.value, epoch, failure, cursor))

  private def opened(script: Script, sink: MilestoneSink) =
    for
      memory  <- InMemory.make(script, sink)
      terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity, sink)
      root    <- ZIO.fromEither(Workspace.from("/work")).mapError(TerretError.WorkspaceInvalid(_))
      session <- terret.open(root)
    yield (terret, session)

  private enum Mark:
    case Started(session: String, epoch: Long, cursor: Option[Long])
    case Appended(session: String, index: Long)
    case Failed(session: String, epoch: Long, failure: HarnessFailure, cursor: Option[Long])
end MilestoneSpec
