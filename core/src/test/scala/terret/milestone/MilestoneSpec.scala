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
import zio.json.*
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
        marked(ToolName("Edit"), """{"secret":"no"}""", started = false) == Right(None),
      )
    },
    test("a finished git commit is a commit, and a dry run or a failed exit is not") {
      val subject  = "[histos-0.7.1 7077bce] Pin Histos 0.7.1"
      val recorded =
        "git add histos && git commit -F target/commit-msg.txt && git status -sb && git log -1 --format='author:%an <%ae>'"
      assertTrue(
        marked(ToolName("run_terminal_command"), command(recorded, s"$subject\n 7 files changed\n", Some("0"))) ==
          Right(Some(Notice(MilestoneKind.Commit, subject))),
        marked(
          ToolName("run_terminal_command"),
          command("/usr/bin/git -C repo commit -m pin", "[main abc] Pin\n", Some("0")),
        ) == Right(Some(Notice(MilestoneKind.Commit, "[main abc] Pin"))),
        marked(ToolName("run_terminal_command"), command("git commit -m pin", "[main abc] Pin\n", None)) ==
          Right(Some(Notice(MilestoneKind.Commit, "[main abc] Pin"))),
        marked(ToolName("run_terminal_command"), command("git commit --dry-run -m x", "dry\n", Some("0"))) ==
          Right(None),
        marked(ToolName("run_terminal_command"), command("git commit -m x", "[main abc] Pin\n", Some("1"))) ==
          Right(None),
        marked(ToolName("run_terminal_command"), command("git commit -m x", "[main abc] Pin\n", Some("1.5"))) ==
          Right(None),
        marked(ToolName("run_terminal_command"), command("git commit -m x", "", Some("0"))) == Right(None),
        marked(ToolName("run_terminal_command"), command("echo git commit", "git commit\n", Some("0"))) == Right(None),
        marked(ToolName("run_terminal_command"), command("rg 'git commit'", "src: git commit -m hello\n", Some("0"))) ==
          Right(None),
      )
    },
    test("a test command reports the last passed or failed line, including a nested result") {
      val rollup = "tests passed: 7 failed: 0 cached: 10 modules: 7"
      val nested =
        s"""{"type":"TaskOutput","Result":{"command":"./histos test --quick","exit_code":0,"status":"completed","output":"grok 62 tests passed. 0 tests failed.\\n$rollup"}}"""
      val runners = List("histos", "sbt", "mill", "cargo", "npm", "pnpm", "yarn", "go")
      assertTrue(
        runners.forall { runner =>
          marked(ToolName("run_terminal_command"), command(s"./$runner test", "tests passed: 1\n", Some("0"))) ==
            Right(Some(Notice(MilestoneKind.TestResult, "tests passed: 1")))
        },
        marked(ToolName("run_terminal_command"), command("pytest -q", "tests failed: 1\n", Some("1"))) ==
          Right(Some(Notice(MilestoneKind.TestResult, "tests failed: 1"))),
        marked(ToolName("get_command_or_subagent_output"), nested) ==
          Right(Some(Notice(MilestoneKind.TestResult, rollup))),
        marked(ToolName("run_terminal_command"), command("./histos test", "tests failed: 2\n", Some("1"))) ==
          Right(Some(Notice(MilestoneKind.TestResult, "tests failed: 2"))),
        marked(ToolName("run_terminal_command"), command("./histos compile", "tests passed: 1\n", Some("0"))) ==
          Right(None),
        marked(
          ToolName("run_terminal_command"),
          """{"type":"BackgroundTaskStarted","command":"./histos test --quick","status":"running","task_id":"c0"}""",
        ) == Right(None),
        marked(
          ToolName("read_file"),
          """{"type":"ReadFile","path":"notes.md","content":"./histos test --quick\ntests passed: 1 failed: 0"}""",
        ) == Right(None),
        marked(
          ToolName("run_terminal_command"),
          command("git commit -m pin && ./histos test", "[main abc] Pin\ntests passed: 1 failed: 0\n", Some("0")),
        ) == Right(Some(Notice(MilestoneKind.Commit, "[main abc] Pin"))),
        marked(
          ToolName("run_terminal_command"),
          command("git commit -m pin && ./histos test", "error\ntests failed: 2\n", Some("1")),
        ) == Right(Some(Notice(MilestoneKind.TestResult, "tests failed: 2"))),
      )
    },
    test("ask_user_question waits on permission when the call starts") {
      val question = "How should a restart keep webhook subscriptions?"
      val asked    = s"""{"questions":[{"question":"$question"}]}"""
      val long     = "q" * (Milestones.SummaryLimit + 20)
      assertTrue(
        marked(ToolName("ask_user_question"), asked, started = true) ==
          Right(Some(Notice(MilestoneKind.WaitingOnPermission, question))),
        marked(ToolName("ask_user_question"), """{"questions":[]}""", started = true) ==
          Right(Some(Notice(MilestoneKind.WaitingOnPermission, "ask_user_question"))),
        marked(ToolName("ask_user_question"), s"""{"questions":[{"question":"$long"}]}""", started = true) ==
          Right(Some(Notice(MilestoneKind.WaitingOnPermission, "q" * Milestones.SummaryLimit))),
        marked(ToolName("ask_user_question"), asked, started = false) == Right(None),
        marked(
          ToolName("Ask: How should a restart keep webhook subscriptions?"),
          s"""{"variant":"AskUserQuestion","questions":[{"question":"$question"}]}""",
          started = false,
        ) == Right(None),
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

  private def marked(name: ToolName, json: String, started: Boolean = false): Either[String, Option[Notice]] =
    json.fromJson[Json].map { parsed =>
      val call  = ToolCall(CallId("c1"), name, parsed)
      val event = if started then Event.ToolStarted(call) else Event.ToolUpdated(call)
      Milestones.classify(event)
    }

  private def command(line: String, output: String, exit: Option[String]): String =
    val code = exit.fold("")(value => s""","exit_code":$value""")
    s"""{"command":${Json.Str(line).toJson},"output":${Json.Str(output).toJson}$code}"""

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
