package terret.mcp

import heddle.http.header.Headers
import heddle.mcp.events.{Events, Principal, WebhookTransport}
import heddle.mcp.protocol.{ClientRequest, EventName}
import slayer.*
import terret.Terret
import terret.config.TerretConfig
import terret.error.TerretError
import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.harness.HarnessFailure
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.milestone.{MilestoneGate, MilestoneKind, Milestones, Notice}
import terret.permission.{Ceiling, Permission}
import terret.prompt.Prompt
import terret.session.Session
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*
import zio.{Chunk, Scope, ZIO}

import java.time.Instant

object MilestoneEventsSpec extends ZIOSpecDefault:
  private val when = Instant.parse("2026-11-21T15:04:05Z")

  private val edit = ToolCall(CallId("c1"), ToolName("Edit"), Json.Obj())

  private val sample = MilestoneData("s", MilestoneKind.TurnStarted, "started", None, when.toString)

  def spec = suite("session milestone")(
    test("a null session and an empty kind list match every milestone") {
      val decoded = """{"session":null,"kinds":[]}""".fromJson[MilestoneFilter]
      val other   = MilestoneFilter(Some("other"), Chunk(MilestoneKind.Commit))
      assertTrue(
        decoded == Right(MilestoneFilter(None, Chunk.empty)),
        decoded.toOption.exists(filter => MilestoneEvents.accept(filter, sample)),
        !MilestoneEvents.accept(other, sample),
        MilestoneEvents.accept(MilestoneFilter(Some("s"), Chunk.empty), sample),
      )
    },
    test("the ring carries the test clock and skips streamed updates") {
      val script = Script((_, _) =>
        ZStream(
          Event.Assistant("hello"),
          Event.ToolStarted(edit),
          Event.TurnFinished(TurnOutcome.Completed),
        )
      )
      drive(script, Milestones.live) { (terret, publisher) =>
        for
          _       <- TestClock.setTime(when)
          session <- turn(terret)
          _       <- publisher.settled
          all     <- poll(MilestoneFilter(None, Chunk.empty), Some("0"))
          tools   <- poll(MilestoneFilter(None, Chunk(MilestoneKind.ToolCall)), Some("0"))
          other   <- poll(MilestoneFilter(Some("missing"), Chunk.empty), Some("0"))
          found   <- ZIO.fromEither(all)
          only    <- ZIO.fromEither(tools)
          missed  <- ZIO.fromEither(other)
        yield assertTrue(
          found.map(row => (row.kind, row.summary, row.cursor, row.at)) == Chunk(
            (MilestoneKind.TurnStarted, "started", None, when.toString),
            (MilestoneKind.ToolCall, "Edit", Some(1), when.toString),
            (MilestoneKind.TurnFinished, "completed", Some(2), when.toString),
          ),
          found.forall(_.session == session.id.value),
          only.map(_.summary) == Chunk("Edit"),
          missed.isEmpty,
        )
      }
    },
    test("the emitter classifies through the milestone service") {
      val script = Script((_, _) => ZStream(Event.Assistant("hello"), Event.TurnFinished(TurnOutcome.Completed)))
      drive(script, stubbed[Milestones]) { (terret, publisher) =>
        for
          _ <- TestClock.setTime(when)
          _ <- stub[Milestones](_.turnStarted)(ZIO.succeed(Notice(MilestoneKind.TurnStarted, "started")))
          _ <- stub[Milestones](_.classify(any[Event]))(
            ZIO.succeed(Some(Notice(MilestoneKind.ToolCall, "stubbed")))
          )
          _ <- stub[Milestones](_.failed(any[HarnessFailure]))((failure: HarnessFailure) =>
            ZIO.succeed(Milestones.failed(failure))
          )
          session <- turn(terret)
          _       <- publisher.settled
          polled  <- poll(MilestoneFilter(None, Chunk.empty), Some("0"))
          found   <- ZIO.fromEither(polled)
        yield assertTrue(
          found.map(row => (row.kind, row.summary)) == Chunk(
            (MilestoneKind.TurnStarted, "started"),
            (MilestoneKind.ToolCall, "stubbed"),
            (MilestoneKind.ToolCall, "stubbed"),
          ),
          found.forall(_.session == session.id.value),
          found.forall(_.at == when.toString),
        )
      }
    },
    test("a header is the principal and a blank header is nobody") {
      val headers = Headers(Chunk.empty)
      for
        empty <- MilestoneEvents.principal(headers)
        blank <- MilestoneEvents.principal(headers.add("Authorization", ""))
        named <- MilestoneEvents.principal(headers.add("Authorization", "Bearer secret"))
      yield assertTrue(empty.isEmpty, blank.isEmpty, named.contains(Principal("Bearer secret")))
    },
  )

  private def drive[R <: Milestones](script: Script, layer: zio.ULayer[R])(
      use: (Terret, MilestoneEvents.Publisher) => ZIO[R & Events.Env, Any, TestResult]
  ) =
    ZIO.scoped {
      for
        gate   <- MilestoneGate.make
        memory <- InMemory.make(script, gate)
        terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity, gate)
        mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test")).mapError(identity)
        base   <- (mcp.eventsLayer ++ WebhookTransport.unavailable).build
        extra  <- layer.build
        scope  <- ZIO.environment[Scope]
        result <-
          val body =
            for
              publisher <- MilestoneEvents.attach(gate)
              value     <- use(terret, publisher)
            yield value
          body.provideEnvironment(base.unionAll(extra).unionAll(scope))
      yield result
    }

  private def turn(terret: Terret): ZIO[Any, TerretError, Session] =
    for
      root    <- ZIO.fromEither(Workspace.from("/work")).mapError(TerretError.WorkspaceInvalid(_))
      session <- terret.open(root, Ceiling(Set(Permission.ReadWorkspace)))
      _       <- terret.run(session.id, Prompt("go")).runDrain
      status  <- terret.status(session.id)
    yield status

  private def poll(
      filter: MilestoneFilter,
      cursor: Option[String],
  ): ZIO[Events.Env, String, Either[String, Chunk[MilestoneData]]] =
    for
      arguments <- ZIO.fromEither(asObj(filter))
      polled    <- ZIO.serviceWithZIO[Events] { events =>
        events.poll(
          None,
          ClientRequest.PollEvents(EventName("session.milestone"), arguments, cursor, None, None),
        )
      }
      result <- ZIO.fromEither(polled).mapError(_.toString)
    yield dataOf(result)

  private def asObj[A: JsonEncoder](value: A): Either[String, Json.Obj] =
    value.toJsonAST.flatMap {
      case obj: Json.Obj => Right(obj)
      case other         => Left(other.toJson)
    }

  private def dataOf(body: Json.Obj): Either[String, Chunk[MilestoneData]] =
    body.get("events") match
      case Some(Json.Arr(rows)) =>
        rows.foldLeft[Either[String, Chunk[MilestoneData]]](Right(Chunk.empty)) { (acc, row) =>
          acc.flatMap { got =>
            row match
              case item: Json.Obj =>
                item.get("data") match
                  case Some(data) => data.as[MilestoneData].map(got :+ _)
                  case None       => Left("missing data")
              case _ => Left("event")
          }
        }
      case _ => Left(body.toJson)
end MilestoneEventsSpec
