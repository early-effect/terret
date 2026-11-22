package terret.mcp

import heddle.mcp.Mcp
import heddle.mcp.events.{Events, WebhookTransport}
import heddle.mcp.protocol.{ClientRequest, Message, RequestId, ToolName}
import slayer.*
import terret.Terret
import terret.config.TerretConfig
import terret.event.{CallId, Event, PlanStep, ToolCall, ToolName as EventTool, TurnOutcome}
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.milestone.{Milestones, Notice}
import terret.permission.{Ceiling, Permission}
import terret.prompt.Prompt
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*
import zio.{Chunk, ZIO, ZLayer}

object ExportCatchUpSpec extends ZIOSpecDefault:
  private type Env = Milestones & Events.Env

  private val edit =
    ToolCall(CallId("c1"), EventTool("Edit"), Json.Obj("secret" -> Json.Str("no")))

  private val script = Script((_, _) =>
    ZStream(
      Event.Assistant("hello"),
      Event.Thought("hmm"),
      Event.ToolStarted(edit),
      Event.ToolUpdated(edit),
      Event.Plan(Chunk(PlanStep("step"))),
      Event.TurnFinished(TurnOutcome.Completed),
    )
  )

  private val toolEvent = WireEvent.ToolStarted(edit.id.value, edit.name.value, edit.input.toJson)

  private val finished = WireEvent.TurnFinished(WireOutcome.Completed)

  private val ended = WireState.Ended(WireOutcome.Completed)

  def spec = suite("export catch-up")(
    test("milestones and latest keep the tail cursor") {
      served(Milestones.live) { (terret, mcp) =>
        for
          id       <- run(terret)
          whole    <- call(mcp, "export", ExportJob(id), 1)
          marked   <- call(mcp, "export", ExportJob(id, milestones = Some(true)), 2)
          newest   <- call(mcp, "export", ExportJob(id, milestones = Some(true), latest = Some(1)), 3)
          after    <- call(mcp, "export", ExportJob(id, after = Some(2), milestones = Some(true)), 4)
          refused  <- call(mcp, "export", ExportJob(id, latest = Some(0)), 5)
          negative <- call(mcp, "export", ExportJob(id, after = Some(-1)), 6)
        yield assertTrue(
          log(whole) == Right(Log(id, full, Some(5), ended)),
          log(marked) == Right(Log(id, Chunk(toolEvent, finished), Some(5), ended)),
          log(newest) == Right(Log(id, Chunk(finished), Some(5), ended)),
          log(after) == Right(Log(id, Chunk(finished), Some(5), ended)),
          failure(refused) == Right(JobFailure.BadLatest),
          failure(negative) == Right(JobFailure.BadCursor),
        )
      }
    },
    test("export asks the milestone service, and a miss still reports the tail") {
      served(stubbed[Milestones]) { (terret, mcp) =>
        for
          id     <- run(terret)
          plain  <- call(mcp, "export", ExportJob(id, milestones = Some(false)), 1)
          _      <- stub[Milestones](_.classify(any[Event]))(ZIO.succeed(None: Option[Notice]))
          caught <- call(mcp, "export", ExportJob(id, milestones = Some(true)), 2)
        yield assertTrue(
          log(plain) == Right(Log(id, full, Some(5), ended)),
          log(caught) == Right(Log(id, Chunk.empty, Some(5), ended)),
        )
      }
    },
  )

  private val full = Chunk(
    WireEvent.Assistant("hello"),
    WireEvent.Thought("hmm"),
    toolEvent,
    WireEvent.ToolUpdated(edit.id.value, edit.name.value, edit.input.toJson),
    WireEvent.Plan(Chunk("step")),
    finished,
  )

  private def served[R <: Milestones](layer: ZLayer[Any, Nothing, R])(
      use: (Terret, Mcp[Env]) => ZIO[R & Env, String, TestResult]
  ) =
    ZIO.scoped {
      for
        memory <- InMemory.make(script)
        terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
        mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test")).mapError(_.toString)
        base   <- (mcp.eventsLayer ++ WebhookTransport.unavailable).build
        extra  <- layer.build
        result <- use(terret, mcp).provideEnvironment(base.unionAll(extra))
      yield result
    }

  private def run(terret: Terret): ZIO[Any, String, String] =
    for
      root    <- ZIO.fromEither(Workspace.from("/work")).mapError(_.toString)
      session <- terret.open(root, Ceiling(Set(Permission.ReadWorkspace))).mapError(_.toString)
      _       <- terret.run(session.id, Prompt("go")).runDrain.mapError(_.toString)
    yield session.id.value

  private def call[A: JsonEncoder](mcp: Mcp[Env], name: String, args: A, id: Long): ZIO[Env, String, Json.Obj] =
    for
      encoded <- ZIO.fromEither(asObj(args))
      tool    <- ZIO.fromEither(ToolName.from(name)).mapError(_ => name)
      json    <- mcp.handle(Message.stateless(RequestId.Num(id), ClientRequest.CallTool(tool, encoded)).json)
      body    <- ZIO.fromOption(json).orElseFail("no response")
      message <- ZIO.fromEither(Message.decode(body).left.map(_.json.toJson))
      result  <- message match
        case Message.Result(_, found) => ZIO.succeed(found)
        case other                    => ZIO.fail(other.json.toJson)
    yield result

  private def asObj[A: JsonEncoder](value: A): Either[String, Json.Obj] =
    value.toJsonAST.flatMap {
      case obj: Json.Obj => Right(obj)
      case other         => Left(other.toJson)
    }

  private def log(body: Json.Obj): Either[String, Log] =
    structured(body).flatMap(_.as[Log])

  private def failure(body: Json.Obj): Either[String, JobFailure] =
    structured(body).flatMap(_.get("value") match
      case Some(json) => json.as[JobFailure]
      case None       => Left("missing value"))

  private def structured(body: Json.Obj): Either[String, Json.Obj] =
    body.get("structuredContent") match
      case Some(obj: Json.Obj) => Right(obj)
      case _                   => Left(body.toJson)
end ExportCatchUpSpec
