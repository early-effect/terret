package terret.mcp

import heddle.*
import terret.Terret
import terret.find.{Enable, FindHint, FindResult, ToolRef}
import terret.milestone.Milestones
import terret.permission.{Ceiling, Permission}
import terret.prompt.Prompt
import terret.session.{SessionId, SessionIdError}
import terret.transcript.Entry
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.{Chunk, IO, ZIO}

private[mcp] object Jobs:
  def api(terret: Terret, version: String, tools: ToolSurface): Api[Milestones] =
    Api(TerretMcp.name, version, Some(TerretMcp.summary))
      .job(open)(openSession(terret, _))
      .job(run)(runTurn(terret, _))
      .job(status)(reported(terret, _))
      .job(resume)(replay(terret, _))
      .job(exportLog)(exported(terret, _))
      .job(cancel)(stop(terret, _))
      .job(find)(searched(tools, _))
      .job(call)(called(tools, _))
      .stream(follow)(followed(terret, _))

  private def fallible[In, Out](endpoint: Endpoint[In, Nothing, Out]): Endpoint[In, JobFailure, Out] =
    endpoint.outErrors[JobFailure](
      ErrorCase[JobFailure.WorkspaceDenied](Status.Forbidden),
      ErrorCase[JobFailure.WorkspaceInvalid](Status.BadRequest),
      ErrorCase[JobFailure.UnknownSession](Status.NotFound),
      ErrorCase[JobFailure.Busy](Status.Conflict),
      ErrorCase[JobFailure.EmptySession.type](Status.BadRequest),
      ErrorCase[JobFailure.BadCursor.type](Status.BadRequest),
      ErrorCase[JobFailure.BadLatest.type](Status.BadRequest),
      ErrorCase[JobFailure.Harness](Status.BadGateway),
      ErrorCase[JobFailure.UnknownModel](Status.BadRequest),
      ErrorCase[JobFailure.ModelsUnreadable](Status.BadGateway),
      ErrorCase[JobFailure.ModelsMalformed](Status.BadGateway),
      ErrorCase[JobFailure.EmptyQuery.type](Status.BadRequest),
      ErrorCase[JobFailure.BadLimit.type](Status.BadRequest),
      ErrorCase[JobFailure.NotCallable.type](Status.Forbidden),
      ErrorCase[JobFailure.NotGranted](Status.Forbidden),
      ErrorCase[JobFailure.UnknownServer](Status.NotFound),
      ErrorCase[JobFailure.BadArguments.type](Status.BadRequest),
      ErrorCase[JobFailure.BadTool.type](Status.BadRequest),
      ErrorCase[JobFailure.ToolUnavailable](Status.BadGateway),
    )

  private val openSummary =
    "Open a session on a workspace. model is optional. When set, it must be a [model.<name>] key in the grok config, " +
      "and a grok process terret spawns is started with --model. An unknown name fails and lists the known keys. " +
      "The result reports the chosen model. Omit model to keep grok's default."

  private val open =
    fallible(Endpoint.post("sessions").in[OpenJob].out[Opened])
      .name("open")
      .summary(openSummary)
      .mcp("open")

  private val runSummary =
    "Accept a prompt and start the turn. Returns when the turn is running, not when it finishes. " +
      "The turn keeps going if this call is cancelled. Poll status until the state is Ended, then export for the log. " +
      "cancel stops the turn."

  private val run =
    fallible(Endpoint.post("sessions" / "run").in[RunJob].out[RunAccepted])
      .name("run")
      .summary(runSummary)
      .mcp("run")

  private val status =
    fallible(Endpoint.post("sessions" / "status").in[SessionJob].out[StatusView])
      .name("status")
      .summary("Return the session state, including the outcome once the turn has ended.")
      .hints(Hint.ReadOnly, Hint.Idempotent)
      .mcp("status")

  private val resume =
    fallible(Endpoint.post("sessions" / "resume").in[SessionJob].out[Log])
      .name("resume")
      .summary("Replay the retained log")
      .hints(Hint.ReadOnly)
      .mcp("resume")

  private val exportSummary =
    "Return the retained log. after is exclusive and is applied first. milestones keeps only milestone events. " +
      "latest keeps the newest N of that selection. cursor is the tail of the whole log, even when the selection is shorter. " +
      "state is the session state."

  private val exportLog =
    fallible(Endpoint.post("sessions" / "export").in[ExportJob].out[Log])
      .name("export")
      .summary(exportSummary)
      .hints(Hint.ReadOnly)
      .mcp("export")

  private val followSummary =
    "Stream the retained log from the start, or strictly after an index. The event id is that index. " +
      "Dropping the stream does not stop the turn."

  private val follow =
    fallible(Endpoint.get("sessions" / string("session") / "events").query[Option[Long]]("after"))
      .name("follow")
      .summary(followSummary)
      .stream[WireEvent]

  private val cancel =
    fallible(Endpoint.post("sessions" / "cancel").in[SessionJob].out[Stopped])
      .name("cancel")
      .summary("Cancel the open turn")
      .hints(Hint.Destructive, Hint.Idempotent)
      .mcp("cancel")

  private val findSummary =
    "Search the indexed MCP tools. Hits are only tools the grant allows and .mcp.json defines. " +
      "withheld names a few stronger matches that were refused, and how to enable them. " +
      "An empty terret.mcp grant returns no hits and hint NoGrants."

  private val find =
    fallible(Endpoint.post("find").in[FindJob].out[Found])
      .name("find")
      .summary(findSummary)
      .hints(Hint.ReadOnly)
      .mcp("find")

  private val callSummary =
    "Call one MCP tool. arguments is a JSON object as text. A tool with no server is NotCallable. " +
      "A missing grant is NotGranted. A granted server that .mcp.json does not define is UnknownServer."

  private val call =
    fallible(Endpoint.post("call").in[CallJob].out[Called])
      .name("call")
      .summary(callSummary)
      .mcp("call")

  private def searched(tools: ToolSurface, job: FindJob): IO[JobFailure, Found] =
    tools.find(job.query, job.k).mapBoth(JobFailure.fromFind, shown)

  private def called(tools: ToolSurface, job: CallJob): IO[JobFailure, Called] =
    for
      _    <- arguments(job.arguments)
      text <- tools.call(job.server, job.tool, job.arguments).mapError(JobFailure.fromCall)
    yield Called(text)

  private def arguments(raw: String): IO[JobFailure, Unit] =
    raw.fromJson[Json] match
      case Right(_: Json.Obj) => ZIO.unit
      case _                  => ZIO.fail(JobFailure.BadArguments)

  private def shown(result: FindResult): Found =
    Found(
      result.hits.collect { case ToolRef(Some(server), tool, description) =>
        FoundTool(server.value, tool.value, description)
      },
      result.withheld.map(row => WithheldTool(row.server.value, row.tool.value, shownEnable(row.enable))),
      result.hint.map { case FindHint.NoGrants => WireHint.NoGrants },
    )

  private def shownEnable(enable: Enable): WireEnable =
    enable match
      case Enable.AddGrant(entry)      => WireEnable.AddGrant(entry)
      case Enable.DefineServer(server) => WireEnable.DefineServer(server.value)

  private def openSession(terret: Terret, job: OpenJob): IO[JobFailure, Opened] =
    for
      workspace <- workspaceOf(job.workspace)
      session   <- terret.open(workspace, Ceiling(job.allow.toSet), job.model).mapError(JobFailure.from)
    yield Opened(
      session.id.value,
      named(session.effective.allow),
      named(session.dropped),
      WireState.from(session.state),
      session.model.map(_.value),
    )

  private def runTurn(terret: Terret, job: RunJob): IO[JobFailure, RunAccepted] =
    for
      id      <- sessionOf(job.session)
      session <- terret.start(id, Prompt(job.prompt)).mapError(JobFailure.from)
    yield RunAccepted(job.session, WireState.from(session.state))

  private def reported(terret: Terret, job: SessionJob): IO[JobFailure, StatusView] =
    for
      id      <- sessionOf(job.session)
      session <- terret.status(id).mapError(JobFailure.from)
    yield StatusView(job.session, WireState.from(session.state))

  private def replay(terret: Terret, job: SessionJob): IO[JobFailure, Log] =
    for
      id      <- sessionOf(job.session)
      events  <- terret.resume(id).map(WireEvent.from).runCollect.mapError(JobFailure.from)
      session <- terret.status(id).mapError(JobFailure.from)
    yield Log(job.session, events, cursor(events.length), WireState.from(session.state))

  private def exported(terret: Terret, job: ExportJob): ZIO[Milestones, JobFailure, Log] =
    for
      _       <- bounded(job.after)
      _       <- positive(job.latest)
      id      <- sessionOf(job.session)
      page    <- terret.page(id, job.after).mapError(JobFailure.from)
      session <- terret.status(id).mapError(JobFailure.from)
      kept    <- select(page.events, job.milestones.contains(true), job.latest)
    yield Log(
      job.session,
      kept.map(entry => WireEvent.from(entry.event)),
      page.cursor,
      WireState.from(session.state),
    )

  private def followed(
      terret: Terret,
      in: (String, Option[Long]),
  ): IO[JobFailure, ZStream[Any, JobFailure, Observed[WireEvent]]] =
    val (raw, after) = in
    for
      _  <- bounded(after)
      id <- sessionOf(raw)
      _  <- terret.status(id).mapError(JobFailure.from)
    yield terret.follow(id, after).mapError(JobFailure.from).map { entry =>
      Observed(SseField.of(entry.index), WireEvent.from(entry.event))
    }

  private def bounded(after: Option[Long]): IO[JobFailure, Unit] =
    after match
      case Some(n) if n < 0 => ZIO.fail(JobFailure.BadCursor)
      case _                => ZIO.unit

  private def positive(latest: Option[Int]): IO[JobFailure, Unit] =
    latest match
      case Some(n) if n < 1 => ZIO.fail(JobFailure.BadLatest)
      case _                => ZIO.unit

  private def select(
      entries: Chunk[Entry],
      milestones: Boolean,
      latest: Option[Int],
  ): ZIO[Milestones, Nothing, Chunk[Entry]] =
    val chosen =
      if milestones then
        ZIO.serviceWithZIO[Milestones] { service =>
          ZIO.foldLeft(entries)(Chunk.empty[Entry]) { (kept, entry) =>
            service.classify(entry.event).map {
              case Some(_) => kept :+ entry
              case None    => kept
            }
          }
        }
      else ZIO.succeed(entries)
    chosen.map(rows => Milestones.latest(rows, latest))

  private def cursor(length: Int): Option[Long] =
    if length == 0 then None else Some(length.toLong - 1)

  private def stop(terret: Terret, job: SessionJob): IO[JobFailure, Stopped] =
    for
      id <- sessionOf(job.session)
      _  <- terret.cancel(id).mapError(JobFailure.from)
    yield Stopped(job.session)

  private def sessionOf(raw: String): IO[JobFailure, SessionId] =
    ZIO.fromEither(SessionId.from(raw)).mapError { case SessionIdError.Empty =>
      JobFailure.EmptySession
    }

  private def workspaceOf(raw: String): IO[JobFailure, Workspace] =
    ZIO.fromEither(Workspace.from(raw)).mapError(error => JobFailure.WorkspaceInvalid(WorkspaceProblem.from(error)))

  private def named(allow: Set[Permission]): Chunk[Permission] =
    Chunk.fromIterable(Permission.values).filter(allow.contains)
end Jobs
