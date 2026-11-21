package terret.mcp

import heddle.*
import terret.Terret
import terret.permission.{Ceiling, Permission}
import terret.prompt.Prompt
import terret.session.{SessionId, SessionIdError}
import terret.workspace.Workspace
import zio.stream.ZStream
import zio.{Chunk, IO, ZIO}

private[mcp] object Jobs:
  def api(terret: Terret, version: String): Api[Any] =
    Api(TerretMcp.name, version, Some(TerretMcp.summary))
      .job(open)(openSession(terret, _))
      .job(run)(runTurn(terret, _))
      .job(status)(reported(terret, _))
      .job(resume)(replay(terret, _))
      .job(exportLog)(exported(terret, _))
      .job(cancel)(stop(terret, _))
      .stream(follow)(followed(terret, _))

  private def fallible[In, Out](endpoint: Endpoint[In, Nothing, Out]): Endpoint[In, JobFailure, Out] =
    endpoint.outErrors[JobFailure](
      ErrorCase[JobFailure.WorkspaceDenied](Status.Forbidden),
      ErrorCase[JobFailure.WorkspaceInvalid](Status.BadRequest),
      ErrorCase[JobFailure.UnknownSession](Status.NotFound),
      ErrorCase[JobFailure.Busy](Status.Conflict),
      ErrorCase[JobFailure.EmptySession.type](Status.BadRequest),
      ErrorCase[JobFailure.BadCursor.type](Status.BadRequest),
      ErrorCase[JobFailure.Harness](Status.BadGateway),
    )

  private val open =
    fallible(Endpoint.post("sessions").in[OpenJob].out[Opened])
      .name("open")
      .summary("Open a session on a workspace")
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

  private val exportLog =
    fallible(Endpoint.post("sessions" / "export").in[ExportJob].out[Log])
      .name("export")
      .summary("Return the retained log. after is exclusive. cursor is the last retained index.")
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

  private def openSession(terret: Terret, job: OpenJob): IO[JobFailure, Opened] =
    for
      workspace <- workspaceOf(job.workspace)
      session   <- terret.open(workspace, Ceiling(job.allow.toSet)).mapError(JobFailure.from)
    yield Opened(
      session.id.value,
      named(session.effective.allow),
      named(session.dropped),
      WireState.from(session.state),
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
      id     <- sessionOf(job.session)
      events <- terret.resume(id).map(WireEvent.from).runCollect.mapError(JobFailure.from)
    yield Log(job.session, events, cursor(events.length))

  private def exported(terret: Terret, job: ExportJob): IO[JobFailure, Log] =
    for
      _    <- bounded(job.after)
      id   <- sessionOf(job.session)
      page <- terret.page(id, job.after).mapError(JobFailure.from)
    yield Log(job.session, page.events.map(entry => WireEvent.from(entry.event)), page.cursor)

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
