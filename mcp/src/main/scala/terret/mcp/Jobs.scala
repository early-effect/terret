package terret.mcp

import heddle.*
import terret.Terret
import terret.permission.{Ceiling, Permission}
import terret.prompt.Prompt
import terret.session.{SessionId, SessionIdError}
import terret.workspace.Workspace
import zio.{Chunk, IO, ZIO}

private[mcp] object Jobs:
  def api(terret: Terret, version: String): Api[Any] =
    Api(TerretMcp.name, version, Some(TerretMcp.summary))
      .job(open)(openSession(terret, _))
      .job(run)(runTurn(terret, _))
      .job(resume)(replay(terret, _))
      .job(exportLog)(exported(terret, _))
      .job(cancel)(stop(terret, _))

  private def fallible[In, Out](endpoint: Endpoint[In, Nothing, Out]): Endpoint[In, JobFailure, Out] =
    endpoint.outErrors[JobFailure](
      ErrorCase[JobFailure.WorkspaceDenied](Status.Forbidden),
      ErrorCase[JobFailure.WorkspaceInvalid](Status.BadRequest),
      ErrorCase[JobFailure.UnknownSession](Status.NotFound),
      ErrorCase[JobFailure.Busy](Status.Conflict),
      ErrorCase[JobFailure.EmptySession.type](Status.BadRequest),
      ErrorCase[JobFailure.Unfinished](Status.BadGateway),
      ErrorCase[JobFailure.Harness](Status.BadGateway),
    )

  private val open =
    fallible(Endpoint.post("sessions").in[OpenJob].out[Opened])
      .name("open")
      .summary("Open a session on a workspace")
      .mcp("open")

  private val run =
    fallible(Endpoint.post("sessions" / "run").in[RunJob].out[RunDone])
      .name("run")
      .summary("Send a prompt")
      .mcp("run")

  private val resume =
    fallible(Endpoint.post("sessions" / "resume").in[SessionJob].out[Log])
      .name("resume")
      .summary("Replay the retained log")
      .hints(Hint.ReadOnly)
      .mcp("resume")

  private val exportLog =
    fallible(Endpoint.post("sessions" / "export").in[SessionJob].out[Log])
      .name("export")
      .summary("Return the retained log")
      .hints(Hint.ReadOnly)
      .mcp("export")

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
    yield Opened(session.id.value, named(session.effective.allow), named(session.dropped), session.status)

  private def runTurn(terret: Terret, job: RunJob): IO[JobFailure, RunDone] =
    for
      id     <- sessionOf(job.session)
      events <- terret
                  .run(id, Prompt(job.prompt))
                  .mapZIO(event => Notice.publish(event).as(WireEvent.from(event)))
                  .runCollect
                  .mapError(JobFailure.from)
      outcome <- finished(job.session, events)
    yield RunDone(job.session, outcome)

  private def replay(terret: Terret, job: SessionJob): IO[JobFailure, Log] =
    for
      id     <- sessionOf(job.session)
      events <- terret.resume(id).map(WireEvent.from).runCollect.mapError(JobFailure.from)
    yield Log(job.session, events)

  private def exported(terret: Terret, job: SessionJob): IO[JobFailure, Log] =
    for
      id  <- sessionOf(job.session)
      log <- terret.transcript(id).mapError(JobFailure.from)
    yield Log(log.session.value, log.events.map(WireEvent.from))

  private def stop(terret: Terret, job: SessionJob): IO[JobFailure, Stopped] =
    for
      id <- sessionOf(job.session)
      _  <- terret.cancel(id).mapError(JobFailure.from)
    yield Stopped(job.session)

  private def finished(session: String, events: Chunk[WireEvent]): IO[JobFailure, WireOutcome] =
    val outcomes = events.collect { case WireEvent.TurnFinished(outcome) => outcome }
    ZIO.fromOption(outcomes.lastOption).orElseFail(JobFailure.Unfinished(session))

  private def sessionOf(raw: String): IO[JobFailure, SessionId] =
    ZIO.fromEither(SessionId.from(raw)).mapError { case SessionIdError.Empty =>
      JobFailure.EmptySession
    }

  private def workspaceOf(raw: String): IO[JobFailure, Workspace] =
    ZIO.fromEither(Workspace.from(raw)).mapError(error => JobFailure.WorkspaceInvalid(WorkspaceProblem.from(error)))

  private def named(allow: Set[Permission]): Chunk[Permission] =
    Chunk.fromIterable(Permission.values).filter(allow.contains)
end Jobs
