package terret

import terret.config.TerretConfig
import terret.error.TerretError
import terret.event.{Event, TurnOutcome}
import terret.harness.Harness
import terret.hooks.Hooks
import terret.permission.Ceiling
import terret.prompt.Prompt
import terret.session.{Session, SessionId, SessionStatus}
import terret.transcript.Transcript
import terret.workspace.Workspace
import zio.stream.ZStream
import zio.{IO, Promise, Ref, UIO, ZIO, ZLayer}

final class Terret private (
    harness: Harness,
    config: TerretConfig,
    hooks: Hooks,
    sessions: Ref[Map[SessionId, Session]],
    turns: Ref[Map[SessionId, Promise[TerretError, Unit]]],
):
  def open(workspace: Workspace, requested: Ceiling = Ceiling.all): IO[TerretError, Session] =
    confine(workspace).flatMap { _ =>
      val (effective, dropped) = requested.clamp(config.policy.ceiling)
      harness.open(workspace, effective, config.policy).mapError(TerretError.Harness(_)).flatMap { id =>
        val session = Session(id, workspace, effective, dropped, SessionStatus.Open)
        sessions.update(_.updated(id, session)).as(session)
      }
    }

  def run(id: SessionId, prompt: Prompt): ZStream[Any, TerretError, Event] =
    ZStream.unwrapScoped {
      for
        (session, previous) <- begin(id)
        started             <- Ref.make(false)
        gate                <- Promise.make[TerretError, Unit]
        _                   <- turns.update(_.updated(id, gate))
        _                   <- ZIO.addFinalizer(release(id, previous, started) *> turns.update(_.removed(id)))
        revised             <- hooks.beforePrompt(session, prompt)
        _                   <- started.set(true)
      yield live(session, revised).interruptWhen(gate)
    }

  def resume(id: SessionId): ZStream[Any, TerretError, Event] =
    ZStream.fromZIO(require(id)).flatMap { _ =>
      harness.resume(id).mapError(TerretError.Harness(_))
    }

  def transcript(id: SessionId): IO[TerretError, Transcript] =
    require(id) *> harness.transcript(id).mapError(TerretError.Harness(_))

  def cancel(id: SessionId): IO[TerretError, Unit] =
    require(id) *> halt(id)

  private def live(session: Session, prompt: Prompt): ZStream[Any, TerretError, Event] =
    harness
      .prompt(session.id, prompt)
      .mapError(TerretError.Harness(_))
      .tap(event => observe(session, event))
      .catchAll { error =>
        ZStream.fromZIO(setStatus(session.id, SessionStatus.Failed)) *> ZStream.fail(error)
      }

  private def observe(session: Session, event: Event): UIO[Unit] =
    val status = event match
      case Event.TurnFinished(outcome) => finished(outcome)
      case _                            => SessionStatus.Running
    setStatus(session.id, status) *> hooks.onEvent(session, event)

  private def finished(outcome: TurnOutcome): SessionStatus =
    outcome match
      case TurnOutcome.Completed  => SessionStatus.Finished
      case TurnOutcome.Cancelled  => SessionStatus.Cancelled
      case TurnOutcome.Failed(_) => SessionStatus.Failed

  private def begin(id: SessionId): IO[TerretError, (Session, SessionStatus)] =
    sessions.modify { rows =>
      rows.get(id) match
        case None =>
          (Left(TerretError.UnknownSession(id)), rows)
        case Some(session) if session.status == SessionStatus.Running =>
          (Left(TerretError.Busy(id)), rows)
        case Some(session) =>
          val running = session.copy(status = SessionStatus.Running)
          (Right((running, session.status)), rows.updated(id, running))
    }.flatMap(ZIO.fromEither)

  private def release(id: SessionId, previous: SessionStatus, started: Ref[Boolean]): UIO[Unit] =
    started.get.flatMap {
      case false => setStatus(id, previous)
      case true  =>
        sessions.get.flatMap { rows =>
          rows.get(id).map(_.status) match
            case Some(SessionStatus.Running) => halt(id).catchAll(_ => setStatus(id, SessionStatus.Failed))
            case _                           => ZIO.unit
        }
    }

  private def halt(id: SessionId): IO[TerretError, Unit] =
    for
      _    <- setStatus(id, SessionStatus.Cancelled)
      gate <- turns.get.map(_.get(id))
      _    <- ZIO.foreachDiscard(gate)(_.succeed(()))
      _    <- harness.cancel(id).mapError(TerretError.Harness(_))
    yield ()

  private def require(id: SessionId): IO[TerretError, Session] =
    sessions.get.map(_.get(id)).flatMap {
      case Some(session) => ZIO.succeed(session)
      case None          => ZIO.fail(TerretError.UnknownSession(id))
    }

  private def setStatus(id: SessionId, status: SessionStatus): UIO[Unit] =
    sessions.update { rows =>
      rows.get(id) match
        case None          => rows
        case Some(session) => rows.updated(id, session.copy(status = status))
    }

  private def confine(workspace: Workspace): IO[TerretError, Unit] =
    config.root match
      case None => ZIO.unit
      case Some(root) =>
        if workspace.path.normalize.startsWith(root.path.normalize) then ZIO.unit
        else ZIO.fail(TerretError.WorkspaceDenied(workspace, root))
end Terret

object Terret:
  def make(harness: Harness, config: TerretConfig, hooks: Hooks): UIO[Terret] =
    for
      sessions <- Ref.make(Map.empty[SessionId, Session])
      turns    <- Ref.make(Map.empty[SessionId, Promise[TerretError, Unit]])
    yield Terret(harness, config, hooks, sessions, turns)

  val layer: ZLayer[Harness & TerretConfig & Hooks, Nothing, Terret] =
    ZLayer.fromZIO {
      for
        harness <- ZIO.service[Harness]
        config  <- ZIO.service[TerretConfig]
        hooks   <- ZIO.service[Hooks]
        terret  <- make(harness, config, hooks)
      yield terret
    }
end Terret
