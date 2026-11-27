package terret

import terret.config.TerretConfig
import terret.error.TerretError
import terret.event.{Event, TurnOutcome}
import terret.harness.{Harness, HarnessFailure, ProtocolFault}
import terret.hooks.Hooks
import terret.milestone.MilestoneSink
import terret.model.{ModelCatalog, ModelKey}
import terret.permission.Ceiling
import terret.prompt.Prompt
import terret.session.{Session, SessionId, SessionState}
import terret.transcript.{Entry, Page, Transcript}
import terret.workspace.Workspace
import zio.stream.ZStream
import zio.{Chunk, Exit, Fiber, IO, Ref, Scope, UIO, URIO, ZIO, ZLayer}

final class Terret private (
    harness: Harness,
    config: TerretConfig,
    hooks: Hooks,
    sessions: Ref[Map[SessionId, Terret.Held]],
    scope: Scope,
    closed: Ref[Boolean],
    sink: MilestoneSink,
    catalog: ModelCatalog,
):
  def open(
      workspace: Workspace,
      requested: Ceiling = Ceiling.all,
      model: Option[String] = None,
  ): IO[TerretError, Session] =
    confine(workspace).flatMap { _ =>
      resolve(model).flatMap { chosen =>
        val (effective, dropped) = requested.clamp(config.policy.ceiling)
        harness.open(workspace, effective, config.policy, chosen).mapError(TerretError.Harness(_)).flatMap { id =>
          val session = Session(id, workspace, effective, dropped, SessionState.Open, chosen)
          sessions.update(_.updated(id, Terret.Held(session, 0L, None))).as(session)
        }
      }
    }

  /** Caller scope. Interrupting the stream cancels the harness. */
  def run(id: SessionId, prompt: Prompt): ZStream[Any, TerretError, Event] =
    ZStream.unwrapScoped(acquire(id, prompt).map(_._2))

  /** Terret's scope. Interrupting the caller does not cancel the harness. `cancel` does. */
  def start(id: SessionId, prompt: Prompt): IO[TerretError, Session] =
    ZIO.uninterruptible {
      scope.fork.flatMap { child =>
        child
          .extend(acquire(id, prompt))
          .flatMap { (session, events) =>
            val drain = events.runDrain.onExit(exit => child.close(exit))
            // forkIn copies this uninterruptible region onto the fiber. An uninterruptible
            // drain ignores scope close, and the close waits forever. The drain sets its own
            // interrupt status. The fork stays in this region, so the launch is not the hole.
            drain.interruptible.forkIn(scope).flatMap { fiber =>
              sessions
                .modify { rows =>
                  rows.get(session.id) match
                    case Some(held) => (false, rows.updated(session.id, held.copy(turn = Some(fiber))))
                    case None       => (true, rows)
                }
                .flatMap {
                  case true  => fiber.interrupt.as(session)
                  case false => ZIO.succeed(session)
                }
            }
          }
          .tapError(_ => child.close(Exit.unit))
      }
    }

  def resume(id: SessionId): ZStream[Any, TerretError, Event] =
    ZStream.fromZIO(require(id)).flatMap { _ =>
      harness.resume(id).mapError(TerretError.Harness(_))
    }

  def transcript(id: SessionId): IO[TerretError, Transcript] =
    require(id) *> harness.transcript(id).mapError(TerretError.Harness(_))

  def page(id: SessionId, after: Option[Long]): IO[TerretError, Page] =
    require(id) *> harness.page(id, after).mapError(TerretError.Harness(_))

  /** Stays open across turns. Interrupting this stream does not cancel the harness. */
  def follow(id: SessionId, after: Option[Long]): ZStream[Any, TerretError, Entry] =
    ZStream.fromZIO(require(id)).flatMap { _ =>
      harness.follow(id, after).mapError(TerretError.Harness(_))
    }

  def status(id: SessionId): IO[TerretError, Session] =
    require(id)

  /** Sessions this process still holds, with the state of each one. */
  def list: UIO[Chunk[Session]] =
    sessions.get.map { rows =>
      Chunk.fromIterable(rows.values.map(_.session)).sortBy(_.id.value)
    }

  def cancel(id: SessionId): IO[TerretError, Unit] =
    require(id) *> halt(id).mapError(TerretError.Harness(_))

  /** Cancel every session, wait for its turn to finish, then close harness children. */
  def shutdown: UIO[Unit] =
    closed.modify(already => (already, true)).flatMap {
      case true  => ZIO.unit
      case false =>
        sessions.get.flatMap { rows =>
          ZIO.foreachDiscard(rows) { (id, held) =>
            halt(id).ignore *> ZIO.foreachDiscard(held.turn)(_.interrupt)
          }
        } *> harness.close
    }

  private def acquire(
      id: SessionId,
      prompt: Prompt,
  ): ZIO[Scope, TerretError, (Session, ZStream[Any, TerretError, Event])] =
    for
      (session, previous, epoch) <- begin(id)
      _                          <- harness.markTurn(id, epoch)
      started                    <- Ref.make(false)
      _                          <- ZIO.addFinalizer(release(id, previous, started, epoch))
      revised                    <- hooks.beforePrompt(session, prompt)
      _                          <- started.set(true)
      cursor                     <- cursorOf(id)
      _                          <- sink.started(id, cursor, epoch)
    // The harness finalizer flushes an open text run. A gate around this stream would discard that run.
    yield (session, live(session, revised, epoch))

  private def live(session: Session, prompt: Prompt, epoch: Long): ZStream[Any, TerretError, Event] =
    harness
      .prompt(session.id, prompt)
      .mapError(TerretError.Harness(_))
      .tap(event => observe(session, event, epoch))
      .catchAll { error =>
        ZStream.fromZIO {
          val outcome = failed(error)
          setIfRunning(session.id, epoch, SessionState.Ended(outcome)).flatMap {
            case false => ZIO.unit
            case true  => cursorOf(session.id).flatMap(cursor => sink.failed(session.id, cause(error), cursor, epoch))
          }
        } *> ZStream.fail(error)
      }

  private def observe(session: Session, event: Event, epoch: Long): UIO[Unit] =
    val state = event match
      case Event.TurnFinished(outcome) => SessionState.Ended(outcome)
      case _                           => SessionState.Running
    setIfRunning(session.id, epoch, state).flatMap {
      case true  => hooks.onEvent(session, event)
      case false => ZIO.unit
    }

  private def failed(error: TerretError): TurnOutcome =
    TurnOutcome.Failed(cause(error))

  private def cause(error: TerretError): HarnessFailure =
    error match
      case TerretError.Harness(failure)      => failure
      case TerretError.UnknownSession(id)    => HarnessFailure.UnknownSession(id)
      case TerretError.Busy(_)               => HarnessFailure.Protocol(ProtocolFault.Incomplete)
      case TerretError.WorkspaceDenied(_, _) => HarnessFailure.Protocol(ProtocolFault.Incomplete)
      case TerretError.WorkspaceInvalid(_)   => HarnessFailure.Protocol(ProtocolFault.Incomplete)
      case TerretError.UnknownModel(_, _)    => HarnessFailure.Protocol(ProtocolFault.Incomplete)
      case TerretError.Models(_)             => HarnessFailure.Protocol(ProtocolFault.Incomplete)

  private def cursorOf(id: SessionId): UIO[Option[Long]] =
    harness.page(id, None).map(_.cursor).catchAll(_ => ZIO.succeed(None))

  private def begin(id: SessionId): IO[TerretError, (Session, SessionState, Long)] =
    sessions
      .modify { rows =>
        rows.get(id) match
          case None =>
            (Left(TerretError.UnknownSession(id)), rows)
          case Some(Terret.Held(session, _, _)) if session.state == SessionState.Running =>
            (Left(TerretError.Busy(id)), rows)
          case Some(Terret.Held(session, epoch, turn)) =>
            val running = session.copy(state = SessionState.Running)
            val next    = epoch + 1
            (Right((running, session.state, next)), rows.updated(id, Terret.Held(running, next, turn)))
      }
      .flatMap(ZIO.fromEither)

  // The epoch is the turn `begin` installed. A finalizer that loses the race with the next
  // turn must not halt that turn: its state is Running too.
  private def release(id: SessionId, previous: SessionState, started: Ref[Boolean], epoch: Long): UIO[Unit] =
    started.get.flatMap {
      case false => setState(id, previous)
      case true  =>
        setIfRunning(id, epoch, SessionState.Ended(TurnOutcome.Cancelled)).flatMap {
          case false => ZIO.unit
          case true  =>
            harness.cancel(id).catchAll(failure => setState(id, SessionState.Ended(TurnOutcome.Failed(failure))))
        }
    }

  private def halt(id: SessionId): IO[HarnessFailure, Unit] =
    setState(id, SessionState.Ended(TurnOutcome.Cancelled)) *> harness.cancel(id)

  private def require(id: SessionId): IO[TerretError, Session] =
    sessions.get.map(_.get(id).map(_.session)).flatMap {
      case Some(session) => ZIO.succeed(session)
      case None          => ZIO.fail(TerretError.UnknownSession(id))
    }

  // A flushed event can land after cancel, or after the next turn has begun. Only the turn that is
  // still Running for this epoch may publish state.
  private def setIfRunning(id: SessionId, epoch: Long, state: SessionState): UIO[Boolean] =
    sessions.modify { rows =>
      rows.get(id) match
        case Some(held @ Terret.Held(current, `epoch`, _)) if current.state == SessionState.Running =>
          (true, rows.updated(id, held.copy(session = current.copy(state = state))))
        case _ =>
          (false, rows)
    }

  private def setState(id: SessionId, state: SessionState): UIO[Unit] =
    sessions.update { rows =>
      rows.get(id) match
        case None       => rows
        case Some(held) => rows.updated(id, held.copy(session = held.session.copy(state = state)))
    }

  private def confine(workspace: Workspace): IO[TerretError, Unit] =
    config.root match
      case None       => ZIO.unit
      case Some(root) =>
        if workspace.path.normalize.startsWith(root.path.normalize) then ZIO.unit
        else ZIO.fail(TerretError.WorkspaceDenied(workspace, root))

  private def resolve(model: Option[String]): IO[TerretError, Option[ModelKey]] =
    model match
      case None       => ZIO.succeed(None)
      case Some(name) =>
        catalog.known.mapError(TerretError.Models(_)).flatMap { keys =>
          ZIO.fromEither(ModelKey.choose(Some(name), keys)).mapError { unknown =>
            TerretError.UnknownModel(unknown.name, unknown.known)
          }
        }
end Terret

object Terret:
  private final case class Held(session: Session, epoch: Long, turn: Option[Fiber[TerretError, Unit]])

  def make(
      harness: Harness,
      config: TerretConfig,
      hooks: Hooks,
      milestones: MilestoneSink = MilestoneSink.discard,
      catalog: ModelCatalog = ModelCatalog.empty,
  ): URIO[Scope, Terret] =
    for
      sessions <- Ref.make(Map.empty[SessionId, Held])
      scope    <- ZIO.scope
      closed   <- Ref.make(false)
    yield Terret(harness, config, hooks, sessions, scope, closed, milestones, catalog)

  val layer: ZLayer[Harness & TerretConfig & Hooks & ModelCatalog, Nothing, Terret] =
    ZLayer.scoped {
      for
        harness <- ZIO.service[Harness]
        config  <- ZIO.service[TerretConfig]
        hooks   <- ZIO.service[Hooks]
        catalog <- ZIO.service[ModelCatalog]
        terret  <- make(harness, config, hooks, catalog = catalog)
      yield terret
    }
end Terret
