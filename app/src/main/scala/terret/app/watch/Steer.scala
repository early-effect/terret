package terret.app.watch

import terret.Terret
import terret.error.TerretError
import terret.event.Event
import terret.harness.HarnessFailure
import terret.permission.{Ceiling, SessionTools}
import terret.prompt.Prompt
import terret.session.{Session, SessionId, SessionState}
import terret.workspace.Workspace
import zio.{Chunk, Clock, IO, Ref, Scope, Semaphore, UIO, URIO, ZIO}

import java.time.Instant

/** One prompt waiting for the current turn to finish. */
final case class Queued(id: String, text: String)

/** A session opened from the watch form. `allow` is the requested ceiling. `tools` is the MCP grant, empty unless the
  * form named a server.
  */
final case class OpenRequest(
    workspace: String,
    model: Option[String],
    allow: Ceiling,
    tools: SessionTools,
)

enum SteerError:
  case Unknown(id: SessionId)
  case Busy(id: SessionId)
  case EmptyPrompt
  case Oversize
  case UnknownItem(id: String)
  case Workspace(reason: WatchPathError)
  case BadAllow(name: String)
  case Mcp(detail: String)
  case Model(name: String, known: Chunk[String])
  case Models
  case Harness(failure: HarnessFailure)

/** Queues a prompt while a turn is running, and starts it when that turn ends. Stop and send cancels the running turn
  * and then starts this prompt ahead of the queue.
  */
trait Steer:
  def send(id: SessionId, text: String, actor: String): IO[SteerError, Unit]
  def stopAndSend(id: SessionId, text: String, actor: String): IO[SteerError, Unit]
  def edit(id: SessionId, item: String, text: String, actor: String): IO[SteerError, Unit]
  def drop(id: SessionId, item: String, actor: String): IO[SteerError, Unit]
  def cancel(id: SessionId, actor: String): IO[SteerError, Unit]
  def resume(id: SessionId, actor: String): IO[SteerError, Unit]
  def open(request: OpenRequest, actor: String): IO[SteerError, Session]
  def queue(id: SessionId): UIO[Chunk[Queued]]

object Steer:
  val PromptLimit: Int = 16384

  def make(terret: Terret, roots: Chunk[Workspace]): URIO[Scope, Steer] =
    for
      scope <- ZIO.scope
      lanes <- Ref.make(Map.empty[SessionId, Steer.Lane])
      armed <- Ref.make(Set.empty[SessionId])
      lock  <- Semaphore.make(1)
    yield SteerLive(terret, roots, scope, lanes, armed, lock)

  private[watch] final case class Item(id: String, text: String, actor: String)
  private[watch] final case class Lane(queue: Chunk[Item], next: Long)

  private[watch] object Lane:
    val empty: Lane = Lane(Chunk.empty, 1L)
end Steer

private final class SteerLive(
    terret: Terret,
    roots: Chunk[Workspace],
    scope: Scope,
    lanes: Ref[Map[SessionId, Steer.Lane]],
    armed: Ref[Set[SessionId]],
    lock: Semaphore,
) extends Steer:

  def send(id: SessionId, text: String, actor: String): IO[SteerError, Unit] =
    checked(text).flatMap { prompt =>
      exclusive(id) { lane =>
        for
          session <- terret.status(id).mapError(lift)
          at      <- Clock.instant
          next    <-
            if session.state == SessionState.Running then enqueue(id, lane, prompt, actor, at)
            else
              launch(id, prompt, actor).foldZIO(
                {
                  case SteerError.Busy(_) => enqueue(id, lane, prompt, actor, at)
                  case other              => ZIO.fail(other)
                },
                _ => ZIO.succeed(lane),
              )
        yield (next, ())
      }
    } *> arm(id)

  def stopAndSend(id: SessionId, text: String, actor: String): IO[SteerError, Unit] =
    checked(text).flatMap { prompt =>
      exclusive(id) { lane =>
        for
          session <- terret.status(id).mapError(lift)
          at      <- Clock.instant
          _       <- ZIO.whenDiscard(session.state == SessionState.Running) {
            terret.cancel(id).mapError(lift) *>
              terret.note(id, Event.TurnCancelled(actor, at)).mapError(lift)
          }
          _ <- launch(id, prompt, actor)
        yield (lane, ())
      }
    } *> arm(id)

  def edit(id: SessionId, item: String, text: String, actor: String): IO[SteerError, Unit] =
    checked(text).flatMap { prompt =>
      exclusive(id) { lane =>
        if lane.queue.exists(_.id == item) then
          val next = lane.queue.map { found =>
            if found.id == item then found.copy(text = prompt, actor = actor) else found
          }
          ZIO.succeed((lane.copy(queue = next), ()))
        else ZIO.fail(SteerError.UnknownItem(item))
      }
    }

  def drop(id: SessionId, item: String, actor: String): IO[SteerError, Unit] =
    val _ = actor
    exclusive(id) { lane =>
      if lane.queue.exists(_.id == item) then
        ZIO.succeed((lane.copy(queue = lane.queue.filter(_.id != item)), ()))
      else ZIO.fail(SteerError.UnknownItem(item))
    }

  def cancel(id: SessionId, actor: String): IO[SteerError, Unit] =
    for
      at <- Clock.instant
      _  <- exclusive(id) { lane =>
        terret.cancel(id).mapError(lift) *>
          terret.note(id, Event.TurnCancelled(actor, at)).mapError(lift).as((lane, ()))
      }
      _ <- pump(id)
    yield ()

  def resume(id: SessionId, actor: String): IO[SteerError, Unit] =
    terret.resume(id).runDrain.mapError(lift) *>
      Clock.instant.flatMap { at =>
        terret.note(id, Event.SessionResumed(actor, at)).mapError(lift)
      }

  def open(request: OpenRequest, actor: String): IO[SteerError, Session] =
    for
      workspace <- ZIO.fromEither(WatchRoots.admit(request.workspace, roots)).mapError(SteerError.Workspace(_))
      session   <- terret.open(workspace, request.allow, request.model, request.tools).mapError(lift)
      at        <- Clock.instant
      _         <- terret
        .note(
          session.id,
          Event.SessionOpened(actor, workspace.path.toString, session.model.map(_.value), at),
        )
        .mapError(lift)
      _ <- arm(session.id)
    yield session

  def queue(id: SessionId): UIO[Chunk[Queued]] =
    lanes.get.map(_.get(id).fold(Chunk.empty)(_.queue.map(item => Queued(item.id, item.text))))

  private def checked(text: String): IO[SteerError, String] =
    val prompt = text.trim
    if prompt.isEmpty then ZIO.fail(SteerError.EmptyPrompt)
    else if prompt.length > Steer.PromptLimit then ZIO.fail(SteerError.Oversize)
    else ZIO.succeed(prompt)

  private def enqueue(
      id: SessionId,
      lane: Steer.Lane,
      prompt: String,
      actor: String,
      at: Instant,
  ): IO[SteerError, Steer.Lane] =
    val item = Steer.Item(s"q${lane.next}", prompt, actor)
    val next = lane.copy(queue = lane.queue :+ item, next = lane.next + 1L)
    terret.note(id, Event.SteerQueued(actor, item.id, prompt, at)).mapError(lift).as(next)

  private def launch(id: SessionId, prompt: String, actor: String): IO[SteerError, Unit] =
    Clock.instant.flatMap { at =>
      terret.start(id, Prompt(prompt)).mapError(lift) *>
        terret.note(id, Event.SteerSent(actor, prompt, at)).mapError(lift)
    }

  private def pump(id: SessionId): IO[SteerError, Unit] =
    exclusive(id) { lane =>
      terret.status(id).mapError(lift).flatMap { session =>
        if session.state == SessionState.Running then ZIO.succeed((lane, ()))
        else
          lane.queue.headOption match
            case None => ZIO.succeed((lane, ()))
            case Some(item) =>
              launch(id, item.text, item.actor).flatMap { _ =>
                ZIO.succeed((lane.copy(queue = lane.queue.drop(1)), ()))
              }
      }
    }

  private def exclusive[A](id: SessionId)(use: Steer.Lane => IO[SteerError, (Steer.Lane, A)]): IO[SteerError, A] =
    lock.withPermit {
      lanes.get.map(_.getOrElse(id, Steer.Lane.empty)).flatMap { lane =>
        use(lane).flatMap { (next, value) =>
          lanes.update(_.updated(id, next)).as(value)
        }
      }
    }

  private def arm(id: SessionId): UIO[Unit] =
    armed.modify { ids =>
      if ids.contains(id) then (false, ids) else (true, ids + id)
    }.flatMap {
      case false => ZIO.unit
      case true  =>
        terret
          .follow(id, None)
          .foreach { entry =>
            entry.event match
              case Event.TurnFinished(_) => pump(id).ignore
              case _                     => ZIO.unit
          }
          .ignore
          .forkIn(scope)
          .unit
    }

  private def lift(error: TerretError): SteerError =
    error match
      case TerretError.UnknownSession(id)        => SteerError.Unknown(id)
      case TerretError.Busy(id)                  => SteerError.Busy(id)
      case TerretError.WorkspaceDenied(_, _)     => SteerError.Workspace(WatchPathError.Outside)
      case TerretError.WorkspaceInvalid(_)       => SteerError.Workspace(WatchPathError.Invalid)
      case TerretError.UnknownModel(name, known) => SteerError.Model(name, known)
      case TerretError.Models(_)                 => SteerError.Models
      case TerretError.Harness(failure)          => SteerError.Harness(failure)
end SteerLive
