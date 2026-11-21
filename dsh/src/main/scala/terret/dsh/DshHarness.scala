package terret.dsh

import heddle.LinePipe
import terret.event.Event
import terret.harness.{Harness, HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.transcript.{Entry, Journal, Page, Retain, Transcript}
import terret.workspace.Workspace
import zio.stream.ZStream
import zio.{IO, Promise, Random, Ref, Scope, UIO, ZIO, ZLayer}

final class DshHarness private (
    config: DshConfig,
    spawner: Spawner,
    sessions: Ref[Map[SessionId, DshHarness.Slot]],
    gates: Ref[Map[SessionId, Promise[HarnessFailure, Unit]]],
    journal: Journal,
) extends Harness:

  def open(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): IO[HarnessFailure, SessionId] =
    for
      uuid <- Random.nextUUID
      id   <- ZIO
        .fromEither(SessionId.from(uuid.toString))
        .mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete))
      _ <- journal.open(id)
      _ <- sessions.update(_.updated(id, DshHarness.Slot.empty(workspace, ceiling, policy)))
    yield id

  def prompt(id: SessionId, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      require(id).flatMap { slot =>
        // A lone "-" is the headless stdin marker. This adapter does not write stdin.
        if prompt.text == "-" then ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Malformed))
        else
          Promise.make[HarnessFailure, Unit].flatMap { gate =>
            gates.update(_.updated(id, gate)).as {
              Retain
                .coalesce(turn(id, slot, prompt).interruptWhen(gate))(append(id, _))
                .tap(append(id, _))
                .ensuring(gates.update(_.removed(id)))
            }
          }
      }
    }

  def resume(id: SessionId): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap(require(id) *> journal.events(id).map(ZStream.fromChunk))

  def transcript(id: SessionId): IO[HarnessFailure, Transcript] =
    require(id) *> journal.events(id).map(Transcript(id, _))

  def follow(id: SessionId, after: Option[Long]): ZStream[Any, HarnessFailure, Entry] =
    ZStream.unwrap(require(id).as(journal.follow(id, after)))

  def page(id: SessionId, after: Option[Long]): IO[HarnessFailure, Page] =
    require(id) *> journal.page(id, after).flatMap {
      case Some(found) => ZIO.succeed(found)
      case None        => ZIO.fail(HarnessFailure.UnknownSession(id))
    }

  def cancel(id: SessionId): IO[HarnessFailure, Unit] =
    require(id) *> gates.get.flatMap(open => ZIO.foreachDiscard(open.get(id))(_.succeed(())))

  def close: UIO[Unit] =
    gates.get.flatMap(open => ZIO.foreachDiscard(open.values)(_.succeed(())))

  private[dsh] def applied(id: SessionId): IO[HarnessFailure, (Ceiling, PermissionPolicy)] =
    require(id).map(slot => (slot.ceiling, slot.policy))

  private def turn(id: SessionId, slot: DshHarness.Slot, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrapScoped {
      val argv = Launch.argv(config, prompt, slot.remote)
      for
        spawned <- ZIO.acquireRelease(spawner.spawn(argv.command, argv.args, slot.workspace))(_.destroy)
        adopted <- Ref.make(slot.remote)
        _       <- Spawner.drain(spawned.stderr).forkScoped
      yield DshTurn
        .events(LinePipe.streams(spawned.stdout, spawned.stdin), spawned.exit, slot.remote, adopted)
        .ensuring(remember(id, adopted))
    }

  private def remember(id: SessionId, adopted: Ref[Option[String]]): UIO[Unit] =
    adopted.get.flatMap {
      case None         => ZIO.unit
      case Some(remote) =>
        sessions.update { rows =>
          rows.get(id) match
            case None       => rows
            case Some(slot) => rows.updated(id, slot.copy(remote = Some(remote)))
        }
    }

  private def append(id: SessionId, event: Event): UIO[Unit] =
    journal.append(id, event)

  private def require(id: SessionId): IO[HarnessFailure, DshHarness.Slot] =
    sessions.get.map(_.get(id)).flatMap {
      case Some(slot) => ZIO.succeed(slot)
      case None       => ZIO.fail(HarnessFailure.UnknownSession(id))
    }
end DshHarness

object DshHarness:
  private[dsh] final case class Slot(
      workspace: Workspace,
      ceiling: Ceiling,
      policy: PermissionPolicy,
      remote: Option[String],
  )

  private[dsh] object Slot:
    def empty(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): Slot =
      Slot(workspace, ceiling, policy, None)

  def make(config: DshConfig, spawner: Spawner): ZIO[Scope, Nothing, DshHarness] =
    for
      sessions <- Ref.make(Map.empty[SessionId, Slot])
      gates    <- Ref.make(Map.empty[SessionId, Promise[HarnessFailure, Unit]])
      journal  <- Journal.make
    yield DshHarness(config, spawner, sessions, gates, journal)

  def layer(config: DshConfig): ZLayer[Scope, Nothing, Harness] =
    ZLayer.scoped(make(config, Spawner.jdk).map(harness => harness: Harness))
end DshHarness
