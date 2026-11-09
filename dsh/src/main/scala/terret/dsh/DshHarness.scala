package terret.dsh

import heddle.LinePipe
import terret.event.Event
import terret.harness.{Harness, HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.transcript.Transcript
import terret.workspace.Workspace
import zio.stream.ZStream
import zio.{Chunk, IO, Promise, Random, Ref, Scope, UIO, ZIO, ZLayer}

final class DshHarness private (
    config: DshConfig,
    spawner: Spawner,
    sessions: Ref[Map[SessionId, DshHarness.Slot]],
    gates: Ref[Map[SessionId, Promise[HarnessFailure, Unit]]],
) extends Harness:

  def open(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): IO[HarnessFailure, SessionId] =
    for
      uuid <- Random.nextUUID
      id   <- ZIO.fromEither(SessionId.from(uuid.toString)).mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete))
      _    <- sessions.update(_.updated(id, DshHarness.Slot.empty(workspace, ceiling, policy)))
    yield id

  def prompt(id: SessionId, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      require(id).flatMap { slot =>
        // A lone "-" is the headless stdin marker. This adapter does not write stdin.
        if prompt.text == "-" then ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Malformed))
        else
          Promise.make[HarnessFailure, Unit].flatMap { gate =>
            gates.update(_.updated(id, gate)).as {
              turn(id, slot, prompt).interruptWhen(gate).ensuring(gates.update(_.removed(id))).tap(append(id, _))
            }
          }
      }
    }

  def resume(id: SessionId): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap(require(id).map(slot => ZStream.fromChunk(slot.events)))

  def transcript(id: SessionId): IO[HarnessFailure, Transcript] =
    require(id).map(slot => Transcript(id, slot.events))

  def cancel(id: SessionId): IO[HarnessFailure, Unit] =
    require(id) *> gates.get.flatMap(open => ZIO.foreachDiscard(open.get(id))(_.succeed(())))

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
      case None => ZIO.unit
      case Some(remote) =>
        sessions.update { rows =>
          rows.get(id) match
            case None       => rows
            case Some(slot) => rows.updated(id, slot.copy(remote = Some(remote)))
        }
    }

  private def append(id: SessionId, event: Event): UIO[Unit] =
    sessions.update { rows =>
      rows.get(id) match
        case None       => rows
        case Some(slot) => rows.updated(id, slot.copy(events = slot.events :+ event))
    }

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
      events: Chunk[Event],
      remote: Option[String],
  )

  private[dsh] object Slot:
    def empty(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): Slot =
      Slot(workspace, ceiling, policy, Chunk.empty, None)
  end Slot

  def make(config: DshConfig, spawner: Spawner): ZIO[Scope, Nothing, DshHarness] =
    for
      sessions <- Ref.make(Map.empty[SessionId, Slot])
      gates    <- Ref.make(Map.empty[SessionId, Promise[HarnessFailure, Unit]])
    yield DshHarness(config, spawner, sessions, gates)

  def layer(config: DshConfig): ZLayer[Scope, Nothing, Harness] =
    ZLayer.scoped(make(config, Spawner.jdk).map(harness => harness: Harness))
end DshHarness
