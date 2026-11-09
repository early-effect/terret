package terret.memory

import terret.event.{Event, TurnOutcome}
import terret.harness.{Harness, HarnessFailure}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.transcript.Transcript
import terret.workspace.Workspace
import zio.stream.ZStream
import zio.{Chunk, IO, Promise, Random, Ref, UIO, ZIO, ZLayer}

final case class Script(onPrompt: (SessionId, Prompt) => ZStream[Any, HarnessFailure, Event])

object Script:
  val echo: Script = Script: (_, prompt) =>
    ZStream(
      Event.Assistant(prompt.text),
      Event.TurnFinished(TurnOutcome.Completed),
    )
end Script

final case class Applied(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy)

final class InMemory private (
    sessions: Ref[Map[SessionId, InMemory.Slot]],
    gates: Ref[Map[SessionId, Promise[HarnessFailure, Unit]]],
    cancelledIds: Ref[Chunk[SessionId]],
    script: Script,
) extends Harness:

  def cancelled: UIO[Chunk[SessionId]] = cancelledIds.get

  def applied(id: SessionId): IO[HarnessFailure, Applied] =
    slot(id).map(found => Applied(found.workspace, found.ceiling, found.policy))

  def open(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): IO[HarnessFailure, SessionId] =
    for
      uuid <- Random.nextUUID
      id = SessionId.trusted(uuid.toString)
      _ <- sessions.update(_.updated(id, InMemory.Slot(workspace, ceiling, policy, Chunk.empty)))
    yield id

  def prompt(id: SessionId, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      slot(id).flatMap { _ =>
        for
          gate <- Promise.make[HarnessFailure, Unit]
          _    <- gates.update(_.updated(id, gate))
        yield script
          .onPrompt(id, prompt)
          .interruptWhen(gate)
          .tap(event => append(id, event))
          .ensuring(gates.update(_.removed(id)))
      }
    }

  def resume(id: SessionId): ZStream[Any, HarnessFailure, Event] =
    ZStream.fromZIO(slot(id)).flatMap(found => ZStream.fromChunk(found.events))

  def transcript(id: SessionId): IO[HarnessFailure, Transcript] =
    slot(id).map(found => Transcript(id, found.events))

  def cancel(id: SessionId): IO[HarnessFailure, Unit] =
    slot(id).flatMap { _ =>
      gates.get.flatMap { open =>
        ZIO.foreachDiscard(open.get(id))(_.succeed(()))
      } *> cancelledIds.update { ids =>
        if ids.contains(id) then ids else ids :+ id
      }
    }

  private def slot(id: SessionId): IO[HarnessFailure, InMemory.Slot] =
    sessions.get.map(_.get(id)).flatMap {
      case Some(found) => ZIO.succeed(found)
      case None        => ZIO.fail(HarnessFailure.UnknownSession(id))
    }

  private def append(id: SessionId, event: Event): UIO[Unit] =
    sessions.update { rows =>
      rows.get(id) match
        case None        => rows
        case Some(found) => rows.updated(id, found.copy(events = found.events :+ event))
    }
end InMemory

object InMemory:
  private[terret] final case class Slot(
      workspace: Workspace,
      ceiling: Ceiling,
      policy: PermissionPolicy,
      events: Chunk[Event],
  )

  def make(script: Script): UIO[InMemory] =
    for
      sessions  <- Ref.make(Map.empty[SessionId, Slot])
      gates     <- Ref.make(Map.empty[SessionId, Promise[HarnessFailure, Unit]])
      cancelled <- Ref.make(Chunk.empty[SessionId])
    yield InMemory(sessions, gates, cancelled, script)

  def layer(script: Script): ZLayer[Any, Nothing, Harness] =
    ZLayer.fromZIO(make(script).map(memory => memory: Harness))
end InMemory
