package terret.memory

import terret.event.{Event, TurnOutcome}
import terret.harness.{Harness, HarnessFailure}
import terret.milestone.MilestoneSink
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.transcript.{Entry, Journal, Page, Retain, Transcript}
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

final case class Applied(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy)

final class InMemory private (
    sessions: Ref[Map[SessionId, InMemory.Slot]],
    gates: Ref[Map[SessionId, Promise[HarnessFailure, Unit]]],
    cancelledIds: Ref[Chunk[SessionId]],
    journal: Journal,
    script: Script,
) extends Harness:

  def cancelled: UIO[Chunk[SessionId]] = cancelledIds.get

  def applied(id: SessionId): IO[HarnessFailure, Applied] =
    slot(id).map(found => Applied(found.workspace, found.ceiling, found.policy))

  def open(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): IO[HarnessFailure, SessionId] =
    for
      uuid <- Random.nextUUID
      id = SessionId.trusted(uuid.toString)
      _ <- journal.open(id)
      _ <- sessions.update(_.updated(id, InMemory.Slot(workspace, ceiling, policy)))
    yield id

  def prompt(id: SessionId, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      slot(id).flatMap { _ =>
        for
          gate <- Promise.make[HarnessFailure, Unit]
          _    <- gates.update(_.updated(id, gate))
        yield Retain
          .coalesce(script.onPrompt(id, prompt).interruptWhen(gate))(append(id, _))
          .tap(event => append(id, event))
          .ensuring(gates.update(_.removed(id)))
      }
    }

  def resume(id: SessionId): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap(slot(id) *> journal.events(id).map(ZStream.fromChunk))

  def transcript(id: SessionId): IO[HarnessFailure, Transcript] =
    slot(id) *> journal.events(id).map(Transcript(id, _))

  def follow(id: SessionId, after: Option[Long]): ZStream[Any, HarnessFailure, Entry] =
    ZStream.unwrap(slot(id).as(journal.follow(id, after)))

  def page(id: SessionId, after: Option[Long]): IO[HarnessFailure, Page] =
    slot(id) *> journal.page(id, after).flatMap {
      case Some(found) => ZIO.succeed(found)
      case None        => ZIO.fail(HarnessFailure.UnknownSession(id))
    }

  def cancel(id: SessionId): IO[HarnessFailure, Unit] =
    slot(id).flatMap { _ =>
      gates.get.flatMap { open =>
        ZIO.foreachDiscard(open.get(id))(_.succeed(()))
      } *> cancelledIds.update { ids =>
        if ids.contains(id) then ids else ids :+ id
      }
    }

  def close: UIO[Unit] =
    sessions.get.flatMap { rows =>
      ZIO.foreachDiscard(rows.keys)(id => cancel(id).ignore)
    }

  private def slot(id: SessionId): IO[HarnessFailure, InMemory.Slot] =
    sessions.get.map(_.get(id)).flatMap {
      case Some(found) => ZIO.succeed(found)
      case None        => ZIO.fail(HarnessFailure.UnknownSession(id))
    }

  private def append(id: SessionId, event: Event): UIO[Unit] =
    journal.append(id, event)
end InMemory

object InMemory:
  private[terret] final case class Slot(
      workspace: Workspace,
      ceiling: Ceiling,
      policy: PermissionPolicy,
  )

  def make(script: Script, milestones: MilestoneSink = MilestoneSink.discard): UIO[InMemory] =
    for
      sessions  <- Ref.make(Map.empty[SessionId, Slot])
      gates     <- Ref.make(Map.empty[SessionId, Promise[HarnessFailure, Unit]])
      cancelled <- Ref.make(Chunk.empty[SessionId])
      journal   <- Journal.make(milestones)
    yield InMemory(sessions, gates, cancelled, journal, script)

  def layer(script: Script): ZLayer[Any, Nothing, Harness] =
    ZLayer.fromZIO(make(script).map(memory => memory: Harness))
end InMemory
