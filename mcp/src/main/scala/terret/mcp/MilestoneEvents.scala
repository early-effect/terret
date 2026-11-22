package terret.mcp

import heddle.Schema
import heddle.http.header.Headers
import heddle.mcp.events.{EmitError, EventId, Events, Principal}
import heddle.mcp.protocol.EventName
import terret.harness.HarnessFailure
import terret.milestone.{MilestoneGate, MilestoneKind, MilestoneSink, Milestones, Notice}
import terret.session.SessionId
import terret.transcript.Entry
import zio.json.*
import zio.{Chunk, Clock, Promise, Queue, Scope, UIO, ZIO}

given Schema[MilestoneKind]    = Schema.derived
given JsonCodec[MilestoneKind] = JsonCodec.derived

final case class MilestoneFilter(session: Option[String], kinds: Chunk[MilestoneKind]) derives Schema, JsonCodec

final case class MilestoneData(
    session: String,
    kind: MilestoneKind,
    summary: String,
    cursor: Option[Long],
    at: String,
) derives Schema,
      JsonCodec

object MilestoneEvents:
  private val name: EventName = EventName("session.milestone")

  def accept(filter: MilestoneFilter, data: MilestoneData): Boolean =
    val session = filter.session.forall(_ == data.session)
    val kinds   = filter.kinds.isEmpty || filter.kinds.contains(data.kind)
    session && kinds

  def principal(headers: Headers): UIO[Option[Principal]] =
    ZIO.succeed(headers.get("Authorization").filter(_.nonEmpty).map(Principal(_)))

  def attach(gate: MilestoneGate): ZIO[Milestones & Events.Env & Scope, Nothing, Publisher] =
    Publisher.make.flatMap(publisher => gate.bind(publisher).as(publisher))

  final class Publisher private (queue: Queue[Work]) extends MilestoneSink:
    def started(id: SessionId, cursor: Option[Long], epoch: Long): UIO[Unit] =
      queue.offer(Work.Started(id, cursor, epoch)).unit

    def appended(id: SessionId, entry: Entry): UIO[Unit] =
      queue.offer(Work.Appended(id, entry)).unit

    def failed(id: SessionId, failure: HarnessFailure, cursor: Option[Long], epoch: Long): UIO[Unit] =
      queue.offer(Work.Failed(id, failure, cursor, epoch)).unit

    /** Completes after every offer made before this call has been published or dropped. */
    def settled: UIO[Unit] =
      for
        done <- Promise.make[Nothing, Unit]
        _    <- queue.offer(Work.Flush(done))
        _    <- done.await
      yield ()
  end Publisher

  private enum Work:
    case Started(id: SessionId, cursor: Option[Long], epoch: Long)
    case Appended(id: SessionId, entry: Entry)
    case Failed(id: SessionId, failure: HarnessFailure, cursor: Option[Long], epoch: Long)
    case Flush(done: Promise[Nothing, Unit])

  object Publisher:
    def make: ZIO[Milestones & Events.Env & Scope, Nothing, Publisher] =
      for
        queue <- Queue.unbounded[Work]
        _     <- drain(queue).forever.forkScoped
      yield Publisher(queue)

    private def drain(queue: Queue[Work]): ZIO[Milestones & Events.Env, Nothing, Unit] =
      queue.take.flatMap {
        case Work.Started(id, cursor, epoch) =>
          ZIO.serviceWithZIO[Milestones](_.turnStarted).flatMap { notice =>
            emit(id, s"${id.value}:turn:$epoch", notice, cursor)
          }
        case Work.Appended(id, entry) =>
          ZIO.serviceWithZIO[Milestones](_.classify(entry.event)).flatMap {
            case None         => ZIO.unit
            case Some(notice) => emit(id, s"${id.value}:${entry.index}", notice, Some(entry.index))
          }
        case Work.Failed(id, failure, cursor, epoch) =>
          ZIO.serviceWithZIO[Milestones](_.failed(failure)).flatMap { notice =>
            emit(id, s"${id.value}:fail:$epoch", notice, cursor)
          }
        case Work.Flush(done) => done.succeed(()).unit
      }

    private def emit(
        id: SessionId,
        raw: String,
        notice: Notice,
        cursor: Option[Long],
    ): ZIO[Events.Env, Nothing, Unit] =
      ZIO
        .fromEither(EventId.from(raw))
        .foldZIO(
          _ => ZIO.unit,
          eventId =>
            Clock.instant.flatMap { at =>
              val data = MilestoneData(id.value, notice.kind, notice.summary, cursor, at.toString)
              Events.publish(name, eventId, data, at).catchAll(defect)
            },
        )

    private def defect(error: EmitError): UIO[Nothing] =
      ZIO.dieMessage(error match
        case EmitError.Unknown(found)     => s"milestone event ${found.value} is not declared"
        case EmitError.Undecodable(found) => s"milestone event ${found.value} did not encode")
  end Publisher
end MilestoneEvents
