package terret.mcp

import heddle.Schema
import heddle.mcp.events.{EmitError, EventId, Events}
import heddle.mcp.protocol.EventName
import terret.activity.{Activity, ActivityEvent, ActivityMemory}
import terret.harness.HarnessFailure
import terret.milestone.{MilestoneGate, MilestoneKind, MilestoneSink, Milestones, Notice}
import terret.redact.Redact
import terret.session.SessionId
import terret.transcript.Entry
import zio.json.*
import zio.{Chunk, Clock, Promise, Queue, Ref, Scope, UIO, ZIO}

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
  val name: EventName = EventName("session.milestone")

  def accept(filter: MilestoneFilter, data: MilestoneData): Boolean =
    val session = filter.session.forall(_ == data.session)
    val kinds   = filter.kinds.isEmpty || filter.kinds.contains(data.kind)
    session && kinds

  def attach(gate: MilestoneGate): ZIO[Milestones & Events.Env & Scope, Nothing, Publisher] =
    Publisher.make.flatMap(publisher => gate.bind(publisher).as(publisher))

  final class Publisher private (
      private val queue: Queue[Work],
      private val seen: Ref[Set[String]],
      private val epochs: Ref[Map[SessionId, Long]],
      private val activity: Ref[Map[SessionId, ActivityMemory]],
  ) extends MilestoneSink:
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
        queue    <- Queue.unbounded[Work]
        seen     <- Ref.make(Set.empty[String])
        epochs   <- Ref.make(Map.empty[SessionId, Long])
        activity <- Ref.make(Map.empty[SessionId, ActivityMemory])
        publisher = Publisher(queue, seen, epochs, activity)
        _ <- drain(publisher).forever.forkScoped
      yield publisher

    private def drain(publisher: Publisher): ZIO[Milestones & Events.Env, Nothing, Unit] =
      publisher.queue.take.flatMap {
        case Work.Started(id, cursor, epoch) =>
          publisher.epochs.update(_.updated(id, epoch)) *>
            ZIO.serviceWithZIO[Milestones](_.turnStarted).flatMap { notice =>
              emit(publisher, id, s"${id.value}:turn:$epoch", notice, cursor, epoch.toString)
            }
        case Work.Appended(id, entry) =>
          publishActivity(publisher, id, entry) *>
            ZIO.serviceWithZIO[Milestones](_.classify(entry.event)).flatMap { notices =>
              publisher.epochs.get.map(_.get(id)).flatMap { epoch =>
                val scope = epoch.fold(entry.index.toString)(_.toString)
                ZIO.foreachDiscard(notices.zipWithIndex) { (notice, nth) =>
                  emit(publisher, id, s"${id.value}:${entry.index}:$nth", notice, Some(entry.index), scope)
                }
              }
            }
        case Work.Failed(id, failure, cursor, epoch) =>
          ZIO.serviceWithZIO[Milestones](_.failed(failure)).flatMap { notice =>
            emit(publisher, id, s"${id.value}:fail:$epoch", notice, cursor, epoch.toString)
          }
        case Work.Flush(done) => done.succeed(()).unit
      }

    private def publishActivity(
        publisher: Publisher,
        id: SessionId,
        entry: Entry,
    ): ZIO[Events.Env, Nothing, Unit] =
      publisher.activity
        .modify { rows =>
          val memory        = rows.getOrElse(id, ActivityMemory.empty)
          val (next, notes) = Activity.step(memory, entry.event)
          (notes, rows.updated(id, next))
        }
        .flatMap { notes =>
          val placed = Activity.place(entry.index, entry.turn, entry.at, notes)
          ZIO.foreachDiscard(placed.zipWithIndex) { (event, nth) =>
            emitActivity(id, s"${id.value}:act:${entry.index}:$nth", event, Some(entry.index))
          }
        }

    private def emit(
        publisher: Publisher,
        id: SessionId,
        raw: String,
        notice: Notice,
        cursor: Option[Long],
        scope: String,
    ): ZIO[Events.Env, Nothing, Unit] =
      val key = fact(id, notice, scope)
      publisher.seen
        .modify { keys =>
          if keys.contains(key) then (false, keys) else (true, keys + key)
        }
        .flatMap {
          case false => ZIO.unit
          case true  =>
            ZIO
              .fromEither(EventId.from(raw))
              .foldZIO(
                _ => ZIO.unit,
                eventId =>
                  Clock.instant.flatMap { at =>
                    val data = MilestoneData(id.value, notice.kind, Redact.text(notice.summary), cursor, at.toString)
                    Events.publish(name, eventId, data, at).catchAll(defect)
                  },
              )
        }

    private def emitActivity(
        id: SessionId,
        raw: String,
        event: ActivityEvent,
        cursor: Option[Long],
    ): ZIO[Events.Env, Nothing, Unit] =
      ZIO
        .fromEither(EventId.from(raw))
        .foldZIO(
          _ => ZIO.unit,
          eventId =>
            val data = ActivityData(
              event.id,
              event.parent,
              id.value,
              event.kind,
              event.call,
              event.name.map(Redact.text),
              event.status,
              Redact.text(event.text),
              cursor,
              event.at.toString,
            )
            Events.publish(ActivityEvents.name, eventId, data, event.at).catchAll(defect),
        )

    private def fact(id: SessionId, notice: Notice, scope: String): String =
      notice.kind match
        case MilestoneKind.TurnStarted | MilestoneKind.TurnFinished | MilestoneKind.Error =>
          s"${id.value}:${notice.key}@$scope"
        case _ =>
          s"${id.value}:${notice.key}"

    private def defect(error: EmitError): UIO[Nothing] =
      ZIO.dieMessage(error match
        case EmitError.Unknown(found)     => s"milestone event ${found.value} is not declared"
        case EmitError.Undecodable(found) => s"milestone event ${found.value} did not encode"
        case EmitError.NotDurable         => "milestone event was not stored")
  end Publisher
end MilestoneEvents
