package terret.transcript

import terret.event.Event
import terret.session.SessionId
import zio.stream.ZStream
import zio.{Chunk, Hub, Ref, Semaphore, UIO, ZIO}

/** One retained event. `index` is stable: the next listen passes it back as `after`. */
final case class Entry(index: Long, event: Event)

/** A page of the log. `cursor` is the last retained index, or nothing when the log is empty. */
final case class Page(events: Chunk[Entry], cursor: Option[Long])

/** The session log. Append is the only writer, so a flushed text run and a live emit share one index. */
final class Journal private (slots: Ref[Map[SessionId, Journal.Slot]]):
  def open(id: SessionId): UIO[Unit] =
    for
      hub  <- Hub.unbounded[Entry]
      lock <- Semaphore.make(1)
      won  <- slots.modify { rows =>
        if rows.contains(id) then (false, rows)
        else (true, rows.updated(id, Journal.Slot(Chunk.empty, hub, lock)))
      }
      _ <- if won then ZIO.unit else hub.shutdown.unit
    yield ()

  def append(id: SessionId, event: Event): UIO[Unit] =
    slots.get.flatMap { rows =>
      rows.get(id) match
        case None       => ZIO.unit
        case Some(slot) =>
          slot.lock.withPermit {
            slots
              .modify { current =>
                current.get(id) match
                  case None        => (None, current)
                  case Some(found) =>
                    val entry = Entry(found.events.length.toLong, event)
                    (Some(found.hub -> entry), current.updated(id, found.copy(events = found.events :+ event)))
              }
              .flatMap {
                case None              => ZIO.unit
                case Some(hub -> item) => hub.publish(item).unit
              }
          }
    }

  def events(id: SessionId): UIO[Chunk[Event]] =
    slots.get.map(_.get(id).fold(Chunk.empty[Event])(_.events))

  def isEmpty(id: SessionId): UIO[Boolean] =
    events(id).map(_.isEmpty)

  /** `after` is exclusive. A value past the end is the current end, so the next event is the next index. */
  def page(id: SessionId, after: Option[Long]): UIO[Option[Page]] =
    slots.get.map { rows =>
      rows.get(id).map { slot =>
        val cursor = Journal.cursor(slot.events.length.toLong)
        Page(Journal.select(slot.events, Journal.floor(after, cursor)), cursor)
      }
    }

  /** Subscribe first, then read. An event that lands in both the queue and the snapshot is emitted once. */
  def follow(id: SessionId, after: Option[Long]): ZStream[Any, Nothing, Entry] =
    ZStream.unwrapScoped {
      slots.get.flatMap { rows =>
        rows.get(id) match
          case None       => ZIO.succeed(ZStream.empty)
          case Some(slot) =>
            slot.hub.subscribe.flatMap { queue =>
              slots.get.map { current =>
                val retained = current.get(id).fold(Chunk.empty[Event])(_.events)
                val length   = retained.length.toLong
                val start    = Journal.floor(after, Journal.cursor(length))
                val replay   = Journal.select(retained, start)
                val live     = ZStream.fromQueue(queue).filter { entry =>
                  entry.index >= length && entry.index > start
                }
                ZStream.fromChunk(replay) ++ live
              }
            }
      }
    }
end Journal

object Journal:
  private final case class Slot(events: Chunk[Event], hub: Hub[Entry], lock: Semaphore)

  def make: UIO[Journal] =
    Ref.make(Map.empty[SessionId, Slot]).map(Journal(_))

  private def cursor(length: Long): Option[Long] =
    if length == 0 then None else Some(length - 1)

  /** A missing or negative cursor reads from the start. The job edge rejects a negative before this runs. */
  private def floor(after: Option[Long], cursor: Option[Long]): Long =
    after match
      case Some(n) if n >= 0 => cursor.fold(-1L)(end => math.min(n, end))
      case _                 => -1L

  private def select(events: Chunk[Event], start: Long): Chunk[Entry] =
    events.zipWithIndex.collect {
      case (event, index) if index.toLong > start => Entry(index.toLong, event)
    }
end Journal
