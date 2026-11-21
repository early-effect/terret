package terret.transcript

import terret.event.Event
import zio.stream.ZStream
import zio.{Cause, Chunk, FiberRef, Ref, UIO, Unsafe, ZIO}

object Retain:
  // A chunk returned from an interrupted pull is discarded. The open run is delivered from the finalizer instead.
  private[terret] val deliver: FiberRef[Event => UIO[Unit]] =
    Unsafe.unsafe { implicit unsafe =>
      FiberRef.unsafe.make[Event => UIO[Unit]](_ => ZIO.unit)
    }

  // The open run is in `pending`. The upstream may already be pulling the next chunk.
  private[terret] val parked: FiberRef[Event => UIO[Unit]] =
    Unsafe.unsafe { implicit unsafe =>
      FiberRef.unsafe.make[Event => UIO[Unit]](_ => ZIO.unit)
    }

  def coalesce[R, E](upstream: ZStream[R, E, Event])(onFlush: Event => UIO[Unit]): ZStream[R, E, Event] =
    ZStream.unwrapScoped {
      for
        pull    <- upstream.toPull
        pending <- Ref.make[Option[Event]](None)
        halt    <- Ref.make[Option[Cause[Option[E]]]](None)
        _       <- ZIO.addFinalizer(flush(pending, onFlush))
      yield ZStream.repeatZIOChunkOption(step(pull, pending, halt, onFlush))
    }

  private def step[R, E](
      pull: ZIO[R, Option[E], Chunk[Event]],
      pending: Ref[Option[Event]],
      halt: Ref[Option[Cause[Option[E]]]],
      onFlush: Event => UIO[Unit],
  ): ZIO[R, Option[E], Chunk[Event]] =
    halt.get.flatMap {
      case Some(cause) => ZIO.failCause(cause)
      case None        =>
        pull.foldCauseZIO(
          cause => settle(cause, pending, halt, onFlush),
          chunk => absorb(pending, chunk),
        )
    }

  private def settle[E](
      cause: Cause[Option[E]],
      pending: Ref[Option[Event]],
      halt: Ref[Option[Cause[Option[E]]]],
      onFlush: Event => UIO[Unit],
  ): ZIO[Any, Option[E], Chunk[Event]] =
    if cause.isInterruptedOnly then flush(pending, onFlush) *> ZIO.failCause(cause)
    else
      pending.getAndSet(None).flatMap {
        case Some(event) => halt.set(Some(cause)).as(Chunk(event))
        case None        => ZIO.failCause(cause)
      }

  private def flush(pending: Ref[Option[Event]], onFlush: Event => UIO[Unit]): UIO[Unit] =
    pending.getAndSet(None).flatMap {
      case Some(event) => onFlush(event) *> deliver.get.flatMap(_(event))
      case None        => ZIO.unit
    }

  private def absorb(pending: Ref[Option[Event]], chunk: Chunk[Event]): UIO[Chunk[Event]] =
    pending
      .modify { start =>
        val (emitted, rest) = chunk.foldLeft((Chunk.empty[Event], start)) { (state, event) =>
          val (out, held)  = state
          val (next, more) = join(held, event)
          (out ++ more, next)
        }
        ((emitted, rest), rest)
      }
      .flatMap { (emitted, rest) =>
        rest match
          case Some(event) => parked.get.flatMap(_(event)).as(emitted)
          case None        => ZIO.succeed(emitted)
      }

  private def join(held: Option[Event], event: Event): (Option[Event], Chunk[Event]) =
    (held, event) match
      case (Some(Event.Assistant(left)), Event.Assistant(right)) =>
        (Some(Event.Assistant(left + right)), Chunk.empty)
      case (Some(Event.Thought(left)), Event.Thought(right)) =>
        (Some(Event.Thought(left + right)), Chunk.empty)
      case (Some(open), next @ (Event.Assistant(_) | Event.Thought(_))) =>
        (Some(next), Chunk(open))
      case (Some(open), other) =>
        (None, Chunk(open, other))
      case (None, next @ (Event.Assistant(_) | Event.Thought(_))) =>
        (Some(next), Chunk.empty)
      case (None, other) =>
        (None, Chunk(other))
end Retain
