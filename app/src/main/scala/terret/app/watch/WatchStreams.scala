package terret.app.watch

import zio.{IO, Ref, UIO, ZIO}

/** Eight live watch streams per viewer. The key is a hash, not the token and not an email. */
final class WatchStreams private (slots: Ref[Map[String, Int]]):
  def acquire(viewer: String): IO[WatchStreamError, Unit] =
    slots
      .modify { rows =>
        val held = rows.getOrElse(viewer, 0)
        if held >= WatchStreams.Limit then (Left(WatchStreamError.Busy), rows)
        else (Right(()), rows.updated(viewer, held + 1))
      }
      .flatMap(ZIO.fromEither)

  def release(viewer: String): UIO[Unit] =
    slots.update { rows =>
      rows.get(viewer) match
        case Some(held) if held <= 1 => rows.removed(viewer)
        case Some(held)              => rows.updated(viewer, held - 1)
        case None                    => rows
    }

  def held(viewer: String): UIO[Int] =
    slots.get.map(_.getOrElse(viewer, 0))
end WatchStreams

object WatchStreams:
  val Limit: Int = 8

  def make: UIO[WatchStreams] =
    Ref.make(Map.empty[String, Int]).map(WatchStreams(_))
end WatchStreams

enum WatchStreamError:
  case Busy
