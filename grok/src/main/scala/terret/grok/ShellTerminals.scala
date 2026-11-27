package terret.grok

import zio.{Chunk, IO, Ref, UIO, ZIO}

final case class TerminalView(output: String, truncated: Boolean, exit: Option[Int])

/** Live terminals for one harness. `shutdown` kills every one still running. */
final class ShellTerminals private (
    sandbox: ShellSandbox,
    ids: Ref[Int],
    live: Ref[Map[String, SandboxHandle]],
):
  def create(argv: Chunk[String], bounds: ShellBounds, limit: Int): IO[SandboxError, String] =
    ZIO.uninterruptibleMask { restore =>
      for
        handle <- restore(sandbox.start(argv, bounds, limit))
        id     <- ids.modify(n => (s"t$n", n + 1))
        _      <- live.update(_.updated(id, handle))
      yield id
    }

  def output(id: String): IO[SandboxError, TerminalView] =
    handle(id).flatMap { found =>
      found.snapshot.map { (text, truncated, code) =>
        TerminalView(text, truncated, code)
      }
    }

  def wait(id: String): IO[SandboxError, Int] =
    handle(id).flatMap(_.awaitExit)

  def kill(id: String): IO[SandboxError, Unit] =
    handle(id).flatMap(_.kill)

  def release(id: String): IO[SandboxError, Unit] =
    live
      .modify { rows =>
        rows.get(id) match
          case None        => (None, rows)
          case Some(found) => (Some(found), rows.removed(id))
      }
      .flatMap {
        case None        => ZIO.fail(SandboxError.Rejected)
        case Some(found) => found.kill.uninterruptible
      }

  def shutdown: UIO[Unit] =
    live.getAndSet(Map.empty).flatMap(rows => ZIO.foreachDiscard(rows.values)(_.kill.uninterruptible))

  private def handle(id: String): IO[SandboxError, SandboxHandle] =
    live.get.flatMap { rows =>
      rows.get(id) match
        case Some(found) => ZIO.succeed(found)
        case None        => ZIO.fail(SandboxError.Rejected)
    }

object ShellTerminals:
  def make(sandbox: ShellSandbox): UIO[ShellTerminals] =
    for
      ids  <- Ref.make(1)
      live <- Ref.make(Map.empty[String, SandboxHandle])
    yield ShellTerminals(sandbox, ids, live)
end ShellTerminals
