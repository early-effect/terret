package terret.grok

import heddle.LinePipe
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.json.*
import zio.json.ast.Json
import zio.{IO, Queue, Scope, UIO, ZIO}

trait RpcPipe:
  def read: IO[HarnessFailure, Option[Json]]
  def write(json: Json): IO[HarnessFailure, Unit]
  def close: UIO[Unit]

final class LineRpc private (pipe: LinePipe, lines: Queue[Option[String]], shutdown: UIO[Unit]) extends RpcPipe:
  def read: IO[HarnessFailure, Option[Json]] =
    lines.take.flatMap {
      case None =>
        lines.offer(None).as(None)
      case Some(line) if line.isBlank =>
        read
      case Some(line) =>
        ZIO.fromEither(line.fromJson[Json]).mapBoth(_ => HarnessFailure.Protocol(ProtocolFault.Malformed), Some(_))
    }

  def write(json: Json): IO[HarnessFailure, Unit] =
    pipe.writeLine(json.toJson).mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete))

  def close: UIO[Unit] = shutdown
end LineRpc

object LineRpc:
  // readLine blocks in a call that ignores interruption. The pump owns that call.
  // A turn stops by ending its take. Closing the agent is the session's job.
  def make(pipe: LinePipe, shutdown: UIO[Unit]): ZIO[Scope, Nothing, RpcPipe] =
    for
      lines <- Queue.unbounded[Option[String]]
      fiber <- pump(pipe, lines).forkDaemon
      _     <- ZIO.addFinalizer(fiber.interruptFork)
    yield LineRpc(pipe, lines, shutdown)

  private def pump(pipe: LinePipe, lines: Queue[Option[String]]): UIO[Unit] =
    pipe.readLine.foldZIO(
      _ => lines.offer(None).unit,
      line =>
        lines.offer(line) *> (line match
          case None    => ZIO.unit
          case Some(_) => pump(pipe, lines)),
    )
end LineRpc
