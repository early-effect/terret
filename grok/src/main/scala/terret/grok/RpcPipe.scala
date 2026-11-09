package terret.grok

import heddle.LinePipe
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.json.*
import zio.json.ast.Json
import zio.{IO, UIO, ZIO}

trait RpcPipe:
  def read: IO[HarnessFailure, Option[Json]]
  def write(json: Json): IO[HarnessFailure, Unit]
  def close: UIO[Unit]
end RpcPipe

final class LineRpc(pipe: LinePipe, shutdown: UIO[Unit]) extends RpcPipe:
  def read: IO[HarnessFailure, Option[Json]] =
    pipe.readLine.mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete)).flatMap {
      case None                      => ZIO.succeed(None)
      case Some(line) if line.isBlank => read
      case Some(line)                =>
        ZIO.fromEither(line.fromJson[Json]).mapBoth(_ => HarnessFailure.Protocol(ProtocolFault.Malformed), Some(_))
    }

  def write(json: Json): IO[HarnessFailure, Unit] =
    pipe.writeLine(json.toJson).mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete))

  def close: UIO[Unit] = shutdown
end LineRpc
