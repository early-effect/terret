package terret.app

import zio.{IO, ULayer, ZIO, ZLayer}

import java.lang.ProcessHandle
import java.nio.file.Path
import scala.jdk.OptionConverters.*

/** Whether the server process recorded in the pid file is gone. This does not ask launchctl. */
trait Liveness:
  def exited: IO[LivenessFailure, Boolean]

enum LivenessFailure:
  case Unreadable
  case Malformed

  def render: String = this match
    case Unreadable => "could not read the pid file"
    case Malformed  => "pid file is malformed"
end LivenessFailure

object Liveness:
  def layer(path: Path): ULayer[Liveness] =
    ZLayer.succeed(PidLiveness(path))

private final class PidLiveness(path: Path) extends Liveness:
  def exited: IO[LivenessFailure, Boolean] =
    PidFile.read(path).flatMap {
      case None         => ZIO.succeed(true)
      case Some(record) =>
        ZIO.attempt(gone(record)).mapError(_ => LivenessFailure.Unreadable)
    }

  private def gone(record: PidFile.Record): Boolean =
    ProcessHandle.of(record.pid).toScala match
      case None         => true
      case Some(handle) =>
        val same = handle.info().startInstant().toScala.exists(_.toEpochMilli == record.startedAt)
        !handle.isAlive || !same
end PidLiveness
