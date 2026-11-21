package terret.app

import zio.{IO, ZIO}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.OptionConverters.*

object PidFile:
  final case class Record(pid: Long, startedAt: Long)

  def defaultPath: Path =
    val home = System.getProperty("user.home")
    if home == null || home.isEmpty then Path.of(".terret", "server.pid")
    else Path.of(home, ".terret", "server.pid")

  def write(path: Path): IO[ServiceError, Unit] =
    ZIO
      .attemptBlockingInterrupt {
        val handle  = java.lang.ProcessHandle.current()
        val started = handle.info().startInstant().toScala.fold(0L)(_.toEpochMilli)
        val parent  = path.getParent
        if parent != null then Files.createDirectories(parent)
        Files.writeString(path, s"${handle.pid()}\n$started\n", StandardCharsets.UTF_8)
        ()
      }
      .mapError(_ => ServiceError.Pid)

  def read(path: Path): IO[LivenessFailure, Option[Record]] =
    ZIO
      .attemptBlockingInterrupt(load(path))
      .mapError(_ => LivenessFailure.Unreadable)
      .flatMap(ZIO.fromEither)

  def parse(text: String): Option[Record] =
    text.split("\n", -1).toList match
      case pidLine :: startLine :: _ =>
        (pidLine.trim.toLongOption, startLine.trim.toLongOption) match
          case (Some(pid), Some(started)) if pid > 0 && started >= 0 => Some(Record(pid, started))
          case _                                                     => None
      case _ => None

  private def load(path: Path): Either[LivenessFailure, Option[Record]] =
    try
      if !Files.exists(path) then Right(None)
      else
        parse(Files.readString(path, StandardCharsets.UTF_8)) match
          case None         => Left(LivenessFailure.Malformed)
          case Some(record) => Right(Some(record))
    catch case _: IOException => Left(LivenessFailure.Unreadable)
end PidFile
