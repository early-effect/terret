package terret.app

import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

/** The only launchctl caller. Labels and the plist directory come from config. */
final class LaunchdSupervisor private (jobs: LaunchdJobs) extends Supervisor:
  def stop: IO[SupervisorFailure, Unit] =
    domain.flatMap { uid =>
      ZIO.foreachDiscard(jobs.labels)(label => stopOne(uid, label))
    }

  def start: IO[SupervisorFailure, Unit] =
    domain.flatMap { uid =>
      ZIO.foreachDiscard(jobs.labels)(label => startOne(uid, label))
    }

  /** The first label is the server. Later labels, such as a tunnel, follow it. */
  def running: IO[SupervisorFailure, Boolean] =
    domain.flatMap { uid =>
      val target = service(uid, jobs.labels.head)
      launchctl(Chunk("print", target), SupervisorAction.Status).map(_ == 0)
    }

  private def stopOne(uid: String, label: JobLabel): IO[SupervisorFailure, Unit] =
    val target = service(uid, label)
    // bootout of a job that is already out is a non-zero exit. The scripts ignore it.
    launchctl(Chunk("bootout", target), SupervisorAction.Stop).unit *>
      launchctl(Chunk("disable", target), SupervisorAction.Stop).flatMap { code =>
        if code == 0 then ZIO.unit else ZIO.fail(SupervisorFailure.Exit(SupervisorAction.Stop, code))
      }

  private def startOne(uid: String, label: JobLabel): IO[SupervisorFailure, Unit] =
    val target = service(uid, label)
    val plist  = jobs.plistDir.resolve(s"${label.value}.plist").toString
    launchctl(Chunk("enable", target), SupervisorAction.Start).flatMap { enabled =>
      if enabled == 0 then ZIO.unit
      else ZIO.fail(SupervisorFailure.Exit(SupervisorAction.Start, enabled))
    } *> launchctl(Chunk("bootstrap", s"gui/$uid", plist), SupervisorAction.Start).flatMap { bootstrapped =>
      if bootstrapped == 0 then ZIO.unit
      else
        launchctl(Chunk("kickstart", target), SupervisorAction.Start).flatMap { kicked =>
          if kicked == 0 then ZIO.unit else ZIO.fail(SupervisorFailure.Exit(SupervisorAction.Start, kicked))
        }
    }

  private def service(uid: String, label: JobLabel): String =
    s"gui/$uid/${label.value}"

  private def domain: IO[SupervisorFailure, String] =
    execute(Chunk("id", "-u"), capture = true, SupervisorAction.Status).flatMap { (code, output) =>
      val text = output.trim
      if code == 0 && text.nonEmpty && text.forall(_.isDigit) then ZIO.succeed(text)
      else ZIO.fail(SupervisorFailure.Launch(SupervisorAction.Status))
    }

  private def launchctl(args: Chunk[String], action: SupervisorAction): IO[SupervisorFailure, Int] =
    execute(Chunk("launchctl") ++ args, capture = false, action).map(_._1)

  private def execute(
      args: Chunk[String],
      capture: Boolean,
      action: SupervisorAction,
  ): IO[SupervisorFailure, (Int, String)] =
    ZIO
      .attemptBlockingInterrupt {
        val builder = new java.lang.ProcessBuilder(args.toList*)
        if !capture then
          builder.redirectOutput(java.lang.ProcessBuilder.Redirect.DISCARD)
          builder.redirectError(java.lang.ProcessBuilder.Redirect.DISCARD)
        val process = builder.start()
        val output  =
          if capture then new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
          else ""
        (process.waitFor(), output)
      }
      .mapError(_ => SupervisorFailure.Launch(action))
end LaunchdSupervisor

object LaunchdSupervisor:
  def layer(jobs: LaunchdJobs): ULayer[Supervisor] =
    ZLayer.succeed(LaunchdSupervisor(jobs))

opaque type JobLabel = String

object JobLabel:
  def from(raw: String): Either[ServiceError, JobLabel] =
    if raw.isEmpty || raw.exists(ch => ch.isWhitespace || ch == '/' || ch == '\\') then Left(ServiceError.Label(raw))
    else Right(raw)

  extension (label: JobLabel) def value: String = label
end JobLabel

final case class LaunchdJobs(labels: zio.NonEmptyChunk[JobLabel], plistDir: Path)
