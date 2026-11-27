package terret.github

import terret.workspace.Workspace
import zio.{Chunk, IO, ZIO, ZLayer}

import java.nio.charset.StandardCharsets

final case class GhOutput(code: Int, stdout: String, stderr: String)

/** Runs one argument vector in a workspace. A non-zero exit is data. Failure to start is `Unavailable`. */
trait GhCommand:
  def execute(argv: Chunk[String], cwd: Workspace, stdin: Option[String]): IO[GhError, GhOutput]

object GhCommand:
  val jdk: GhCommand = Jdk

  val layer: ZLayer[Any, Nothing, GhCommand] =
    ZLayer.succeed(jdk)

  private val Cap: Int = 1000000

  /** Drops `GIT_DIR`, `GIT_WORK_TREE`, and `GH_REPO` so the workspace directory is the only repository. */
  private object Jdk extends GhCommand:
    def execute(argv: Chunk[String], cwd: Workspace, stdin: Option[String]): IO[GhError, GhOutput] =
      ZIO.scoped:
        ZIO.acquireRelease(start(argv, cwd))(stop).flatMap(run(_, stdin))

    private def start(argv: Chunk[String], cwd: Workspace): IO[GhError, Process] =
      ZIO
        .attemptBlockingInterrupt {
          val builder = new ProcessBuilder(argv.toList*)
          builder.directory(cwd.path.toFile)
          val env = builder.environment()
          env.remove("GIT_DIR")
          env.remove("GIT_WORK_TREE")
          env.remove("GH_REPO")
          env.put("GH_FORCE_TTY", "0")
          env.put("GH_PAGER", "")
          env.put("PAGER", "")
          env.put("GH_NO_UPDATE_NOTIFIER", "1")
          builder.start()
        }
        .refineToOrDie[java.io.IOException]
        .mapError(_ => GhError.Unavailable)

    private def stop(process: Process): zio.UIO[Unit] =
      ZIO.attemptBlocking(process.destroy()).ignore

    private def run(process: Process, stdin: Option[String]): IO[GhError, GhOutput] =
      val code: IO[GhError, Int] =
        write(process.getOutputStream, stdin) *>
          ZIO.attemptBlockingInterrupt(process.waitFor()).orDie
      val pipes: IO[GhError, (String, String)] =
        slurp(process.getInputStream).zipWithPar(slurp(process.getErrorStream))((out, err) => (out, err))
      pipes.zipWithPar(code) { (texts, status) =>
        val (out, err) = texts
        GhOutput(status, out, err)
      }

    private def write(stream: java.io.OutputStream, stdin: Option[String]): IO[GhError, Unit] =
      ZIO
        .attemptBlockingInterrupt {
          stdin match
            case Some(body) => stream.write(body.getBytes(StandardCharsets.UTF_8))
            case None       => ()
          stream.close()
        }
        .refineToOrDie[java.io.IOException]
        .mapError(_ => GhError.Unavailable)

    private def slurp(in: java.io.InputStream): IO[GhError, String] =
      ZIO
        .attemptBlockingInterrupt {
          val bytes = in.readNBytes(Cap + 1)
          if bytes.length > Cap then None
          else Some(new String(bytes, StandardCharsets.UTF_8))
        }
        .refineToOrDie[java.io.IOException]
        .mapError(_ => GhError.Unavailable)
        .flatMap {
          case Some(text) => ZIO.succeed(text)
          case None       => ZIO.fail(GhError.TooLarge)
        }
end GhCommand
