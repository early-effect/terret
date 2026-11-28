package terret.app

import zio.{Chunk, Duration, IO, UIO, ULayer, ZIO, ZLayer}
import zio.durationInt

import java.nio.charset.StandardCharsets

/** One login-keychain item. Tests stub this. The live layer is the `security` CLI, and the password is never an
  * argument.
  */
trait Keychain:
  def password(service: String, account: String): IO[KeyError, String]

object Keychain:
  val live: ULayer[Keychain] =
    ZLayer.succeed(Live)

  def argv(service: String, account: String): Chunk[String] =
    Chunk("security", "find-generic-password", "-s", service, "-a", account, "-w")

  /** A non-zero status, including a missing item, drops both streams. One trailing newline is the `-w` terminator. */
  def interpret(code: Int, stdout: String): Either[KeyError, String] =
    if code != 0 then Left(KeyError.Missing)
    else
      val value = stripOneNewline(stdout)
      if value.isEmpty then Left(KeyError.Empty) else Right(value)

  private def stripOneNewline(text: String): String =
    if text.endsWith("\n") then
      val cut = text.dropRight(1)
      if cut.endsWith("\r") then cut.dropRight(1) else cut
    else text

  private val Wait: Duration = 5.seconds

  private object Live extends Keychain:
    def password(service: String, account: String): IO[KeyError, String] =
      ZIO.scoped:
        ZIO.acquireRelease(start(argv(service, account)))(stop).flatMap { process =>
          val drained =
            closeIn(process) *>
              slurp(process.getInputStream).zipPar(slurp(process.getErrorStream)).flatMap { (out, _) =>
                ZIO.attemptBlockingInterrupt(process.waitFor()).orDie.map(code => interpret(code, out))
              }
          drained.timeout(Wait).flatMap {
            case None         => ZIO.fail(KeyError.Unavailable)
            case Some(parsed) => ZIO.fromEither(parsed)
          }
        }

    private def start(args: Chunk[String]): IO[KeyError, Process] =
      ZIO
        .attemptBlockingInterrupt(new ProcessBuilder(args.toList*).start())
        .refineToOrDie[java.io.IOException]
        .mapError(_ => KeyError.Unavailable)

    private def stop(process: Process): UIO[Unit] =
      ZIO.attemptBlocking(process.destroy()).ignore

    private def closeIn(process: Process): IO[KeyError, Unit] =
      ZIO
        .attemptBlockingInterrupt(process.getOutputStream.close())
        .refineToOrDie[java.io.IOException]
        .mapError(_ => KeyError.Unavailable)

    private def slurp(in: java.io.InputStream): IO[KeyError, String] =
      ZIO
        .attemptBlockingInterrupt(new String(in.readAllBytes(), StandardCharsets.UTF_8))
        .refineToOrDie[java.io.IOException]
        .mapError(_ => KeyError.Unavailable)
  end Live
end Keychain
