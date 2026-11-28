package terret.grok

import zio.{IO, UIO, ZIO}

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, Path}

/** One private directory for a sandbox. The parent is not part of the grant. */
private[grok] object SessionTemp:
  def open(parent: Path): IO[SandboxError, Path] =
    ZIO
      .attemptBlocking {
        Files.createDirectories(parent)
        Files.createTempDirectory(parent, "terret-")
      }
      .refineOrDie {
        case _: IOException | _: SecurityException | _: InvalidPathException => SandboxError.Unavailable
      }
      .flatMap(resolve)

  def openDefault: IO[SandboxError, Path] =
    ZIO
      .attemptBlocking {
        val raw = Option(System.getProperty("java.io.tmpdir")).filter(_.nonEmpty).getOrElse("/tmp")
        Path.of(raw)
      }
      .refineOrDie {
        case _: IOException | _: SecurityException | _: InvalidPathException => SandboxError.Unavailable
      }
      .flatMap(open)

  def remove(dir: Path): UIO[Unit] =
    ZIO.attemptBlocking(DirTree.remove(dir)).catchAll { _ =>
      ZIO.logWarning(s"session temp directory was not removed: $dir")
    }

  private def resolve(dir: Path): IO[SandboxError, Path] =
    ZIO
      .attemptBlocking(dir.toRealPath())
      .refineOrDie {
        case _: IOException | _: SecurityException | _: InvalidPathException => SandboxError.Unavailable
      }
      .tapError(_ => remove(dir))
      .onInterrupt(remove(dir))
end SessionTemp
