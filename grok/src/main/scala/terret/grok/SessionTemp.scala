package terret.grok

import zio.{IO, UIO, ZIO}

import java.io.IOException
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}

/** One private directory for a sandbox. The parent is not part of the grant. */
private[grok] object SessionTemp:
  def open(parent: Path): IO[SandboxError, Path] =
    ZIO
      .attemptBlocking(create(parent))
      .refineOrDie {
        case _: IOException | _: SecurityException | _: InvalidPathException => SandboxError.Unavailable
      }

  def openDefault: IO[SandboxError, Path] =
    ZIO
      .attemptBlocking {
        val raw = Option(System.getProperty("java.io.tmpdir")).filter(_.nonEmpty).getOrElse("/tmp")
        create(Path.of(raw))
      }
      .refineOrDie {
        case _: IOException | _: SecurityException | _: InvalidPathException => SandboxError.Unavailable
      }

  def remove(dir: Path): UIO[Unit] =
    ZIO.attemptBlocking(DirTree.remove(dir)).ignore

  private def create(parent: Path): Path =
    Files.createDirectories(parent)
    val dir   = Files.createTempDirectory(parent, "terret-")
    val perms = PosixFilePermissions.fromString("rwx------")
    Files.setPosixFilePermissions(dir, perms)
    dir.toRealPath(LinkOption.NOFOLLOW_LINKS)
end SessionTemp
