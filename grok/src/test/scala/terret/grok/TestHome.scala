package terret.grok

import zio.ZIO

import java.io.IOException
import java.nio.file.{Files, LinkOption, Path}

/** Absolute real scratch directory. `decide` refuses a relative home. */
object TestHome:
  def scratch(prefix: String): ZIO[Any, IOException, Path] =
    ZIO
      .attemptBlocking {
        val base = Path.of("target").toAbsolutePath
        Files.createDirectories(base)
        val made = Files.createTempDirectory(base, prefix)
        made.toRealPath(LinkOption.NOFOLLOW_LINKS)
      }
      .refineToOrDie[IOException]

  def delete(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(remove(root)).orDie

  def lay(prepare: => Unit): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(prepare).orDie

  def denied(out: SandboxOutput): Boolean =
    out.code != 0 &&
      (out.stderr.contains("Operation not permitted") || out.stdout.contains("Operation not permitted"))

  def remove(path: Path): Unit =
    DirTree.remove(path)
end TestHome
