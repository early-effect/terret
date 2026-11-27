package terret.grok

import zio.test.*
import zio.{Scope, ZIO}

import java.nio.file.{Files, LinkOption, Path}

object FileRootsSpec extends ZIOSpecDefault:
  def spec = suite("FileRoots")(
    test("tilde and dotdot stay lexical") {
      val base = Path.of("/work")
      val home = Path.of("/Users/someone")
      assertTrue(
        FileRoots.lexical("~/.cursor", base, home).contains(Path.of("/Users/someone/.cursor")),
        FileRoots.lexical("~/src", base, home).contains(Path.of("/Users/someone/src")),
        FileRoots.lexical("~", base, home).contains(home),
        FileRoots.lexical("/work/../outside", base, home).contains(Path.of("/outside")),
        FileRoots.lexical("../outside", base, home).contains(Path.of("/outside")),
        FileRoots.lexical("src/A.scala", base, home).contains(Path.of("/work/src/A.scala")),
        FileRoots.lexical("~not-a-user/secret", base, home).isEmpty ||
          System.getProperty("user.name") == "not-a-user",
      )
    },
    test("a symlink is resolved when the call runs, and a missing suffix stays under its real parent") {
      prepared.flatMap { (root, outside) =>
        val link    = root.resolve("link")
        val rel     = root.resolve("rel")
        val missing = root.resolve("new").resolve("file.txt")
        val hop     = root.relativize(outside)
        for
          _       <- ZIO.succeed(Files.createSymbolicLink(rel, hop))
          escaped <- FileRoots.live.resolve(link)
          hopped  <- FileRoots.live.resolve(rel)
          created <- FileRoots.live.resolve(missing)
          realOut <- FileRoots.live.resolve(outside)
          realDir <- FileRoots.live.resolve(root)
        yield assertTrue(
          escaped == realOut,
          hopped == realOut,
          created == realDir.map(_.resolve("new").resolve("file.txt")),
          escaped.exists(path => realDir.exists(dir => !path.startsWith(dir))),
        )
      }
    },
  )

  private def prepared: ZIO[Scope, Nothing, (Path, Path)] =
    ZIO.acquireRelease {
      ZIO.succeed {
        val base = Path.of("target").toAbsolutePath
        Files.createDirectories(base)
        val root    = Files.createTempDirectory(base, "confine-")
        val outside = Files.createTempDirectory(base, "confine-out-")
        Files.createSymbolicLink(root.resolve("link"), outside)
        (root, outside)
      }
    } { (root, outside) =>
      ZIO.succeed {
        deleteTree(root)
        deleteTree(outside)
      }
    }

  private def deleteTree(path: Path): Unit =
    if Files.exists(path, LinkOption.NOFOLLOW_LINKS) then
      if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) && !Files.isSymbolicLink(path) then
        val listed = Files.list(path)
        try drain(listed.iterator)
        finally listed.close()
      Files.deleteIfExists(path)

  private def drain(rest: java.util.Iterator[Path]): Unit =
    if rest.hasNext then
      deleteTree(rest.next())
      drain(rest)
end FileRootsSpec
