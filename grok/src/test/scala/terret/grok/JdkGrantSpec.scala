package terret.grok

import zio.test.*
import zio.{Chunk, ZIO}

import java.io.IOException
import java.nio.file.{Files, LinkOption, Path}

import scala.jdk.CollectionConverters.*

object JdkGrantSpec extends ZIOSpecDefault:
  def spec = suite("sdkman jdk grant")(
    test("a directory inside candidates/java is granted, and the link path is not") {
      withHome { home =>
        for
          version <- install(home, "25.0.1")
          real    <- ZIO.attemptBlocking(version.toRealPath().toString).orDie
          got     <- JdkGrant.decide(home)
          text    <- ZIO.fromEither(profile(got))
        yield
          val link  = current(home).toString
          val lines = text.split('\n')
          assertTrue(
            got.text == real,
            lines.contains(s"(allow file-read* (subpath \"$real\"))"),
            lines.contains(s"(allow file-map-executable (subpath \"$real\"))"),
            lines.contains("(allow file-map-executable)"),
            !lines.exists(line => line.contains("file-write") && line.contains(real)),
            !text.contains(link),
          )
      }
    },
    test("a target of / is outside") {
      refused(_.resolve("/"), JdkRefuse.Outside)
    },
    test("a target of the home directory is outside") {
      withHome { home =>
        point(home, home) *> decided(home, JdkRefuse.Outside)
      }
    },
    test("a target beside the home directory is outside") {
      withHome { home =>
        for
          ssh <- ZIO.attemptBlocking(Files.createDirectories(home.resolve("ssh-like"))).orDie
          _   <- point(home, ssh)
          got <- decided(home, JdkRefuse.Outside)
        yield got
      }
    },
    test("a sibling outside candidates/java is outside") {
      withHome { home =>
        lay {
          val scala = Files.createDirectories(home.resolve(".sdkman").resolve("candidates").resolve("scala"))
          val java  = javaDir(home)
          Files.createDirectories(java.resolve("25.0.1"))
          Files.createSymbolicLink(java.resolve("current"), Path.of("25.0.1"))
          Files.delete(current(home))
          Files.createSymbolicLink(current(home), scala)
        } *> decided(home, JdkRefuse.Outside)
      }
    },
    test("a candidates directory that resolves outside home is outside") {
      withHome { home =>
        lay {
          val parent = home.resolve(".sdkman").resolve("candidates")
          Files.createDirectories(parent)
          Files.createSymbolicLink(parent.resolve("java"), Path.of("/"))
        } *> decided(home, JdkRefuse.Outside)
      }
    },
    test("a link that steps out with .. is outside") {
      withHome { home =>
        lay {
          val java    = javaDir(home)
          val outside = Files.createDirectories(home.resolve("outside"))
          Files.createDirectories(home.resolve("marker"))
          Files.createDirectories(java.resolve("marker"))
          Files.createSymbolicLink(java.resolve("out"), outside)
          Files.createSymbolicLink(java.resolve("current"), Path.of("out/../marker"))
        } *> decided(home, JdkRefuse.Outside)
      }
    },
    test("a dangling link is dangling") {
      withHome { home =>
        lay {
          javaDir(home)
          Files.createSymbolicLink(current(home), Path.of("missing"))
        } *> decided(home, JdkRefuse.Dangling)
      }
    },
    test("a missing current link is absent") {
      withHome { home =>
        ZIO.attemptBlocking(javaDir(home)).orDie *> decided(home, JdkRefuse.Absent)
      }
    },
    test("a symlink loop is a loop") {
      withHome { home =>
        lay {
          val java = javaDir(home)
          Files.createSymbolicLink(java.resolve("current"), Path.of("next"))
          Files.createSymbolicLink(java.resolve("next"), Path.of("current"))
        } *> decided(home, JdkRefuse.Loop)
      }
    },
    test("a quote or a control character is rejected and not rendered") {
      val quote   = """/sdk/java/25"x"""
      val control = "/sdk/java/25\nx"
      for
        quoted  <- named("jdk\"25")
        newline <- named("jdk\n25")
      yield assertTrue(
        AbsPath.from(quote) == Left(JdkRefuse.BadPath),
        AbsPath.from(control) == Left(JdkRefuse.BadPath),
        quoted == Left(JdkRefuse.BadPath),
        newline == Left(JdkRefuse.BadPath),
      )
    },
  )

  private def named(name: String): ZIO[Any, Any, Either[JdkRefuse, AbsPath]] =
    withHome { home =>
      lay {
        val java = javaDir(home)
        Files.createDirectory(java.resolve(name))
        Files.createSymbolicLink(current(home), Path.of(name))
      } *> JdkGrant.decide(home).either
    }

  private def refused(target: Path => Path, why: JdkRefuse) =
    withHome { home =>
      point(home, target(home)) *> decided(home, why)
    }

  private def decided(home: Path, why: JdkRefuse) =
    JdkGrant.decide(home).either.map(got => assertTrue(got == Left(why)))

  private def lay(prepare: => Unit): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(prepare).orDie

  private def profile(root: AbsPath): Either[SandboxError, String] =
    SeatbeltProfile.render(
      SeatbeltPolicy(Chunk(root.text), Chunk.empty, Chunk("/"), false, Chunk.empty, Chunk(root.text))
    )

  private def point(home: Path, target: Path): ZIO[Any, Nothing, Unit] =
    lay {
      javaDir(home)
      Files.createSymbolicLink(current(home), target)
    }

  private def install(home: Path, version: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      val java = javaDir(home)
      val dir  = java.resolve(version)
      Files.createDirectories(dir)
      Files.createSymbolicLink(java.resolve("current"), Path.of(version))
      dir
    }.orDie

  private def javaDir(home: Path): Path =
    Files.createDirectories(home.resolve(".sdkman").resolve("candidates").resolve("java"))

  private def current(home: Path): Path =
    home.resolve(".sdkman").resolve("candidates").resolve("java").resolve("current")

  private def withHome[A](use: Path => ZIO[Any, Any, A]): ZIO[Any, Any, A] =
    ZIO.scoped {
      ZIO.acquireRelease(scratch)(deleteTree).flatMap(use)
    }

  private def scratch: ZIO[Any, IOException, Path] =
    ZIO.attemptBlocking {
      val root = Path.of("target")
      Files.createDirectories(root)
      Files.createTempDirectory(root, "jdk-grant")
    }.refineToOrDie[IOException]

  private def deleteTree(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(remove(root)).orDie

  private def remove(path: Path): Unit =
    if !Files.isSymbolicLink(path) && Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then
      val kids = Files.list(path)
      try Chunk.from(kids.iterator.asScala).foreach(remove)
      finally kids.close()
    Files.deleteIfExists(path)
end JdkGrantSpec
