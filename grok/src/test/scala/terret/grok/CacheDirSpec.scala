package terret.grok

import zio.durationInt
import zio.test.*
import zio.{Chunk, Trace, ZIO}

import java.io.IOException
import java.nio.file.{Files, LinkOption, Path}

import scala.jdk.CollectionConverters.*

object CacheDirSpec extends ZIOSpecDefault:
  def spec = suite("build cache grant")(
    test("the allowlist is five cache directories and nothing above them") {
      val got = CacheDir.allowlist.map(_.segments)
      assertTrue(
        got == Chunk(
          Chunk(".histos"),
          Chunk(".cache", "coursier"),
          Chunk("Library", "Caches", "Coursier"),
          Chunk(".ivy2"),
          Chunk(".m2"),
        ),
      )
    },
    test("a relative home grants nothing") {
      CacheDir.decide(Path.of("relative-home")).map(got => assertTrue(got.isEmpty))
    },
    test("a real cache directory is readable and writable, and its parent is not") {
      withYard { yard =>
        lay {
          Files.createDirectories(yard.home.resolve(".cache").resolve("coursier"))
          Files.createDirectories(yard.home.resolve("Library").resolve("Caches").resolve("Coursier"))
          Files.createDirectories(yard.home.resolve("Library").resolve("Caches").resolve("Other"))
          Files.createDirectories(yard.home.resolve(".histos"))
          Files.createDirectories(yard.home.resolve(".ivy2"))
          Files.createDirectories(yard.home.resolve(".m2"))
        } *> CacheDir.decide(yard.home).map { got =>
          val texts = got.map(_.text)
          texts.find(_.endsWith("/.cache/coursier")) match
            case None => assertTrue(false)
            case Some(coursier) =>
              val parent = coursier.stripSuffix("/coursier")
              val lines  = profile(got).split('\n')
              assertTrue(
                texts.length == 5,
                texts.exists(_.endsWith("/Library/Caches/Coursier")),
                texts.exists(_.endsWith("/.histos")),
                texts.exists(_.endsWith("/.ivy2")),
                texts.exists(_.endsWith("/.m2")),
                !texts.exists(_.endsWith("/.cache")),
                !texts.exists(_.endsWith("/Library")),
                !texts.exists(_.endsWith("/Library/Caches")),
                !texts.exists(_.endsWith("/Library/Caches/Other")),
                lines.contains(s"(allow file-read* (subpath \"$coursier\"))"),
                lines.contains(s"(allow file-write* (subpath \"$coursier\"))"),
                !lines.contains(s"(allow file-read* (subpath \"$parent\"))"),
                !lines.contains(s"(allow file-write* (subpath \"$parent\"))"),
              )
        }
      }
    },
    test("a cache symlink that leaves home is skipped") {
      withYard { yard =>
        lay {
          val escaped = yard.root.resolve("escaped")
          Files.createDirectories(escaped.resolve("coursier"))
          Files.createSymbolicLink(yard.home.resolve(".cache"), escaped)
        } *> CacheDir.decide(yard.home).map { got =>
          assertTrue(got.isEmpty, !got.exists(_.text.contains("escaped")))
        }
      }
    },
    test("a cache symlink to a wider directory inside home is skipped") {
      withYard { yard =>
        lay {
          val library = Files.createDirectories(yard.home.resolve("Library"))
          Files.createDirectories(yard.home.resolve(".cache"))
          Files.createSymbolicLink(yard.home.resolve(".cache").resolve("coursier"), library)
        } *> CacheDir.decide(yard.home).map { got =>
          assertTrue(got.isEmpty, !got.exists(_.text.endsWith("/Library")))
        }
      }
    },
    test("a file where a cache directory should be is skipped") {
      withYard { yard =>
        lay {
          Files.writeString(yard.home.resolve(".histos"), "x")
          Files.createDirectories(yard.home.resolve(".m2"))
        } *> CacheDir.decide(yard.home).map { got =>
          assertTrue(got.length == 1, got.exists(_.text.endsWith("/.m2")), !got.exists(_.text.endsWith("/.histos")))
        }
      }
    },
    test("a cache directory is writable and a sibling cache is denied") {
      withYard[SandboxError, TestResult] { yard =>
        for
          tree    <- cacheTree(yard.home)
          sandbox = ShellSandbox.atHome(yard.home)
          bounds  = ShellBounds(tree.work, Chunk(tree.work), Chunk(tree.work), false)
          roots   <- CacheDir.decide(yard.home)
          text    <- sandbox.profile(bounds)
          wrote   <- sandbox.run(Chunk("/usr/bin/touch", tree.marker.toString), bounds)
          sibling <- sandbox.run(Chunk("/usr/bin/touch", tree.other.toString), bounds)
          exists  <- ZIO.attemptBlocking(Files.exists(tree.marker)).orDie
          leaked  <- ZIO.attemptBlocking(Files.exists(tree.other)).orDie
        yield
          roots.find(_.text.endsWith("/Library/Caches/Coursier")) match
            case None => assertTrue(false)
            case Some(root) =>
              val parent = root.text.stripSuffix("/Coursier")
              val lines  = text.split('\n')
              assertTrue(
                wrote.code == 0,
                exists,
                denied(sibling),
                !leaked,
                lines.contains(s"(allow file-read* (subpath \"${root.text}\"))"),
                lines.contains(s"(allow file-write* (subpath \"${root.text}\"))"),
                !lines.contains(s"(allow file-read* (subpath \"$parent\"))"),
                !lines.contains(s"(allow file-write* (subpath \"$parent\"))"),
                !lines.contains(s"(allow file-write* (subpath \"${parent.stripSuffix("/Caches")}\"))"),
              )
      }
    } @@ macSeatbelt @@ TestAspect.timeout(30.seconds),
  )

  private val macSeatbelt: TestAspect[Nothing, Any, Nothing, Any] =
    new TestAspect[Nothing, Any, Nothing, Any]:
      def some[R >: Nothing <: Any, E >: Nothing <: Any](
        spec: Spec[R, E]
      )(implicit trace: Trace): Spec[R, E] =
        spec.when(ShellSandbox.available)

  private final case class Yard(root: Path, home: Path)

  private final case class CacheTree(work: Path, marker: Path, other: Path)

  private def cacheTree(home: Path): ZIO[Any, Nothing, CacheTree] =
    ZIO.attemptBlocking {
      val coursier = home.resolve("Library").resolve("Caches").resolve("Coursier")
      Files.createDirectories(coursier)
      val otherDir = home.resolve("Library").resolve("Caches").resolve("Other")
      Files.createDirectories(otherDir)
      val work = Files.createDirectories(home.resolve("work"))
      CacheTree(work, coursier.resolve("landed"), otherDir.resolve("landed"))
    }.orDie

  private def profile(roots: Chunk[AbsPath]): String =
    SeatbeltProfile.render(SeatbeltPolicy(roots, roots, Chunk.empty, false))

  private def denied(out: SandboxOutput): Boolean =
    out.code != 0 &&
      (out.stderr.contains("Operation not permitted") || out.stdout.contains("Operation not permitted"))

  private def lay(prepare: => Unit): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(prepare).orDie

  private def withYard[E, A](use: Yard => ZIO[Any, E, A]): ZIO[Any, E | IOException, A] =
    ZIO.scoped {
      ZIO.acquireRelease(scratch)(yard => deleteTree(yard.root)).flatMap(use)
    }

  private def scratch: ZIO[Any, IOException, Yard] =
    ZIO.attemptBlocking {
      val base = Path.of("target")
      Files.createDirectories(base)
      val root = Files.createTempDirectory(base, "cache-grant")
      val home = Files.createDirectories(root.resolve("home"))
      Yard(root, home)
    }.refineToOrDie[IOException]

  private def deleteTree(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(remove(root)).orDie

  private def remove(path: Path): Unit =
    if !Files.isSymbolicLink(path) && Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then
      val kids = Files.list(path)
      try Chunk.from(kids.iterator.asScala).foreach(remove)
      finally kids.close()
    Files.deleteIfExists(path)
end CacheDirSpec
