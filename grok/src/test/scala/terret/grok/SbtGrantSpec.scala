package terret.grok

import zio.durationInt
import zio.test.*
import zio.{Chunk, ZIO}

import java.io.IOException
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

object SbtGrantSpec extends ZIOSpecDefault:
  def spec = suite("sdkman sbt grant")(
    test("current may resolve under sdks, and a sibling there is not granted") {
      withHome { home =>
        for
          version <- install(home, home.resolve("sdks").resolve("sbt-v"))
          other   <- ZIO.attemptBlocking(Files.createDirectories(home.resolve("sdks").resolve("other"))).orDie
          got     <- SbtGrant.decide(home)
        yield
          val root  = version.toString
          val link  = current(home).toString
          val lines = profile(got).split('\n')
          assertTrue(
            got.root.text == root,
            got.link.text == link,
            lines.contains(s"(allow file-read* (subpath \"$root\"))"),
            lines.contains(s"(allow file-read-metadata (literal \"$link\"))"),
            lines.contains(s"(allow file-read-data (literal \"$link\"))"),
            !lines.contains(s"(allow file-read* (subpath \"${home.resolve("sdks").toString}\"))"),
            !lines.contains(s"(allow file-read* (subpath \"${other.toString}\"))"),
            !lines.exists(line => line.contains("file-write") && line.contains(root)),
          )
      }
    },
    test("a target inside candidates/sbt is granted") {
      withHome { home =>
        for
          version <- install(home, home.resolve(".sdkman").resolve("candidates").resolve("sbt").resolve("2"))
          got     <- SbtGrant.decide(home)
        yield assertTrue(got.root.text == version.toString)
      }
    },
    test("a target under home but outside sdks is outside") {
      withHome { home =>
        TestHome.lay {
          val notes = Files.createDirectories(home.resolve("notes"))
          Files.createSymbolicLink(sbtDir(home).resolve("current"), notes)
        } *> SbtGrant.decide(home).either.map(got => assertTrue(got == Left(JdkRefuse.Outside)))
      }
    },
    test("a link to the sdks directory itself is outside") {
      withHome { home =>
        TestHome.lay {
          val sdks = Files.createDirectories(home.resolve("sdks"))
          Files.createSymbolicLink(sbtDir(home).resolve("current"), sdks)
        } *> SbtGrant.decide(home).either.map(got => assertTrue(got == Left(JdkRefuse.Outside)))
      }
    },
    test("sdks symlinked elsewhere inside home is outside") {
      withHome { home =>
        TestHome.lay {
          val other   = Files.createDirectories(home.resolve("other-sdks"))
          val version = Files.createDirectories(other.resolve("sbt-v"))
          Files.createSymbolicLink(home.resolve("sdks"), other)
          Files.createSymbolicLink(sbtDir(home).resolve("current"), version)
        } *> SbtGrant.decide(home).either.map(got => assertTrue(got == Left(JdkRefuse.Outside)))
      }
    },
    test("a target outside home is outside") {
      withHome { home =>
        TestHome.lay {
          val sbt = sbtDir(home)
          Files.createSymbolicLink(sbt.resolve("current"), Path.of("/"))
        } *> SbtGrant.decide(home).either.map(got => assertTrue(got == Left(JdkRefuse.Outside)))
      }
    },
    test("candidates/sbt symlinked elsewhere inside home is outside") {
      withHome { home =>
        TestHome.lay {
          val other  = Files.createDirectories(home.resolve("other-sbt"))
          val parent = home.resolve(".sdkman").resolve("candidates")
          Files.createDirectories(parent)
          Files.createSymbolicLink(parent.resolve("sbt"), other)
        } *> SbtGrant.decide(home).either.map(got => assertTrue(got == Left(JdkRefuse.Outside)))
      }
    },
    suite("macos sandbox")(
      test("current/bin/tool runs, and a sibling under sdks is denied") {
        withHome[SandboxError, TestResult]: home =>
          ZIO.scoped:
            ShellSandbox.make(home).flatMap: sandbox =>
              for
                tree    <- toolTree(home)
                bounds  = ShellBounds(tree.work, Chunk(tree.work), Chunk.empty, network = false, writable = false)
                ran     <- sandbox.run(Chunk(tree.bin.toString), bounds)
                sibling <- sandbox.run(Chunk("/bin/cat", tree.sibling.toString), bounds)
              yield assertTrue(ran.code == 0, ran.stdout == "ok", TestHome.denied(sibling))
      } @@ TestAspect.timeout(30.seconds),
    ).when(macPresent),
  )

  private val macPresent: Boolean =
    val present = ShellSandbox.available
    ShellSandbox.skipReason(present).foreach(reason => System.err.println(s"SKIP macos sandbox: $reason"))
    present

  private final case class ToolTree(work: Path, bin: Path, sibling: Path)

  private def profile(grant: Granted): String =
    SeatbeltProfile.render(
      ShellSandbox.policyOf(
        Chunk.empty,
        Chunk.empty,
        grant.root,
        network = false,
        writable = false,
        jdk = None,
        sbt = Some(grant),
        caches = Chunk.empty,
        temp = None,
        existingSystem = _ => None,
      )
    )

  private def toolTree(home: Path): ZIO[Any, Nothing, ToolTree] =
    ZIO
      .attemptBlocking {
        val root = Files.createDirectories(home.resolve("sdks").resolve("sbt-v"))
        val bin  = Files.createDirectories(root.resolve("bin"))
        val tool = bin.resolve("tool")
        Files.writeString(tool, "#!/bin/sh\nprintf ok\n")
        Files.setPosixFilePermissions(tool, PosixFilePermissions.fromString("rwxr-xr-x"))
        val sbt = sbtDir(home)
        Files.createSymbolicLink(sbt.resolve("current"), root)
        val other = Files.createDirectories(home.resolve("sdks").resolve("other"))
        val note  = other.resolve("hidden")
        Files.writeString(note, "no")
        val work = Files.createDirectories(home.resolve("work"))
        val via  = sbt.resolve("current").resolve("bin").resolve("tool")
        ToolTree(work, via, note)
      }
      .orDie

  private def install(home: Path, version: Path): ZIO[Any, Nothing, Path] =
    ZIO
      .attemptBlocking {
        val dir = Files.createDirectories(version)
        val sbt = sbtDir(home)
        Files.createSymbolicLink(sbt.resolve("current"), dir)
        dir.toRealPath()
      }
      .orDie

  private def sbtDir(home: Path): Path =
    Files.createDirectories(home.resolve(".sdkman").resolve("candidates").resolve("sbt"))

  private def current(home: Path): Path =
    home.resolve(".sdkman").resolve("candidates").resolve("sbt").resolve("current")

  private def withHome[E, A](use: Path => ZIO[Any, E, A]): ZIO[Any, E | IOException, A] =
    ZIO.scoped {
      ZIO.acquireRelease(TestHome.scratch("sbt-grant"))(TestHome.delete).flatMap(use)
    }
end SbtGrantSpec
