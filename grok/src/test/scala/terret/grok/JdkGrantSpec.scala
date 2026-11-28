package terret.grok

import zio.durationInt
import zio.test.*
import zio.{Chunk, ZIO}

import java.io.IOException
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}

object JdkGrantSpec extends ZIOSpecDefault:
  def spec = suite("sdkman jdk grant")(
    test("a directory inside candidates/java is granted, and the link is only a literal") {
      withHome { home =>
        for
          version <- install(home, "25.0.1")
          real    <- ZIO.attemptBlocking(version.toRealPath()).refineToOrDie[IOException]
          got     <- JdkGrant.decide(home)
        yield
          val root = real.toString
          val link = real.resolveSibling("current").toString
          val lines = SeatbeltProfile
            .render(
              ShellSandbox.policyOf(
                Chunk.empty,
                Chunk.empty,
                got.root,
                network = false,
                writable = false,
                jdk = Some(got),
                sbt = None,
                caches = Chunk.empty,
                temp = None,
                existingSystem = _ => None,
              )
            )
            .split('\n')
          assertTrue(
            got.root.text == root,
            got.link.text == link,
            lines.contains(s"(allow file-read* (subpath \"$root\"))"),
            lines.contains(s"(allow file-read-metadata (literal \"$link\"))"),
            lines.contains(s"(allow file-read-data (literal \"$link\"))"),
            !lines.contains(s"(allow file-read* (subpath \"$link\"))"),
            !lines.exists(_.startsWith("(allow file-map-executable (subpath")),
            !lines.exists(line => line.contains("file-write") && line.contains(root)),
          )
      }
    },
    test("a relative home is a bad path") {
      JdkGrant.decide(Path.of("relative-home")).either.map(got => assertTrue(got == Left(JdkRefuse.BadPath)))
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
        TestHome.lay {
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
        TestHome.lay {
          val parent = home.resolve(".sdkman").resolve("candidates")
          Files.createDirectories(parent)
          Files.createSymbolicLink(parent.resolve("java"), Path.of("/"))
        } *> decided(home, JdkRefuse.Outside)
      }
    },
    test("a file inside candidates/java is not a directory") {
      withHome { home =>
        TestHome.lay {
          val java = javaDir(home)
          Files.writeString(java.resolve("note"), "x")
          Files.createSymbolicLink(current(home), Path.of("note"))
        } *> decided(home, JdkRefuse.NotDirectory)
      }
    },
    test("candidates/java symlinked elsewhere inside home is outside") {
      withHome { home =>
        TestHome.lay {
          val other  = Files.createDirectories(home.resolve("other-java"))
          val parent = home.resolve(".sdkman").resolve("candidates")
          Files.createDirectories(parent)
          Files.createSymbolicLink(parent.resolve("java"), other)
        } *> decided(home, JdkRefuse.Outside)
      }
    },
    test("a symlinked .sdkman fails closed") {
      withHome { home =>
        TestHome.lay {
          val real = Files.createDirectories(home.resolve("sdkman-real").resolve("candidates").resolve("java"))
          Files.createDirectories(real.resolve("v1"))
          Files.createSymbolicLink(real.resolve("current"), Path.of("v1"))
          Files.createSymbolicLink(home.resolve(".sdkman"), home.resolve("sdkman-real"))
        } *> decided(home, JdkRefuse.Outside)
      }
    },
    test("a link that steps out with .. is outside") {
      withHome { home =>
        TestHome.lay {
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
        TestHome.lay {
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
        TestHome.lay {
          val java = javaDir(home)
          Files.createSymbolicLink(java.resolve("current"), Path.of("next"))
          Files.createSymbolicLink(java.resolve("next"), Path.of("current"))
        } *> decided(home, JdkRefuse.Loop)
      }
    },
    test("a control character is rejected and not rendered") {
      named("jdk\n25").map { got =>
        assertTrue(
          AbsPath.from("/sdk/java/25\nx") == Left(AbsPathError.Illegal),
          got == Left(JdkRefuse.BadPath),
        )
      }
    },
    suite("macos sandbox")(
      test("current/bin/tool runs through the link, and a sibling read or a write is denied") {
        withHome[SandboxError, TestResult]: home =>
          ZIO.scoped:
            ShellSandbox.make(home).flatMap: sandbox =>
              for
                tree    <- toolTree(home)
                bounds  = ShellBounds(
                  tree.work,
                  Chunk(tree.work),
                  Chunk(tree.work),
                  network = false,
                  writable = false,
                )
                ran     <- sandbox.run(Chunk(tree.bin.toString), bounds)
                sibling <- sandbox.run(Chunk("/bin/cat", tree.sibling.toString), bounds)
                etc     <- sandbox.run(Chunk("/bin/cat", tree.etc.toString), bounds)
                wrote   <- sandbox.run(Chunk("/usr/bin/touch", tree.marker.toString), bounds)
                exists  <- ZIO.attemptBlocking(Files.exists(tree.marker)).orDie
              yield assertTrue(
                ran.code == 0,
                ran.stdout == "ok",
                TestHome.denied(sibling),
                TestHome.denied(etc),
                TestHome.denied(wrote),
                !exists,
              )
      } @@ TestAspect.timeout(30.seconds),
    ).when(macPresent),
  )

  private val macPresent: Boolean =
    val present = ShellSandbox.available
    ShellSandbox.skipReason(present).foreach(reason => System.err.println(s"SKIP macos sandbox: $reason"))
    present

  private def named(name: String): ZIO[Any, IOException, Either[JdkRefuse, Granted]] =
    withHome { home =>
      TestHome.lay {
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

  private final case class ToolTree(work: Path, bin: Path, sibling: Path, etc: Path, marker: Path)

  private def toolTree(home: Path): ZIO[Any, Nothing, ToolTree] =
    ZIO.attemptBlocking {
      val java   = javaDir(home)
      val binDir = java.resolve("v1").resolve("bin")
      val tool   = binDir.resolve("tool")
      Files.createDirectories(binDir)
      Files.writeString(tool, "#!/bin/sh\nprintf ok\n")
      Files.setPosixFilePermissions(tool, PosixFilePermissions.fromString("rwxr-xr-x"))
      Files.createSymbolicLink(java.resolve("current"), Path.of("v1"))
      val siblingDir = java.resolve("v2")
      Files.createDirectories(siblingDir)
      val sibling = siblingDir.resolve("hidden")
      Files.writeString(sibling, "no")
      val etcDir = home.resolve(".sdkman").resolve("etc")
      Files.createDirectories(etcDir)
      val note = etcDir.resolve("note")
      Files.writeString(note, "no")
      val work   = Files.createDirectories(home.resolve("work"))
      val via    = java.resolve("current").resolve("bin").resolve("tool")
      val marker = java.resolve("v1").resolve("new-file")
      ToolTree(work, via, sibling, note, marker)
    }.orDie

  private def point(home: Path, target: Path): ZIO[Any, Nothing, Unit] =
    TestHome.lay {
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

  private def withHome[E, A](use: Path => ZIO[Any, E, A]): ZIO[Any, E | IOException, A] =
    ZIO.scoped {
      ZIO.acquireRelease(TestHome.scratch("jdk-grant"))(TestHome.delete).flatMap(use)
    }
end JdkGrantSpec
