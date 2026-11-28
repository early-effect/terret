package terret.grok

import zio.durationInt
import zio.test.*
import zio.{Chunk, ZIO}

import java.io.IOException
import java.nio.file.{Files, Path}

object CacheDirSpec extends ZIOSpecDefault:
  def spec = suite("build cache grant")(
    test("the allowlist is seven cache directories and nothing above them") {
      val got = CacheDir.allowlist.map(_.segments)
      assertTrue(
        got == Chunk(
          Chunk(".histos"),
          Chunk(".cache", "histos"),
          Chunk(".cache", "coursier"),
          Chunk("Library", "Caches", "Coursier"),
          Chunk(".cache", "sbt"),
          Chunk(".ivy2"),
          Chunk(".m2"),
        ),
      )
    },
    test("a relative home grants nothing") {
      CacheDir.decide(Path.of("relative-home")).map(got => assertTrue(got.isEmpty))
    },
    test("a real cache directory is granted, and its parent is not") {
      withYard { yard =>
        TestHome.lay {
          Files.createDirectories(yard.home.resolve(".cache").resolve("coursier"))
          Files.createDirectories(yard.home.resolve(".cache").resolve("histos"))
          Files.createDirectories(yard.home.resolve(".cache").resolve("sbt").resolve("boot"))
          Files.createDirectories(yard.home.resolve("Library").resolve("Caches").resolve("Coursier"))
          Files.createDirectories(yard.home.resolve("Library").resolve("Caches").resolve("Other"))
          Files.createDirectories(yard.home.resolve(".histos"))
          Files.createDirectories(yard.home.resolve(".ivy2"))
          Files.createDirectories(yard.home.resolve(".m2"))
        } *> CacheDir.decide(yard.home).map { got =>
          val texts = got.map(_.root.text)
          texts.find(_.endsWith("/.cache/coursier")) match
            case None           => assertTrue(false)
            case Some(coursier) =>
              val parent        = coursier.stripSuffix("/coursier")
              val lines         = render(got, writable = true).split('\n')
              val readCoursier  = s"(allow file-read* (subpath \"$coursier\"))"
              val writeCoursier = s"(allow file-write* (subpath \"$coursier\"))"
              val readParent    = s"(allow file-read* (subpath \"$parent\"))"
              val writeParent   = s"(allow file-write* (subpath \"$parent\"))"
              assertTrue(
                texts.length == 7,
                texts.exists(_.endsWith("/.cache/histos")),
                texts.exists(_.endsWith("/.cache/sbt")),
                !texts.exists(_.endsWith("/.cache/sbt/boot")),
              ) && assertTrue(
                texts.exists(_.endsWith("/Library/Caches/Coursier")),
                texts.exists(_.endsWith("/.histos")),
                texts.exists(_.endsWith("/.ivy2")),
                texts.exists(_.endsWith("/.m2")),
              ) && assertTrue(
                !texts.exists(_.endsWith("/.cache")),
                !texts.exists(_.endsWith("/Library")),
                !texts.exists(_.endsWith("/Library/Caches")),
                !texts.exists(_.endsWith("/Library/Caches/Other")),
              ) && assertTrue(
                lines.contains(readCoursier),
                lines.contains(writeCoursier),
                !lines.contains(readParent),
                !lines.contains(writeParent),
              )
        }
      }
    },
    test("caches are readable without WriteWorkspace and writable only with it") {
      withYard { yard =>
        TestHome.lay {
          Files.createDirectories(yard.home.resolve(".m2"))
          Files.createDirectories(yard.home.resolve(".ivy2"))
        } *> CacheDir.decide(yard.home).map { got =>
          val maven = got.find(_.dir == CacheDir.Maven)
          val ivy   = got.find(_.dir == CacheDir.Ivy)
          (maven, ivy) match
            case (Some(m2), Some(ivyDir)) =>
              val root         = m2.root.text
              val secret       = root + "/settings.xml"
              val guard        = root + "/settings-security.xml"
              val creds        = ivyDir.root.text + "/.credentials"
              val off          = render(got, writable = false).split('\n')
              val on           = render(got, writable = true).split('\n')
              val allowRead    = s"(allow file-read* (subpath \"$root\"))"
              val allowWrite   = s"(allow file-write* (subpath \"$root\"))"
              val denyReadSec  = s"(deny file-read* (literal \"$secret\"))"
              val denyReadGrd  = s"(deny file-read* (literal \"$guard\"))"
              val denyReadCrd  = s"(deny file-read* (literal \"$creds\"))"
              val denyWriteSec = s"(deny file-write* (literal \"$secret\"))"
              val denyWriteGrd = s"(deny file-write* (literal \"$guard\"))"
              val denyWriteCrd = s"(deny file-write* (literal \"$creds\"))"
              val readDenies   = denyFollows(off, allowRead, denyReadSec)
              val writeDenies  = denyFollows(on, allowWrite, denyWriteSec)
              assertTrue(
                off.contains(allowRead),
                !off.contains(allowWrite),
                on.contains(allowWrite),
                off.contains(denyReadSec),
              ) && assertTrue(
                off.contains(denyReadGrd),
                off.contains(denyReadCrd),
                off.contains(denyWriteSec),
                off.contains(denyWriteGrd),
              ) && assertTrue(
                off.contains(denyWriteCrd),
                on.contains(denyWriteSec),
                readDenies,
                writeDenies,
              ) && assertTrue(!off.contains("(allow network*)"))
            case _ => assertTrue(false)
        }
      }
    },
    test("a cache symlink that leaves home is skipped") {
      withYard { yard =>
        TestHome.lay {
          val escaped = yard.root.resolve("escaped")
          Files.createDirectories(escaped.resolve("coursier"))
          Files.createSymbolicLink(yard.home.resolve(".cache"), escaped)
        } *> CacheDir.decide(yard.home).map { got =>
          assertTrue(got.isEmpty, !got.exists(_.root.text.contains("escaped")))
        }
      }
    },
    test("a cache symlink to a wider directory inside home is skipped") {
      withYard { yard =>
        TestHome.lay {
          val library = Files.createDirectories(yard.home.resolve("Library"))
          Files.createDirectories(yard.home.resolve(".cache"))
          Files.createSymbolicLink(yard.home.resolve(".cache").resolve("coursier"), library)
        } *> CacheDir.decide(yard.home).map { got =>
          assertTrue(got.isEmpty, !got.exists(_.root.text.endsWith("/Library")))
        }
      }
    },
    test("a file where a cache directory should be is skipped") {
      withYard { yard =>
        TestHome.lay {
          Files.writeString(yard.home.resolve(".histos"), "x")
          Files.createDirectories(yard.home.resolve(".m2"))
        } *> CacheDir.decide(yard.home).map { got =>
          assertTrue(
            got.length == 1,
            got.exists(_.root.text.endsWith("/.m2")),
            !got.exists(_.root.text.endsWith("/.histos")),
          )
        }
      }
    },
    suite("macos sandbox")(
      test("a cache directory is writable and a sibling cache is denied") {
        withYard[SandboxError, TestResult]: yard =>
          ZIO.scoped:
            ShellSandbox.make(yard.home).flatMap: sandbox =>
              for
                tree    <- cacheTree(yard.home)
                bounds  = ShellBounds(
                  tree.work,
                  Chunk(tree.work),
                  Chunk(tree.work),
                  network = false,
                  writable = true,
                )
                roots   <- CacheDir.decide(yard.home)
                text    <- sandbox.profile(bounds)
                wrote   <- sandbox.run(Chunk("/usr/bin/touch", tree.marker.toString), bounds)
                sibling <- sandbox.run(Chunk("/usr/bin/touch", tree.other.toString), bounds)
                exists  <- ZIO.attemptBlocking(Files.exists(tree.marker)).orDie
                leaked  <- ZIO.attemptBlocking(Files.exists(tree.other)).orDie
              yield
                roots.find(_.root.text.endsWith("/Library/Caches/Coursier")) match
                  case None       => assertTrue(false)
                  case Some(root) =>
                    val parent      = root.root.text.stripSuffix("/Coursier")
                    val library     = parent.stripSuffix("/Caches")
                    val lines       = text.split('\n')
                    val readRoot    = s"(allow file-read* (subpath \"${root.root.text}\"))"
                    val writeRoot   = s"(allow file-write* (subpath \"${root.root.text}\"))"
                    val readParent  = s"(allow file-read* (subpath \"$parent\"))"
                    val writeParent = s"(allow file-write* (subpath \"$parent\"))"
                    val writeLib    = s"(allow file-write* (subpath \"$library\"))"
                    assertTrue(
                      wrote.code == 0,
                      exists,
                      TestHome.denied(sibling),
                      !leaked,
                    ) && assertTrue(
                      lines.contains(readRoot),
                      lines.contains(writeRoot),
                      !lines.contains(readParent),
                      !lines.contains(writeParent),
                    ) && assertTrue(!lines.contains(writeLib))
      } @@ TestAspect.timeout(30.seconds),
      test("a cache write is denied without WriteWorkspace") {
        withYard[SandboxError, TestResult]: yard =>
          ZIO.scoped:
            ShellSandbox.make(yard.home).flatMap: sandbox =>
              for
                tree   <- cacheTree(yard.home)
                bounds = ShellBounds(tree.work, Chunk(tree.work), Chunk.empty, network = false, writable = false)
                text   <- sandbox.profile(bounds)
                wrote  <- sandbox.run(Chunk("/usr/bin/touch", tree.marker.toString), bounds)
                exists <- ZIO.attemptBlocking(Files.exists(tree.marker)).orDie
              yield
                val lines    = text.split('\n')
                val readRoot = s"(allow file-read* (subpath \"${tree.root.toString}\"))"
                val starts   = "(allow file-write* (subpath"
                val writes   = lines.exists(line => line.startsWith(starts) && line.contains("Coursier"))
                assertTrue(TestHome.denied(wrote), !exists, lines.contains(readRoot), !writes)
      } @@ TestAspect.timeout(30.seconds),
      test("maven settings and ivy credentials are not readable") {
        withYard[SandboxError, TestResult]: yard =>
          ZIO.scoped:
            ShellSandbox.make(yard.home).flatMap: sandbox =>
              for
                _       <- laySecrets(yard.home)
                work    <- ZIO.attemptBlocking(Files.createDirectories(yard.home.resolve("work"))).orDie
                bounds   = ShellBounds(work, Chunk(work), Chunk.empty, network = false, writable = false)
                m2       = yard.home.resolve(".m2")
                ivy      = yard.home.resolve(".ivy2")
                note    <- sandbox.run(Chunk("/bin/cat", m2.resolve("note").toString), bounds)
                hidden  <- sandbox.run(Chunk("/bin/cat", m2.resolve("settings.xml").toString), bounds)
                ivyNote <- sandbox.run(Chunk("/bin/cat", ivy.resolve("note").toString), bounds)
                creds   <- sandbox.run(Chunk("/bin/cat", ivy.resolve(".credentials").toString), bounds)
              yield assertTrue(
                note.code == 0,
                note.stdout == "plain",
                TestHome.denied(hidden),
                !hidden.stdout.contains("secret"),
              ) && assertTrue(
                ivyNote.code == 0,
                ivyNote.stdout == "plain",
                TestHome.denied(creds),
                !creds.stdout.contains("secret"),
              )
      } @@ TestAspect.timeout(30.seconds),
      test("maven settings cannot be overwritten when the cache is writable") {
        withYard[SandboxError, TestResult]: yard =>
          ZIO.scoped:
            ShellSandbox.make(yard.home).flatMap: sandbox =>
              for
                _      <- laySecrets(yard.home)
                work   <- ZIO.attemptBlocking(Files.createDirectories(yard.home.resolve("work"))).orDie
                bounds  = ShellBounds(work, Chunk(work), Chunk(work), network = false, writable = true)
                target  = yard.home.resolve(".m2").resolve("settings.xml")
                wrote  <- sandbox.run(Chunk("/bin/sh", "-c", "printf pwned > \"$1\"", "sh", target.toString), bounds)
                body   <- ZIO.attemptBlocking(Files.readString(target)).orDie
              yield assertTrue(TestHome.denied(wrote), body == "secret")
      } @@ TestAspect.timeout(30.seconds),
    ).when(macPresent),
  )

  private val macPresent: Boolean =
    val present = ShellSandbox.available
    ShellSandbox.skipReason(present).foreach(reason => System.err.println(s"SKIP macos sandbox: $reason"))
    present

  private final case class Yard(root: Path, home: Path)

  private final case class CacheTree(work: Path, root: Path, marker: Path, other: Path)

  private def laySecrets(home: Path): ZIO[Any, Nothing, Unit] =
    TestHome.lay {
      val m2 = home.resolve(".m2")
      Files.createDirectories(m2)
      Files.writeString(m2.resolve("settings.xml"), "secret")
      Files.writeString(m2.resolve("note"), "plain")
      val ivy = home.resolve(".ivy2")
      Files.createDirectories(ivy)
      Files.writeString(ivy.resolve(".credentials"), "secret")
      Files.writeString(ivy.resolve("note"), "plain")
    }

  private def cacheTree(home: Path): ZIO[Any, Nothing, CacheTree] =
    ZIO
      .attemptBlocking {
        val coursier = home.resolve("Library").resolve("Caches").resolve("Coursier")
        Files.createDirectories(coursier)
        val otherDir = home.resolve("Library").resolve("Caches").resolve("Other")
        Files.createDirectories(otherDir)
        val work = Files.createDirectories(home.resolve("work"))
        CacheTree(work, coursier, coursier.resolve("landed"), otherDir.resolve("landed"))
      }
      .orDie

  private def render(grants: Chunk[CacheGrant], writable: Boolean): String =
    grants.headOption match
      case None => ""
      case Some(first) =>
        SeatbeltProfile.render(
          ShellSandbox.policyOf(
            Chunk.empty,
            Chunk.empty,
            first.root,
            network = false,
            writable = writable,
            jdk = None,
            sbt = None,
            caches = grants,
            temp = None,
            existingSystem = _ => None,
          )
        )

  private def denyFollows(lines: Array[String], allow: String, deny: String): Boolean =
    val allowed = lines.indexOf(allow)
    val denied  = lines.indexOf(deny)
    allowed >= 0 && denied > allowed

  private def withYard[E, A](use: Yard => ZIO[Any, E, A]): ZIO[Any, E | IOException, A] =
    ZIO.scoped {
      ZIO.acquireRelease(scratch)(yard => TestHome.delete(yard.root)).flatMap(use)
    }

  private def scratch: ZIO[Any, IOException, Yard] =
    TestHome.scratch("cache-grant").flatMap { root =>
      ZIO
        .attemptBlocking {
          val home = Files.createDirectories(root.resolve("home")).toRealPath()
          Yard(root, home)
        }
        .refineToOrDie[IOException]
    }
end CacheDirSpec
