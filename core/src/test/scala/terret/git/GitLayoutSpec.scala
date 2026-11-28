package terret.git

import zio.test.*
import zio.{Chunk, IO, ZIO}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}

import scala.jdk.CollectionConverters.*

object GitLayoutSpec extends ZIOSpecDefault:
  def spec = suite("git layout")(
    test("a worktree resolves the admin dir and the common dir inside the project") {
      withRoot { root =>
        for
          tree <- linked(root)
          got  <- GitLayout.locate(tree.work, Some(tree.project))
        yield
          val inside = got.exists { git =>
            GitLayout.strictlyInside(tree.project, git.admin) && GitLayout.strictlyInside(tree.project, git.common)
          }
          assertTrue(
            got.map(_.admin) == Some(tree.admin),
            got.map(_.common) == Some(tree.common),
            got.map(_.project) == Some(tree.project),
            inside,
          )
      }
    },
    test("an unset root uses the ancestor that contains the worktree and the common dir") {
      withRoot { root =>
        for
          tree <- linked(root)
          got  <- GitLayout.locate(tree.work, None)
        yield assertTrue(
          got.map(_.admin) == Some(tree.admin),
          got.map(_.common) == Some(tree.common),
          got.map(_.project) == Some(tree.project),
        )
      }
    },
    test("a regular repository gets no extra git dir") {
      withRoot { root =>
        for
          main <- plain(root)
          got  <- GitLayout.locate(main, Some(root.resolve("project")))
        yield assertTrue(got.isEmpty)
      }
    },
    test("a common dir that resolves outside the project is refused") {
      withRoot { root =>
        for
          tree <- linked(root)
          _    <- escape(tree)
          got  <- GitLayout.locate(tree.work, Some(tree.project)).either
        yield assertTrue(got == Left(GitFault.Escaped))
      }
    },
    test("a gitdir pointer outside the project is refused") {
      withRoot { root =>
        for
          tree <- linked(root)
          _    <- ZIO.attemptBlocking {
            Files.writeString(tree.work.resolve(".git"), s"gitdir: ${tree.outside.resolve("missing")}\n")
          }.orDie
          got <- GitLayout.locate(tree.work, Some(tree.project)).either
        yield assertTrue(got == Left(GitFault.Escaped))
      }
    },
    test("a gitdir that names the common dir is not granted as the admin dir") {
      withRoot { root =>
        for
          tree <- linked(root)
          _    <- ZIO.attemptBlocking {
            Files.writeString(tree.work.resolve(".git"), s"gitdir: ${tree.common}\n")
          }.orDie
          got <- GitLayout.locate(tree.work, Some(tree.project)).either
        yield assertTrue(got == Left(GitFault.Mismatch))
      }
    },
    test("a root that is only the worktree does not grant the common dir") {
      withRoot { root =>
        for
          tree <- linked(root)
          got  <- GitLayout.locate(tree.work, Some(tree.work)).either
        yield assertTrue(got == Left(GitFault.Escaped))
      }
    },
  )

  private def withRoot[A](use: Path => ZIO[Any, Any, A]): ZIO[Any, Any, A] =
    ZIO.scoped {
      ZIO.acquireRelease(scratch)(remove).flatMap(use)
    }

  private def scratch: IO[IOException, Path] =
    ZIO
      .attemptBlocking {
        val base = Path.of("target").toAbsolutePath
        Files.createDirectories(base)
        Files.createTempDirectory(base, "git-layout").toRealPath()
      }
      .refineToOrDie[IOException]

  private def linked(root: Path): IO[IOException | String, Tree] =
    val project = root.resolve("project")
    val main    = project.resolve("main")
    val work    = project.resolve("wt")
    val outside = root.resolve("outside")
    for
      _        <- io(Files.createDirectories(main))
      _        <- io(Files.createDirectories(outside))
      _        <- git(main, Chunk("init", "-q"))
      _        <- git(main, Chunk("config", "user.email", "t@example.com"))
      _        <- git(main, Chunk("config", "user.name", "t"))
      _        <- io(Files.writeString(main.resolve("a.txt"), "a\n"))
      _        <- git(main, Chunk("add", "a.txt"))
      _        <- git(main, Chunk("commit", "-q", "-m", "init"))
      _        <- git(main, Chunk("worktree", "add", "-q", work.toString, "-b", "feature"))
      projectR <- real(project)
      workR    <- real(work)
      admin    <- readAdmin(workR)
      common   <- parent(admin).flatMap(parent).flatMap(real)
      outsideR <- real(outside)
    yield Tree(projectR, main, workR, admin, common, outsideR)

  private def plain(root: Path): IO[IOException | String, Path] =
    val main = root.resolve("project").resolve("plain")
    for
      _   <- io(Files.createDirectories(main))
      _   <- git(main, Chunk("init", "-q"))
      got <- real(main)
    yield got

  private def escape(tree: Tree): IO[IOException, Unit] =
    io {
      val gitDir = tree.main.resolve(".git")
      val moved  = tree.outside.resolve("git")
      Files.move(gitDir, moved)
      Files.createSymbolicLink(gitDir, moved)
    }

  private def readAdmin(work: Path): IO[IOException | String, Path] =
    ZIO
      .attemptBlocking {
        val text = Files.readString(work.resolve(".git")).stripSuffix("\n").stripSuffix("\r")
        if !text.startsWith("gitdir:") then Left("gitdir")
        else Right(Path.of(text.drop("gitdir:".length).trim).toRealPath())
      }
      .refineToOrDie[IOException]
      .flatMap(ZIO.fromEither)

  private def git(cwd: Path, args: Chunk[String]): IO[IOException | String, String] =
    ZIO
      .attemptBlockingInterrupt {
        val builder = new ProcessBuilder((Chunk("git", "-C", cwd.toString) ++ args).toList*)
        builder.redirectError(ProcessBuilder.Redirect.DISCARD)
        val process = builder.start()
        try
          val text = new String(process.getInputStream.readNBytes(8192), StandardCharsets.UTF_8)
          if process.waitFor() == 0 then Right(text) else Left("git")
        finally process.destroy()
      }
      .refineToOrDie[IOException]
      .flatMap(ZIO.fromEither)

  private def parent(path: Path): IO[String, Path] =
    Option(path.getParent) match
      case Some(value) => ZIO.succeed(value)
      case None        => ZIO.fail("parent")

  private def real(path: Path): IO[IOException, Path] =
    ZIO.attemptBlocking(path.toRealPath()).refineToOrDie[IOException]

  private def io(body: => Unit): IO[IOException, Unit] =
    ZIO.attemptBlocking(body).refineToOrDie[IOException]

  private def remove(root: Path): zio.UIO[Unit] =
    ZIO.attemptBlocking(delete(root)).orDie

  private def delete(path: Path): Unit =
    if Files.isSymbolicLink(path) then Files.deleteIfExists(path)
    else if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then
      val kids = Files.list(path)
      try Chunk.from(kids.iterator.asScala).foreach(delete)
      finally kids.close()
      Files.deleteIfExists(path)
    else Files.deleteIfExists(path)

  private final case class Tree(
      project: Path,
      main: Path,
      work: Path,
      admin: Path,
      common: Path,
      outside: Path,
  )
end GitLayoutSpec
