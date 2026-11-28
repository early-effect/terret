package terret.grok

import terret.git.{GitFault, GitLayout}
import zio.test.*
import zio.{Chunk, IO, ZIO}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}

import scala.jdk.CollectionConverters.*

object GitGrantSpec extends ZIOSpecDefault:
  def spec = suite("git grant")(
    test("a worktree profile writes the admin dir and the shared git data") {
      withRoot { root =>
        for
          tree <- linked(root)
          seat <- granted(tree, writable = true)
        yield
          val lines = seat match
            case Right(got) => render(tree.work, got, writable = true)
            case Left(_)    => Array.empty[String]
          val inside = seat match
            case Right(got) =>
              got.reads.forall(path => GitLayout.strictlyInside(tree.project, Path.of(path.text))) &&
              got.writes.forall(path => GitLayout.strictlyInside(tree.project, Path.of(path.text)))
            case Left(_) => false
          val shown = seat match
            case Left(fault) => Left(fault)
            case Right(_)    => Right(())
          assertTrue(shown == Right(())) && assertTrue(
            lines.contains(write(tree.admin)),
            lines.contains(write(tree.common.resolve("objects"))),
            lines.contains(write(tree.common.resolve("refs"))),
            lines.contains(write(tree.common.resolve("logs"))),
          ) && assertTrue(
            lines.contains(read(tree.common.resolve("hooks"))),
            !lines.contains(write(tree.common.resolve("hooks"))),
            lines.contains(read(tree.common.resolve("config"))),
            !lines.contains(write(tree.common.resolve("config"))),
          ) && assertTrue(
            lines.contains(read(tree.common.resolve("HEAD"))),
            !lines.contains(write(tree.common.resolve("HEAD"))),
            !lines.contains(write(tree.common)),
            !lines.contains(write(tree.main)),
          ) && assertTrue(
            inside,
            lines.contains(write(tree.common.resolve("packed-refs"))),
            lines.contains(write(tree.common.resolve("packed-refs.lock"))),
            !seat.exists(_.reads.exists(_.text.startsWith(tree.outside.toString))),
          )
      }
    },
    test("without WriteWorkspace the shared git data is readable and not writable") {
      withRoot { root =>
        for
          tree <- linked(root)
          seat <- granted(tree, writable = false)
        yield
          val lines = seat match
            case Right(got) => render(tree.work, got, writable = false)
            case Left(_)    => Array.empty[String]
          val objects = tree.common.resolve("objects").toString
          val shown   = seat match
            case Left(fault) => Left(fault)
            case Right(_)    => Right(())
          assertTrue(shown == Right(())) && assertTrue(
            lines.contains(read(tree.common.resolve("objects"))),
            !lines.exists(line => line.contains("file-write") && line.contains(objects)),
            seat.forall(_.writes.isEmpty),
            lines.contains(read(tree.admin)),
          )
      }
    },
    test("a common-dir symlink onto the main checkout grants nothing") {
      withRoot { root =>
        for
          tree <- linked(root)
          _    <- pointObjectsAt(tree.common.resolve("objects"), tree.main.resolve("objects"))
          got  <- granted(tree, writable = true)
        yield assertTrue(got == Left(GitFault.Mismatch))
      }
    },
    test("a symlink under the common dir that leaves the project grants nothing") {
      withRoot { root =>
        for
          tree <- linked(root)
          _    <- relocateObjects(tree)
          got  <- granted(tree, writable = true)
        yield
          val outside = tree.outside.resolve("objects").toString
          val lines   = render(tree.work, GitSeat.empty, writable = true)
          assertTrue(got == Left(GitFault.Escaped), !lines.exists(_.contains(outside)))
      }
    },
    test("a quote or a control character is refused and not rendered") {
      val quote    = GitGrant.admit(Path.of("/work/a\"b"))
      val control  = GitGrant.admit(Path.of("/work/a\nb"))
      val rendered = quote match
        case Left(_)     => ""
        case Right(path) => SeatbeltProfile.render(SeatbeltPolicy(Chunk(path), Chunk.empty, Chunk.empty, false))
      assertTrue(
        quote == Left(GitFault.BadPath),
        control == Left(GitFault.BadPath),
        rendered.isEmpty,
        !rendered.contains("(allow"),
      )
    },
    test("a regular repository does not grant a sibling checkout") {
      withRoot { root =>
        for
          tree  <- linked(root)
          plain <- plainRepo(root)
          got   <- GitLayout.locate(plain, Some(tree.project))
        yield
          val lines   = render(plain, GitSeat.empty, writable = true)
          val sibling = tree.common.resolve("objects")
          assertTrue(got.isEmpty, !lines.contains(write(sibling)), !lines.contains(read(sibling)))
      }
    },
  )

  private def granted(tree: Tree, writable: Boolean): IO[GitFault, Either[GitFault, GitSeat]] =
    GitLayout.locate(tree.work, Some(tree.project)).flatMap {
      case None      => ZIO.succeed(Left(GitFault.Mismatch))
      case Some(git) => GitGrant.seat(git, writable).either
    }

  private def render(work: Path, seat: GitSeat, writable: Boolean): Array[String] =
    AbsPath.from(work.toString) match
      case Right(cwd) =>
        val writes = if writable then Chunk(cwd) else Chunk.empty
        SeatbeltProfile
          .render(
            ShellSandbox.policyOf(
              Chunk(cwd),
              writes,
              cwd,
              network = false,
              writable = writable,
              jdk = None,
              sbt = None,
              caches = Chunk.empty,
              temp = None,
              existingSystem = _ => None,
              git = seat,
            )
          )
          .split('\n')
      case Left(AbsPathError.NotAbsolute) => Array.empty
      case Left(AbsPathError.Illegal)     => Array.empty

  private def read(path: Path): String =
    s"(allow file-read* (subpath \"$path\"))"

  private def write(path: Path): String =
    s"(allow file-write* (subpath \"$path\"))"

  private def withRoot[A](use: Path => ZIO[Any, Any, A]): ZIO[Any, Any, A] =
    ZIO.scoped(ZIO.acquireRelease(scratch)(remove).flatMap(use))

  private def scratch: IO[IOException, Path] =
    ZIO
      .attemptBlocking {
        val base = Path.of("target").toAbsolutePath
        Files.createDirectories(base)
        Files.createTempDirectory(base, "git-grant").toRealPath()
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
      mainR    <- real(main)
    yield Tree(projectR, mainR, workR, admin, common, outsideR)

  private def plainRepo(root: Path): IO[IOException | String, Path] =
    val main = root.resolve("project").resolve("plain")
    for
      _   <- io(Files.createDirectories(main))
      _   <- git(main, Chunk("init", "-q"))
      got <- real(main)
    yield got

  private def relocateObjects(tree: Tree): IO[IOException, Unit] =
    pointObjectsAt(tree.common.resolve("objects"), tree.outside.resolve("objects"))

  private def pointObjectsAt(objects: Path, moved: Path): IO[IOException, Unit] =
    io {
      Files.move(objects, moved)
      Files.createSymbolicLink(objects, moved)
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
end GitGrantSpec
