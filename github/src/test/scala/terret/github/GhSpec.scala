package terret.github

import slayer.*
import terret.workspace.{Workspace, WorkspaceError}
import zio.test.*
import zio.{Chunk, IO, Ref, ZIO}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}

import scala.jdk.CollectionConverters.*

object GhSpec extends ZIOSpecDefault:
  private val parsed: Either[WorkspaceError, Workspace] =
    Workspace.from(java.nio.file.Path.of("target", "gh-tool").toAbsolutePath.normalize.toString)

  private val banned = Set(
    "--auto",
    "--admin",
    "--delete-branch",
    "--web",
    "--watch",
    "--fill",
    "--repo",
    "-R",
    "sh",
    "-c",
  )

  def spec = suite("gh")(
    test("the vector never shells out and merge names its method") {
      val title   = GhArgs.prepare(GhOp.PrCreate("a; rm -rf /", "secret body", None, Some("feature")))
      val comment = GhArgs.prepare(GhOp.PrComment(7, "secret body"))
      val methods = Chunk(MergeMethod.Merge, MergeMethod.Squash, MergeMethod.Rebase)
      val merges  = methods.map(method => GhArgs.prepare(GhOp.PrMerge(4, method)))
      assertTrue(
        title.map(_.stdin) == Right(Some("secret body")),
        title.map(_.argv.exists(_ == "--title=a; rm -rf /")) == Right(true),
        title.map(_.argv.exists(_.contains("secret body"))) == Right(false),
        title.map(invocation => clean(invocation.argv)) == Right(true),
        comment.map(_.stdin) == Right(Some("secret body")),
        comment.map(_.argv.exists(_.contains("secret body"))) == Right(false),
        merges.forall(_.exists(invocation => clean(invocation.argv))),
        merges.exists(_.exists(_.argv.contains("--merge"))),
        merges.exists(_.exists(_.argv.contains("--squash"))),
        merges.exists(_.exists(_.argv.contains("--rebase"))),
        GhArgs.prepare(GhOp.PrCreate("t", "", None, Some("owner:branch"))) ==
          Left(GhProblem.Branch("owner:branch")),
        GhArgs.prepare(GhOp.PrCreate("t", "", None, None)) == Left(GhProblem.Detached),
        GhArgs.prepare(GhOp.PrComment(1, "")) == Left(GhProblem.Body),
        GhArgs.prepare(GhOp.PrList(0, PrState.Open)) == Left(GhProblem.Limit(0)),
        GhArgs.prepare(GhOp.PrList(31, PrState.Open)) == Left(GhProblem.Limit(31)),
        GhArgs.prepare(GhOp.PrView(0)) == Left(GhProblem.Number(0)),
      )
    },
    test("an empty grant never starts gh") {
      parsed match
        case Left(error) => ZIO.succeed(assertTrue(error == WorkspaceError.Empty))
        case Right(root) =>
          val program =
            for failed <- ZIO.serviceWithZIO[Gh](_.perform(GhOp.PrList(10, PrState.Open), root, GhAccess.empty)).either
            yield assertTrue(failed == Left(GhError.NotGranted(GhGrant.Read, "pr list")))
          program.provide(stubbed[GhCommand] >+> Gh.layer)
    },
    test("read does not grant merge and write does not grant list") {
      parsed match
        case Left(error) => ZIO.succeed(assertTrue(error == WorkspaceError.Empty))
        case Right(root) =>
          val read    = GhAccess(Set(GhGrant.Read))
          val write   = GhAccess(Set(GhGrant.Write))
          val program =
            for
              merge <- ZIO.serviceWithZIO[Gh](_.perform(GhOp.PrMerge(1, MergeMethod.Squash), root, read)).either
              list  <- ZIO.serviceWithZIO[Gh](_.perform(GhOp.PrList(10, PrState.Open), root, write)).either
            yield assertTrue(
              merge == Left(GhError.NotGranted(GhGrant.Write, "pr merge")),
              list == Left(GhError.NotGranted(GhGrant.Read, "pr list")),
            )
          program.provide(stubbed[GhCommand] >+> Gh.layer)
    },
    test("a read runs in the session workspace and redacts tokens") {
      parsed match
        case Left(error) => ZIO.succeed(assertTrue(error == WorkspaceError.Empty))
        case Right(root) =>
          val program =
            for
              seen <- Ref.make(Chunk.empty[(Chunk[String], Workspace, Option[String])])
              _    <- stub[GhCommand](_.execute(any[Chunk[String]], any[Workspace], any[Option[String]])) {
                (argv, cwd, stdin) =>
                  seen.update(_ :+ (argv, cwd, stdin)) *> ZIO.succeed(GhOutput(0, "ghp_Abcdefghij\n", ""))
              }
              text <- ZIO.serviceWithZIO[Gh](
                _.perform(GhOp.PrList(10, PrState.Open), root, GhAccess(Set(GhGrant.Read)))
              )
              calls <- seen.get
            yield assertTrue(
              text == "[redacted]\n",
              calls.length == 1,
              calls.exists { (argv, cwd, stdin) =>
                argv == Chunk(
                  "gh",
                  "pr",
                  "list",
                  "--json",
                  "number,title,state,url,headRefName,baseRefName",
                  "--limit",
                  "10",
                  "--state",
                  "open",
                ) && cwd == root && stdin.isEmpty && clean(argv)
              },
            )
          program.provide(stubbed[GhCommand] >+> Gh.layer)
    },
    test("failed checks are a read and a pending exit is too") {
      parsed match
        case Left(error) => ZIO.succeed(assertTrue(error == WorkspaceError.Empty))
        case Right(root) =>
          val access  = GhAccess(Set(GhGrant.Read))
          val program =
            for
              code <- Ref.make(1)
              _    <- stub[GhCommand](_.execute(any[Chunk[String]], any[Workspace], any[Option[String]])) { (_, _, _) =>
                code.get.map(current => GhOutput(current, "[]", "ghp_Abcdefghij"))
              }
              failed  <- ZIO.serviceWithZIO[Gh](_.perform(GhOp.PrChecks(3), root, access))
              _       <- code.set(8)
              pending <- ZIO.serviceWithZIO[Gh](_.perform(GhOp.PrChecks(3), root, access))
              _       <- code.set(2)
              other   <- ZIO.serviceWithZIO[Gh](_.perform(GhOp.PrChecks(3), root, access)).either
            yield assertTrue(
              failed == "[]",
              pending == "[]",
              other == Left(GhError.Failed(2, "[redacted]")),
            )
          program.provide(stubbed[GhCommand] >+> Gh.layer)
    },
    test("create discovers the branch and does not call gh when detached") {
      parsed match
        case Left(error) => ZIO.succeed(assertTrue(error == WorkspaceError.Empty))
        case Right(root) =>
          val access  = GhAccess(Set(GhGrant.Write))
          val program =
            for
              mode <- Ref.make("feature")
              seen <- Ref.make(Chunk.empty[Chunk[String]])
              _ <- stub[GhCommand](_.execute(any[Chunk[String]], any[Workspace], any[Option[String]])) { (argv, _, _) =>
                argv.headOption match
                  case Some("git") => mode.get.map(name => GhOutput(0, s"$name\n", ""))
                  case _           => seen.update(_ :+ argv) *> ZIO.succeed(GhOutput(0, "url\n", ""))
              }
              created <- ZIO.serviceWithZIO[Gh] { gh =>
                gh.perform(GhOp.PrCreate("title", "body", None, None), root, access)
              }
              calls    <- seen.get
              _        <- mode.set("HEAD")
              _        <- seen.set(Chunk.empty)
              detached <- ZIO
                .serviceWithZIO[Gh] { gh =>
                  gh.perform(GhOp.PrCreate("title", "body", None, None), root, access)
                }
                .either
              after <- seen.get
            yield assertTrue(
              created == "url\n",
              calls.exists(_.contains("--head=feature")),
              calls.forall(clean),
              detached == Left(GhError.BadRequest(GhProblem.Detached)),
              after.isEmpty,
            )
          program.provide(stubbed[GhCommand] >+> Gh.layer)
    },
    test("the jdk runner passes arguments as a list") {
      parsed match
        case Left(error) => ZIO.succeed(assertTrue(error == WorkspaceError.Empty))
        case Right(root) =>
          for
            _       <- ZIO.attemptBlocking(java.nio.file.Files.createDirectories(root.path)).orDie
            echoed  <- GhCommand.jdk.execute(Chunk("/bin/echo", "a;b", "c"), root, None)
            written <- GhCommand.jdk.execute(Chunk("/bin/cat"), root, Some("hello"))
          yield assertTrue(
            echoed == GhOutput(0, "a;b c\n", ""),
            written == GhOutput(0, "hello", ""),
          )
    },
    test("a worktree runs git in that worktree's admin dir and common dir") {
      withLinked { tree =>
        val command = GhCommand.make(Some(tree.project))
        for
          workspace <- workspaceOf(tree.work)
          dir       <- command.execute(Chunk("git", "rev-parse", "--git-dir"), workspace, None)
          common    <- command.execute(Chunk("git", "rev-parse", "--git-common-dir"), workspace, None)
          top       <- command.execute(Chunk("git", "rev-parse", "--show-toplevel"), workspace, None)
        yield assertTrue(dir.code == 0, dir.stdout.trim == tree.admin.toString) &&
          assertTrue(common.code == 0, common.stdout.trim == tree.common.toString) &&
          assertTrue(top.code == 0, top.stdout.trim == tree.work.toString)
      }
    },
    test("a regular repository does not set a worktree git dir") {
      withLinked { tree =>
        for
          plain     <- plainRepo(tree.project)
          workspace <- workspaceOf(plain)
          shown     <- GhCommand
            .make(Some(tree.project))
            .execute(
              Chunk("git", "rev-parse", "--git-dir"),
              workspace,
              None,
            )
          text = shown.stdout.trim
        yield assertTrue(shown.code == 0, text == ".git" || text.endsWith("/.git"), !text.contains("/worktrees/"))
      }
    },
    test("a git dir outside the project is refused") {
      withLinked { tree =>
        val command = GhCommand.make(Some(tree.project))
        for
          _         <- moveGit(tree)
          workspace <- workspaceOf(tree.work)
          got       <- command.execute(Chunk("git", "rev-parse", "--git-dir"), workspace, None).either
        yield assertTrue(got == Left(GhError.BadRequest(GhProblem.GitDir)))
      }
    },
  )

  private def clean(argv: Chunk[String]): Boolean =
    argv.headOption.contains("gh") && argv.forall(arg => !banned.contains(arg) && !arg.startsWith("--repo="))

  private def withLinked[A](use: Linked => ZIO[Any, Any, A]): ZIO[Any, Any, A] =
    ZIO.scoped(ZIO.acquireRelease(scratch)(remove).flatMap(root => linked(root).flatMap(use)))

  private def scratch: IO[IOException, Path] =
    ZIO
      .attemptBlocking {
        val base = Path.of("target").toAbsolutePath
        Files.createDirectories(base)
        Files.createTempDirectory(base, "gh-git").toRealPath()
      }
      .refineToOrDie[IOException]

  private def linked(root: Path): IO[IOException | String, Linked] =
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
    yield Linked(projectR, main, workR, admin, common, outsideR)

  private def plainRepo(project: Path): IO[IOException | String, Path] =
    val main = project.resolve("plain")
    for
      _   <- io(Files.createDirectories(main))
      _   <- git(main, Chunk("init", "-q"))
      got <- real(main)
    yield got

  private def moveGit(tree: Linked): IO[IOException, Unit] =
    io {
      val gitDir = tree.main.resolve(".git")
      val moved  = tree.outside.resolve("git")
      Files.move(gitDir, moved)
      Files.createSymbolicLink(gitDir, moved)
    }

  private def workspaceOf(path: Path): IO[WorkspaceError, Workspace] =
    ZIO.fromEither(Workspace.from(path.toString))

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

  private final case class Linked(
      project: Path,
      main: Path,
      work: Path,
      admin: Path,
      common: Path,
      outside: Path,
  )
end GhSpec
