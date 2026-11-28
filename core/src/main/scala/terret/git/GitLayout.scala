package terret.git

import zio.{Chunk, IO, ZIO}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}

/** Why a linked worktree did not yield git directories inside the project root. */
enum GitFault:
  case Escaped
  case Unreadable
  case BadPointer
  case Mismatch
  case BadPath

  def explain: String = this match
    case Escaped    => "git dir resolves outside the project root"
    case Unreadable => "git dir is unreadable"
    case BadPointer => "git dir pointer is rejected"
    case Mismatch   => "git dir does not match the common dir"
    case BadPath    => "git dir path cannot be granted"
end GitFault

/** Real admin directory and real common directory of one linked worktree. Both sit strictly inside `project`. */
final case class WorktreeGit(admin: Path, common: Path, project: Path)

/** Resolves a linked worktree's git directories.
  *
  * `GitLayout.locate(worktree, Some(project))` is that worktree's admin directory and the common git directory when
  * both real paths sit strictly inside `project`. A normal repository is `None`. A `gitdir:` pointer or symlink that
  * leaves `project` is `Escaped`.
  */
object GitLayout:
  def locate(workspace: Path, configured: Option[Path]): IO[GitFault, Option[WorktreeGit]] =
    pointer(workspace).flatMap {
      case None      => ZIO.succeed(None)
      case Some(raw) => linked(workspace, raw, configured).map(Some(_))
    }

  /** `child` is strictly inside `parent`. `/` and a path with `.` or `..` are outside. */
  def strictlyInside(parent: Path, child: Path): Boolean =
    val root = parent.toString
    val text = child.toString
    root != "/" && text.startsWith(root + "/") && plain(text)

  private def linked(workspace: Path, raw: Path, configured: Option[Path]): IO[GitFault, WorktreeGit] =
    for
      root     <- real(workspace)
      fromFile <- pointed(raw, workspace, configured)
      fromGit  <- revParse(workspace, "--git-dir")
      admin    <- real(fromGit)
      _        <- same(fromFile, admin)
      common   <- revParse(workspace, "--git-common-dir").flatMap(real)
      _        <- ZIO.fromEither(underWorktrees(common, admin))
      project  <- boundary(root, configured, admin, common)
      _        <- ZIO.fromEither(contained(project, root, admin, common))
    yield WorktreeGit(admin, common, project)

  private def same(file: Path, parsed: Path): IO[GitFault, Unit] =
    if file.toString == parsed.toString then ZIO.unit else ZIO.fail(GitFault.Mismatch)

  private def boundary(
      workspace: Path,
      configured: Option[Path],
      admin: Path,
      common: Path,
  ): IO[GitFault, Path] =
    configured match
      case Some(root) => real(root)
      case None       => ZIO.fromEither(nearest(workspace, admin, common))

  private def contained(project: Path, workspace: Path, admin: Path, common: Path): Either[GitFault, Unit] =
    if holds(project, workspace, admin, common) then Right(()) else Left(GitFault.Escaped)

  /** Nearest ancestor of `workspace` that strictly contains the workspace and both git dirs. `/` is refused. */
  private def nearest(workspace: Path, admin: Path, common: Path): Either[GitFault, Path] =
    def go(current: Path): Either[GitFault, Path] =
      if current.toString == "/" then Left(GitFault.Escaped)
      else if holds(current, workspace, admin, common) then Right(current)
      else
        Option(current.getParent) match
          case Some(parent) => go(parent)
          case None         => Left(GitFault.Escaped)
    Option(workspace.getParent) match
      case Some(parent) => go(parent)
      case None         => Left(GitFault.Escaped)

  private def holds(root: Path, workspace: Path, admin: Path, common: Path): Boolean =
    strictlyInside(root, workspace) && strictlyInside(root, admin) && strictlyInside(root, common)

  private def underWorktrees(common: Path, admin: Path): Either[GitFault, Unit] =
    val prefix = common.toString + "/worktrees/"
    if admin.toString.startsWith(prefix) && plain(admin.toString) then Right(())
    else Left(GitFault.Mismatch)

  private def plain(text: String): Boolean =
    text.split('/').forall(part => part != ".." && part != ".")

  private def pointer(workspace: Path): IO[GitFault, Option[Path]] =
    ZIO
      .attemptBlocking(readPointer(workspace))
      .refineOrDie { case _: IOException | _: SecurityException =>
        GitFault.Unreadable
      }
      .flatMap(ZIO.fromEither)

  private def readPointer(workspace: Path): Either[GitFault, Option[Path]] =
    val dot = workspace.resolve(".git")
    if Files.isSymbolicLink(dot) then Right(None)
    else if !Files.isRegularFile(dot, LinkOption.NOFOLLOW_LINKS) then Right(None)
    else if Files.size(dot) > Cap then Left(GitFault.BadPointer)
    else parseGitdir(Files.readString(dot), workspace)

  // A relative `gitdir:` is relative to the `.git` file, which lives in the workspace.
  private def parseGitdir(text: String, workspace: Path): Either[GitFault, Option[Path]] =
    val stripped = text.stripSuffix("\n").stripSuffix("\r")
    if stripped.exists(illegal) then Left(GitFault.BadPointer)
    else if !stripped.startsWith("gitdir:") then Left(GitFault.BadPointer)
    else
      val raw = stripped.drop("gitdir:".length).trim
      if raw.isEmpty then Left(GitFault.BadPointer)
      else
        try
          val path = Path.of(raw)
          val abs  = if path.isAbsolute then path else workspace.resolve(path)
          Right(Some(abs))
        catch case _: InvalidPathException => Left(GitFault.BadPath)

  private def revParse(workspace: Path, flag: String): IO[GitFault, Path] =
    runGit(workspace, Chunk("--path-format=absolute", flag)).catchSome { case GitFault.BadPointer =>
      // Git before `--path-format` prints a path relative to the workspace.
      runGit(workspace, Chunk(flag)).map { path =>
        if path.isAbsolute then path else workspace.resolve(path)
      }
    }

  private def runGit(workspace: Path, args: Chunk[String]): IO[GitFault, Path] =
    ZIO
      .attemptBlockingInterrupt(spawn(workspace, args))
      .refineOrDie { case _: IOException | _: SecurityException =>
        GitFault.Unreadable
      }
      .flatMap(ZIO.fromEither)

  private def spawn(workspace: Path, args: Chunk[String]): Either[GitFault, Path] =
    val command = Chunk("git", "-C", workspace.toString, "rev-parse") ++ args
    val builder = new ProcessBuilder(command.toList*)
    val env     = builder.environment()
    env.remove("GIT_DIR")
    env.remove("GIT_WORK_TREE")
    env.remove("GIT_COMMON_DIR")
    env.remove("GH_REPO")
    env.put("GIT_CONFIG_NOSYSTEM", "1")
    builder.redirectError(ProcessBuilder.Redirect.DISCARD)
    val process = builder.start()
    try
      val bytes = process.getInputStream.readNBytes(Cap + 1)
      val code  = process.waitFor()
      if code != 0 || bytes.length > Cap then Left(GitFault.BadPointer)
      else parsePath(new String(bytes, StandardCharsets.UTF_8))
    finally process.destroy()

  private def parsePath(raw: String): Either[GitFault, Path] =
    val text = raw.stripSuffix("\n").stripSuffix("\r")
    if text.isEmpty || text.exists(illegal) then Left(GitFault.BadPointer)
    else
      try Right(Path.of(text))
      catch case _: InvalidPathException => Left(GitFault.BadPath)

  /** A missing target is `Escaped` when its resolved path is outside the fence, and `Unreadable` when it stays inside.
    */
  private def pointed(path: Path, workspace: Path, configured: Option[Path]): IO[GitFault, Path] =
    val fence = configured match
      case Some(root) => root
      case None       => workspace
    real(fence).flatMap { bound =>
      ZIO
        .attemptBlocking(reveal(path, bound))
        .refineOrDie { case _: IOException | _: SecurityException | _: InvalidPathException =>
          GitFault.Unreadable
        }
        .flatMap(ZIO.fromEither)
    }

  private def reveal(path: Path, bound: Path): Either[GitFault, Path] =
    try Right(path.toRealPath())
    catch
      case _: java.nio.file.NoSuchFileException =>
        absent(path.toAbsolutePath.normalize, path.toAbsolutePath.normalize, bound, 0)
      case _: IOException | _: SecurityException | _: InvalidPathException =>
        absent(path.toAbsolutePath.normalize, path.toAbsolutePath.normalize, bound, 0) match
          case Left(GitFault.Escaped) => Left(GitFault.Escaped)
          case _                      => Left(GitFault.Unreadable)

  private def absent(original: Path, cursor: Path, bound: Path, depth: Int): Either[GitFault, Path] =
    if depth > 40 || !plain(original.toString) then Left(GitFault.BadPath)
    else if Files.isSymbolicLink(cursor) then
      val link = Files.readSymbolicLink(cursor)
      val base = Option(cursor.getParent).map(parent => parent.resolve(link)).getOrElse(link)
      val next = (if link.isAbsolute then link else base).normalize
      if next == cursor.normalize then Left(GitFault.BadPath)
      else absent(next, next, bound, depth + 1)
    else if Files.exists(cursor, LinkOption.NOFOLLOW_LINKS) then
      try
        val suffix   = cursor.relativize(original)
        val resolved = cursor.toRealPath().resolve(suffix).normalize
        if !plain(resolved.toString) then Left(GitFault.BadPath)
        else place(resolved, bound)
      catch case _: IOException | _: SecurityException | _: InvalidPathException => Left(GitFault.Unreadable)
    else
      Option(cursor.getParent) match
        case Some(parent) => absent(original, parent, bound, depth + 1)
        case None         => place(original, bound)

  private def place(path: Path, bound: Path): Either[GitFault, Path] =
    if strictlyInside(bound, path) then Left(GitFault.Unreadable) else Left(GitFault.Escaped)

  private def real(path: Path): IO[GitFault, Path] =
    ZIO
      .attemptBlocking(path.toRealPath())
      .refineOrDie { case _: IOException | _: SecurityException | _: InvalidPathException =>
        GitFault.Unreadable
      }

  private def illegal(ch: Char): Boolean =
    ch < ' ' || ch == '\u007f'

  private val Cap: Int = 4096
end GitLayout
