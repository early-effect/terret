package terret.grok

import terret.git.{GitFault, GitLayout, WorktreeGit}
import zio.{Chunk, IO, ZIO}

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}

/** Read and write paths added for one linked worktree. Writes are empty unless the session may write. */
final case class GitSeat(reads: Chunk[AbsPath], writes: Chunk[AbsPath])

object GitSeat:
  val empty: GitSeat = GitSeat(Chunk.empty, Chunk.empty)

/** Seatbelt paths for a linked worktree that already resolved inside its project.
  *
  * `GitGrant.seat(found, writable = true)` grants the worktree admin directory and the common `objects`, `refs`,
  * `logs`, and `packed-refs`. `config`, `hooks`, and the common `HEAD` are read-only. The common `HEAD` stays read-only
  * because writing it would move the main checkout.
  */
object GitGrant:
  def seat(found: WorktreeGit, writable: Boolean): IO[GitFault, GitSeat] =
    for
      admin <- admitPath(found.admin)
      kids  <- ZIO.foreach(Child.names)(name => child(found.common, found.project, name))
      paths <- ZIO.foreach(kids)(admitPath)
    yield
      val paired = Child.names.zip(paths)
      val reads  = admin +: paths
      val writes =
        if writable then admin +: Child.writes.flatMap(name => picked(paired, name))
        else Chunk.empty
      GitSeat(reads, writes)

  /** A quote, a backslash, or a control character cannot be rendered into a profile. */
  private[grok] def admit(path: Path): Either[GitFault, AbsPath] =
    val text = path.toString
    if text.exists(rejected) then Left(GitFault.BadPath)
    else
      AbsPath.from(text) match
        case Right(abs)                     => Right(abs)
        case Left(AbsPathError.NotAbsolute) => Left(GitFault.BadPath)
        case Left(AbsPathError.Illegal)     => Left(GitFault.BadPath)

  private def admitPath(path: Path): IO[GitFault, AbsPath] =
    ZIO.fromEither(admit(path))

  private def picked(paired: Chunk[(String, AbsPath)], name: String): Chunk[AbsPath] =
    paired.find((got, _) => got == name) match
      case Some((_, path)) => Chunk(path)
      case None            => Chunk.empty

  private def child(common: Path, project: Path, name: String): IO[GitFault, Path] =
    ZIO
      .attemptBlocking(resolve(common, project, name))
      .refineOrDie { case _: IOException | _: SecurityException =>
        GitFault.Unreadable
      }
      .flatMap(ZIO.fromEither)

  private def resolve(common: Path, project: Path, name: String): Either[GitFault, Path] =
    val lexical = common.resolve(name)
    if Files.isSymbolicLink(lexical) || Files.exists(lexical, LinkOption.NOFOLLOW_LINKS) then
      try
        val resolved = lexical.toRealPath()
        placed(project, common, resolved)
      catch
        case _: IOException | _: SecurityException => Left(GitFault.Unreadable)
        case _: InvalidPathException               => Left(GitFault.BadPath)
    else placed(project, common, lexical)

  /** A child that real-paths outside the common dir is not granted, even when it stays inside the project. */
  private def placed(project: Path, common: Path, path: Path): Either[GitFault, Path] =
    if !GitLayout.strictlyInside(project, path) then Left(GitFault.Escaped)
    else if GitLayout.strictlyInside(common, path) then Right(path)
    else Left(GitFault.Mismatch)

  private def rejected(ch: Char): Boolean =
    ch < ' ' || ch == '\u007f' || ch == '"' || ch == '\\'

  // `packed-refs.lock` is created beside `packed-refs` while a ref update rewrites the file.
  private object Child:
    val names: Chunk[String] =
      Chunk("objects", "refs", "logs", "packed-refs", "packed-refs.lock", "config", "hooks", "HEAD")

    val writes: Chunk[String] =
      Chunk("objects", "refs", "logs", "packed-refs", "packed-refs.lock")
  end Child
end GitGrant
