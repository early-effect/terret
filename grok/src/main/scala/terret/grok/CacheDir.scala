package terret.grok

import zio.{Chunk, UIO, ZIO}

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}

/** One build cache the shell may read and write. The path is these segments under home, and nothing above them. */
enum CacheDir:
  case Histos
  case Coursier
  case MacCoursier
  case Ivy
  case Maven

  def segments: Chunk[String] =
    this match
      case Histos      => Chunk(".histos")
      case Coursier    => Chunk(".cache", "coursier")
      case MacCoursier => Chunk("Library", "Caches", "Coursier")
      case Ivy         => Chunk(".ivy2")
      case Maven       => Chunk(".m2")

/** Why one cache directory was not granted. */
enum CacheSkip:
  case Absent
  case Outside
  case Moved
  case Dangling
  case Loop
  case BadPath
  case Unreadable
  case NotDirectory

  def explain(dir: CacheDir): String =
    val name = dir.segments.mkString("/")
    this match
      case Absent       => s"$name is absent"
      case Outside      => s"$name resolves outside home"
      case Moved        => s"$name is a symlink to somewhere else"
      case Dangling     => s"$name link is dangling"
      case Loop         => s"$name link loops"
      case BadPath      => s"$name path is rejected"
      case Unreadable   => s"$name path is unreadable"
      case NotDirectory => s"$name is not a directory"

object CacheDir:
  val allowlist: Chunk[CacheDir] =
    Chunk(CacheDir.Histos, CacheDir.Coursier, CacheDir.MacCoursier, CacheDir.Ivy, CacheDir.Maven)

  /** Directories from `allowlist` that are real, strictly inside `home`, and not a symlink elsewhere.
    *
    * `CacheDir.decide(home)` grants `home/.cache/coursier` when that directory is real. A missing directory is
    * skipped. A link to another directory, including one inside `home`, is skipped.
    */
  def decide(home: Path): UIO[Chunk[AbsPath]] =
    if !home.isAbsolute then ZIO.logWarning("cache home is not absolute").as(Chunk.empty)
    else
      ZIO.foreach(allowlist)(one(home, _)).map { found =>
        found.foldLeft(Chunk.empty[AbsPath]) { (acc, next) =>
          next match
            case Some(root) => acc :+ root
            case None       => acc
        }
      }

  private def one(home: Path, dir: CacheDir): UIO[Option[AbsPath]] =
    ZIO
      .attemptBlocking(inspect(home, dir))
      .refineOrDie {
        case _: IOException | _: SecurityException | _: InvalidPathException => CacheSkip.Unreadable
      }
      .flatMap(ZIO.fromEither)
      .foldZIO(
        {
          case CacheSkip.Absent => ZIO.succeed(None)
          case why              => ZIO.logWarning(why.explain(dir)).as(None)
        },
        root => ZIO.succeed(Some(root)),
      )

  private def inspect(home: Path, dir: CacheDir): Either[CacheSkip, AbsPath] =
    for
      base     <- walked(home, Set.empty)
      _        <- directory(base)
      lexical  <- Right(dir.segments.foldLeft(base)((acc, name) => acc.resolve(name)))
      _        <- present(lexical)
      resolved <- walked(lexical, Set.empty)
      _        <- placed(base, resolved)
      _        <- directory(resolved)
      _        <- same(lexical, resolved)
      root     <- abs(resolved)
    yield root

  private def walked(start: Path, seen: Set[String]): Either[CacheSkip, Path] =
    HomePath.real(start, seen).left.map {
      case WalkFault.Outside  => CacheSkip.Outside
      case WalkFault.Dangling => CacheSkip.Dangling
      case WalkFault.Loop     => CacheSkip.Loop
      case WalkFault.BadPath  => CacheSkip.BadPath
    }

  /** The cache directory is that directory, not a symlink to somewhere else. */
  private def same(lexical: Path, resolved: Path): Either[CacheSkip, Unit] =
    if lexical.toAbsolutePath.toString == resolved.toAbsolutePath.toString then Right(())
    else Left(CacheSkip.Moved)

  private def placed(parent: Path, child: Path): Either[CacheSkip, Unit] =
    val base = parent.toAbsolutePath.toString
    val text = child.toAbsolutePath.toString
    if base != "/" && text.startsWith(base + "/") && noDots(text) then Right(())
    else Left(CacheSkip.Outside)

  private def noDots(text: String): Boolean =
    text.split('/').forall(part => part != ".." && part != ".")

  private def present(path: Path): Either[CacheSkip, Unit] =
    if Files.exists(path, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(path) then Right(())
    else Left(CacheSkip.Absent)

  private def directory(path: Path): Either[CacheSkip, Unit] =
    if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then Right(())
    else Left(CacheSkip.NotDirectory)

  private def abs(path: Path): Either[CacheSkip, AbsPath] =
    AbsPath.from(path.toString) match
      case Right(value)                   => Right(value)
      case Left(AbsPathError.NotAbsolute) => Left(CacheSkip.BadPath)
      case Left(AbsPathError.Illegal)     => Left(CacheSkip.BadPath)
end CacheDir
