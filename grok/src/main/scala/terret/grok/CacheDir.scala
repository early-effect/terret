package terret.grok

import zio.{Chunk, UIO, ZIO}

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}

/** One build cache directory. The path is these segments under home, and nothing above them. */
enum CacheDir:
  case Histos
  case CacheHistos
  case Coursier
  case MacCoursier
  case CacheSbt
  case Ivy
  case Maven

  def segments: Chunk[String] =
    this match
      case Histos      => Chunk(".histos")
      case CacheHistos => Chunk(".cache", "histos")
      case Coursier    => Chunk(".cache", "coursier")
      case MacCoursier => Chunk("Library", "Caches", "Coursier")
      case CacheSbt    => Chunk(".cache", "sbt")
      case Ivy         => Chunk(".ivy2")
      case Maven       => Chunk(".m2")

/** A cache directory that resolved inside home. Writability is decided with the profile, not here. */
final case class CacheGrant(dir: CacheDir, root: AbsPath)

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
    Chunk(
      CacheDir.Histos,
      CacheDir.CacheHistos,
      CacheDir.Coursier,
      CacheDir.MacCoursier,
      CacheDir.CacheSbt,
      CacheDir.Ivy,
      CacheDir.Maven,
    )

  /** Directories from `allowlist` that are real, strictly inside `home`, and not a symlink elsewhere.
    *
    * `CacheDir.decide(home)` grants `home/.cache/coursier` when that directory is real. A missing directory is
    * skipped. A link to another directory, including one inside `home`, is skipped. `.cache/sbt` covers `boot`.
    */
  def decide(home: Path): UIO[Chunk[CacheGrant]] =
    if !home.isAbsolute then ZIO.logWarning("cache home is not absolute").as(Chunk.empty)
    else ZIO.foreach(allowlist)(one(home, _)).map(_.collect { case Some(grant) => grant })

  private def one(home: Path, dir: CacheDir): UIO[Option[CacheGrant]] =
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

  private def inspect(home: Path, dir: CacheDir): Either[CacheSkip, CacheGrant] =
    for
      base <- walked(home, Set.empty)
      _    <- directory(base)
      lexical = dir.segments.foldLeft(base)((acc, name) => acc.resolve(name))
      _        <- present(lexical)
      resolved <- walked(lexical, Set.empty)
      _        <- placed(base, resolved)
      _        <- directory(resolved)
      _        <- same(lexical, resolved)
      root     <- abs(resolved)
    yield CacheGrant(dir, root)

  private def walked(start: Path, seen: Set[String]): Either[CacheSkip, Path] =
    HomePath.real(start, seen).left.map {
      case WalkFault.Outside  => CacheSkip.Outside
      case WalkFault.Dangling => CacheSkip.Dangling
      case WalkFault.Loop     => CacheSkip.Loop
      case WalkFault.BadPath  => CacheSkip.BadPath
    }

  /** The cache directory is that directory, not a symlink to somewhere else. */
  private def same(lexical: Path, resolved: Path): Either[CacheSkip, Unit] =
    HomePath.unmoved(lexical, resolved).left.map {
      case WalkFault.Outside  => CacheSkip.Moved
      case WalkFault.Dangling => CacheSkip.Moved
      case WalkFault.Loop     => CacheSkip.Moved
      case WalkFault.BadPath  => CacheSkip.Moved
    }

  private def placed(parent: Path, child: Path): Either[CacheSkip, Unit] =
    HomePath.within(parent, child).left.map {
      case WalkFault.Outside  => CacheSkip.Outside
      case WalkFault.Dangling => CacheSkip.Outside
      case WalkFault.Loop     => CacheSkip.Outside
      case WalkFault.BadPath  => CacheSkip.Outside
    }

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
