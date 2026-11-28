package terret.grok

import zio.{IO, ZIO}

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}

/** Why `current` did not yield a JDK directory. */
enum JdkRefuse:
  case Absent
  case Outside
  case Dangling
  case Loop
  case BadPath
  case Unreadable
  case NotDirectory

  def explain: String =
    this match
      case Absent       => "sdkman jdk absent"
      case Outside      => "sdkman jdk outside candidates/java"
      case Dangling     => "sdkman jdk link is dangling"
      case Loop         => "sdkman jdk link loops"
      case BadPath      => "sdkman jdk path is rejected"
      case Unreadable   => "sdkman jdk path is unreadable"
      case NotDirectory => "sdkman jdk is not a directory"

/** The resolved JDK directory and the `current` symlink node. The link is not a subpath. */
final case class Granted(root: AbsPath, link: AbsPath)

/** The directory `current` resolves to, when that directory sits inside `.sdkman/candidates/java`.
  *
  * `JdkGrant.decide(home)` grants `home/.sdkman/candidates/java/21` when `current` points at it, and the
  * literal path `candidates/java/current`. A link to `/`, the home directory, or anywhere else is refused.
  */
object JdkGrant:
  def decide(home: Path): IO[JdkRefuse, Granted] =
    ZIO
      .attemptBlocking(inspect(home))
      .refineOrDie {
        case _: IOException | _: SecurityException | _: InvalidPathException => JdkRefuse.Unreadable
      }
      .flatMap(ZIO.fromEither)

  private def inspect(home: Path): Either[JdkRefuse, Granted] =
    if !home.isAbsolute then Left(JdkRefuse.BadPath)
    else
      for
        base     <- realpath(home, Set.empty)
        _        <- directory(base)
        boundary <- boundary(base)
        current  <- Right(boundary.resolve("current"))
        _        <- present(current)
        target   <- realpath(current, Set.empty)
        _        <- placed(boundary, target)
        _        <- directory(target)
        root     <- abs(target)
        link     <- abs(current)
      yield Granted(root, link)

  private def boundary(home: Path): Either[JdkRefuse, Path] =
    val lexical = home.resolve(".sdkman").resolve("candidates").resolve("java")
    for
      _        <- present(lexical)
      resolved <- realpath(lexical, Set.empty)
      _        <- placed(home, resolved)
      _        <- directory(resolved)
      _        <- same(lexical, resolved)
    yield resolved

  /** The resolved candidates directory is that directory, not a symlink to somewhere else. */
  private def same(lexical: Path, resolved: Path): Either[JdkRefuse, Unit] =
    if lexical.toAbsolutePath.toString == resolved.toAbsolutePath.toString then Right(())
    else Left(JdkRefuse.Outside)

  private def placed(parent: Path, child: Path): Either[JdkRefuse, Unit] =
    val base = parent.toAbsolutePath.toString
    val text = child.toAbsolutePath.toString
    if base != "/" && inside(base, text) then Right(()) else Left(JdkRefuse.Outside)

  private def inside(base: String, text: String): Boolean =
    text.startsWith(base + "/") && text.split('/').forall(part => part != ".." && part != ".")

  private def present(path: Path): Either[JdkRefuse, Unit] =
    if Files.exists(path, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(path) then Right(())
    else Left(JdkRefuse.Absent)

  private def directory(path: Path): Either[JdkRefuse, Unit] =
    if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then Right(())
    else Left(JdkRefuse.NotDirectory)

  private def abs(path: Path): Either[JdkRefuse, AbsPath] =
    AbsPath.from(path.toString) match
      case Right(value)                    => Right(value)
      case Left(AbsPathError.NotAbsolute)  => Left(JdkRefuse.BadPath)
      case Left(AbsPathError.Illegal)      => Left(JdkRefuse.BadPath)

  private def realpath(start: Path, seen: Set[String]): Either[JdkRefuse, Path] =
    HomePath.real(start, seen).left.map {
      case WalkFault.Outside  => JdkRefuse.Outside
      case WalkFault.Dangling => JdkRefuse.Dangling
      case WalkFault.Loop     => JdkRefuse.Loop
      case WalkFault.BadPath  => JdkRefuse.BadPath
    }
end JdkGrant
