package terret.grok

import zio.{IO, ZIO}

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}

/** Where the resolved `current` target has to sit. */
enum LinkBound:
  /** Strictly inside the resolved `candidates/<name>` directory. */
  case UnderBoundary

  /** Strictly inside a real `home/sdks`, or strictly inside `candidates/<name>`. `sdks` itself is refused. */
  case UnderSdks

/** Shared walk for an SDKMAN `candidates/<name>/current` link. */
object SdkmanLink:
  def decide(home: Path, name: String, bound: LinkBound): IO[JdkRefuse, Granted] =
    ZIO
      .attemptBlocking(inspect(home, name, bound))
      .refineOrDie {
        case _: IOException | _: SecurityException | _: InvalidPathException => JdkRefuse.Unreadable
      }
      .flatMap(ZIO.fromEither)

  private def inspect(home: Path, name: String, bound: LinkBound): Either[JdkRefuse, Granted] =
    if !home.isAbsolute then Left(JdkRefuse.BadPath)
    else
      for
        base       <- realpath(home, Set.empty)
        _          <- directory(base)
        candidates <- candidateDir(base, name)
        current = candidates.resolve("current")
        _      <- present(current)
        target <- realpath(current, Set.empty)
        _      <- contained(bound, base, candidates, target)
        _      <- directory(target)
        root   <- abs(target)
        link   <- abs(current)
      yield Granted(root, link)

  private def contained(
      bound: LinkBound,
      home: Path,
      candidates: Path,
      target: Path,
  ): Either[JdkRefuse, Unit] =
    bound match
      case LinkBound.UnderBoundary => within(candidates, target)
      case LinkBound.UnderSdks     =>
        within(candidates, target) match
          case Right(_) => Right(())
          case Left(_)  => underSdks(home, target)

  /** `sdks` is a real directory, not a symlink, and `target` is strictly inside it. Anything else is outside. */
  private def underSdks(home: Path, target: Path): Either[JdkRefuse, Unit] =
    val lexical = home.resolve("sdks")
    val checked =
      for
        _        <- present(lexical)
        resolved <- realpath(lexical, Set.empty)
        _        <- unmoved(lexical, resolved)
        _        <- directory(resolved)
        _        <- within(resolved, target)
      yield ()
    checked.left.map(_ => JdkRefuse.Outside)

  private def candidateDir(home: Path, name: String): Either[JdkRefuse, Path] =
    val lexical = home.resolve(".sdkman").resolve("candidates").resolve(name)
    for
      _        <- present(lexical)
      resolved <- realpath(lexical, Set.empty)
      _        <- within(home, resolved)
      _        <- directory(resolved)
      _        <- unmoved(lexical, resolved)
    yield resolved

  private def within(parent: Path, child: Path): Either[JdkRefuse, Unit] =
    HomePath.within(parent, child).left.map(outside)

  private def unmoved(lexical: Path, resolved: Path): Either[JdkRefuse, Unit] =
    HomePath.unmoved(lexical, resolved).left.map(outside)

  private def outside(fault: WalkFault): JdkRefuse =
    fault match
      case WalkFault.Outside  => JdkRefuse.Outside
      case WalkFault.Dangling => JdkRefuse.Outside
      case WalkFault.Loop     => JdkRefuse.Outside
      case WalkFault.BadPath  => JdkRefuse.Outside

  private def present(path: Path): Either[JdkRefuse, Unit] =
    if Files.exists(path, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(path) then Right(())
    else Left(JdkRefuse.Absent)

  private def directory(path: Path): Either[JdkRefuse, Unit] =
    if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then Right(())
    else Left(JdkRefuse.NotDirectory)

  private def abs(path: Path): Either[JdkRefuse, AbsPath] =
    AbsPath.from(path.toString) match
      case Right(value)                   => Right(value)
      case Left(AbsPathError.NotAbsolute) => Left(JdkRefuse.BadPath)
      case Left(AbsPathError.Illegal)     => Left(JdkRefuse.BadPath)

  private def realpath(start: Path, seen: Set[String]): Either[JdkRefuse, Path] =
    HomePath.real(start, seen).left.map {
      case WalkFault.Outside  => JdkRefuse.Outside
      case WalkFault.Dangling => JdkRefuse.Dangling
      case WalkFault.Loop     => JdkRefuse.Loop
      case WalkFault.BadPath  => JdkRefuse.BadPath
    }
end SdkmanLink
