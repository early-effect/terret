package terret.grok

import zio.{Chunk, IO, ZIO}

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}

import scala.jdk.CollectionConverters.*

/** An absolute path that can be quoted into a seatbelt profile. */
opaque type AbsPath = String

object AbsPath:
  def from(raw: String): Either[JdkRefuse, AbsPath] =
    if raw.startsWith("/") && raw.length > 1 && !raw.exists(illegal) then Right(raw)
    else Left(JdkRefuse.BadPath)

  extension (path: AbsPath) def text: String = path

  private def illegal(ch: Char): Boolean =
    ch < ' ' || ch == '\u007f' || ch == '"' || ch == '\\'
end AbsPath

/** Why `current` did not yield a JDK directory. */
enum JdkRefuse:
  case Absent
  case Outside
  case Dangling
  case Loop
  case BadPath
  case Unreadable

/** The directory `current` resolves to, when that directory sits inside `.sdkman/candidates/java`.
  *
  * `JdkGrant.decide(home)` grants `home/.sdkman/candidates/java/21` when `current` points at it.
  * A link to `/`, the home directory, or anywhere else is refused. The link path itself is not granted.
  */
object JdkGrant:
  def decide(home: Path): IO[JdkRefuse, AbsPath] =
    ZIO
      .attemptBlocking(inspect(home))
      .catchAll {
        case _: IOException | _: SecurityException | _: InvalidPathException => ZIO.fail(JdkRefuse.Unreadable)
        case defect => ZIO.die(defect)
      }
      .flatMap(ZIO.fromEither)

  private def inspect(home: Path): Either[JdkRefuse, AbsPath] =
    if !home.isAbsolute then Left(JdkRefuse.BadPath)
    else
      for
        base     <- realpath(home, Set.empty)
        _        <- directory(base)
        boundary <- boundary(base)
        current  <- Right(boundary.resolve("current"))
        _        <- present(current)
        target   <- realpath(current, Set.empty)
        _        <- directory(target)
        _        <- placed(boundary, target)
        path     <- AbsPath.from(target.toString)
      yield path

  private def boundary(home: Path): Either[JdkRefuse, Path] =
    val lexical = home.resolve(".sdkman").resolve("candidates").resolve("java")
    for
      _        <- present(lexical)
      resolved <- realpath(lexical, Set.empty)
      _        <- directory(resolved)
      _        <- placed(home, resolved)
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
    try
      if Files.exists(path, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(path) then Right(())
      else Left(JdkRefuse.Absent)
    catch case _: IOException | _: SecurityException | _: InvalidPathException => Left(JdkRefuse.Unreadable)

  private def directory(path: Path): Either[JdkRefuse, Unit] =
    try
      if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then Right(())
      else Left(JdkRefuse.Outside)
    catch case _: IOException | _: SecurityException | _: InvalidPathException => Left(JdkRefuse.Unreadable)

  // One component at a time. Normalizing `link/../jdk` first would follow `link` too late and
  // could grant a local directory when `link` points at `/`.
  private def realpath(start: Path, seen: Set[String]): Either[JdkRefuse, Path] =
    rooted(start).flatMap { abs =>
      val key = abs.toString
      if seen.contains(key) then Left(JdkRefuse.Loop)
      else descend(Path.of("/"), names(abs), seen)
    }

  private def names(path: Path): Chunk[String] =
    Chunk.from(path.iterator.asScala.map(_.toString))

  private def descend(acc: Path, names: Chunk[String], seen: Set[String]): Either[JdkRefuse, Path] =
    names.headOption match
      case None => Right(acc)
      case Some(name) =>
        step(acc, name, seen).flatMap(next => descend(next, names.drop(1), seen))

  private def step(acc: Path, name: String, seen: Set[String]): Either[JdkRefuse, Path] =
    if name == "." then Right(acc)
    else if name == ".." then Right(Option(acc.getParent).getOrElse(acc))
    else if name.isEmpty then Left(JdkRefuse.BadPath)
    else enter(acc.resolve(name), seen)

  private def enter(path: Path, seen: Set[String]): Either[JdkRefuse, Path] =
    rooted(path) match
      case Left(why) => Left(why)
      case Right(abs) =>
        val key = abs.toString
        if seen.contains(key) then Left(JdkRefuse.Loop)
        else
          try
            if Files.isSymbolicLink(abs) then follow(abs, seen + key)
            else if Files.exists(abs, LinkOption.NOFOLLOW_LINKS) then Right(abs)
            else Left(JdkRefuse.Dangling)
          catch case _: IOException | _: SecurityException | _: InvalidPathException =>
            Left(JdkRefuse.Unreadable)

  private def rooted(path: Path): Either[JdkRefuse, Path] =
    try
      val abs = path.toAbsolutePath
      if abs.isAbsolute then Right(abs) else Left(JdkRefuse.BadPath)
    catch case _: InvalidPathException => Left(JdkRefuse.BadPath)

  private def follow(link: Path, seen: Set[String]): Either[JdkRefuse, Path] =
    try
      val raw = Files.readSymbolicLink(link)
      linked(link, raw) match
        case Left(why)   => Left(why)
        case Right(path) => realpath(path, seen)
    catch case _: IOException | _: SecurityException | _: InvalidPathException =>
      Left(JdkRefuse.Unreadable)

  private def linked(link: Path, raw: Path): Either[JdkRefuse, Path] =
    if raw.isAbsolute then Right(raw)
    else
      Option(link.getParent) match
        case Some(parent) => Right(parent.resolve(raw))
        case None         => Left(JdkRefuse.Outside)
end JdkGrant
