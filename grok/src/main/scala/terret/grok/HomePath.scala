package terret.grok

import zio.Chunk

import java.nio.file.{Files, LinkOption, Path}

import scala.jdk.CollectionConverters.*

/** Why a component walk did not produce a directory. */
enum WalkFault:
  case Outside
  case Dangling
  case Loop
  case BadPath

/** Follows each path component. `..` is applied after a link, not before it. */
object HomePath:
  // FileRoots.live.resolve normalizes before following links, and None hides a loop, a dangling target, and IO.
  def real(start: Path, seen: Set[String]): Either[WalkFault, Path] =
    rooted(start).flatMap { abs =>
      val key = abs.toString
      if seen.contains(key) then Left(WalkFault.Loop)
      else descend(Path.of("/"), names(abs), seen)
    }

  private def names(path: Path): Chunk[String] =
    Chunk.from(path.iterator.asScala.map(_.toString))

  private def descend(acc: Path, names: Chunk[String], seen: Set[String]): Either[WalkFault, Path] =
    names.headOption match
      case None => Right(acc)
      case Some(name) =>
        step(acc, name, seen).flatMap(next => descend(next, names.drop(1), seen))

  private def step(acc: Path, name: String, seen: Set[String]): Either[WalkFault, Path] =
    if name == "." then Right(acc)
    else if name == ".." then Right(Option(acc.getParent).getOrElse(acc))
    else if name.isEmpty then Left(WalkFault.BadPath)
    else enter(acc.resolve(name), seen)

  private def enter(path: Path, seen: Set[String]): Either[WalkFault, Path] =
    rooted(path) match
      case Left(why) => Left(why)
      case Right(abs) =>
        val key = abs.toString
        if seen.contains(key) then Left(WalkFault.Loop)
        else if Files.isSymbolicLink(abs) then follow(abs, seen + key)
        else if Files.exists(abs, LinkOption.NOFOLLOW_LINKS) then Right(abs)
        else Left(WalkFault.Dangling)

  private def rooted(path: Path): Either[WalkFault, Path] =
    val abs = path.toAbsolutePath
    if abs.isAbsolute then Right(abs) else Left(WalkFault.BadPath)

  private def follow(link: Path, seen: Set[String]): Either[WalkFault, Path] =
    val raw = Files.readSymbolicLink(link)
    linked(link, raw) match
      case Left(why)   => Left(why)
      case Right(path) => real(path, seen)

  private def linked(link: Path, raw: Path): Either[WalkFault, Path] =
    if raw.isAbsolute then Right(raw)
    else
      Option(link.getParent) match
        case Some(parent) => Right(parent.resolve(raw))
        case None         => Left(WalkFault.Outside)
end HomePath
