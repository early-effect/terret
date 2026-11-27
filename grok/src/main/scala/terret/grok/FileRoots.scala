package terret.grok

import zio.{UIO, ZIO}

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}

/** Resolves a path at the moment of the call. A path that is lexically outside every granted root is refused before
  * this runs, so that path is not touched.
  */
trait FileRoots:
  def resolve(path: Path): UIO[Option[Path]]

object FileRoots:
  val live: FileRoots      = Live
  val unchanged: FileRoots = Unchanged

  def home: Path =
    val raw = System.getProperty("user.home")
    if raw == null || raw.isEmpty then Path.of("/")
    else
      try Path.of(raw)
      catch case _: InvalidPathException => Path.of("/")

  /** Absolute lexical path after `~` and `..`. `None` when the text is not a path this process can place. */
  def lexical(target: String, base: Path, home: Path): Option[Path] =
    if target.isEmpty then None
    else
      expand(target, home).flatMap { text =>
        try
          val raw = Path.of(text)
          val abs = if raw.isAbsolute then raw else base.resolve(raw)
          Some(abs.normalize)
        catch case _: InvalidPathException => None
      }

  private def expand(target: String, home: Path): Option[String] =
    if target == "~" then Some(home.toString)
    else if target.startsWith("~/") then Some(home.resolve(target.drop(2)).toString)
    else if target.startsWith("~") then expandUser(target, home)
    else Some(target)

  private def expandUser(target: String, home: Path): Option[String] =
    val slash = target.indexOf('/')
    val name  = if slash < 0 then target.drop(1) else target.substring(1, slash)
    currentUser match
      case Some(user) if user == name =>
        val rest = if slash < 0 then "" else target.drop(slash + 1)
        if rest.isEmpty then Some(home.toString) else Some(home.resolve(rest).toString)
      case _ => None

  private def currentUser: Option[String] =
    val raw = System.getProperty("user.name")
    if raw == null || raw.isEmpty then None else Some(raw)

  private object Unchanged extends FileRoots:
    def resolve(path: Path): UIO[Option[Path]] = ZIO.succeed(Some(path))

  private object Live extends FileRoots:
    def resolve(path: Path): UIO[Option[Path]] =
      ZIO.blocking(ZIO.succeed(walk(path, Set.empty)))

    private def walk(path: Path, seen: Set[Path]): Option[Path] =
      val norm = path.toAbsolutePath.normalize
      if seen.contains(norm) then None
      else
        try
          if Files.isSymbolicLink(norm) then
            val link = Files.readSymbolicLink(norm)
            val next = if link.isAbsolute then link else parentOf(norm).fold(link)(_.resolve(link))
            walk(next, seen + norm)
          else if Files.exists(norm, LinkOption.NOFOLLOW_LINKS) then
            Some(norm.toRealPath(LinkOption.NOFOLLOW_LINKS))
          else
            parentOf(norm) match
              case None => Some(norm)
              case Some(parent) =>
                nameOf(norm) match
                  case None       => walk(parent, seen + norm)
                  case Some(name) => walk(parent, seen + norm).map(_.resolve(name))
        catch case _: IOException | _: InvalidPathException | _: SecurityException => None

    private def parentOf(path: Path): Option[Path] = Option(path.getParent)

    private def nameOf(path: Path): Option[Path] = Option(path.getFileName)
end FileRoots
