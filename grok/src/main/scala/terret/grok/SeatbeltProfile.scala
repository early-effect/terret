package terret.grok

import zio.Chunk

import java.io.IOException
import java.nio.file.{Files, InvalidPathException, LinkOption}

/** Already-resolved absolute paths. The command string is not an input. */
final case class SeatbeltPolicy(
    reads: Chunk[String],
    writes: Chunk[String],
    ancestors: Chunk[String],
    network: Boolean,
    links: Chunk[String] = Chunk.empty,
    executables: Chunk[String] = Chunk.empty,
)

/** Read and exec roots for the SDKMAN JDK on `PATH`. Never a write.
  *
  * The grant is the `current` link and the directory it resolves to. A target of `/`, or any directory that covers
  * the home or the link itself, drops the link too: a subpath of that symlink would follow it.
  */
object JdkGrant:
  def paths(link: String, real: Option[String], home: String): Chunk[String] =
    real match
      case Some(target) if !kept(target) || wide(target, link, home) => Chunk.empty
      case Some(target) if kept(link) && target != link              => Chunk(link, target)
      case Some(_) if kept(link)                                     => Chunk(link)
      case None if kept(link)                                        => Chunk(link)
      case _                                                          => Chunk.empty

  /** The host `~/.sdkman/candidates/java/current` link, when this process can see it. */
  def host: Chunk[String] =
    val home = FileRoots.home.toAbsolutePath.normalize
    val link = home.resolve(".sdkman").resolve("candidates").resolve("java").resolve("current")
    try
      val lexical = link.toAbsolutePath.normalize.toString
      if Files.isSymbolicLink(link) then
        val real =
          try Some(link.toRealPath().normalize.toString)
          catch case _: IOException | _: SecurityException | _: InvalidPathException => None
        // An unresolved symlink is not granted. Its target might be the home directory.
        real match
          case Some(resolved) => paths(lexical, Some(resolved), home.toString)
          case None           => Chunk.empty
      else if Files.exists(link, LinkOption.NOFOLLOW_LINKS) then paths(lexical, None, home.toString)
      else Chunk.empty
    catch case _: IOException | _: SecurityException | _: InvalidPathException => Chunk.empty

  private def kept(path: String): Boolean =
    path.startsWith("/") && path.length > 1 && !path.exists(control)

  private def wide(real: String, link: String, home: String): Boolean =
    covers(real, home) || covers(real, link)

  private def covers(root: String, path: String): Boolean =
    root == "/" || path == root || path.startsWith(root + "/")

  private def control(ch: Char): Boolean =
    ch < ' ' || ch == '\u007f'
end JdkGrant

object SeatbeltProfile:
  def render(policy: SeatbeltPolicy): Either[SandboxError, String] =
    for
      reads       <- each(policy.reads)
      writes      <- each(policy.writes)
      ancestors   <- each(policy.ancestors)
      links       <- each(policy.links)
      executables <- each(policy.executables)
      services    <- SeatbeltServices.lines
    yield
      val body =
        preamble ++
          services ++
          reads.map(quoted => s"(allow file-read* (subpath $quoted))") ++
          executables.map(quoted => s"(allow file-map-executable (subpath $quoted))") ++
          writes.map(quoted => s"(allow file-write* (subpath $quoted))") ++
          unix(writes) ++
          ancestors.map(quoted => s"(allow file-read-metadata (literal $quoted))") ++
          links.map(quoted => s"(allow file-read-data (literal $quoted))") ++
          (if policy.network then Chunk("(allow network*)") else Chunk.empty)
      body.mkString("", "\n", "\n")

  def quote(raw: String): Either[SandboxError, String] =
    if !raw.startsWith("/") then Left(SandboxError.BadPath(raw))
    else if raw.exists(ch => ch < ' ' || ch == '\u007f') then Left(SandboxError.BadPath(raw))
    else
      val escaped = raw.flatMap {
        case '\\' => "\\\\"
        case '"'  => "\\\""
        case ch   => ch.toString
      }
      Right("\"" + escaped + "\"")

  /** Parents of `path`, not `path` itself. `getcwd` stats them. Their contents stay closed. */
  def ancestors(path: String): Chunk[String] =
    def go(current: java.nio.file.Path): Chunk[String] =
      Option(current.getParent) match
        case None => Chunk.empty
        case Some(parent) =>
          val text = parent.toString
          if text == "/" then Chunk("/") else text +: go(parent)
    go(java.nio.file.Path.of(path))

  /** A local server binds a Unix socket under a write root. The filter is that directory, so TCP stays closed. */
  private def unix(quoted: Chunk[String]): Chunk[String] =
    quoted.flatMap { path =>
      Chunk(
        "; Unix socket under a write root. TCP stays with the network grant.",
        s"(allow network-bind (local unix-socket (subpath $path)))",
        s"(allow network-outbound (remote unix-socket (subpath $path)))",
        s"(allow network-inbound (local unix-socket (subpath $path)))",
      )
    }

  private def each(paths: Chunk[String]): Either[SandboxError, Chunk[String]] =
    paths.foldLeft[Either[SandboxError, Chunk[String]]](Right(Chunk.empty)) { (acc, path) =>
      acc.flatMap(done => quote(path).map(done :+ _))
    }

  private val preamble: Chunk[String] =
    Chunk(
      "(version 1)",
      "(deny default)",
      "(allow process*)",
      "(allow signal)",
      "(allow sysctl*)",
      "(allow file-map-executable)",
      "(allow file-ioctl (literal \"/dev/null\"))",
      "(allow file-ioctl (literal \"/dev/dtracehelper\"))",
      // Seatbelt's FIFO vnode type. Inherited stdout and stderr are pipes, and a
      // deny-default profile kills the process when it writes them.
      "(allow file-write-data (vnode-type 7))",
      "(allow file-read-data (vnode-type 7))",
      "(allow file-write* (literal \"/dev/null\"))",
      "(allow file-write* (literal \"/dev/dtracehelper\"))",
      // Absolute lookup needs the root directory inode. A subpath of /usr does not cover it.
      "(allow file-read* (literal \"/\"))",
    )
end SeatbeltProfile
