package terret.grok

import zio.Chunk

/** Already-resolved absolute paths. The command string is not an input. */
final case class SeatbeltPolicy(
    reads: Chunk[String],
    writes: Chunk[String],
    ancestors: Chunk[String],
    network: Boolean,
    links: Chunk[String] = Chunk.empty,
    executables: Chunk[String] = Chunk.empty,
)

object SeatbeltProfile:
  def render(policy: SeatbeltPolicy): Either[SandboxError, String] =
    for
      reads       <- each(policy.reads)
      writes      <- each(policy.writes)
      ancestors   <- each(policy.ancestors)
      links       <- each(policy.links)
      executables <- each(policy.executables)
    yield
      val body =
        preamble ++
          reads.map(quoted => s"(allow file-read* (subpath $quoted))") ++
          executables.map(quoted => s"(allow file-map-executable (subpath $quoted))") ++
          writes.map(quoted => s"(allow file-write* (subpath $quoted))") ++
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
      "(allow mach*)",
      "(allow ipc*)",
      "(allow system*)",
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
