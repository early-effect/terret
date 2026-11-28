package terret.grok

import zio.Chunk

import java.nio.file.Path

/** Why a string is not a profile path. */
enum AbsPathError:
  case NotAbsolute
  case Illegal

/** An absolute path that can be quoted into a seatbelt profile. Controls are rejected. `"` and `\` are escaped. */
opaque type AbsPath = String

object AbsPath:
  def from(raw: String): Either[AbsPathError, AbsPath] =
    if !raw.startsWith("/") || raw.isEmpty then Left(AbsPathError.NotAbsolute)
    else if raw.exists(illegal) then Left(AbsPathError.Illegal)
    else Right(raw)

  extension (path: AbsPath) def text: String = path

  private def illegal(ch: Char): Boolean =
    ch < ' ' || ch == '\u007f'
end AbsPath

/** Already-resolved absolute paths. The command string is not an input. */
final case class SeatbeltPolicy(
    reads: Chunk[AbsPath],
    writes: Chunk[AbsPath],
    ancestors: Chunk[AbsPath],
    network: Boolean,
    links: Chunk[AbsPath] = Chunk.empty,
    denies: Chunk[AbsPath] = Chunk.empty,
)

object SeatbeltProfile:
  def render(policy: SeatbeltPolicy): String =
    val body =
      preamble ++
        policy.reads.map(path => s"(allow file-read* (subpath ${quote(path)}))") ++
        policy.writes.map(path => s"(allow file-write* (subpath ${quote(path)}))") ++
        policy.ancestors.map(path => s"(allow file-read-metadata (literal ${quote(path)}))") ++
        policy.links.map(path => s"(allow file-read-data (literal ${quote(path)}))") ++
        // Last matching rule wins, so a literal deny has to follow the cache subpath allow.
        policy.denies.map(path => s"(deny file-read* (literal ${quote(path)}))") ++
        (if policy.network then Chunk("(allow network*)") else Chunk.empty)
    body.mkString("", "\n", "\n")

  def quote(path: AbsPath): String =
    val escaped = path.text.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case ch   => ch.toString
    }
    "\"" + escaped + "\""

  /** Parents of `path`, not `path` itself. `getcwd` stats them. Their contents stay closed. */
  def ancestors(path: AbsPath): Chunk[AbsPath] =
    def go(current: Path): Chunk[AbsPath] =
      Option(current.getParent) match
        case None => Chunk.empty
        case Some(parent) =>
          AbsPath.from(parent.toString) match
            case Right(abs) =>
              val rest = if parent.toString == "/" then Chunk.empty else go(parent)
              abs +: rest
            case Left(AbsPathError.NotAbsolute) => Chunk.empty
            case Left(AbsPathError.Illegal)     => Chunk.empty
    go(Path.of(path.text))

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
