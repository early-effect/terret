package terret.find

import zio.Chunk

/** A tool `find` can return. `signature` is the typed ZIO shape. */
final case class ToolRef(
    name: String,
    signature: String,
    doc: String,
    capabilities: Chunk[Capability],
)

object ToolRef:
  def from(entry: Entry): Either[FindError, ToolRef] =
    decode(entry.capabilities).map: caps =>
      ToolRef(entry.name, entry.signature, entry.doc, caps)

  private def decode(raw: Chunk[String]): Either[FindError, Chunk[Capability]] =
    raw.foldLeft[Either[FindError, Chunk[Capability]]](Right(Chunk.empty)):
      case (Left(error), _)   => Left(error)
      case (Right(acc), name) => Capability.from(name).map(acc :+ _)
end ToolRef
