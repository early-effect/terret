package terret.model

import zio.Chunk

/** A key of a `[model.<name>]` table in the grok config. */
opaque type ModelKey = String

object ModelKey:
  def apply(value: String): ModelKey = value

  extension (key: ModelKey) def value: String = key

  /** `None` keeps the harness default and does not consult `known`. */
  def choose(requested: Option[String], known: Chunk[ModelKey]): Either[UnknownModel, Option[ModelKey]] =
    requested match
      case None       => Right(None)
      case Some(name) =>
        val names = Chunk.fromIterable(known.map(_.value).distinct).sorted
        if names.contains(name) then Right(Some(ModelKey(name)))
        else Left(UnknownModel(name, names))
end ModelKey

final case class UnknownModel(name: String, known: Chunk[String])
