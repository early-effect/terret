package terret.find

import zio.{IO, ZIO}

/** Turns text into one normalized vector.
  *
  * `embedQuery` is the search string. `embedPassage` is a stored description. The bge-small implementation prefixes
  * only the query.
  */
trait Embedder:
  def embedQuery(text: String): IO[FindError, Embedding]
  def embedPassage(text: String): IO[FindError, Embedding]

object Embedder:
  /** Rejects a blank query before the service runs. */
  def embedQuery(text: String): ZIO[Embedder, FindError, Embedding] =
    ZIO.fromEither(nonBlank(text)).flatMap(value => ZIO.serviceWithZIO[Embedder](_.embedQuery(value)))

  /** Rejects a blank passage before the service runs. */
  def embedPassage(text: String): ZIO[Embedder, FindError, Embedding] =
    ZIO.fromEither(nonBlank(text)).flatMap(value => ZIO.serviceWithZIO[Embedder](_.embedPassage(value)))

  def nonBlank(text: String): Either[FindError, String] =
    val trimmed = text.trim
    if trimmed.isEmpty then Left(FindError.EmptyQuery) else Right(trimmed)
end Embedder
