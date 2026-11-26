package terret.find

import zio.Chunk

/** Which model produced a vector. Stored beside the floats so a later cosine cannot mix spaces. */
opaque type ModelId = String

object ModelId:
  def apply(value: String): ModelId = value

  extension (id: ModelId) def value: String = id

  val bgeSmall: ModelId = ModelId("bge-small-en-v1.5")
end ModelId

/** One L2-normalized vector and the model that produced it. */
final case class Embedding(model: ModelId, values: Chunk[Float]):
  def dims: Int = values.length
