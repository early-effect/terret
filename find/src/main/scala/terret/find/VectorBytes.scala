package terret.find

import zio.Chunk

import java.nio.{ByteBuffer, ByteOrder}

/** Big-endian float bits. The width of the chunk is the vector's dimension. */
object VectorBytes:
  def encode(values: Chunk[Float]): Chunk[Byte] =
    val buffer = ByteBuffer.allocate(values.length * java.lang.Float.BYTES).order(ByteOrder.BIG_ENDIAN)
    values.foreach(value => buffer.putFloat(value))
    Chunk.fromArray(buffer.array())

  def decode(bytes: Chunk[Byte]): Either[FindError, Chunk[Float]] =
    if bytes.length % java.lang.Float.BYTES != 0 then Left(FindError.Embedding(EmbeddingProblem.Shape))
    else
      val buffer = ByteBuffer.wrap(bytes.toArray).order(ByteOrder.BIG_ENDIAN)
      val count  = bytes.length / java.lang.Float.BYTES
      Right(Chunk.fromArray(Array.fill(count)(buffer.getFloat())))
end VectorBytes
