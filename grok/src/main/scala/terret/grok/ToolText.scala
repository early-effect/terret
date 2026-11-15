package terret.grok

import zio.json.*
import zio.Chunk

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{CodingErrorAction, StandardCharsets}

opaque type ToolText = String

object ToolText:
  def apply(text: String): ToolText = text

  extension (value: ToolText) def text: String = value

  given JsonCodec[ToolText] =
    JsonCodec(
      JsonEncoder[String].contramap(_.text),
      JsonDecoder[String].map(apply).orElse(JsonDecoder[Chunk[Int]].mapOrFail(utf8)),
    )

  // The three-argument decode reports a CoderResult. The one-argument decode throws.
  private def utf8(bytes: Chunk[Int]): Either[String, ToolText] =
    if bytes.exists(value => value < 0 || value > 255) then Left("byte out of range")
    else
      val raw     = bytes.map(_.toByte).toArray
      val decoder = StandardCharsets.UTF_8
        .newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      val input   = ByteBuffer.wrap(raw)
      val output  = CharBuffer.allocate(math.max(raw.length, 1))
      val decoded = decoder.decode(input, output, true)
      val flushed = decoder.flush(output)
      if decoded.isError || flushed.isError then Left("invalid utf-8")
      else
        output.flip()
        Right(apply(output.toString))
end ToolText
