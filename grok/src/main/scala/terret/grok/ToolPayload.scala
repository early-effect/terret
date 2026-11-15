package terret.grok

import zio.json.*
import zio.json.ast.Json

opaque type ToolPayload = Json

object ToolPayload:
  def apply(json: Json): ToolPayload = json

  extension (value: ToolPayload) def json: Json = value

  // `ToolText` is opaque in this file, so stdout bytes use that codec. Inside `ToolText` they would decode as `String`.
  given JsonCodec[ToolPayload] = JsonCodec[Json].transform(rewrite, identity)

  private def rewrite(json: Json): ToolPayload = apply(walk(json))

  private def walk(json: Json): Json =
    json match
      case obj: Json.Obj    => Json.Obj(obj.fields.map(rewriteField))
      case Json.Arr(values) => Json.Arr(values.map(walk))
      case other            => other

  private def rewriteField(field: (String, Json)): (String, Json) =
    val (name, value) = field
    val rewritten     = walk(value)
    if textField(name) then
      rewritten.as[ToolText] match
        case Right(text) => (name, Json.Str(text.text))
        case Left(_)     => (name, rewritten)
    else (name, rewritten)

  private def textField(name: String): Boolean =
    name match
      case "stdout" | "stderr" | "output" => true
      case _                              => false
end ToolPayload
