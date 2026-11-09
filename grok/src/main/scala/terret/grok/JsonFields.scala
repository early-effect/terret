package terret.grok

import zio.json.ast.Json
import zio.Chunk

object JsonFields:
  def obj(json: Json): Option[Json.Obj] =
    json match
      case found: Json.Obj => Some(found)
      case _               => None

  def field(obj: Json.Obj, name: String): Option[Json] =
    obj.fields.collectFirst { case (key, value) if key == name => value }

  def str(obj: Json.Obj, name: String): Option[String] =
    field(obj, name).collect { case Json.Str(text) => text }

  def text(json: Json): Option[String] =
    json match
      case Json.Str(value) if value.nonEmpty => Some(value)
      case found: Json.Obj                   =>
        str(found, "text").filter(_.nonEmpty).orElse(str(found, "data").filter(_.nonEmpty)).orElse {
          field(found, "content").flatMap(text)
        }
      case Json.Arr(values) => firstText(values)
      case _                => None

  private def firstText(values: Chunk[Json]): Option[String] =
    values.foldLeft(Option.empty[String])((found, item) => found.orElse(text(item)))
end JsonFields
