package terret.grok

import terret.harness.ProtocolFault
import zio.json.ast.Json

enum Wire:
  case Call(id: Json, method: String, params: Json)
  case Note(method: String, params: Json)
  case Ok(id: Json, result: Json)
  case Err(id: Json, code: Int)
end Wire

object Wire:
  def request(id: Int, method: String, params: Json): Json =
    request(Json.Num(BigDecimal(id)), method, params)

  def request(id: Json, method: String, params: Json): Json =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> id,
      "method"  -> Json.Str(method),
      "params"  -> params,
    )

  def notify(method: String, params: Json): Json =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "method"  -> Json.Str(method),
      "params"  -> params,
    )

  def ok(id: Json, result: Json): Json =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> id,
      "result"  -> result,
    )

  def fail(id: Json, code: Int, message: String): Json =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> id,
      "error"   -> Json.Obj("code" -> Json.Num(BigDecimal(code)), "message" -> Json.Str(message)),
    )

  val methodNotFound: Int = -32601

  def parse(json: Json): Either[ProtocolFault, Wire] =
    json match
      case obj: Json.Obj =>
        val method = JsonFields.str(obj, "method")
        val id     = JsonFields.field(obj, "id").filter(_ != Json.Null)
        val params = JsonFields.field(obj, "params").getOrElse(Json.Obj())
        val result = JsonFields.field(obj, "result")
        val error  = JsonFields.field(obj, "error")
        (method, id, result, error) match
          case (Some(name), Some(found), _, _)            => Right(Wire.Call(found, name, params))
          case (Some(name), None, _, _)                   => Right(Wire.Note(name, params))
          case (None, Some(found), _, Some(err))          => Right(Wire.Err(found, codeOf(err)))
          case (None, Some(found), Some(value), None)     => Right(Wire.Ok(found, value))
          case _                                          => Left(ProtocolFault.Malformed)
      case _ => Left(ProtocolFault.Malformed)

  def sameId(left: Json, right: Json): Boolean =
    (left, right) match
      case (Json.Num(x), Json.Num(y)) => x.compareTo(y) == 0
      case (Json.Str(x), Json.Str(y)) => x == y
      case _                          => left == right

  def isUpdate(method: String): Boolean =
    method match
      case "session/update" | "_x.ai/session/update" | "x.ai/session/update" => true
      case _                                                                 => false

  private def codeOf(error: Json): Int =
    error match
      case obj: Json.Obj =>
        JsonFields.field(obj, "code") match
          case Some(Json.Num(number)) => number.intValue()
          case _                      => 0
      case _ => 0
end Wire
