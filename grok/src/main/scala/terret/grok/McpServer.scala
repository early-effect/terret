package terret.grok

import zio.json.*
import zio.json.ast.Json
import zio.Chunk

enum McpServer:
  case Stdio(name: String, command: String, args: Chunk[String], env: Chunk[(String, String)])
  case Http(name: String, url: String, headers: Chunk[(String, String)])
  case Sse(name: String, url: String, headers: Chunk[(String, String)])

  def server: String = this match
    case Stdio(name, _, _, _) => name
    case Http(name, _, _)     => name
    case Sse(name, _, _)      => name
end McpServer

object McpServer:
  def encodeAll(servers: Chunk[McpServer]): Json =
    Json.Arr(servers.map(encode)*)

  def encode(server: McpServer): Json =
    server match
      case McpServer.Stdio(name, command, args, env) =>
        Json.Obj(
          "name"    -> Json.Str(name),
          "command" -> Json.Str(command),
          "args"    -> Json.Arr(args.map(Json.Str(_))*),
          "env"     -> pairs(env),
        )
      case McpServer.Http(name, url, headers) =>
        Json.Obj(
          "type"    -> Json.Str("http"),
          "name"    -> Json.Str(name),
          "url"     -> Json.Str(url),
          "headers" -> pairs(headers),
        )
      case McpServer.Sse(name, url, headers) =>
        Json.Obj(
          "type"    -> Json.Str("sse"),
          "name"    -> Json.Str(name),
          "url"     -> Json.Str(url),
          "headers" -> pairs(headers),
        )

  def parse(raw: String): Either[McpDocumentError, Chunk[McpServer]] =
    raw.fromJson[Json] match
      case Left(_)     => Left(McpDocumentError.Malformed)
      case Right(json) => document(json)

  def document(json: Json): Either[McpDocumentError, Chunk[McpServer]] =
    JsonFields.obj(json) match
      case None       => Left(McpDocumentError.NotObject)
      case Some(root) =>
        JsonFields.field(root, "mcpServers") match
          case None        => Left(McpDocumentError.ServersMissing)
          case Some(value) =>
            JsonFields.obj(value) match
              case None      => Left(McpDocumentError.NotObject)
              case Some(obj) => servers(obj)

  private def servers(obj: Json.Obj): Either[McpDocumentError, Chunk[McpServer]] =
    obj.fields.foldLeft[Either[McpDocumentError, Chunk[McpServer]]](Right(Chunk.empty)): (acc, field) =>
      acc.flatMap: got =>
        val (name, value) = field
        serverOf(name, value).map(server => got :+ server)

  private def serverOf(name: String, json: Json): Either[McpDocumentError, McpServer] =
    if name.isEmpty then Left(McpDocumentError.NameEmpty)
    else
      JsonFields.obj(json) match
        case None      => Left(McpDocumentError.ServerNotObject(name))
        case Some(obj) =>
          val command = JsonFields.str(obj, "command").filter(_.nonEmpty)
          val url     = JsonFields.str(obj, "url").filter(_.nonEmpty)
          val kind    = JsonFields.str(obj, "type").filter(_.nonEmpty)
          (kind, command, url) match
            case (_, Some(_), Some(_))              => Left(McpDocumentError.Transport(name, "both"))
            case (Some("http"), _, Some(address))   => remote(name, address, obj, http = true)
            case (Some("sse"), _, Some(address))    => remote(name, address, obj, http = false)
            case (Some("http") | Some("sse"), _, _) => Left(McpDocumentError.UrlMissing(name))
            case (Some(other), _, _)                => Left(McpDocumentError.Transport(name, other))
            case (_, None, Some(_))                 => Left(McpDocumentError.Transport(name, "url"))
            case (_, Some(cmd), None)               => stdio(name, cmd, obj)
            case _                                  => Left(McpDocumentError.CommandMissing(name))

  private def stdio(name: String, command: String, obj: Json.Obj): Either[McpDocumentError, McpServer] =
    for
      args <- fieldOr(name, obj, "args", arguments, Chunk.empty)
      env  <- fieldOr(name, obj, "env", pairsOf, Chunk.empty)
    yield McpServer.Stdio(name, command, args, env)

  private def remote(
      name: String,
      url: String,
      obj: Json.Obj,
      http: Boolean,
  ): Either[McpDocumentError, McpServer] =
    fieldOr(name, obj, "headers", pairsOf, Chunk.empty).map: headers =>
      if http then McpServer.Http(name, url, headers) else McpServer.Sse(name, url, headers)

  private def fieldOr[A](
      name: String,
      obj: Json.Obj,
      key: String,
      read: (String, Json) => Either[McpDocumentError, A],
      empty: A,
  ): Either[McpDocumentError, A] =
    JsonFields.field(obj, key) match
      case None        => Right(empty)
      case Some(value) => read(name, value)

  private def arguments(name: String, json: Json): Either[McpDocumentError, Chunk[String]] =
    json match
      case Json.Arr(values) =>
        values.foldLeft[Either[McpDocumentError, Chunk[String]]](Right(Chunk.empty)): (acc, item) =>
          acc.flatMap: got =>
            item match
              case Json.Str(text) => Right(got :+ text)
              case _              => Left(McpDocumentError.Args(name))
      case _ => Left(McpDocumentError.Args(name))

  private def pairsOf(name: String, json: Json): Either[McpDocumentError, Chunk[(String, String)]] =
    json match
      case obj: Json.Obj  => objectPairs(name, obj)
      case Json.Arr(rows) => arrayPairs(name, rows)
      case _              => Left(McpDocumentError.Pair(name))

  private def objectPairs(name: String, obj: Json.Obj): Either[McpDocumentError, Chunk[(String, String)]] =
    obj.fields.foldLeft[Either[McpDocumentError, Chunk[(String, String)]]](Right(Chunk.empty)): (acc, field) =>
      acc.flatMap: got =>
        val (key, value) = field
        value match
          case Json.Str(text) => Right(got :+ (key -> text))
          case _              => Left(McpDocumentError.Pair(name))

  private def arrayPairs(
      name: String,
      rows: Chunk[Json],
  ): Either[McpDocumentError, Chunk[(String, String)]] =
    rows.foldLeft[Either[McpDocumentError, Chunk[(String, String)]]](Right(Chunk.empty)): (acc, row) =>
      acc.flatMap: got =>
        JsonFields.obj(row) match
          case None      => Left(McpDocumentError.Pair(name))
          case Some(obj) =>
            (JsonFields.str(obj, "name"), JsonFields.str(obj, "value")) match
              case (Some(key), Some(value)) => Right(got :+ (key -> value))
              case _                        => Left(McpDocumentError.Pair(name))

  private def pairs(rows: Chunk[(String, String)]): Json =
    val encoded = rows
      .sortBy((name, _) => name)
      .map: (name, value) =>
        Json.Obj("name" -> Json.Str(name), "value" -> Json.Str(value))
    Json.Arr(encoded*)
end McpServer

enum McpDocumentError:
  case Malformed
  case NotObject
  case ServersMissing
  case NameEmpty
  case ServerNotObject(name: String)
  case CommandMissing(name: String)
  case UrlMissing(name: String)
  case Transport(name: String, kind: String)
  case Args(name: String)
  case Pair(name: String)
  case Unreadable
