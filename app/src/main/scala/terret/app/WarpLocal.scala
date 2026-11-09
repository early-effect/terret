package terret.app

import zio.json.ast.Json

/** Warp's local agent reads `.warp/.mcp.json`. There is no Warp.local library. */
object WarpLocal:
  val File: String = ".warp/.mcp.json"
  val Host: String = "127.0.0.1"
  val Port: Int    = 8080
  val Path: String = "/mcp"

  /** A wildcard bind is not an address a local client can open. */
  def url(host: String, port: Int): String =
    val client = if host == "0.0.0.0" || host == "::" then Host else host
    val shown  = if client.contains(':') then s"[$client]" else client
    s"http://$shown:$port$Path"

  def http(url: String): Json =
    Json.Obj(
      "mcpServers" -> Json.Obj(
        "terret" -> Json.Obj("url" -> Json.Str(url))
      )
    )

  def http(listen: Listen.Http): Json =
    http(url(listen.host, listen.port))
end WarpLocal
