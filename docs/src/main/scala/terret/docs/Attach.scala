package terret.docs

import specular.*
import terret.app.{Listen, ProcessConfig, ProcessError, WarpLocal}
import zio.test.*
import zio.{ConfigProvider, ZIO}

/** HTTP is the attach path. Warp reads the local file. There is no Warp.local library. */
object Attach extends DocSpec:
  def doc = page("Attach")(
    md"""
The process listens on HTTP at `127.0.0.1`, port 8080, path `/mcp`. Warp's local agent reads
`.warp/.mcp.json` in the project or in the home directory. The file names that URL.

A bind on `0.0.0.0` or `::` is not an address a local client can open. The file uses
`127.0.0.1` instead. An IPv6 address is written in brackets.

Stdio is the other listen mode. The same jobs are on that pipe. HTTP is the attach path this
page documents.
""",
    exampleZIO {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map.empty))
    }.assert { config =>
      val url = config.listen match
        case http: Listen.Http => WarpLocal.url(http.host, http.port)
        case Listen.Stdio      => ""
      assertTrue(
        config == ProcessConfig.local,
        url == "http://127.0.0.1:8080/mcp",
        WarpLocal.File == ".warp/.mcp.json",
      )
    },
    exampleValue(WarpLocal.url("0.0.0.0", WarpLocal.Port)).assert { url =>
      assertTrue(url == "http://127.0.0.1:8080/mcp")
    },
    exampleValue(WarpLocal.url("::1", 9)).assert { url =>
      assertTrue(url == "http://[::1]:9/mcp")
    },
    exampleValue(WarpLocal.http(WarpLocal.url("0.0.0.0", WarpLocal.Port))).assert { json =>
      assertTrue(endpoint(json) == Some("http://127.0.0.1:8080/mcp"))
    },
  )

  private def endpoint(doc: zio.json.ast.Json): Option[String] =
    doc match
      case root: zio.json.ast.Json.Obj =>
        root.get("mcpServers") match
          case Some(servers: zio.json.ast.Json.Obj) =>
            servers.get("terret") match
              case Some(entry: zio.json.ast.Json.Obj) =>
                entry.get("url") match
                  case Some(zio.json.ast.Json.Str(url)) => Some(url)
                  case _                                 => None
              case _ => None
          case _ => None
      case _ => None
end Attach
