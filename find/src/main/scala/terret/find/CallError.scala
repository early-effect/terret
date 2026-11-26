package terret.find

/** Why a tool was not called. The first three are the guard. The rest are the call itself. */
enum CallError:
  /** The tool has no server. A seed is one of these. */
  case NotCallable

  /** `terret.mcp` does not allow this tool on this server. */
  case NotGranted(server: ServerName, tool: ToolId)

  /** The server is granted and `.mcp.json` does not define it. */
  case UnknownServer(server: ServerName)

  /** The tool name was empty. */
  case BadTool

  /** `arguments` was not a JSON object. */
  case BadArguments

  /** The server did not answer. The detail is diagnostic. Callers do not branch on it. */
  case Unavailable(detail: String)
end CallError
