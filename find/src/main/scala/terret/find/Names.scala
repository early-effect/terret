package terret.find

import scala.quoted.*

/** Why a server or tool name was rejected. */
enum NameError:
  case Empty

/** The name of an MCP server in `.mcp.json` or in `terret.mcp`. */
opaque type ServerName = String

object ServerName:
  def from(raw: String): Either[NameError, ServerName] =
    val name = raw.trim
    if name.isEmpty then Left(NameError.Empty) else Right(name)

  /** A literal name. A runtime value goes through `from`. */
  inline def apply(inline raw: String): ServerName = ${ literal('raw) }

  extension (name: ServerName) def value: String = name

  private[find] def make(raw: String): ServerName = raw

  private def literal(raw: Expr[String])(using Quotes): Expr[ServerName] =
    import quotes.reflect.report
    raw.value match
      case None => report.errorAndAbort("ServerName(...) takes a literal; use ServerName.from")
      case Some(name) if name.trim.isEmpty => report.errorAndAbort("an empty name is not a server")
      case Some(name)                      => '{ make(${ Expr(name.trim) }) }
end ServerName

/** The name of one tool on a server. */
opaque type ToolId = String

object ToolId:
  def from(raw: String): Either[NameError, ToolId] =
    val name = raw.trim
    if name.isEmpty then Left(NameError.Empty) else Right(name)

  /** A literal name. A runtime value goes through `from`. */
  inline def apply(inline raw: String): ToolId = ${ literal('raw) }

  extension (name: ToolId) def value: String = name

  private[find] def make(raw: String): ToolId = raw

  private def literal(raw: Expr[String])(using Quotes): Expr[ToolId] =
    import quotes.reflect.report
    raw.value match
      case None                            => report.errorAndAbort("ToolId(...) takes a literal; use ToolId.from")
      case Some(name) if name.trim.isEmpty => report.errorAndAbort("an empty name is not a tool")
      case Some(name)                      => '{ make(${ Expr(name.trim) }) }
end ToolId
