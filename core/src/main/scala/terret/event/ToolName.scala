package terret.event

import scala.annotation.publicInBinary
import scala.quoted.*

opaque type ToolName = String

object ToolName:
  inline def apply(inline raw: String): ToolName = ${ ToolNameMacro.literal('raw) }

  def from(raw: String): Either[ToolNameError, ToolName] =
    if raw.isEmpty then Left(ToolNameError.Empty)
    else Right(trusted(raw))

  extension (name: ToolName) def value: String = name

  @publicInBinary private[terret] def trusted(raw: String): ToolName = raw
end ToolName

enum ToolNameError:
  case Empty

  def message: String = this match
    case Empty => "tool name is empty"
end ToolNameError

private object ToolNameMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[ToolName] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("ToolName(...) takes a literal. Use ToolName.from at runtime.", raw)
      case Some(text) =>
        ToolName.from(text) match
          case Left(error) => report.errorAndAbort(error.message, raw)
          case Right(_)   => '{ ToolName.trusted(${ Expr(text) }) }
end ToolNameMacro
