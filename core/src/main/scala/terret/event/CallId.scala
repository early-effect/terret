package terret.event

import scala.annotation.publicInBinary
import scala.quoted.*

opaque type CallId = String

object CallId:
  inline def apply(inline raw: String): CallId = ${ CallIdMacro.literal('raw) }

  def from(raw: String): Either[CallIdError, CallId] =
    if raw.isEmpty then Left(CallIdError.Empty)
    else Right(trusted(raw))

  extension (id: CallId) def value: String = id

  @publicInBinary private[terret] def trusted(raw: String): CallId = raw
end CallId

enum CallIdError:
  case Empty

  def message: String = this match
    case Empty => "call id is empty"
end CallIdError

private object CallIdMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[CallId] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("CallId(...) takes a literal. Use CallId.from at runtime.", raw)
      case Some(text) =>
        CallId.from(text) match
          case Left(error) => report.errorAndAbort(error.message, raw)
          case Right(_)   => '{ CallId.trusted(${ Expr(text) }) }
end CallIdMacro
