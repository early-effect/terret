package terret.session

import scala.annotation.publicInBinary
import scala.quoted.*

opaque type SessionId = String

object SessionId:
  inline def apply(inline raw: String): SessionId = ${ SessionIdMacro.literal('raw) }

  def from(raw: String): Either[SessionIdError, SessionId] =
    if raw.isEmpty then Left(SessionIdError.Empty)
    else Right(trusted(raw))

  extension (id: SessionId) def value: String = id

  @publicInBinary private[terret] def trusted(raw: String): SessionId = raw
end SessionId

enum SessionIdError:
  case Empty

  def message: String = this match
    case Empty => "session id is empty"
end SessionIdError

private object SessionIdMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[SessionId] =
    import quotes.reflect.report
    raw.value match
      case None       => report.errorAndAbort("SessionId(...) takes a literal. Use SessionId.from at runtime.", raw)
      case Some(text) =>
        SessionId.from(text) match
          case Left(error) => report.errorAndAbort(error.message, raw)
          case Right(_)   => '{ SessionId.trusted(${ Expr(text) }) }
end SessionIdMacro
