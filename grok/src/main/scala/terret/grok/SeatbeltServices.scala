package terret.grok

import zio.Chunk

/** Mach lookups, ipc, and system operations a confined command may use.
  *
  * `(deny default)` closes the rest, including the keychain. Each rule carries the reason it exists.
  * A rule cannot use a wildcard.
  */
object SeatbeltServices:
  def lines: Either[SandboxError, Chunk[String]] =
    mach.foldLeft[Either[SandboxError, Chunk[String]]](Right(Chunk.empty)) { (acc, item) =>
      acc.flatMap(done => accept(item, done))
    }.flatMap(done => fold(ipc ++ system, done))

  private def accept(item: Mach, done: Chunk[String]): Either[SandboxError, Chunk[String]] =
    if item.name.forall(nameChar) && item.name.nonEmpty then
      fold(Chunk(Rule(item.why, s"(allow mach-lookup (global-name \"${item.name}\"))")), done)
    else Left(SandboxError.Rejected)

  private def fold(rules: Chunk[Rule], done: Chunk[String]): Either[SandboxError, Chunk[String]] =
    rules.foldLeft[Either[SandboxError, Chunk[String]]](Right(done)) { (acc, rule) =>
      acc.flatMap { ready =>
        if legal(rule) then Right(ready ++ Chunk(s"; ${rule.why}", rule.text))
        else Left(SandboxError.Rejected)
      }
    }

  private def legal(rule: Rule): Boolean =
    rule.why.nonEmpty &&
      !rule.why.exists(control) &&
      rule.text.startsWith("(allow ") &&
      rule.text.endsWith(")") &&
      !rule.text.contains("*") &&
      !rule.text.exists(control) &&
      (!rule.text.startsWith("(allow mach-lookup") || rule.text.contains("(global-name \""))

  private def nameChar(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '.' || ch == '_' || ch == '@' || ch == '-'

  private def control(ch: Char): Boolean =
    ch < ' ' || ch == '\u007f'

  // A name is added only when a denial shows the confined command needs it. sh and git need none.
  private val mach: Chunk[Mach]   = Chunk.empty
  private val ipc: Chunk[Rule]    = Chunk.empty
  private val system: Chunk[Rule] = Chunk.empty

  private final case class Mach(name: String, why: String)

  private final case class Rule(why: String, text: String)
end SeatbeltServices
