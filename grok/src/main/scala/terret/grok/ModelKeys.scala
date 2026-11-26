package terret.grok

import terret.model.ModelKey
import zio.Chunk

/** Keys of the `[model.<name>]` tables. Nested tables and `[models]` are not names. */
object ModelKeys:
  enum Error:
    case Malformed(line: Int)

  def parse(text: String): Either[Error, Chunk[ModelKey]] =
    val lines = text.stripPrefix("\uFEFF").replace("\r\n", "\n").replace("\r", "\n").split("\n", -1)
    lines.zipWithIndex
      .foldLeft[Either[Error, Set[String]]](Right(Set.empty)) { (acc, row) =>
        val (line, index) = row
        acc.flatMap { names =>
          readHeader(line) match
            case Read.Ignore     => Right(names)
            case Read.Name(name) => Right(names + name)
            case Read.Bad        => Left(Error.Malformed(index + 1))
        }
      }
      .map(names => Chunk.fromIterable(names.toList.sorted).map(ModelKey(_)))

  private enum Read:
    case Ignore
    case Name(name: String)
    case Bad

  private def readHeader(raw: String): Read =
    val text = uncomment(raw).trim
    if text.isEmpty || !text.startsWith("[") then Read.Ignore
    else if text.startsWith("[[") && text.endsWith("]]") then
      tableKeys(text.substring(2, text.length - 2)) match
        case None    => Read.Bad
        case Some(_) => Read.Ignore
    else if text.endsWith("]") then
      tableKeys(text.substring(1, text.length - 1)) match
        case Some(List("model", name)) if name.nonEmpty => Read.Name(name)
        case Some(_)                                    => Read.Ignore
        case None                                       => Read.Bad
    else Read.Bad

  private def uncomment(text: String): String =
    def loop(i: Int, quoted: Boolean, quote: Char): Int =
      if i >= text.length then text.length
      else
        val ch = text.charAt(i)
        if quoted then
          if ch == quote then loop(i + 1, false, quote)
          else if ch == '\\' && quote == '"' && i + 1 < text.length then loop(i + 2, true, quote)
          else loop(i + 1, true, quote)
        else if ch == '#' then i
        else if ch == '"' || ch == '\'' then loop(i + 1, true, ch)
        else loop(i + 1, false, quote)
    text.substring(0, loop(0, false, '"'))

  private def tableKeys(inner: String): Option[List[String]] =
    def loop(i: Int, acc: List[String]): Option[List[String]] =
      val start = skip(inner, i)
      if start >= inner.length then if acc.isEmpty then None else Some(acc.reverse)
      else
        readKey(inner, start) match
          case None            => None
          case Some(key, next) =>
            val after = skip(inner, next)
            if after >= inner.length then Some((key :: acc).reverse)
            else if inner.charAt(after) == '.' then loop(after + 1, key :: acc)
            else None
    loop(0, Nil)

  private def readKey(text: String, i: Int): Option[(String, Int)] =
    if i >= text.length then None
    else
      text.charAt(i) match
        case '"'            => readQuoted(text, i + 1, '"')
        case '\''           => readQuoted(text, i + 1, '\'')
        case ch if bare(ch) =>
          val end = bareEnd(text, i)
          Some((text.substring(i, end), end))
        case _ => None

  private def readQuoted(text: String, i: Int, mark: Char): Option[(String, Int)] =
    def loop(j: Int, acc: String): Option[(String, Int)] =
      if j >= text.length then None
      else
        val ch = text.charAt(j)
        if ch == mark then Some((acc, j + 1))
        else if mark == '"' && ch == '\\' then
          if j + 1 >= text.length then None
          else
            escaped(text.charAt(j + 1)) match
              case Some(decoded) => loop(j + 2, acc + decoded)
              case None          => None
        else loop(j + 1, acc + ch)
    loop(i, "")

  private def escaped(ch: Char): Option[String] =
    ch match
      case '\\' => Some("\\")
      case '"'  => Some("\"")
      case 'n'  => Some("\n")
      case 't'  => Some("\t")
      case 'r'  => Some("\r")
      case _    => None

  private def bare(ch: Char): Boolean =
    val code = ch.toInt
    (code >= 'A' && code <= 'Z') || (code >= 'a' && code <= 'z') || (code >= '0' && code <= '9') || ch == '_' ||
    ch == '-'

  private def bareEnd(text: String, i: Int): Int =
    if i >= text.length || !bare(text.charAt(i)) then i else bareEnd(text, i + 1)

  private def skip(text: String, i: Int): Int =
    if i >= text.length || !text.charAt(i).isWhitespace then i else skip(text, i + 1)
end ModelKeys
