package terret.grok

import zio.Chunk

enum ShellEffect:
  case ReadOnly
  case Writes(paths: Chunk[String])
  case Opaque

object ShellWrite:
  def effect(command: String): ShellEffect =
    val trimmed = stripHeredocs(command.trim)
    if trimmed.isEmpty then ShellEffect.Opaque
    else
      splitSegments(trimmed) match
        case None        => ShellEffect.Opaque
        case Some(parts) =>
          parts.foldLeft(ShellEffect.ReadOnly: ShellEffect)((acc, part) => combine(acc, segment(part)))

  private def combine(left: ShellEffect, right: ShellEffect): ShellEffect =
    (left, right) match
      case (ShellEffect.Opaque, _) | (_, ShellEffect.Opaque)       => ShellEffect.Opaque
      case (ShellEffect.ReadOnly, other)                           => other
      case (other, ShellEffect.ReadOnly)                           => other
      case (ShellEffect.Writes(first), ShellEffect.Writes(second)) => ShellEffect.Writes(first ++ second)

  private def segment(raw: String): ShellEffect =
    tokenize(raw) match
      case None        => ShellEffect.Opaque
      case Some(words) =>
        (redirects(words), commandTargets(words)) match
          case (None, _) | (_, None)                   => ShellEffect.Opaque
          case (Some(fromRedirect), Some(fromCommand)) =>
            val paths = fromRedirect ++ fromCommand
            if paths.nonEmpty then ShellEffect.Writes(Chunk.fromIterable(paths))
            else ShellEffect.ReadOnly

  private def stripHeredocs(raw: String): String =
    rewrite(raw, 0, None, "").getOrElse(raw)

  private def rewrite(raw: String, index: Int, quote: Option[Char], out: String): Option[String] =
    if index >= raw.length then Some(out)
    else
      val ch = raw.charAt(index)
      quote match
        case Some(mark) =>
          val next = if ch == mark then None else quote
          rewrite(raw, index + 1, next, out + ch)
        case None if ch == '<' && heredocAt(raw, index) =>
          closeHeredoc(raw, index) match
            case None                 => None
            case Some((kept, resume)) => rewrite(raw, resume, None, out + kept)
        case None =>
          val next = if ch == '\'' || ch == '"' then Some(ch) else None
          rewrite(raw, index + 1, next, out + ch)

  private def heredocAt(raw: String, index: Int): Boolean =
    index + 1 < raw.length && raw.charAt(index + 1) == '<' &&
      (index + 2 >= raw.length || raw.charAt(index + 2) != '<')

  private def closeHeredoc(raw: String, index: Int): Option[(String, Int)] =
    val dashed  = index + 2 < raw.length && raw.charAt(index + 2) == '-'
    val tokenAt = if dashed then index + 3 else index + 2
    skipInline(raw, tokenAt).flatMap { start =>
      readDelim(raw, start).flatMap { (word, after) =>
        if word.isEmpty then None
        else
          findEnd(raw, after, word, dashed).map { resume =>
            val lineEnd = raw.indexOf('\n', after)
            val keptEnd = if lineEnd < 0 then raw.length else lineEnd
            (raw.substring(after, keptEnd), resume)
          }
      }
    }

  private def skipInline(raw: String, index: Int): Option[Int] =
    if index >= raw.length then None
    else
      raw.charAt(index) match
        case ' ' | '\t'  => skipInline(raw, index + 1)
        case '\n' | '\r' => None
        case _           => Some(index)

  private def readDelim(raw: String, index: Int): Option[(String, Int)] =
    if index >= raw.length then None
    else
      val ch = raw.charAt(index)
      if ch == '\'' || ch == '"' then
        val end = raw.indexOf(ch, index + 1)
        if end < 0 then None else Some((raw.substring(index + 1, end), end + 1))
      else
        val end = inlineEnd(raw, index)
        Some((raw.substring(index, end), end))

  private def inlineEnd(raw: String, index: Int): Int =
    if index >= raw.length then index
    else
      raw.charAt(index) match
        case ' ' | '\t' | '\n' | '\r' => index
        case _                        => inlineEnd(raw, index + 1)

  private def findEnd(raw: String, after: Int, delim: String, dashed: Boolean): Option[Int] =
    val body = raw.indexOf('\n', after)
    if body < 0 then None else matchLine(raw, body + 1, delim, dashed)

  private def matchLine(raw: String, line: Int, delim: String, dashed: Boolean): Option[Int] =
    if line > raw.length then None
    else
      val next      = raw.indexOf('\n', line)
      val end       = if next < 0 then raw.length else next
      val text      = raw.substring(line, end)
      val candidate = if dashed then text.dropWhile(_ == '\t') else text
      if candidate == delim then Some(if next < 0 then end else next + 1)
      else if next < 0 then None
      else matchLine(raw, next + 1, delim, dashed)

  private def splitSegments(raw: String): Option[List[String]] =
    def loop(index: Int, quote: Option[Char], start: Int, acc: List[String]): Option[List[String]] =
      if index >= raw.length then
        quote match
          case Some(_) => None
          case None    =>
            val piece = raw.substring(start).trim
            val all   = if piece.isEmpty then acc else piece :: acc
            Some(all.reverse)
      else
        val ch = raw.charAt(index)
        quote match
          case Some(mark) =>
            val next = if ch == mark then None else quote
            loop(index + 1, next, start, acc)
          case None =>
            ch match
              case '\'' | '"' => loop(index + 1, Some(ch), start, acc)
              case '|'        =>
                val skip  = if index + 1 < raw.length && raw.charAt(index + 1) == '|' then 2 else 1
                val piece = raw.substring(start, index).trim
                val next  = if piece.isEmpty then acc else piece :: acc
                loop(index + skip, None, index + skip, next)
              case '&' if index + 1 < raw.length && raw.charAt(index + 1) == '&' =>
                val piece = raw.substring(start, index).trim
                val next  = if piece.isEmpty then acc else piece :: acc
                loop(index + 2, None, index + 2, next)
              case ';' =>
                val piece = raw.substring(start, index).trim
                val next  = if piece.isEmpty then acc else piece :: acc
                loop(index + 1, None, index + 1, next)
              case _ => loop(index + 1, None, start, acc)
    loop(0, None, 0, Nil)

  private def tokenize(raw: String): Option[List[String]] =
    def loop(index: Int, quote: Option[Char], buf: String, acc: List[String]): Option[List[String]] =
      if index == raw.length then
        quote match
          case Some(_) => None
          case None    =>
            val words = if buf.isEmpty then acc else buf :: acc
            Some(words.reverse)
      else
        val ch = raw.charAt(index)
        quote match
          case Some(mark) =>
            if ch == mark then loop(index + 1, None, buf, acc)
            else loop(index + 1, quote, buf + ch, acc)
          case None =>
            ch match
              case ' ' | '\t' | '\n' | '\r' =>
                val next = if buf.isEmpty then acc else buf :: acc
                loop(index + 1, None, "", next)
              case '\'' | '"' => loop(index + 1, Some(ch), buf, acc)
              case _          => loop(index + 1, None, buf + ch, acc)
    loop(0, None, "", Nil)

  private def redirects(words: List[String]): Option[List[String]] =
    def loop(rest: List[String], acc: List[String]): Option[List[String]] =
      rest match
        case Nil                                                => Some(acc.reverse)
        case word :: tail if fdDup.matches(word)                => loop(tail, acc)
        case word :: target :: tail if redirectOp.matches(word) =>
          note(target) match
            case None       => None
            case Some(path) => loop(tail, path.fold(acc)(_ :: acc))
        case word :: tail =>
          attached(word) match
            case Some(target) =>
              note(target) match
                case None       => None
                case Some(path) => loop(tail, path.fold(acc)(_ :: acc))
            case None => loop(tail, acc)
    loop(words, Nil)

  private def note(target: String): Option[Option[String]] =
    if target.isEmpty || redirectOp.matches(target) then None
    else if fdDup.matches(target) || target == "/dev/null" || fdTarget.matches(target) then Some(None)
    else Some(Some(target))

  private def attached(word: String): Option[String] =
    val marker = word.indexOf('>')
    if marker < 0 then None
    else
      val op     = word.substring(marker)
      val prefix = word.substring(0, marker)
      if redirectOp.matches(op.takeWhile(ch => ch == '>' || ch == '|')) && (prefix.isEmpty || prefix.forall(
          _.isDigit
        ) || prefix == "&")
      then
        val operator = op.takeWhile(ch => ch == '>' || ch == '|')
        val target   = op.substring(operator.length)
        if target.isEmpty then None else Some(target)
      else None

  private def commandTargets(words: List[String]): Option[List[String]] =
    dropWrappers(words) match
      case Nil         => Some(Nil)
      case cmd :: rest =>
        val name = commandName(cmd)
        name match
          case "tee"                             => Some(operands(rest))
          case "cp" | "mv" | "install" | "rsync" =>
            explicitDest(rest) match
              case Some(dest) => Some(List(dest))
              case None       =>
                lastOperand(operands(skipValueFlags(rest, valueFlags(name)))) match
                  case None if rest.isEmpty => Some(Nil)
                  case None                 => None
                  case Some(dest)           => Some(List(dest))
          case "dd"                                              => Some(rest.flatMap(ofTarget))
          case "sed" | "perl" if editsInPlace(name, cmd :: rest) =>
            operands(rest) match
              case Nil   => None
              case files => Some(files)
          case _ => Some(Nil)

  private def explicitDest(rest: List[String]): Option[String] =
    rest match
      case Nil                                                                                    => None
      case "-t" :: dest :: _                                                                      => Some(dest)
      case "--target-directory" :: dest :: _                                                      => Some(dest)
      case word :: _ if word.startsWith("--target-directory=") && word.length > targetFlag.length =>
        Some(word.substring(targetFlag.length))
      case _ :: tail => explicitDest(tail)

  private def ofTarget(word: String): Option[String] =
    if word.startsWith("of=") && word.length > 3 then Some(word.substring(3)) else None

  private def lastOperand(paths: List[String]): Option[String] =
    paths match
      case Nil         => None
      case path :: Nil => Some(path)
      case _ :: tail   => lastOperand(tail)

  private def operands(rest: List[String]): List[String] =
    def loop(left: List[String], acc: List[String]): List[String] =
      left match
        case Nil                          => acc.reverse
        case "--" :: tail                 => acc.reverse ++ tail
        case word :: tail if isFlag(word) => loop(tail, acc)
        case word :: tail                 => loop(tail, word :: acc)
    loop(rest, Nil)

  private def isFlag(word: String): Boolean =
    word.startsWith("-") && word != "-"

  private def skipValueFlags(rest: List[String], takes: Set[String]): List[String] =
    rest match
      case Nil                                  => Nil
      case "--" :: tail                         => tail
      case word :: tail if takes.contains(word) => skipValueFlags(tail.drop(1), takes)
      case word :: tail if isFlag(word)         => skipValueFlags(tail, takes)
      case other                                => other

  private def valueFlags(name: String): Set[String] =
    name match
      case "cp" | "mv" => Set("-t", "-S", "--suffix")
      case "install"   => Set("-m", "-o", "-g", "-S", "--mode", "--owner", "--group", "--suffix")
      case "rsync"     => Set("-e", "--rsh", "--chmod", "--chown", "--include", "--exclude")
      case _           => Set.empty

  private def editsInPlace(name: String, words: List[String]): Boolean =
    words.exists { word =>
      word == "-i" || word.startsWith("-i.") || word.startsWith("--in-place") ||
      (name == "perl" && word.startsWith("-") && !word.startsWith("--") && word.drop(1).forall(_.isLetter) && word
        .contains('i'))
    }

  private def dropWrappers(words: List[String]): List[String] =
    words match
      case word :: rest if assignment(word)                     => dropWrappers(rest)
      case word :: rest if wrappers.contains(commandName(word)) =>
        dropWrappers(skipWrapper(commandName(word), rest))
      case other => other

  private def skipWrapper(name: String, rest: List[String]): List[String] =
    name match
      case "sudo"   => skipValueFlags(rest, Set("-u", "-g", "-C", "-p", "-h", "-D", "-R", "-T"))
      case "env"    => skipEnv(rest)
      case "stdbuf" => skipValueFlags(rest, Set("-i", "-o", "-e"))
      case "nice"   => skipValueFlags(rest, Set("-n", "--adjustment"))
      case _        => rest

  private def skipEnv(rest: List[String]): List[String] =
    rest match
      case word :: tail if assignment(word)            => skipEnv(tail)
      case "-u" :: _ :: tail                           => skipEnv(tail)
      case word :: tail if word.startsWith("--unset=") => skipEnv(tail)
      case word :: tail if isFlag(word)                => skipEnv(tail)
      case other                                       => other

  private def assignment(word: String): Boolean =
    val eq = word.indexOf('=')
    eq > 0 && word.substring(0, eq).forall(ch => ch.isLetterOrDigit || ch == '_')

  private def commandName(word: String): String =
    val slash = word.lastIndexOf('/')
    if slash < 0 || slash == word.length - 1 then word else word.substring(slash + 1)

  private val wrappers   = Set("sudo", "command", "env", "nice", "nohup", "time", "stdbuf")
  private val fdDup      = """^(?:\d*>&?\d+|>&\d+|\d*<&\d+)$""".r
  private val fdTarget   = """^&\d+$""".r
  private val redirectOp = """^(?:\d*|&)?>>?\|?$""".r
  private val targetFlag = "--target-directory="
end ShellWrite
