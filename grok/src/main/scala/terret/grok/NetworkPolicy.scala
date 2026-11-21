package terret.grok

final case class NetworkPolicy(hosts: Set[String]):
  def allows(host: String): Boolean =
    hosts.contains(host.toLowerCase)

object NetworkPolicy:
  val empty: NetworkPolicy = NetworkPolicy(Set.empty)

  def parse(raw: String): Either[NetworkGrantError, NetworkPolicy] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Right(empty)
    else
      trimmed
        .split(",", -1)
        .toList
        .foldLeft[Either[NetworkGrantError, Set[String]]](Right(Set.empty)): (acc, part) =>
          acc.flatMap: hosts =>
            val token = part.trim
            host(token).flatMap: name =>
              if hosts.contains(name) then Left(NetworkGrantError.Duplicate(name))
              else Right(hosts + name)
        .map(NetworkPolicy(_))

  private def host(raw: String): Either[NetworkGrantError, String] =
    if raw.isEmpty then Left(NetworkGrantError.BadToken(raw))
    else
      val folded = raw.toLowerCase
      if ipv4(folded) || dns(folded) then Right(folded)
      else Left(NetworkGrantError.BadToken(raw))

  private def ipv4(raw: String): Boolean =
    val parts = raw.split("\\.", -1)
    parts.length == 4 && parts.forall(octet)

  private def octet(raw: String): Boolean =
    raw.length match
      case 1 => digit(raw.charAt(0))
      case 2 => raw.forall(digit) && raw.charAt(0) != '0'
      case 3 => raw.forall(digit) && raw.charAt(0) != '0' && value(raw) <= 255
      case _ => false

  private def value(raw: String): Int =
    raw.foldLeft(0)((acc, char) => acc * 10 + (char - '0'))

  private def dns(raw: String): Boolean =
    raw.length <= 253 && {
      val labels = raw.split("\\.", -1)
      labels.nonEmpty && labels.forall(label) && !labels.forall(digits)
    }

  private def label(raw: String): Boolean =
    raw.length >= 1 && raw.length <= 63 && head(raw.charAt(0)) && head(raw.charAt(raw.length - 1)) && raw.forall(body)

  private def digits(raw: String): Boolean =
    raw.nonEmpty && raw.forall(digit)

  private def head(char: Char): Boolean =
    char.isLetterOrDigit

  private def body(char: Char): Boolean =
    char.isLetterOrDigit || char == '-'

  private def digit(char: Char): Boolean =
    char.isDigit
end NetworkPolicy

enum NetworkGrantError:
  case BadToken(value: String)
  case Duplicate(host: String)

  def render: String = this match
    case BadToken(value) => s"'$value' is not a network host"
    case Duplicate(host) => s"$host is granted twice"
end NetworkGrantError
