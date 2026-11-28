package terret.app

import java.nio.file.Path

/** Where the one key is read from. The secret itself is not config. */
enum KeySource:
  case File(path: Path)
  case Keychain(service: String, account: String)

object KeySource:
  def parse(
      file: Option[String],
      service: Option[String],
      account: Option[String],
  ): Either[ProcessError, Option[KeySource]] =
    (present(file), present(service), present(account)) match
      case (None, None, None)            => Right(None)
      case (Some(raw), None, None)       => fileOf(raw)
      case (None, Some(name), Some(who)) => Right(Some(KeySource.Keychain(name, who)))
      case (None, _, _)                  => Left(ProcessError.KeyPair)
      case (Some(_), _, _)               => Left(ProcessError.KeyBoth)

  private def fileOf(raw: String): Either[ProcessError, Option[KeySource]] =
    val path = Path.of(raw)
    if path.isAbsolute then Right(Some(KeySource.File(path))) else Left(ProcessError.KeyPath)

  private def present(raw: Option[String]): Option[String] =
    raw.map(_.trim).filter(_.nonEmpty)
end KeySource

enum KeyError:
  case Missing
  case Empty
  case Exposed
  case Unreadable
  case Malformed
  case Unavailable

  def render: String = this match
    case Missing     => "keychain item was not found"
    case Empty       => "the configured key is empty"
    case Exposed     => "key file must be mode 600"
    case Unreadable  => "could not read the key file"
    case Malformed   => "key file must set TERRET_KEY once"
    case Unavailable => "could not read the key from the keychain"
end KeyError

object KeyGate:
  /** Stdio may omit the key. The pipe is the trust boundary, and `events/subscribe` has no headers, so it stays
    * forbidden. HTTP may not. A source that is set is still loaded for stdio, so an exposed file fails before the
    * pipe opens.
    */
  def ready(listen: Listen, source: Option[KeySource]): Either[ProcessError, Option[KeySource]] =
    (listen, source) match
      case (Listen.Stdio, chosen)      => Right(chosen)
      case (_: Listen.Http, None)      => Left(ProcessError.KeyMissing)
      case (_: Listen.Http, Some(src)) => Right(Some(src))
end KeyGate
