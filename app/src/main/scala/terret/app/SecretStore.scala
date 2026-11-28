package terret.app

import terret.mcp.EventsKey
import zio.{IO, ZIO}

object SecretStore:
  def load(source: KeySource): ZIO[Keychain, KeyError, EventsKey] =
    source match
      case KeySource.File(path)                 => KeyFile.load(path).flatMap(material)
      case KeySource.Keychain(service, account) =>
        ZIO.serviceWithZIO[Keychain](_.password(service, account)).flatMap(material)

  private def material(raw: String): IO[KeyError, EventsKey] =
    ZIO.fromOption(EventsKey.from(raw)).orElseFail(KeyError.Empty)
end SecretStore
