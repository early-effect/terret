package terret.grok

import terret.model.{CatalogError, ModelCatalog, ModelKey}
import zio.{Chunk, Config, IO, ZIO, ZLayer}

import java.nio.file.{Files, Path}

object GrokModels:
  def defaultPath: Path =
    Path.of(System.getProperty("user.home", ""), ".grok", "config.toml")

  /** `grok.config`, read under `terret` so the key is `terret.grok.config`. Absent is `defaultPath`. */
  val config: Config[Option[String]] =
    Config.string("config").nested("grok").optional

  def resolve(raw: Option[String]): Either[GrokModelsPathError, Path] =
    raw match
      case None                       => Right(defaultPath)
      case Some(text) if text.isEmpty => Left(GrokModelsPathError.Empty)
      case Some(text)                 => Right(Path.of(text))

  def layer(file: Path): ZLayer[Any, Nothing, ModelCatalog] =
    ZLayer.succeed(read(file))

  /** Reads `[model.<name>]` keys when `known` runs. A missing file fails that call. */
  def read(file: Path): ModelCatalog = new:
    def known: IO[CatalogError, Chunk[ModelKey]] =
      ZIO
        .attemptBlocking(if Files.isRegularFile(file) then Some(Files.readString(file)) else None)
        .mapError(_ => CatalogError.Unreadable(file.toString))
        .flatMap:
          case None       => ZIO.fail(CatalogError.Unreadable(file.toString))
          case Some(text) =>
            ZIO.fromEither(ModelKeys.parse(text)).mapError { case ModelKeys.Error.Malformed(line) =>
              CatalogError.Malformed(file.toString, line)
            }
end GrokModels

enum GrokModelsPathError:
  case Empty
