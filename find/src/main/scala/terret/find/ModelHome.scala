package terret.find

import zio.{Chunk, IO, ZIO, ZLayer}

import java.nio.file.{Files, InvalidPathException, Path}

/** The two files a bge-small session needs. */
final case class ModelFiles(onnx: Path, tokenizer: Path)

/** Locates the bge-small files. It does not download them.
  *
  * `TERRET_BGE_HOME` wins when it is set to a non-blank path. Otherwise the files live under
  * `~/.cache/terret/bge-small-en-v1.5/`.
  */
trait ModelHome:
  def locations: IO[FindError, ModelFiles]
  def files: IO[FindError, ModelFiles]

object ModelHome:
  val Env: String           = "TERRET_BGE_HOME"
  val OnnxName: String      = "model_quantized.onnx"
  val TokenizerName: String = "tokenizer.json"
  val CacheFolder: String   = "bge-small-en-v1.5"

  /** The directory that holds the two files. A blank override is treated as unset. */
  def directory(overrideHome: Option[String], userHome: Option[Path]): Either[FindError, Path] =
    overrideHome.map(_.trim).filter(_.nonEmpty) match
      case Some(dir) => pathOf(dir)
      case None      =>
        userHome match
          case Some(home) => Right(home.resolve(".cache").resolve("terret").resolve(CacheFolder))
          case None       => Left(FindError.HomeUnavailable)

  def filesIn(dir: Path): ModelFiles =
    ModelFiles(dir.resolve(OnnxName), dir.resolve(TokenizerName))

  /** Paths that `present` rejects. Order is the ONNX file, then the tokenizer. */
  def missing(files: ModelFiles, present: Path => Boolean): Chunk[Path] =
    Chunk(files.onnx, files.tokenizer).filterNot(present)

  def requirePresent(files: ModelFiles, present: Path => Boolean): Either[FindError, ModelFiles] =
    val absent = missing(files, present)
    if absent.isEmpty then Right(files) else Left(FindError.ModelMissing(absent))

  /** Reads the environment when `locations` or `files` runs. A missing file is `ModelMissing`, not a download. */
  def live: ZLayer[Any, Nothing, ModelHome] =
    ZLayer.succeed(Live)

  private object Live extends ModelHome:
    def locations: IO[FindError, ModelFiles] =
      ZIO
        .attemptBlocking {
          val overrideHome = Option(System.getenv(Env))
          val userHome     = Option(System.getProperty("user.home")).filter(_.nonEmpty).flatMap(pathOf(_).toOption)
          directory(overrideHome, userHome).map(filesIn)
        }
        .mapError(_ => FindError.HomeUnavailable)
        .flatMap(ZIO.fromEither)

    def files: IO[FindError, ModelFiles] =
      locations.flatMap: located =>
        ZIO
          .attemptBlocking(requirePresent(located, Files.exists(_)))
          .mapError(thrown => FindError.Embedding(EmbeddingProblem.Runtime(detail(thrown))))
          .flatMap(ZIO.fromEither)
  end Live

  private[find] def detail(thrown: Throwable): String =
    Option(thrown.getMessage).getOrElse(thrown.getClass.getName)

  private def pathOf(raw: String): Either[FindError, Path] =
    try Right(Path.of(raw))
    catch case _: InvalidPathException => Left(FindError.HomeUnavailable)
end ModelHome
