package terret.find

import zio.{IO, Scope, ZIO, ZLayer}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.time.Duration as JDuration

/** Downloads whichever bge-small file is missing, then returns both paths.
  *
  * `BgeSmall.layer` does not require this service. Only `BgeSmall.live` does, and it runs one attempt per file.
  */
trait ModelFetch:
  def ensure: IO[FindError, ModelFiles]

object ModelFetch:
  private val OnnxUrl: String =
    "https://huggingface.co/Xenova/bge-small-en-v1.5/resolve/main/onnx/model_quantized.onnx"
  private val TokenizerUrl: String =
    "https://huggingface.co/Xenova/bge-small-en-v1.5/resolve/main/tokenizer.json"

  /** Fills the cache, then offers a home whose files are the ones just verified. */
  def ensuring: ZLayer[ModelHome & ModelFetch, FindError, ModelHome] =
    ZLayer.fromZIO(ZIO.serviceWithZIO[ModelFetch](_.ensure).map(Ready(_)))

  def live: ZLayer[ModelHome, Nothing, ModelFetch] =
    ZLayer.scoped:
      for
        home   <- ZIO.service[ModelHome]
        client <- ZIO.acquireRelease(ZIO.succeed(openClient))(client => ZIO.attempt(client.close()).ignore)
      yield Live(home, client)

  private def openClient: HttpClient =
    HttpClient
      .newBuilder()
      .followRedirects(HttpClient.Redirect.NORMAL)
      .connectTimeout(JDuration.ofSeconds(10))
      .build()

  private final class Ready(found: ModelFiles) extends ModelHome:
    def locations: IO[FindError, ModelFiles] = ZIO.succeed(found)
    def files: IO[FindError, ModelFiles]     = ZIO.succeed(found)

  private final class Live(home: ModelHome, client: HttpClient) extends ModelFetch:
    def ensure: IO[FindError, ModelFiles] =
      for
        located <- home.locations
        absent  <- ZIO.attemptBlocking(ModelHome.missing(located, Files.exists(_))).mapError(unavailable(OnnxUrl))
        _       <- ZIO.foreachDiscard(absent)(pull(client))
        ready   <- ZIO
          .attemptBlocking(ModelHome.requirePresent(located, present))
          .mapError(unavailable(OnnxUrl))
          .flatMap(ZIO.fromEither)
      yield ready

  private def present(path: Path): Boolean =
    Files.exists(path) && substantial(path)

  private def pull(client: HttpClient)(dest: Path): IO[FindError, Unit] =
    urlOf(dest) match
      case Some(url) => transfer(client, url, dest)
      case None      => ZIO.fail(FindError.Fetch(dest.toString, FetchProblem.Unavailable("no source for this path")))

  private def urlOf(dest: Path): Option[String] =
    Option(dest.getFileName).map(_.toString) match
      case Some(ModelHome.OnnxName)      => Some(OnnxUrl)
      case Some(ModelHome.TokenizerName) => Some(TokenizerUrl)
      case _                             => None

  private def transfer(client: HttpClient, url: String, dest: Path): IO[FindError, Unit] =
    ZIO.scoped:
      for
        tmp      <- stage(url, dest)
        response <- ZIO
          .attemptBlockingInterrupt(client.send(request(url), HttpResponse.BodyHandlers.ofFile(tmp)))
          .mapError(unavailable(url))
        _ <-
          if response.statusCode() == 200 then install(url, tmp, dest)
          else ZIO.fail(FindError.Fetch(url, FetchProblem.Status(response.statusCode())))
      yield ()

  private def stage(url: String, dest: Path): ZIO[Scope, FindError, Path] =
    ZIO.acquireRelease(
      ZIO
        .attemptBlocking {
          val parent = dest.getParent
          if parent != null then Files.createDirectories(parent)
          val dir = if parent != null then parent else Path.of(".")
          Files.createTempFile(dir, ".bge-", ".part")
        }
        .mapError(unavailable(url))
    )(path => ZIO.attemptBlocking(Files.deleteIfExists(path)).ignore)

  private def request(url: String): HttpRequest =
    HttpRequest
      .newBuilder(URI.create(url))
      .timeout(JDuration.ofMinutes(2))
      .header("User-Agent", "terret-find")
      .GET()
      .build()

  private def install(url: String, tmp: Path, dest: Path): IO[FindError, Unit] =
    ZIO
      .attemptBlockingInterrupt {
        try Files.move(tmp, dest, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch case _: AtomicMoveNotSupportedException => Files.move(tmp, dest, StandardCopyOption.REPLACE_EXISTING)
        substantial(dest)
      }
      .mapError(unavailable(url))
      .flatMap:
        case true  => ZIO.unit
        case false =>
          ZIO.attemptBlocking(Files.deleteIfExists(dest)).ignore *>
            ZIO.fail(FindError.Fetch(url, FetchProblem.Unavailable("response was not a model file")))

  /** A quantized model is megabytes. A tokenizer is a JSON document. Anything smaller is an error page. */
  private def substantial(path: Path): Boolean =
    val size = Files.size(path)
    Option(path.getFileName).map(_.toString) match
      case Some(ModelHome.OnnxName)      => size > 1024L * 1024L
      case Some(ModelHome.TokenizerName) => size > 100L
      case _                             => false

  private def unavailable(url: String)(thrown: Throwable): FindError =
    FindError.Fetch(url, FetchProblem.Unavailable(ModelHome.detail(thrown)))
end ModelFetch
