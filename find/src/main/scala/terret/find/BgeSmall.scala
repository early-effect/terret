package terret.find

import zio.{Chunk, IO, Scope, Semaphore, ZIO, ZLayer}

import ai.djl.huggingface.tokenizers.HuggingFaceTokenizer
import ai.onnxruntime.{OnnxTensor, OnnxValue, OrtEnvironment, OrtSession}

import java.util.{HashMap, Map as JMap, Set as JSet}
import scala.jdk.OptionConverters.*

/** bge-small-en, in process, through ONNX Runtime.
  *
  * Queries are prefixed so they sit in the same space as passages. The first token is the vector, then it is
  * L2-normalized. `layer` reads a cache that is already present. `live` is the one graph that downloads.
  */
object BgeSmall:
  val Dims: Int           = 384
  val MaxTokens: Int      = 512
  val QueryPrefix: String = "Represent this sentence for searching relevant passages: "

  def queryText(text: String): String = s"$QueryPrefix$text"

  def passageText(text: String): String = text

  def layer: ZLayer[ModelHome, FindError, Embedder] =
    ZLayer.scoped(ZIO.serviceWithZIO[ModelHome](_.files).flatMap(open))

  def live: ZLayer[Any, FindError, Embedder] =
    ((ModelHome.live >+> ModelFetch.live) >>> ModelFetch.ensuring) >>> layer

  private def open(files: ModelFiles): ZIO[Scope, FindError, Embedder] =
    for
      gate    <- Semaphore.make(1)
      env     <- ZIO.succeed(OrtEnvironment.getEnvironment())
      options <- ZIO.acquireRelease(ZIO.attempt(new OrtSession.SessionOptions()).mapError(runtime))(close)
      session <- ZIO.acquireRelease(
        ZIO.attempt(env.createSession(files.onnx.toString, options)).mapError(runtime)
      )(close)
      tokenizer <- ZIO.acquireRelease(
        ZIO.attempt(HuggingFaceTokenizer.newInstance(files.tokenizer, tokenizerOptions)).mapError(tokenFailure)
      )(close)
      inputs  <- ZIO.attempt(names(session.getInputNames)).mapError(runtime)
      outputs <- ZIO.attempt(names(session.getOutputNames)).mapError(runtime)
      output  <- ZIO.fromEither(outputName(outputs))
    yield Loaded(env, tokenizer, session, inputs, output, gate)

  private def tokenizerOptions: JMap[String, String] =
    val options = HashMap[String, String]()
    options.put("truncation", "true")
    options.put("maxLength", MaxTokens.toString)
    options.put("addSpecialTokens", "true")
    options

  private def names(values: JSet[String]): Set[String] =
    values.toArray(Array.empty[String]).toSet

  private def outputName(outputs: Set[String]): Either[FindError, String] =
    if outputs.contains("last_hidden_state") then Right("last_hidden_state")
    else
      outputs.toList.sorted.headOption match
        case Some(name) => Right(name)
        case None       => Left(FindError.Embedding(EmbeddingProblem.Shape))

  private def runtime(thrown: Throwable): FindError =
    FindError.Embedding(EmbeddingProblem.Runtime(ModelHome.detail(thrown)))

  private def tokenFailure(thrown: Throwable): FindError =
    FindError.Embedding(EmbeddingProblem.Tokenizer(ModelHome.detail(thrown)))

  private def close(resource: AutoCloseable): IO[Nothing, Unit] =
    ZIO.attempt(resource.close()).ignore

  /** `OrtEnvironment` is process-global. Closing it drops every other session in the JVM, so this one stays open.
    * `OrtSession.run` is not safe concurrently. The permit covers the tensors, so a close cannot race a run.
    */
  private final class Loaded(
      env: OrtEnvironment,
      tokenizer: HuggingFaceTokenizer,
      session: OrtSession,
      inputs: Set[String],
      output: String,
      gate: Semaphore,
  ) extends Embedder:
    def embedQuery(text: String): IO[FindError, Embedding] =
      ZIO.fromEither(Embedder.nonBlank(text)).flatMap(value => embed(queryText(value)))

    def embedPassage(text: String): IO[FindError, Embedding] =
      ZIO.fromEither(Embedder.nonBlank(text)).flatMap(value => embed(passageText(value)))

    private def embed(text: String): IO[FindError, Embedding] =
      gate.withPermit:
        ZIO.scoped:
          for
            encoded <- ZIO.attempt(tokenizer.encode(text)).mapError(tokenFailure)
            ids     <- ZIO.fromEither(tokens(encoded.getIds))
            mask  = fill(encoded.getAttentionMask, ids.length, 1L)
            types = fill(encoded.getTypeIds, ids.length, 0L)
            idTensor   <- tensor(Array(ids))
            maskTensor <- tensor(Array(mask))
            typeTensor <- tensor(Array(types))
            result     <- ZIO.acquireRelease(
              ZIO.attempt(session.run(feed(idTensor, maskTensor, typeTensor))).mapError(runtime)
            )(close)
            vector     <- ZIO.attempt(read(result)).mapError(runtime).flatMap(ZIO.fromEither)
            normalized <- ZIO.fromEither(VectorMath.normalize(vector))
          yield Embedding(ModelId.bgeSmall, normalized)

    private def feed(ids: OnnxTensor, mask: OnnxTensor, types: OnnxTensor): JMap[String, OnnxTensor] =
      val batch = HashMap[String, OnnxTensor]()
      batch.put("input_ids", ids)
      batch.put("attention_mask", mask)
      if inputs.contains("token_type_ids") then batch.put("token_type_ids", types)
      batch

    private def tensor(data: Array[Array[Long]]): ZIO[Scope, FindError, OnnxTensor] =
      ZIO.acquireRelease(ZIO.attempt(OnnxTensor.createTensor(env, data)).mapError(runtime))(close)

    private def read(result: OrtSession.Result): Either[FindError, Chunk[Float]] =
      result.get(output).toScala match
        case Some(value) => cls(value)
        case None        => Left(FindError.Embedding(EmbeddingProblem.Shape))

    private def cls(value: OnnxValue): Either[FindError, Chunk[Float]] =
      value match
        case tensor: OnnxTensor =>
          tensor.getValue match
            case batch: Array[Array[Array[Float]]] => firstToken(batch)
            case _                                 => Left(FindError.Embedding(EmbeddingProblem.Shape))
        case _ => Left(FindError.Embedding(EmbeddingProblem.Shape))

    private def firstToken(batch: Array[Array[Array[Float]]]): Either[FindError, Chunk[Float]] =
      if batch.length != 1 then Left(FindError.Embedding(EmbeddingProblem.Shape))
      else
        val sequence = batch(0)
        if sequence.length == 0 then Left(FindError.Embedding(EmbeddingProblem.Shape))
        else
          val vector = sequence(0)
          if vector.length != Dims then Left(FindError.DimMismatch(vector.length, Dims))
          else Right(Chunk.fromArray(vector))

    private def tokens(ids: Array[Long]): Either[FindError, Array[Long]] =
      Option(ids).filter(_.nonEmpty).toRight(FindError.Embedding(EmbeddingProblem.Shape))

    private def fill(values: Array[Long], length: Int, pad: Long): Array[Long] =
      Option(values).filter(_.length == length).getOrElse(Array.fill(length)(pad))
  end Loaded
end BgeSmall
