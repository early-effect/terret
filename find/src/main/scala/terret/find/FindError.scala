package terret.find

import zio.Chunk

import java.nio.file.Path

/** Failures of finding a tool. None of these are exceptions. */
enum FindError:
  /** The query or the passage was empty or blank. */
  case EmptyQuery

  /** No model directory could be chosen. `TERRET_BGE_HOME` was unset or illegal, and so was `user.home`. */
  case HomeUnavailable

  /** The ONNX file or the tokenizer is not in the cache. The paths are the ones that were missing. */
  case ModelMissing(paths: Chunk[Path])

  /** Two vectors have different widths, so they are not in one space. */
  case DimMismatch(left: Int, right: Int)

  /** Two vectors were embedded by different models. */
  case ModelMismatch(left: String, right: String)

  /** A vector has no direction: it is empty or it is all zeroes. */
  case Degenerate

  /** `limit` was zero or negative. */
  case NonPositive(limit: Int)

  /** The index could not read or write. The detail is diagnostic. Callers do not branch on it. */
  case Storage(detail: String)

  /** A stored capability is not one this service knows. */
  case UnknownCapability(raw: String)

  /** A weight download failed. */
  case Fetch(url: String, problem: FetchProblem)

  /** The session could not produce a vector. The detail is diagnostic. Callers do not branch on it. */
  case Embedding(problem: EmbeddingProblem)
end FindError

enum FetchProblem:
  case Status(code: Int)
  case Unavailable(detail: String)

enum EmbeddingProblem:
  case Shape
  case Tokenizer(detail: String)
  case Runtime(detail: String)
