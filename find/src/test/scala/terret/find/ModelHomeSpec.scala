package terret.find

import zio.Chunk
import zio.test.*

import java.nio.file.Path

object ModelHomeSpec extends ZIOSpecDefault:
  private val home      = Path.of("/var/terret")
  private val onnx      = Path.of("/var/terret/model_quantized.onnx")
  private val tokenizer = Path.of("/var/terret/tokenizer.json")
  private val files     = ModelFiles(onnx, tokenizer)

  def spec = suite("model home")(
    test("TERRET_BGE_HOME wins, and a blank override uses the cache under the home directory") {
      assertTrue(
        ModelHome.directory(Some("/opt/bge"), Some(home)) == Right(Path.of("/opt/bge")),
        ModelHome.directory(Some("  "), Some(home)) == Right(
          home.resolve(".cache").resolve("terret").resolve("bge-small-en-v1.5")
        ),
        ModelHome.directory(None, Some(home)) == Right(
          home.resolve(".cache").resolve("terret").resolve("bge-small-en-v1.5")
        ),
        ModelHome.directory(None, None) == Left(FindError.HomeUnavailable),
        ModelHome.filesIn(Path.of("/opt/bge")) == ModelFiles(
          Path.of("/opt/bge/model_quantized.onnx"),
          Path.of("/opt/bge/tokenizer.json"),
        ),
      )
    },
    test("a missing file is named, and a present pair is accepted") {
      assertTrue(
        ModelHome.missing(files, _ => false) == Chunk(onnx, tokenizer),
        ModelHome.requirePresent(files, _ == tokenizer) == Left(FindError.ModelMissing(Chunk(onnx))),
        ModelHome.requirePresent(files, _ => true) == Right(files),
      )
    },
  )
end ModelHomeSpec
