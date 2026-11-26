package terret.grok

import terret.model.{CatalogError, ModelKey}
import zio.{Chunk, ZIO}
import zio.test.*

import java.nio.file.{Files, Path}

object GrokModelsSpec extends ZIOSpecDefault:
  def spec = suite("Grok models")(
    test("a model table key is a name, and nested tables are not") {
      val text =
        """|[model.grok-4]
           |name = "unused"
           |# [model.commented]
           |[model."grok-4.7"]
           |[model."grok-4.7"]
           |[model."sonnet-5-bedrock".extra_headers]
           |[[model."vertigo-qwen".reasoning_efforts]]
           |[models]
           |default = "grok-4.7"
           |[other.table]
           |""".stripMargin
      assertTrue(
        ModelKeys.parse(text) == Right(Chunk(ModelKey("grok-4"), ModelKey("grok-4.7")))
      )
    },
    test("a broken model header fails on that line") {
      val first = "[model.\"unterminated]\n[model.ok]\n"
      val later = "[model.ok]\n[model.\"no]\n"
      assertTrue(
        ModelKeys.parse(first) == Left(ModelKeys.Error.Malformed(1)),
        ModelKeys.parse(later) == Left(ModelKeys.Error.Malformed(2)),
      )
    },
    test("known reads the file, and a missing file is unreadable") {
      file(
        "present",
        """|[model."grok-4.7"]
           |[model.vertigo-qwen]
           |[models]
           |default = "grok-4.7"
           |""".stripMargin,
      ).flatMap { path =>
        val missing = path.resolveSibling("absent.toml")
        for
          keys <- GrokModels.read(path).known
          gone <- GrokModels.read(missing).known.either
        yield assertTrue(
          keys == Chunk(ModelKey("grok-4.7"), ModelKey("vertigo-qwen")),
          gone == Left(CatalogError.Unreadable(missing.toString)),
        )
      }
    },
    test("a malformed file names the line") {
      file("broken", "[model.\"unterminated]\n").flatMap { path =>
        GrokModels.read(path).known.either.map { gone =>
          assertTrue(gone == Left(CatalogError.Malformed(path.toString, 1)))
        }
      }
    },
    test("an override path is kept, an empty path is rejected, and absent is the default file") {
      assertTrue(
        GrokModels.resolve(Some("/var/terret/grok.toml")) == Right(Path.of("/var/terret/grok.toml")),
        GrokModels.resolve(Some("")) == Left(GrokModelsPathError.Empty),
        GrokModels.resolve(None) == Right(GrokModels.defaultPath),
      )
    },
  )

  private def file(name: String, text: String) =
    val dir = Path.of("target", "model-keys", name)
    ZIO.acquireRelease(
      ZIO.attemptBlocking {
        Files.createDirectories(dir)
        val path = dir.resolve("config.toml")
        Files.writeString(path, text)
        path
      }.orDie
    )(path =>
      ZIO.attemptBlocking {
        Files.deleteIfExists(path)
        Files.deleteIfExists(path.getParent)
      }.ignore
    )
end GrokModelsSpec
