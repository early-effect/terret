package terret.docs

import zio.test.*
import zio.{ConfigProvider, ZIO}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object SiteSpec extends ZIOSpecDefault:
  def spec = suite("site")(
    test("the index names the local HTTP endpoint") {
      ZIO.scoped {
        ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("terret-site")))(remove).flatMap { dir =>
          val props = Map(
            "specular.meta.name"         -> "terret",
            "specular.meta.organization" -> "rocks.earlyeffect",
            "specular.meta.version"      -> "unreleased",
            "specular.meta.scalaVersion" -> "3",
            "specular.meta.title"        -> "Terret",
            "specular.meta.description"  -> "Hosts talk to coding harnesses.",
            "specular.site.dir"          -> dir.toString,
          )
          BuildSite.build.withConfigProvider(ConfigProvider.fromMap(props)).flatMap { _ =>
            ZIO.attempt {
              val index = Files.readString(dir.resolve("index.html"))
              val jobs  = Files.readString(dir.resolve("jobs.html"))
              (index, jobs)
            }
          }.map { (index, jobs) =>
            assertTrue(
              index.contains("http://127.0.0.1:8080/mcp"),
              !index.contains("libraryDependencies"),
              !index.contains("unreleased"),
              jobs.contains("open"),
              jobs.contains("run"),
              jobs.contains("resume"),
              jobs.contains("export"),
              jobs.contains("cancel"),
            )
          }
        }
      }
    }
  )

  private def remove(path: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attempt(delete(path)).ignore

  private def delete(path: Path): Unit =
    if Files.isDirectory(path) then
      val kids = Files.list(path)
      try kids.iterator.asScala.foreach(delete)
      finally kids.close()
    Files.deleteIfExists(path)
end SiteSpec
