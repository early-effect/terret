package terret.docs

import zio.test.*
import zio.{ConfigProvider, ZIO}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object SiteSpec extends ZIOSpecDefault:
  def spec = suite("site")(
    test("the index tells why, how to run, and the ship coordinate") {
      ZIO.scoped {
        ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("terret-site")))(remove).flatMap { dir =>
          val props = Map(
            "specular.meta.name"         -> "terret",
            "specular.meta.organization" -> "rocks.earlyeffect",
            "specular.meta.version"      -> "0.0.1",
            "specular.meta.scalaVersion" -> "3",
            "specular.meta.title"        -> "Terret",
            "specular.meta.description"  -> "Hosts talk to coding harnesses.",
            "specular.site.dir"          -> dir.toString,
          )
          BuildSite.build
            .withConfigProvider(ConfigProvider.fromMap(props))
            .flatMap { _ =>
              ZIO.attempt {
                val index      = Files.readString(dir.resolve("index.html"))
                val jobs       = Files.readString(dir.resolve("jobs.html"))
                val attach     = Files.readString(dir.resolve("attach.html"))
                val events     = Files.readString(dir.resolve("events.html"))
                val security   = Files.readString(dir.resolve("security.html"))
                val operations = Files.readString(dir.resolve("operations.html"))
                val config     = Files.readString(dir.resolve("config.html"))
                (index, jobs, attach, events, security, operations, config)
              }
            }
            .map { (index, jobs, attach, events, security, operations, config) =>
              assertTrue(
                index.contains("the ring a rein passes through"),
                index.contains("a guide on a loom"),
                index.contains("It is not the host"),
                index.contains("http://127.0.0.1:8080/mcp"),
                index.contains("rocks.earlyeffect::terret:0.0.1"),
                index.contains("rocks.earlyeffect::terret-app:0.0.1"),
                index.contains("./histos run app"),
                index.contains("latest stable"),
                index.contains("images/logo.png"),
                !index.contains("unreleased"),
                jobs.contains("open"),
                jobs.contains("run"),
                jobs.contains("resume"),
                jobs.contains("export"),
                jobs.contains("cancel"),
                jobs.contains("status"),
                jobs.contains("find"),
                jobs.contains("call"),
                attach.contains("x-terret-key"),
                events.contains("session.milestone"),
                events.contains("session.activity"),
                events.contains("[redacted]"),
                security.contains("x-terret-key"),
                security.contains("Cf-Access-Jwt-Assertion"),
                operations.contains("scripts/terret-restart"),
                operations.contains("TERRET_SUPERVISOR"),
                operations.contains("TERRET_LAUNCHD_LABELS"),
                operations.contains("https://www.earlyeffect.rocks/terret/"),
                config.contains("key.file"),
                config.contains("TERRET_KEY_SERVICE"),
                config.contains("TERRET_KEY_FILE"),
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
