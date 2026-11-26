import dev.earlyeffect.specular.{Site, SiteCi, Specular}
import histos.*
import histos.plugin.{BuildServices, NoteKey, NoteText, PluginId, Site as _, SitePublish}
import zio.*

object build extends HistosBuild:
  def graph =
    val ship       = Ship("terret")
    val library    = ship.next(0, 0, 1)
    val scala3     = scala"3.10.1"
    val zio        = dep"dev.zio::zio:2.1.27"
    val zioStreams = dep"dev.zio::zio-streams:2.1.27"
    val zioJson    = dep"dev.zio::zio-json:1.1.2"
    val zioTest    = dep"dev.zio::zio-test:2.1.27"
    val zioTestSbt = dep"dev.zio::zio-test-sbt:2.1.27"
    val core       = ship
      .parent(Jvm)(scala3)
      .in(path"core")
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(zio, zioStreams, zioJson)
      .testDependencies(zioTest, zioTestSbt)
    val grok = module("grok", Jvm)(scala3)
      .dependsOn(core)
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(zio, zioStreams, zioJson, dep"rocks.earlyeffect::heddle:0.16.0-a4fe367ee229-SNAPSHOT")
      .testDependencies(zioTest, zioTestSbt, dep"rocks.earlyeffect::slayer:0.3.0")
    val dsh = module("dsh", Jvm)(scala3)
      .dependsOn(core)
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(zio, zioStreams, zioJson, dep"rocks.earlyeffect::heddle:0.16.0-a4fe367ee229-SNAPSHOT")
      .testDependencies(zioTest, zioTestSbt)
    val find = module("find", Jvm)(scala3)
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(
        zio,
        zioStreams,
        dep"com.microsoft.onnxruntime:onnxruntime:1.32.0",
        dep"ai.djl.huggingface:tokenizers:0.40.0",
        dep"rocks.earlyeffect::saferis:0.21.0",
        dep"rocks.earlyeffect::saferis-sqlite-jdbc:0.21.0",
      )
      .testDependencies(zioTest, zioTestSbt, dep"rocks.earlyeffect::slayer:0.3.0")
    val mcp = module("mcp", Jvm)(scala3)
      .dependsOn(core, find)
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(zio, zioStreams, zioJson, dep"rocks.earlyeffect::heddle-mcp:0.16.0-a4fe367ee229-SNAPSHOT")
      .testDependencies(zioTest, zioTestSbt, dep"rocks.earlyeffect::slayer:0.3.0")
    val app = module("app", Jvm)(scala3)
      .dependsOn(mcp, grok, dsh, find)
      .main(MainClass("terret.app.Main"))
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(
        zio,
        zioStreams,
        zioJson,
        dep"rocks.earlyeffect::heddle:0.16.0-a4fe367ee229-SNAPSHOT",
        dep"rocks.earlyeffect::heddle-mcp:0.16.0-a4fe367ee229-SNAPSHOT",
      )
      .testDependencies(zioTest, zioTestSbt, dep"rocks.earlyeffect::slayer:0.3.0")
    val docs = module("docs", Jvm)(scala3)
      .dependsOn(app, dsh)
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(
        dep"rocks.earlyeffect::specular-core:0.21.4",
        dep"rocks.earlyeffect::specular-zio-test:0.21.4",
        dep"rocks.earlyeffect::specular-site:0.21.4",
        dep"rocks.earlyeffect::early-effect-docs-theme:0.21.4",
        dep"rocks.earlyeffect::heddle:0.16.0-a4fe367ee229-SNAPSHOT",
        zio,
        zioJson,
      )
      .testDependencies(zioTest, zioTestSbt)
      .note(PluginId("dev.earlyeffect.specular"), NoteKey("library"), shipNote(library.released.revision))
    Projects
      .organization(organization"rocks.earlyeffect")
      .license(License(license"Apache-2.0", MetadataUri("https://www.apache.org/licenses/LICENSE-2.0.txt")))
      .developer(Developer(developer"russwyte", pom"Russ White"))
      .scm(
        Scm(
          MetadataUri("https://github.com/early-effect/terret"),
          connection"scm:git@github.com:early-effect/terret.git",
        )
      )
      .description(pom"Terret lets coding hosts talk to coding harnesses.")
      .url(MetadataUri("https://github.com/early-effect/terret"))
      .locked(
        ships(library(core, grok, dsh, mcp, app)),
        core,
        grok,
        dsh,
        mcp,
        app,
        docs,
        find,
      )

  override def services =
    BuildServices.default
      .add[Specular](ZLayer.succeed(site) >>> Specular.layer)
      .tasks[Specular](ZLayer.succeed(site) >>> Specular.layer)

  private val site =
    Site
      .forProject("docs", "terret.docs.BuildSite")
      .basePath("/terret")
      .docsUrl("https://www.earlyeffect.rocks/terret/")
      .name("terret")
      .organization("rocks.earlyeffect")
      .title("Terret")
      .description("Hosts talk to coding harnesses.")
      .homepage("https://github.com/early-effect/terret")
      .libraryShip
      .ci(SiteCi.Publish(SitePublish.Automatic))

  /** A ship revision is one line. `not-a-ship` is the branch a revision cannot take. */
  private def shipNote(revision: String): NoteText =
    NoteText.from(revision) match
      case Right(text) => text
      case Left(_)     => NoteText("not-a-ship")
end build
