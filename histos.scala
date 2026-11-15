import histos.*
import histos.plugin.{NoteKey, NoteText, PluginId}

object build extends HistosBuild:
  def graph =
    val library = Ship("terret").next(0, 0, 1)
    val scala3     = scala"3.10.1"
    val zio        = dep"dev.zio::zio:2.1.27"
    val zioStreams = dep"dev.zio::zio-streams:2.1.27"
    val zioJson    = dep"dev.zio::zio-json:1.1.2"
    val zioTest    = dep"dev.zio::zio-test:2.1.27"
    val zioTestSbt = dep"dev.zio::zio-test-sbt:2.1.27"
    val core       = module("core", Jvm)(scala3)
      .artifact(artifact"terret")
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(zio, zioStreams, zioJson)
      .testDependencies(zioTest, zioTestSbt)
    val grok = module("grok", Jvm)(scala3)
      .artifact(artifact"terret-grok")
      .dependsOn(core)
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(zio, zioStreams, zioJson, dep"rocks.earlyeffect::heddle:0.15.1")
      .testDependencies(zioTest, zioTestSbt)
    val dsh = module("dsh", Jvm)(scala3)
      .artifact(artifact"terret-dsh")
      .dependsOn(core)
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(zio, zioStreams, zioJson, dep"rocks.earlyeffect::heddle:0.15.1")
      .testDependencies(zioTest, zioTestSbt)
    val mcp = module("mcp", Jvm)(scala3)
      .artifact(artifact"terret-mcp")
      .dependsOn(core)
      .scalac(
        ScalacFlag.Deprecation,
        ScalacFlag.Feature,
        ScalacFlag.UnusedAll,
        ScalacFlag.ImplicitConversions,
      )
      .libraryDependencies(zio, zioStreams, zioJson, dep"rocks.earlyeffect::heddle-mcp:0.15.1")
      .testDependencies(zioTest, zioTestSbt)
    val app = module("app", Jvm)(scala3)
      .artifact(artifact"terret-app")
      .dependsOn(mcp, grok, dsh)
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
        dep"rocks.earlyeffect::heddle:0.15.1",
        dep"rocks.earlyeffect::heddle-mcp:0.15.1",
      )
      .testDependencies(zioTest, zioTestSbt)
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
        dep"rocks.earlyeffect::heddle:0.15.1",
        zio,
        zioJson,
      )
      .testDependencies(zioTest, zioTestSbt)
      .note(PluginId("dev.earlyeffect.specular"), NoteKey("library"), shipNote(library.released.revision))
    Projects
      .organization(organization"rocks.earlyeffect")
      .license(License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0.txt"))
      .developer(Developer(developer"russwyte", pom"Russ White"))
      .scm(
        Scm(
          url"https://github.com/early-effect/terret",
          connection"scm:git@github.com:early-effect/terret.git",
        )
      )
      .description(pom"Terret lets coding hosts talk to coding harnesses.")
      .url(url"https://github.com/early-effect/terret")
      .locked
      .plugins(PluginSources.ship(dep"rocks.earlyeffect::specular-histos:0.0.2"))(
        ships(library(core, grok, dsh, mcp, app)),
        core,
        grok,
        dsh,
        mcp,
        app,
        docs,
      )

  /** A ship revision is one line. `not-a-ship` is the branch a revision cannot take. */
  private def shipNote(revision: String): NoteText =
    NoteText.from(revision) match
      case Right(text) => text
      case Left(_)     => NoteText("not-a-ship")
