import histos.*

object build extends HistosBuild:
  def graph =
    val scala3     = scala"3.10.0"
    val zio        = dep"dev.zio::zio:2.1.27"
    val zioStreams = dep"dev.zio::zio-streams:2.1.27"
    val zioJson    = dep"dev.zio::zio-json:1.1.1"
    val zioTest    = dep"dev.zio::zio-test:2.1.27"
    val zioTestSbt = dep"dev.zio::zio-test-sbt:2.1.27"
    val core = module("core", Jvm)(scala3)
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
      .libraryDependencies(zio, zioStreams, zioJson, dep"rocks.earlyeffect::heddle:0.14.2")
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
      .libraryDependencies(zio, zioStreams, zioJson, dep"rocks.earlyeffect::heddle-mcp:0.14.2")
      .testDependencies(zioTest, zioTestSbt)
    Projects
      .organization(organization"rocks.earlyeffect")
      .license(License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0.txt"))
      .developer(Developer(developer"russwyte", pom"Russ White"))
      .description(pom"Terret lets coding hosts talk to coding harnesses.")(core, grok, mcp)
