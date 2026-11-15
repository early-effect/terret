import dev.earlyeffect.specular.{Site, SiteCi, Specular}
import histos.plugin.*
import zio.*

object HistosWiring:
  def layer =
    ZLayer.succeed(site) >>> wire[Specular]

  private val site =
    Site
      .forProject("docs", "terret.docs.BuildSite")
      .name("terret")
      .organization("rocks.earlyeffect")
      .version("unreleased")
      .ci(SiteCi.Check)
end HistosWiring
