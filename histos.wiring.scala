import dev.earlyeffect.specular.{Site, SiteCi, Specular}
import histos.plugin.{Site as _, SitePublish, *}
import zio.*

object HistosWiring:
  def layer =
    ZLayer.succeed(site) >>> wire[Specular]

  private val site =
    Site
      .forProject("docs", "terret.docs.BuildSite")
      .basePath("/terret")
      .docsUrl("https://www.earlyeffect.rocks/terret/")
      .name("terret")
      .organization("rocks.earlyeffect")
      .version("unreleased")
      .ci(SiteCi.Publish(SitePublish.Automatic))
end HistosWiring
