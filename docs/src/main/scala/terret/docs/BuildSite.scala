package terret.docs

import specular.site.{CodeSnippet, DocsSettings, DocsSite, SiteModel}
import terret.app.WarpLocal
import zio.json.*

/** The site task forks this main on the docs test classpath. */
object BuildSite extends DocsSite:
  def pages: Vector[specular.DocPage] = Vector(Attach.doc, Jobs.doc, CeilingPage.doc, Config.doc, DshAdapter.doc)

  override def site(settings: DocsSettings): SiteModel =
    val url = WarpLocal.url(WarpLocal.Host, WarpLocal.Port)
    super.site(settings).copy(
      summaryMarkdown = Some("Hosts attach over MCP. HTTP is the local path."),
      installSnippets = Vector(CodeSnippet("Attach", WarpLocal.http(url).toJson)),
      meta = Some(settings.meta.copy(version = "")),
    )
end BuildSite
