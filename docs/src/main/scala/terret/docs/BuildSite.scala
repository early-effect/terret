package terret.docs

import earlyeffect.docs.EarlyEffectTheme
import specular.*
import specular.site.*
import zio.{IO, ZIO, ZLayer}

import java.nio.file.{Files, Path, StandardCopyOption}

/** The site task forks this main on the docs test classpath. */
object BuildSite extends DocsSite:
  private val guide: Vector[DocPage] =
    Vector(Run.doc, Attach.doc, Jobs.doc, CeilingPage.doc, Config.doc, DshAdapter.doc)

  def pages: Vector[DocPage] = guide

  override def site(settings: DocsSettings): SiteModel =
    val version = settings.meta.docsVersion
    val branded = EarlyEffectTheme.brand(super.site(settings))
    branded.copy(
      nav = Some(NavModel.flat(guide)),
      pages = stamp(guide, version),
      summaryMarkdown = None,
      installSnippets = Vector(CodeSnippet("Histos", install(settings.meta))),
      brand = Some(
        Brand(
          name = settings.meta.displayTitle,
          links = Vector(EarlyEffectTheme.github("https://github.com/early-effect/terret")),
        )
      ),
    )

  override def layers: ZLayer[Any, Nothing, SiteBuilder] =
    EarlyEffectTheme.layers

  override def afterBuild(out: Path, result: SiteOutput): IO[SiteError, Unit] =
    val _ = result
    val front   = out.resolve(s"${Run.doc.slug}.html")
    val index   = out.resolve("index.html")
    val promote =
      ZIO.attemptBlockingIO(Files.copy(front, index, StandardCopyOption.REPLACE_EXISTING)).mapError { err =>
        SiteError.WriteFailed(index, err)
      }
    promote *> EarlyEffectTheme.writeLogo(out)

  private def install(meta: ProjectMeta): String =
    List("terret", "terret-grok", "terret-dsh", "terret-mcp", "terret-app")
      .map(artifact => s"""dep"${meta.organization}::$artifact:${meta.docsVersion}"""")
      .mkString("\n")

  private def stamp(pages: Vector[DocPage], version: String): Vector[DocPage] =
    pages.map(page => page.copy(children = page.children.map(stampNode(version))))

  private def stampNode(version: String)(node: DocNode): DocNode =
    node match
      case Prose(markdown)      => Prose(markdown.replace("<terret ship>", version))
      case Section(title, kids) => Section(title, kids.map(stampNode(version)))
      case other                => other
end BuildSite
