package terret.find

import zio.ZIO
import zio.durationInt
import zio.test.*

/** The one test that loads the real ONNX session. Select it with `--tag live-embedder`. A CI run skips it so the suite
  * does not download weights.
  */
object BgeLiveSpec extends ZIOSpecDefault:
  private val ci: Boolean =
    def on(name: String): Boolean = sys.env.get(name).exists(_.equalsIgnoreCase("true"))
    val skip                      = on("CI") || on("GITHUB_ACTIONS")
    if skip then System.err.println("SKIP live embedder: CI")
    skip

  def spec = suite("live embedder")(
    test("a query is closer to a matching passage than to an unrelated one") {
      for
        query     <- ZIO.serviceWithZIO[Embedder](_.embedQuery("build the scala project"))
        near      <- ZIO.serviceWithZIO[Embedder](_.embedPassage("compile and build the scala project"))
        far       <- ZIO.serviceWithZIO[Embedder](_.embedPassage("recipe for sourdough bread"))
        nearScore <- ZIO.fromEither(VectorMath.similarity(query, near))
        farScore  <- ZIO.fromEither(VectorMath.similarity(query, far))
      yield assertTrue(
        query.model == ModelId.bgeSmall,
        query.values.length == BgeSmall.Dims,
        near.values.length == BgeSmall.Dims,
        nearScore > farScore,
      )
    } @@ TestAspect.tag("live-embedder")
      @@ TestAspect.timeout(3.minutes)
      @@ TestAspect.withLiveClock
  ).when(!ci).provide(BgeSmall.live)
end BgeLiveSpec
