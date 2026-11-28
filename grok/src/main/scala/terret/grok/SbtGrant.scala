package terret.grok

import zio.IO

import java.nio.file.Path

/** The directory `candidates/sbt/current` resolves to, when that directory sits inside home.
  *
  * `SbtGrant.decide(home)` grants `home/sdks/sbt-2` when `current` points at it, and the literal path
  * `candidates/sbt/current`. The target may sit outside `candidates/sbt`. A link outside home is refused.
  * Nothing else under `sdks` is granted.
  */
object SbtGrant:
  def decide(home: Path): IO[JdkRefuse, Granted] =
    SdkmanLink.decide(home, "sbt", LinkBound.UnderHome)
end SbtGrant
