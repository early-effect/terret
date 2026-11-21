package terret.grok

import zio.json.*
import zio.json.ast.Json
import zio.{Task, ZIO}

import java.nio.charset.StandardCharsets

object LiveBodies:
  def json(name: String): Task[Json] =
    ZIO
      .attemptBlocking {
        val in = getClass.getResourceAsStream(s"/terret/grok/live/$name")
        if in == null then Left(name)
        else
          try Right(new String(in.readAllBytes(), StandardCharsets.UTF_8))
          finally in.close()
      }
      .flatMap {
        case Left(missing) => ZIO.die(new IllegalArgumentException(missing))
        case Right(text)   =>
          ZIO.fromEither(text.fromJson[Json]).orDieWith(_ => new IllegalArgumentException(name))
      }
