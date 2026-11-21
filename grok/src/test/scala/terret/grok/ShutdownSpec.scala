package terret.grok

import slayer.*
import terret.config.TerretConfig
import terret.error.TerretError
import terret.harness.HarnessFailure
import terret.hooks.Hooks
import terret.permission.{Ceiling, PermissionPolicy}
import terret.workspace.Workspace
import terret.Terret
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, Ref, Scope, ZIO}

object ShutdownSpec extends ZIOSpecDefault:
  def spec = suite("shutdown")(
    test("shutdown cancels the session and then closes the ACP child") {
      ZIO.scoped {
        (for
          pipe    <- ZIO.service[RpcPipe]
          acp     <- ZIO.service[AcpOpen]
          spawner <- ZIO.service[Spawner]
          pending <- Ref.make(handshake)
          trace   <- Ref.make(Chunk.empty[String])
          _       <- stub[AcpOpen](_.open(any[Workspace], any[PermissionPolicy], any[Ceiling])) { (_, _, _) =>
            ZIO.succeed(pipe)
          }
          _       <- stub[RpcPipe](_.read)(pull(pending))
          _       <- stub[RpcPipe](_.write(any[Json]))(json => record(trace, json))
          _       <- stub[RpcPipe](_.close)(trace.update(_ :+ "close"))
          harness <- GrokHarness.make(GrokConfig.stdio, spawner, acp)
          root    <- ZIO.fromEither(Workspace.from("/work").left.map(TerretError.WorkspaceInvalid(_)))
          terret  <- Terret.make(harness, TerretConfig.closed, Hooks.identity)
          _       <- terret.open(root)
          _       <- terret.shutdown
          seen    <- trace.get
          cancelAt = seen.indexOf("session/cancel")
          closeAt  = seen.indexOf("close")
        yield assertTrue(cancelAt >= 0, closeAt > cancelAt))
          .provideSome[Scope](stubbed[RpcPipe], stubbed[AcpOpen], stubbed[Spawner])
      }
    }
  )

  private val handshake: Chunk[Json] =
    Chunk(
      Wire.ok(Json.Num(BigDecimal(1)), Json.Obj("protocolVersion" -> Json.Num(BigDecimal(1)))),
      Wire.ok(Json.Num(BigDecimal(2)), Json.Obj("sessionId" -> Json.Str("sess-1"))),
    )

  private def pull(pending: Ref[Chunk[Json]]): ZIO[Any, HarnessFailure, Option[Json]] =
    pending.modify { waiting =>
      waiting.headOption match
        case Some(frame) => (ZIO.succeed(Some(frame)), waiting.drop(1))
        case None        => (ZIO.succeed(None), waiting)
    }.flatten

  private def record(trace: Ref[Chunk[String]], json: Json): ZIO[Any, Nothing, Unit] =
    Wire.parse(json) match
      case Right(Wire.Call(_, method, _)) => trace.update(_ :+ method)
      case Right(Wire.Note(method, _))    => trace.update(_ :+ method)
      case _                              => ZIO.unit
end ShutdownSpec
