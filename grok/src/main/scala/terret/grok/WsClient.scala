package terret.grok

import terret.harness.{HarnessFailure, ProtocolFault}
import zio.json.*
import zio.json.ast.Json
import zio.{IO, Scope, UIO, ZIO}

import java.net.URI
import java.net.http.{HttpClient, WebSocket}
import java.nio.ByteBuffer
import java.time.Duration
import java.util.concurrent.{CompletableFuture, CompletionStage, LinkedBlockingQueue}

object WsClient:
  private val connectTimeout = Duration.ofSeconds(5)

  def connect(bind: AcpBind): ZIO[Scope, HarnessFailure, RpcPipe] =
    ZIO.acquireRelease(open(bind))(_.close)

  private def open(bind: AcpBind): IO[HarnessFailure, RpcPipe] =
    val incomplete = HarnessFailure.Protocol(ProtocolFault.Incomplete)
    ZIO
      .attemptBlocking {
        HttpClient.newBuilder().connectTimeout(connectTimeout).build()
      }
      .mapError(_ => incomplete)
      .flatMap { client =>
        val inbound = LinkedBlockingQueue[Option[String]]()
        val future  = client
          .newWebSocketBuilder()
          .header("Authorization", s"Bearer ${bind.secret}")
          .buildAsync(URI.create(s"ws://${bind.host}:${bind.port}/"), Listener(inbound))
        ZIO
          .fromCompletableFuture(future)
          .mapBoth(_ => incomplete, socket => WsPipe(socket, inbound, client))
          .tapError(_ => ZIO.attemptBlocking(client.close()).ignore)
      }
end WsClient

final class WsPipe(
    socket: WebSocket,
    inbound: LinkedBlockingQueue[Option[String]],
    client: HttpClient,
) extends RpcPipe:
  def read: IO[HarnessFailure, Option[Json]] =
    ZIO.attemptBlocking(inbound.take()).mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete)).flatMap {
      case None       => ZIO.succeed(None)
      case Some(text) =>
        ZIO.fromEither(text.fromJson[Json]).mapBoth(_ => HarnessFailure.Protocol(ProtocolFault.Malformed), Some(_))
    }

  def write(json: Json): IO[HarnessFailure, Unit] =
    ZIO
      .fromCompletableFuture(socket.sendText(json.toJson, true))
      .mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete))
      .unit

  // HttpClient.close waits for the socket operation. abort finishes it when the peer is still up.
  def close: UIO[Unit] =
    ZIO.attempt {
      inbound.offer(None)
      socket.abort()
    }.ignore *> ZIO.attemptBlocking(client.close()).ignore
end WsPipe

// WebSocket.Listener completes each callback with null.
private final class Listener(inbound: LinkedBlockingQueue[Option[String]]) extends WebSocket.Listener:
  private val pending = StringBuilder()

  override def onOpen(webSocket: WebSocket): Unit =
    webSocket.request(1)

  override def onText(webSocket: WebSocket, data: CharSequence, last: Boolean): CompletionStage[Void] =
    pending.append(data)
    if last then
      val text = pending.toString
      pending.clear()
      inbound.offer(Some(text))
      ()
    webSocket.request(1)
    done

  override def onBinary(webSocket: WebSocket, data: ByteBuffer, last: Boolean): CompletionStage[Void] =
    webSocket.request(1)
    done

  override def onClose(webSocket: WebSocket, statusCode: Int, reason: String): CompletionStage[Void] =
    inbound.offer(None)
    done

  override def onError(webSocket: WebSocket, error: Throwable): Unit =
    inbound.offer(None)
    ()

  private def done: CompletionStage[Void] =
    CompletableFuture.completedFuture(null)
end Listener
