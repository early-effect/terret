package terret.app

import zio.{IO, ULayer, ZIO, ZLayer}

import java.net.{
  BindException,
  ConnectException,
  InetSocketAddress,
  NoRouteToHostException,
  PortUnreachableException,
  ServerSocket,
  Socket,
  SocketTimeoutException,
}

trait PortProbe:
  def free(host: String, port: Int): IO[PortProbeFailure, Boolean]
  def listening(host: String, port: Int): IO[PortProbeFailure, Boolean]

enum PortProbeFailure:
  case Failed(port: Int)

  def render: String = this match
    case Failed(port) => s"could not probe port $port"
end PortProbeFailure

object PortProbe:
  val live: ULayer[PortProbe] = ZLayer.succeed(SocketProbe)

  private val connectTimeoutMillis: Int = 200

  private object SocketProbe extends PortProbe:
    def free(host: String, port: Int): IO[PortProbeFailure, Boolean] =
      ZIO.attemptBlockingInterrupt(bind(host, port)).mapError(_ => PortProbeFailure.Failed(port))

    def listening(host: String, port: Int): IO[PortProbeFailure, Boolean] =
      ZIO.attemptBlockingInterrupt(connect(host, port)).mapError(_ => PortProbeFailure.Failed(port))

    private def bind(host: String, port: Int): Boolean =
      val server = new ServerSocket()
      try
        server.setReuseAddress(false)
        server.bind(new InetSocketAddress(host, port))
        true
      catch case _: BindException => false
      finally server.close()

    private def connect(host: String, port: Int): Boolean =
      val socket = new Socket()
      try
        socket.connect(new InetSocketAddress(host, port), connectTimeoutMillis)
        true
      catch
        case _: ConnectException | _: SocketTimeoutException | _: NoRouteToHostException |
            _: PortUnreachableException =>
          false
      finally socket.close()
  end SocketProbe
end PortProbe
