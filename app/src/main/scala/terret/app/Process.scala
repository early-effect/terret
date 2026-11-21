package terret.app

import heddle.Server
import heddle.server.Port
import terret.harness.Harness
import terret.hooks.Hooks
import terret.mcp.{Served, TerretMcp}
import terret.Terret
import terret.config.TerretConfig
import zio.{Scope, ZIO}

object Process:
  def listen(config: ProcessConfig): ZIO[Harness & Scope, ProcessError, Unit] =
    ZIO.serviceWithZIO[Harness](listen(config, _))

  def listen(config: ProcessConfig, harness: Harness): ZIO[Scope, ProcessError, Unit] =
    assemble(config, harness).flatMap { served =>
      config.listen match
        case Listen.Stdio            => served.mcp.stdio().mapError(ProcessError.Stdio(_))
        case Listen.Http(host, port) => bind(served, host, port) *> ZIO.never
    }

  /** Bind HTTP and leave the socket open until the scope closes. */
  def bind(config: ProcessConfig, harness: Harness): ZIO[Scope, ProcessError, Server] =
    config.listen match
      case Listen.Stdio            => ZIO.fail(ProcessError.Listen("stdio"))
      case Listen.Http(host, port) => assemble(config, harness).flatMap(bind(_, host, port))

  private def bind(served: Served, host: String, port: Int): ZIO[Scope, ProcessError, Server] =
    for
      bound  <- ZIO.fromEither(Port.make(port).left.map(_ => ProcessError.Port(port)))
      server <- Server
        .install(served.routes, Server.Config.default.copy(host = host, port = bound))
        .mapError(ProcessError.Server(_))
    yield server

  private def assemble(config: ProcessConfig, harness: Harness): ZIO[Scope, ProcessError, Served] =
    for
      terret <- Terret.make(harness, TerretConfig(config.policy, config.root), Hooks.identity)
      served <- ZIO.fromEither(TerretMcp.served(terret, config.version)).mapError(ProcessError.Mcp(_))
    yield served
end Process
