package terret.app

import heddle.Server
import heddle.mcp.events.{Events, SubscriptionStore, WebhookTransport}
import heddle.server.Port
import terret.Terret
import terret.config.TerretConfig
import terret.harness.Harness
import terret.hooks.Hooks
import terret.model.ModelCatalog
import terret.mcp.{EventKeep, MilestoneEvents, Served, TerretMcp}
import terret.milestone.{MilestoneGate, Milestones}
import zio.{Scope, UIO, ZIO}

object Process:
  def listen(config: ProcessConfig, gate: MilestoneGate): ZIO[Harness & ModelCatalog & Scope, ProcessError, Unit] =
    ZIO.serviceWithZIO[Harness](listen(config, _, gate))

  def listen(
      config: ProcessConfig,
      harness: Harness,
      gate: MilestoneGate,
  ): ZIO[ModelCatalog & Scope, ProcessError, Unit] =
    assemble(config, harness, gate).flatMap { prepared =>
      val body = config.listen match
        case Listen.Stdio            => prepared.served.mcp.stdio().mapError(ProcessError.Stdio(_))
        case Listen.Http(host, port) => bind(prepared.served, host, port) *> ZIO.never
      // Join the turn while this scope is still open. Joining a fiber forked into the
      // scope from that scope's finalizer waits forever.
      armed(prepared.served, gate, config)(body).ensuring(prepared.shutdown)
    }

  /** Bind HTTP and leave the socket open until the scope closes. */
  def bind(config: ProcessConfig, harness: Harness): ZIO[ModelCatalog & Scope, ProcessError, Server] =
    config.listen match
      case Listen.Stdio            => ZIO.fail(ProcessError.Listen("stdio"))
      case Listen.Http(host, port) =>
        for
          gate     <- MilestoneGate.make
          prepared <- assemble(config, harness, gate)
          server   <- armed(prepared.served, gate, config)(bind(prepared.served, host, port))
        yield server

  private def bind(
      served: Served,
      host: String,
      port: Int,
  ): ZIO[Milestones & Events.Env & Scope, ProcessError, Server] =
    for
      bound  <- ZIO.fromEither(Port.make(port).left.map(_ => ProcessError.Port(port)))
      server <- Server
        .install(served.routes, Server.Config.default.copy(host = host, port = bound))
        .mapError(ProcessError.Server(_))
    yield server

  private def assemble(
      config: ProcessConfig,
      harness: Harness,
      gate: MilestoneGate,
  ): ZIO[ModelCatalog & Scope, ProcessError, Prepared] =
    for
      catalog <- ZIO.service[ModelCatalog]
      terret  <- Terret.make(harness, TerretConfig(config.policy, config.root), Hooks.identity, gate, catalog)
      surface <- ToolRuntime.make(config)
      served  <- ZIO
        .fromEither(
          TerretMcp.served(terret, config.version, config.events, config.eventsKey, surface)
        )
        .mapError(ProcessError.Mcp(_))
    yield Prepared(served, terret.shutdown)

  /** The caller's scope owns the ring, the subscription file, the worker, and the accept loop. */
  private def armed[A](served: Served, gate: MilestoneGate, config: ProcessConfig)(
      body: ZIO[Milestones & Events.Env & Scope, ProcessError, A]
  ): ZIO[Scope, ProcessError, A] =
    val layers =
      served.mcp.eventsLayer(
        SubscriptionStore.file(config.subscriptions.toString, config.events.ttl.maxSubscriptions)
      ) ++
        WebhookTransport.live ++
        Milestones.live
    for
      env   <- layers.build.mapError(ProcessError.Subscriptions(_))
      scope <- ZIO.environment[Scope]
      value <-
        val open =
          for
            _ <- MilestoneEvents.attach(gate)
            _ <- Events.maintained
            _ <- EventKeep.renewed(config.events.default, EventKeep.pace(config.events.floor))
            a <- body
          yield a
        open.provideEnvironment(env ++ scope)
    yield value

  private final case class Prepared(served: Served, shutdown: UIO[Unit])
end Process
