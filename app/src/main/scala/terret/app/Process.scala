package terret.app

import heddle.Server
import heddle.http.Response
import heddle.mcp.events.{Events, SubscriptionStore, WebhookTransport}
import heddle.route.Routes
import heddle.server.Port
import terret.Terret
import terret.config.TerretConfig
import terret.harness.Harness
import terret.hooks.Hooks
import terret.model.ModelCatalog
import terret.app.watch.{CsrfTokens, Steer, WatchRoutes, WatchStreams}
import terret.github.Gh
import terret.mcp.{EventKeep, EventsKey, MilestoneEvents, Served, TerretMcp}
import terret.milestone.{MilestoneGate, Milestones}
import zio.{Chunk, Scope, ULayer, ZIO, ZLayer}

object Process:
  def listen(
      config: ProcessConfig,
      gate: MilestoneGate,
  ): ZIO[Harness & ModelCatalog & Scope, ProcessError, Unit] =
    ZIO.serviceWithZIO[Harness](listen(config, _, gate))

  def listen(
      config: ProcessConfig,
      harness: Harness,
      gate: MilestoneGate,
  ): ZIO[ModelCatalog & Scope, ProcessError, Unit] =
    assemble(config, harness, gate).flatMap { prepared =>
      val body = config.listen match
        case Listen.Stdio            => prepared.served.mcp.stdio().mapError(ProcessError.Stdio(_))
        case Listen.Http(host, port) =>
          prepared.key match
            case None      => ZIO.fail(ProcessError.KeyMissing)
            case Some(key) => http(prepared.served, prepared.terret, host, port, config, key) *> ZIO.never
      // Join the turn while this scope is still open. Joining a fiber forked into the
      // scope from that scope's finalizer waits forever.
      armed(prepared.served, prepared.terret, gate, config)(body).ensuring(prepared.terret.shutdown)
    }

  /** Bind HTTP and leave the socket open until the scope closes. */
  def bind(config: ProcessConfig, harness: Harness): ZIO[ModelCatalog & Scope, ProcessError, Server] =
    config.listen match
      case Listen.Stdio            => ZIO.fail(ProcessError.Listen("stdio"))
      case Listen.Http(host, port) =>
        for
          gate     <- MilestoneGate.make
          prepared <- assemble(config, harness, gate)
          key      <- ZIO.fromEither(prepared.key.toRight(ProcessError.KeyMissing))
          server   <- armed(prepared.served, prepared.terret, gate, config)(
            http(prepared.served, prepared.terret, host, port, config, key)
          )
        yield server

  /** The local listener has no Access check and does not serve `/watch`. `/mcp` on both listeners requires the
    * configured key. Every other tunnel path requires a JWT. Stdio does not use this gate.
    */
  private def http(
      served: Served,
      terret: Terret,
      host: String,
      port: Int,
      config: ProcessConfig,
      key: EventsKey,
  ): ZIO[Milestones & Events.Env & AccessGate & AuthLog & Scope, ProcessError, Server] =
    for
      streams <- WatchStreams.make
      csrf    <- CsrfTokens.make
      steer   <- Steer.make(terret, Chunk.from(config.root))
      watch = WatchRoutes.routes(terret, streams, steer, csrf)
      local <- openListener(served, host, port, key)
      _     <- ZIO.foreachDiscard(config.access)(cfg => tunnelListener(served, watch, cfg.port, key))
    yield local

  private def openListener(
      served: Served,
      host: String,
      port: Int,
      key: EventsKey,
  ): ZIO[Milestones & Events.Env & AccessGate & AuthLog & Scope, ProcessError, Server] =
    bind(Ingress.local(served.routes, key), host, port)

  private def tunnelListener(
      served: Served,
      watch: heddle.route.Routes[AuthLog, Nothing],
      port: Int,
      key: EventsKey,
  ): ZIO[Milestones & Events.Env & AccessGate & AuthLog & Scope, ProcessError, Server] =
    bind(Ingress.tunnel(served.routes, watch, key), AccessConfig.TunnelHost, port)

  private def bind[R](
      routes: Routes[R, Response],
      host: String,
      port: Int,
  ): ZIO[R & Scope, ProcessError, Server] =
    for
      bound  <- ZIO.fromEither(Port.make(port).left.map(_ => ProcessError.Port(port)))
      server <- Server
        .install(routes, Server.Config.default.copy(host = host, port = bound))
        .mapError(ProcessError.Server(_))
    yield server

  private def assemble(
      config: ProcessConfig,
      harness: Harness,
      gate: MilestoneGate,
  ): ZIO[ModelCatalog & Scope, ProcessError, Prepared] =
    for
      key     <- loadKey(config)
      catalog <- ZIO.service[ModelCatalog]
      terret  <- Terret.make(harness, TerretConfig(config.policy, config.root), Hooks.identity, gate, catalog)
      surface <- ToolRuntime.make(config)
      gh      <- ZIO.service[Gh].provide(Gh.live(config.root.map(_.path)))
      served  <- ZIO
        .fromEither(TerretMcp.served(terret, config.version, config.events, key, surface, gh, config.gh))
        .mapError(ProcessError.Mcp(_))
    yield Prepared(served, terret, key)

  /** Stdio with a source still reads it, so a world-readable file fails before the pipe opens. */
  private def loadKey(config: ProcessConfig): ZIO[Any, ProcessError, Option[EventsKey]] =
    ZIO.fromEither(KeyGate.ready(config.listen, config.key)).flatMap {
      case None         => ZIO.succeed(None)
      case Some(source) =>
        SecretStore.load(source).mapBoth(ProcessError.Key(_), Some(_)).provide(Keychain.live)
    }

  /** The caller's scope owns the ring, the subscription file, the worker, and the accept loop. */
  private def armed[A](served: Served, terret: Terret, gate: MilestoneGate, config: ProcessConfig)(
      body: ZIO[Milestones & Events.Env & AccessGate & AuthLog & Scope, ProcessError, A]
  ): ZIO[Scope, ProcessError, A] =
    val layers =
      served.mcp.eventsLayer(
        SubscriptionStore.file(config.subscriptions.toString, config.events.ttl.maxSubscriptions)
      ) ++
        WebhookTransport.live ++
        Milestones.live ++
        access(config) ++
        AuthLog.live
    for
      skipped <- SubscriptionResume.prepare
        .provide(SubscriptionResume.layer, OpenSessions.layer(terret), SubscriptionJournal.file(config.subscriptions))
        .mapError(ProcessError.Subscriptions(_))
      _ <- ZIO.foreachDiscard(skipped) { row =>
        ZIO.consoleWith(_.printLineError(row.render)).ignore
      }
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

  private def access(config: ProcessConfig): ULayer[AccessGate] =
    config.access match
      case None      => AccessGate.open
      case Some(cfg) => ZLayer.succeed(AccessCerts.remote(cfg.team)) >>> AccessGate.layer(cfg.team, cfg.audience)

  private final case class Prepared(served: Served, terret: Terret, key: Option[EventsKey])
end Process
