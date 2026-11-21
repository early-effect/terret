package terret.grok

import heddle.LinePipe
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.{Scope, ZIO}

trait AcpOpen:
  def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe]

object AcpOpen:
  def live(config: GrokConfig, spawner: Spawner): AcpOpen = new:
    def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe] =
      config.transport match
        case GrokTransport.AcpStdio =>
          val argv = Launch.stdio(config, policy, ceiling)
          for
            spawned <- ZIO.acquireRelease(spawner.spawn(argv.command, argv.args, workspace))(_.destroy)
            // stderr's read ignores interruption. Do not join it from this scope.
            drain <- Spawner.drain(spawned.stderr).forkDaemon
            _     <- ZIO.addFinalizer(drain.interruptFork)
            pipe  <- LineRpc.make(LinePipe.streams(spawned.stdout, spawned.stdin), spawned.destroy)
          yield pipe
        case GrokTransport.AcpWebSocket =>
          config.bind match
            case None       => ZIO.fail(HarnessFailure.Protocol(ProtocolFault.BindMissing))
            case Some(bind) => WsClient.connect(bind)
        case GrokTransport.Cli =>
          ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))

  def initialize: Json =
    Json.Obj(
      "protocolVersion"    -> Json.Num(BigDecimal(1)),
      "clientCapabilities" -> Json.Obj(
        "fs" -> Json.Obj(
          "readTextFile"  -> Json.Bool(false),
          "writeTextFile" -> Json.Bool(false),
        ),
        "terminal" -> Json.Bool(false),
      ),
      "clientInfo" -> Json.Obj(
        "name"    -> Json.Str("terret"),
        "title"   -> Json.Str("Terret"),
        "version" -> Json.Str("0"),
      ),
    )

  def sessionNew(config: GrokConfig, workspace: Workspace): Json =
    val fields = List(
      "cwd"        -> Json.Str(workspace.path.toString),
      "mcpServers" -> Json.Arr(),
    )
    val withMeta =
      if config.approval == GrokApproval.Always then fields :+ ("_meta" -> Json.Obj("yoloMode" -> Json.Bool(true)))
      else fields
    Json.Obj(withMeta*)
end AcpOpen
