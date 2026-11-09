package terret.grok

import heddle.LinePipe
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.{Scope, ZIO}

trait AcpOpen:
  def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe]
end AcpOpen

object AcpOpen:
  def live(config: GrokConfig, spawner: Spawner): AcpOpen = new:
    def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe] =
      config.transport match
        case GrokTransport.AcpStdio =>
          val argv = Launch.stdio(config, policy, ceiling)
          for
            spawned <- ZIO.acquireRelease(spawner.spawn(argv.command, argv.args, workspace))(_.destroy)
            _       <- Spawner.drain(spawned.stderr).forkScoped
          yield LineRpc(LinePipe.streams(spawned.stdout, spawned.stdin), spawned.destroy)
        case GrokTransport.AcpWebSocket =>
          config.bind match
            case None       => ZIO.fail(HarnessFailure.Protocol(ProtocolFault.BindMissing))
            case Some(bind) => WsClient.connect(bind)
        case GrokTransport.Cli =>
          ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))

  def initialize(ceiling: Ceiling): Json =
    Json.Obj(
      "protocolVersion" -> Json.Num(BigDecimal(1)),
      "clientCapabilities" -> Json.Obj(
        "fs" -> Json.Obj(
          "readTextFile"  -> Json.Bool(ceiling.allow.contains(Permission.ReadWorkspace)),
          "writeTextFile" -> Json.Bool(ceiling.allow.contains(Permission.WriteWorkspace)),
        ),
        "terminal" -> Json.Bool(ceiling.allow.contains(Permission.RunCommands)),
      ),
      "clientInfo" -> Json.Obj("name" -> Json.Str("terret"), "title" -> Json.Str("Terret")),
    )

  def sessionNew(config: GrokConfig, workspace: Workspace): Json =
    val fields = List(
      "cwd"        -> Json.Str(workspace.path.toString),
      "mcpServers" -> Json.Arr(),
    )
    val withMeta =
      if config.approval == GrokApproval.Always then
        fields :+ ("_meta" -> Json.Obj("yoloMode" -> Json.Bool(true)))
      else fields
    Json.Obj(withMeta*)
end AcpOpen
