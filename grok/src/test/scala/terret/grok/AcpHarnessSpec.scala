package terret.grok

import heddle.LinePipe
import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.harness.HarnessFailure
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.transcript.Retain
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, Promise, Ref, Scope, UIO, ZIO}

object AcpHarnessSpec extends ZIOSpecDefault:
  private val edit    = ToolCall(CallId("call_1"), ToolName("Edit"), Json.Obj("path" -> Json.Str("a")))
  private val printed = ToolCall(CallId("call_1"), ToolName("Edit"), Json.Obj("stdout" -> Json.Str("<work")))

  def spec = suite("ACP harness")(
    test("closed denies an edit, unknown methods get -32601, and an empty log loads once") {
      scripted(reply = true).flatMap { (harness, seen, _) =>
        for
          root     <- workspace
          id       <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          loaded   <- harness.resume(id).runCollect
          again    <- harness.resume(id).runCollect
          streamed <- harness.prompt(id, Prompt("hello")).runCollect
          record   <- awaitSeen(seen)(_.payloads.length >= 2)
          methods = record.methods
        yield assertTrue(
          id.value == "sess-1",
          loaded == Chunk(Event.Assistant("welcome back"), Event.ToolStarted(edit)),
          again == loaded,
          methods.count(_ == "session/load") == 1,
          streamed == Chunk(
            Event.Thought("hmm"),
            Event.Assistant("hello"),
            Event.ToolUpdated(printed),
            Event.TurnFinished(TurnOutcome.Completed),
          ),
          record.payloads.exists(hasCode(_, -32601)),
          record.payloads.exists(option(_, "reject-once")),
        )
      }
    },
    test("inherit allows the edit") {
      scripted(reply = true).flatMap { (harness, seen, _) =>
        for
          root   <- workspace
          id     <- harness.open(root, Ceiling.all, PermissionPolicy.Inherit)
          _      <- harness.prompt(id, Prompt("hello")).runDrain
          record <- awaitSeen(seen)(_.payloads.exists(option(_, "allow-once")))
        yield assertTrue(record.payloads.exists(option(_, "allow-once")))
      }
    },
    test("a workspace write is allowed when both grants are present") {
      val ceiling = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      shell("echo hi > out.txt").flatMap { (harness, seen) =>
        turn(harness, seen, ceiling, "allow-once").flatMap { (got, record) =>
          assertTrue(
            record.payloads.exists(option(_, "allow-once")),
            finished(got, TurnOutcome.Completed),
            got.exists {
              case Event.ToolUpdated(call) => call.input == Json.Obj("stdout" -> Json.Str("hi"))
              case _                       => false
            },
          )
        }
      }
    },
    test("tee is allowed inside the workspace and rejected without WriteWorkspace") {
      val ceiling = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      val run     = Ceiling(Set(Permission.ReadWorkspace, Permission.RunCommands))
      shell("echo hi | tee out.txt").flatMap { (harness, seen) =>
        turn(harness, seen, ceiling, "allow-once").flatMap { (got, _) =>
          shell("echo hi | tee out.txt").flatMap { (narrow, denied) =>
            turn(narrow, denied, run, "reject-once").map { (refused, record) =>
              assertTrue(
                finished(got, TurnOutcome.Completed),
                record.payloads.exists(option(_, "reject-once")),
                refused.exists {
                  case Event.ToolUpdated(call) =>
                    call.input match
                      case Json.Str(text) => text.contains("needs WriteWorkspace")
                      case _              => false
                  case _ => false
                },
                finished(refused, TurnOutcome.Completed),
                !finished(refused, TurnOutcome.Cancelled),
              )
            }
          }
        }
      }
    },
    test("a write outside the workspace is rejected until that root is configured") {
      val ceiling = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      val command = "echo hi > /tmp/terret-probe.log"
      shell(command).flatMap { (harness, seen) =>
        turn(harness, seen, ceiling, "reject-once").flatMap { (refused, _) =>
          ZIO.fromEither(Workspace.from("/tmp")).orDieWith(_ => new IllegalArgumentException("workspace")).flatMap {
            scratch =>
              shell(command, Chunk(scratch)).flatMap { (opened, allowedSeen) =>
                turn(opened, allowedSeen, ceiling, "allow-once").map { (allowed, record) =>
                  assertTrue(
                    refused.exists {
                      case Event.ToolUpdated(call) =>
                        call.input match
                          case Json.Str(text) => text.contains("outside the workspace")
                          case _              => false
                      case _ => false
                    },
                    record.payloads.exists(option(_, "allow-once")),
                    finished(allowed, TurnOutcome.Completed),
                  )
                }
              }
          }
        }
      }
    },
    test("a redirect to /dev/null does not need WriteWorkspace") {
      val run = Ceiling(Set(Permission.ReadWorkspace, Permission.RunCommands))
      shell("echo hi > /dev/null").flatMap { (harness, seen) =>
        turn(harness, seen, run, "allow-once").map { (_, record) =>
          assertTrue(record.payloads.exists(option(_, "allow-once")))
        }
      }
    },
    test("cancel tells the agent") {
      scripted(reply = false).flatMap { (harness, seen, cancelled) =>
        for
          root   <- workspace
          id     <- harness.open(root, Ceiling.all, PermissionPolicy.Inherit)
          parked <- Promise.make[Nothing, Unit]
          fiber  <- Retain.parked.locally(_ => parked.succeed(()).unit) {
            harness.prompt(id, Prompt("stay")).runCollect.fork
          }
          _      <- parked.await
          _      <- harness.cancel(id)
          _      <- cancelled.await
          got    <- fiber.join
          record <- seen.get
        yield assertTrue(
          got.exists {
            case Event.Thought("hmm") => true
            case _                    => false
          },
          record.methods.contains("session/cancel"),
        )
      }
    },
  ) @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private def finished(events: Chunk[Event], outcome: TurnOutcome): Boolean =
    events.exists {
      case Event.TurnFinished(found) => found == outcome
      case _                         => false
    }

  private def turn(harness: GrokHarness, seen: Ref[Seen], ceiling: Ceiling, optionId: String) =
    for
      root   <- workspace
      id     <- harness.open(root, ceiling, PermissionPolicy.Clamp(ceiling))
      got    <- harness.prompt(id, Prompt("go")).runCollect
      record <- awaitSeen(seen)(_.payloads.exists(option(_, optionId)))
    yield (got, record)

  private def shell(command: String, roots: Chunk[Workspace] = Chunk.empty) =
    for
      seen     <- Ref.make(Seen(Chunk.empty, Chunk.empty))
      promptId <- Ref.make(Option.empty[Json])
      harness  <- GrokHarness.make(GrokConfig.stdio, idle, shellAgent(seen, promptId, command), roots)
    yield (harness, seen)

  private def shellAgent(seen: Ref[Seen], promptId: Ref[Option[Json]], command: String): AcpOpen = new:
    def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe] =
      for
        ends <- LinePipe.connected
        (client, agentEnd) = ends
        _ <- shellTalk(agentEnd, seen, promptId, command).forkScoped
        _ <- ZIO.addFinalizer(agentEnd.close *> client.close)
      yield LineRpc(client, client.close)

  private def shellTalk(
      agent: LinePipe,
      seen: Ref[Seen],
      promptId: Ref[Option[Json]],
      command: String,
  ): UIO[Unit] =
    def step: UIO[Unit] =
      agent.readLine.catchAll(_ => ZIO.succeed(None)).flatMap {
        case None       => ZIO.unit
        case Some(line) =>
          line.fromJson[Json].toOption match
            case None       => step
            case Some(json) =>
              Wire.parse(json) match
                case Left(_)     => step
                case Right(wire) => shellHandle(agent, seen, promptId, command, wire) *> step
      }
    step

  private def shellHandle(
      agent: LinePipe,
      seen: Ref[Seen],
      promptId: Ref[Option[Json]],
      command: String,
      wire: Wire,
  ): UIO[Unit] =
    wire match
      case Wire.Call(id, "initialize", _) =>
        send(agent, Wire.ok(id, Json.Obj("protocolVersion" -> Json.Num(BigDecimal(1)))))
      case Wire.Call(id, "session/new", _) =>
        send(agent, Wire.ok(id, Json.Obj("sessionId" -> Json.Str("sess-1"))))
      case Wire.Call(id, "session/prompt", _) =>
        promptId.set(Some(id)) *>
          send(agent, shellCall(command)) *>
          send(agent, shellPermission(command))
      case Wire.Ok(_, result) =>
        seen.update(found => found.copy(payloads = found.payloads :+ result)) *>
          promptId.get.flatMap {
            case Some(id) =>
              val allowed = option(result, "allow-once")
              promptId.set(None) *>
                send(agent, if allowed then shellOutput else shellFailed) *>
                send(agent, Wire.ok(id, Json.Obj("stopReason" -> Json.Str("end_turn"))))
            case None => ZIO.unit
          }
      case Wire.Call(_, _, _) => ZIO.unit
      case Wire.Note(_, _)    => ZIO.unit
      case Wire.Err(_, _)     => ZIO.unit

  private def shellCall(command: String): Json =
    Wire.notify(
      "session/update",
      Json.Obj(
        "sessionId" -> Json.Str("sess-1"),
        "update"    -> Json.Obj(
          "sessionUpdate" -> Json.Str("tool_call"),
          "toolCallId"    -> Json.Str("call_9"),
          "toolName"      -> Json.Str("run_terminal_command"),
          "kind"          -> Json.Str("execute"),
          "rawInput"      -> Json.Obj("command" -> Json.Str(command)),
        ),
      ),
    )

  private def shellPermission(command: String): Json =
    Wire.request(
      Json.Str("perm-9"),
      "session/request_permission",
      Json.Obj(
        "toolCall" -> Json.Obj(
          "toolCallId" -> Json.Str("call_9"),
          "kind"       -> Json.Str("execute"),
          "rawInput"   -> Json.Obj("command" -> Json.Str(command)),
        ),
        "options" -> Json.Arr(
          Json.Obj("optionId" -> Json.Str("allow-once"), "kind"  -> Json.Str("allow_once")),
          Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
        ),
      ),
    )

  private val shellOutput: Json =
    Wire.notify(
      "session/update",
      Json.Obj(
        "sessionId" -> Json.Str("sess-1"),
        "update"    -> Json.Obj(
          "sessionUpdate" -> Json.Str("tool_call_update"),
          "toolCallId"    -> Json.Str("call_9"),
          "rawOutput"     -> Json.Obj("stdout" -> Json.Str("hi")),
        ),
      ),
    )

  private val shellFailed: Json =
    Wire.notify(
      "session/update",
      Json.Obj(
        "sessionId" -> Json.Str("sess-1"),
        "update"    -> Json.Obj(
          "sessionUpdate" -> Json.Str("tool_call_update"),
          "toolCallId"    -> Json.Str("call_9"),
          "status"        -> Json.Str("failed"),
          "rawOutput"     -> Json.Null,
        ),
      ),
    )

  private def workspace =
    ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))

  private def scripted(
      reply: Boolean
  ): ZIO[Scope, Nothing, (GrokHarness, Ref[Seen], Promise[Nothing, Unit])] =
    for
      seen      <- Ref.make(Seen(Chunk.empty, Chunk.empty))
      cancelled <- Promise.make[Nothing, Unit]
      config = GrokConfig.stdio
      harness <- GrokHarness.make(config, idle, agent(seen, cancelled, reply))
    yield (harness, seen, cancelled)

  private val idle: Spawner = new:
    def spawn(command: String, args: Chunk[String], cwd: Workspace) =
      ZIO.fail(HarnessFailure.Protocol(terret.harness.ProtocolFault.Incomplete))

  private def agent(
      seen: Ref[Seen],
      cancelled: Promise[Nothing, Unit],
      reply: Boolean,
  ): AcpOpen = new:
    def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe] =
      for
        ends <- LinePipe.connected
        (client, agentEnd) = ends
        _ <- talk(agentEnd, seen, cancelled, reply).forkScoped
        _ <- ZIO.addFinalizer(agentEnd.close *> client.close)
      yield LineRpc(client, client.close)

  private def talk(
      agent: LinePipe,
      seen: Ref[Seen],
      cancelled: Promise[Nothing, Unit],
      reply: Boolean,
  ): UIO[Unit] =
    def step: UIO[Unit] =
      agent.readLine.catchAll(_ => ZIO.succeed(None)).flatMap {
        case None       => ZIO.unit
        case Some(line) =>
          line.fromJson[Json].toOption match
            case None       => step
            case Some(json) =>
              Wire.parse(json) match
                case Left(_)     => step
                case Right(wire) => handle(agent, seen, cancelled, reply, wire) *> step
      }
    step

  private def handle(
      agent: LinePipe,
      seen: Ref[Seen],
      cancelled: Promise[Nothing, Unit],
      reply: Boolean,
      wire: Wire,
  ): UIO[Unit] =
    wire match
      case Wire.Call(id, "initialize", _) =>
        note(seen, "initialize") *> send(agent, Wire.ok(id, Json.Obj("protocolVersion" -> Json.Num(BigDecimal(1)))))
      case Wire.Call(id, "session/new", _) =>
        note(seen, "session/new") *>
          send(
            agent,
            Wire.notify(
              "session/update",
              Json.Obj("update" -> Json.Obj("sessionUpdate" -> Json.Str("available_commands_update"))),
            ),
          ) *>
          send(agent, Wire.ok(id, Json.Obj("sessionId" -> Json.Str("sess-1"))))
      case Wire.Call(id, "session/load", _) =>
        note(seen, "session/load") *>
          send(agent, update("agent_message_chunk", "welcome back")) *>
          send(agent, tool) *>
          send(agent, Wire.ok(id, Json.Obj("sessionId" -> Json.Str("sess-1"))))
      case Wire.Call(id, "session/prompt", _) =>
        note(seen, "session/prompt") *>
          send(agent, update("agent_thought_chunk", "hmm")) *>
          ZIO
            .when(reply) {
              send(agent, update("agent_message_chunk", "hello")) *>
                send(agent, Wire.request(Json.Str("ping-1"), "custom/ping", Json.Obj())) *>
                send(agent, permission) *>
                send(agent, toolUpdate) *>
                send(agent, Wire.ok(id, Json.Obj("stopReason" -> Json.Str("end_turn"))))
            }
            .unit
      case Wire.Call(_, method, _) =>
        note(seen, method)
      case Wire.Note(method, _) =>
        note(seen, method) *> cancelled.succeed(()).when(method == "session/cancel").unit
      case Wire.Ok(_, result) =>
        seen.update(found => found.copy(payloads = found.payloads :+ result))
      case Wire.Err(_, code) =>
        seen.update(found => found.copy(payloads = found.payloads :+ Json.Obj("code" -> Json.Num(BigDecimal(code)))))

  private def note(seen: Ref[Seen], method: String): UIO[Unit] =
    seen.update(found => found.copy(methods = found.methods :+ method))

  private def awaitSeen(seen: Ref[Seen])(ready: Seen => Boolean): UIO[Seen] =
    seen.get.flatMap { found =>
      if ready(found) then ZIO.succeed(found) else ZIO.yieldNow *> awaitSeen(seen)(ready)
    }

  private def send(agent: LinePipe, json: Json): UIO[Unit] =
    agent.writeLine(json.toJson).ignore

  private def update(kind: String, text: String): Json =
    Wire.notify(
      "session/update",
      Json.Obj(
        "sessionId" -> Json.Str("sess-1"),
        "update"    -> Json.Obj(
          "sessionUpdate" -> Json.Str(kind),
          "content"       -> Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(text)),
        ),
      ),
    )

  private val permission: Json =
    Wire.request(
      Json.Str("perm-1"),
      "session/request_permission",
      Json.Obj(
        "toolCall" -> Json
          .Obj("toolCallId" -> Json.Str("call_1"), "title" -> Json.Str("Edit"), "kind" -> Json.Str("edit")),
        "options" -> Json.Arr(
          Json.Obj("optionId" -> Json.Str("allow-once"), "kind"  -> Json.Str("allow_once")),
          Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
        ),
      ),
    )

  private val toolUpdate: Json =
    Wire.notify(
      "session/update",
      Json.Obj(
        "sessionId" -> Json.Str("sess-1"),
        "update"    -> Json.Obj(
          "sessionUpdate" -> Json.Str("tool_call_update"),
          "toolCallId"    -> Json.Str("call_1"),
          "rawOutput"     -> Json.Obj(
            "stdout" -> Json.Arr(Json.Num(60), Json.Num(119), Json.Num(111), Json.Num(114), Json.Num(107))
          ),
        ),
      ),
    )

  private val tool: Json =
    Wire.notify(
      "session/update",
      Json.Obj(
        "sessionId" -> Json.Str("sess-1"),
        "update"    -> Json.Obj(
          "sessionUpdate" -> Json.Str("tool_call"),
          "toolCallId"    -> Json.Str("call_1"),
          "title"         -> Json.Str("Edit"),
          "kind"          -> Json.Str("edit"),
          "rawInput"      -> Json.Obj("path" -> Json.Str("a")),
        ),
      ),
    )

  private def option(json: Json, optionId: String): Boolean =
    json match
      case obj: Json.Obj =>
        JsonFields.field(obj, "outcome") match
          case Some(inner: Json.Obj) => JsonFields.str(inner, "optionId").contains(optionId)
          case _                     => false
      case _ => false

  private def hasCode(json: Json, code: Int): Boolean =
    json match
      case obj: Json.Obj =>
        JsonFields.field(obj, "code") match
          case Some(Json.Num(number)) => number.intValue() == code
          case _                      => false
      case _ => false

  private final case class Seen(methods: Chunk[String], payloads: Chunk[Json])
end AcpHarnessSpec
