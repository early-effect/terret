package terret.grok

import heddle.LinePipe
import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.harness.HarnessFailure
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, Promise, Ref, Scope, UIO, ZIO}

object AcpHarnessSpec extends ZIOSpecDefault:
  private val edit = ToolCall(CallId("call_1"), ToolName("Edit"), Json.Obj("path" -> Json.Str("a")))

  def spec = suite("ACP harness")(
    test("closed denies an edit, unknown methods get -32601, and an empty log loads once") {
      scripted(reply = true).flatMap { (harness, seen, _, _) =>
        for
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          loaded  <- harness.resume(id).runCollect
          again   <- harness.resume(id).runCollect
          streamed <- harness.prompt(id, Prompt("hello")).runCollect
          record  <- seen.get.repeatUntil(_.payloads.length >= 2)
          methods = record.methods
        yield assertTrue(
          id.value == "sess-1",
          loaded == Chunk(Event.Assistant("welcome back")),
          again == loaded,
          methods.count(_ == "session/load") == 1,
          streamed == Chunk(
            Event.Thought("hmm"),
            Event.Assistant("hello"),
            Event.ToolStarted(edit),
            Event.TurnFinished(TurnOutcome.Completed),
          ),
          record.payloads.exists(hasCode(_, -32601)),
          record.payloads.exists(option(_, "reject-once")),
        )
      }
    },
    test("inherit allows the edit") {
      scripted(reply = true).flatMap { (harness, seen, _, _) =>
        for
          root <- workspace
          id   <- harness.open(root, Ceiling.all, PermissionPolicy.Inherit)
          _    <- harness.prompt(id, Prompt("hello")).runDrain
          record <- seen.get.repeatUntil(_.payloads.exists(option(_, "allow-once")))
        yield assertTrue(record.payloads.exists(option(_, "allow-once")))
      }
    },
    test("cancel tells the agent") {
      scripted(reply = false).flatMap { (harness, seen, started, cancelled) =>
        for
          root  <- workspace
          id    <- harness.open(root, Ceiling.all, PermissionPolicy.Inherit)
          fiber <- harness.prompt(id, Prompt("stay")).runCollect.fork
          _     <- started.await
          _     <- harness.cancel(id)
          _     <- cancelled.await
          got   <- fiber.join
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

  private def workspace =
    ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))

  private def scripted(
      reply: Boolean,
  ): ZIO[Scope, Nothing, (GrokHarness, Ref[Seen], Promise[Nothing, Unit], Promise[Nothing, Unit])] =
    for
      seen      <- Ref.make(Seen(Chunk.empty, Chunk.empty))
      started   <- Promise.make[Nothing, Unit]
      cancelled <- Promise.make[Nothing, Unit]
      config = GrokConfig.stdio
      harness <- GrokHarness.make(config, idle, agent(seen, started, cancelled, reply))
    yield (harness, seen, started, cancelled)

  private val idle: Spawner = new:
    def spawn(command: String, args: Chunk[String], cwd: Workspace) =
      ZIO.fail(HarnessFailure.Protocol(terret.harness.ProtocolFault.Incomplete))

  private def agent(
      seen: Ref[Seen],
      started: Promise[Nothing, Unit],
      cancelled: Promise[Nothing, Unit],
      reply: Boolean,
  ): AcpOpen = new:
    def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe] =
      for
        ends <- LinePipe.connected
        (client, agentEnd) = ends
        _ <- talk(agentEnd, seen, started, cancelled, reply).forkScoped
        _ <- ZIO.addFinalizer(agentEnd.close *> client.close)
      yield LineRpc(client, client.close)

  private def talk(
      agent: LinePipe,
      seen: Ref[Seen],
      started: Promise[Nothing, Unit],
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
                case Right(wire) => handle(agent, seen, started, cancelled, reply, wire) *> step
      }
    step

  private def handle(
      agent: LinePipe,
      seen: Ref[Seen],
      started: Promise[Nothing, Unit],
      cancelled: Promise[Nothing, Unit],
      reply: Boolean,
      wire: Wire,
  ): UIO[Unit] =
    wire match
      case Wire.Call(id, "initialize", _) =>
        note(seen, "initialize") *> send(agent, Wire.ok(id, Json.Obj("protocolVersion" -> Json.Num(BigDecimal(1)))))
      case Wire.Call(id, "session/new", _) =>
        note(seen, "session/new") *>
          send(agent, Wire.notify("session/update", Json.Obj("update" -> Json.Obj("sessionUpdate" -> Json.Str("available_commands_update"))))) *>
          send(agent, Wire.ok(id, Json.Obj("sessionId" -> Json.Str("sess-1"))))
      case Wire.Call(id, "session/load", _) =>
        note(seen, "session/load") *>
          send(agent, update("agent_message_chunk", "welcome back")) *>
          send(agent, Wire.ok(id, Json.Obj("sessionId" -> Json.Str("sess-1"))))
      case Wire.Call(id, "session/prompt", _) =>
        note(seen, "session/prompt") *>
          send(agent, update("agent_thought_chunk", "hmm")) *>
          started.succeed(()).unit *>
          ZIO.when(reply) {
            send(agent, update("agent_message_chunk", "hello")) *>
              send(agent, Wire.request(Json.Str("ping-1"), "custom/ping", Json.Obj())) *>
              send(agent, permission) *>
              send(agent, tool) *>
              send(agent, Wire.ok(id, Json.Obj("stopReason" -> Json.Str("end_turn"))))
          }.unit
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
        "toolCall" -> Json.Obj("toolCallId" -> Json.Str("call_1"), "title" -> Json.Str("Edit"), "kind" -> Json.Str("edit")),
        "options"  -> Json.Arr(
          Json.Obj("optionId" -> Json.Str("allow-once"), "kind" -> Json.Str("allow_once")),
          Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
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
