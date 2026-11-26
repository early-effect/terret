package terret.grok

import slayer.*
import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.model.ModelKey
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.transcript.Retain
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, Exit, Promise, Ref, Scope, UIO, ZIO}

object AcpHarnessSpec extends ZIOSpecDefault:
  private val edit    = ToolCall(CallId("call_1"), ToolName("Edit"), Json.Obj("path" -> Json.Str("a")))
  private val printed = ToolCall(CallId("call_1"), ToolName("Edit"), Json.Obj("stdout" -> Json.Str("<work")))

  def spec = suite("ACP harness")(
    test("closed denies an edit, unknown methods get -32601, and an empty log loads once") {
      drive(closedFrames) { (harness, seen) =>
        for
          root     <- workspace
          id       <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          loaded   <- harness.resume(id).runCollect
          again    <- harness.resume(id).runCollect
          streamed <- harness.prompt(id, Prompt("hello")).runCollect
          record   <- seen.get
        yield assertTrue(
          id.value == "sess-1",
          loaded == Chunk(Event.Assistant("welcome back"), Event.ToolStarted(edit)),
          again == loaded,
          record.methods.count(_ == "session/load") == 1,
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
      drive(inheritFrames) { (harness, seen) =>
        for
          root   <- workspace
          id     <- harness.open(root, Ceiling.all, PermissionPolicy.Inherit)
          _      <- harness.prompt(id, Prompt("hello")).runDrain
          record <- seen.get
        yield assertTrue(record.payloads.exists(option(_, "allow-once")))
      }
    },
    test("a workspace write is allowed when both grants are present") {
      val ceiling = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      drive(shellFrames("echo hi > out.txt", allowed = true)) { (harness, seen) =>
        turn(harness, seen, ceiling).map { (got, record) =>
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
      val command = "echo hi | tee out.txt"
      drive(shellFrames(command, allowed = true)) { (harness, seen) =>
        turn(harness, seen, ceiling).flatMap { (got, _) =>
          drive(shellFrames(command, allowed = false)) { (narrow, denied) =>
            turn(narrow, denied, run).map { (refusedEvents, record) =>
              assertTrue(
                finished(got, TurnOutcome.Completed),
                record.payloads.exists(option(_, "reject-once")),
                record.payloads.exists(followup(_, "clamped by server policy")),
                refusedEvents.exists {
                  case Event.ToolUpdated(call) =>
                    call.input match
                      case Json.Str(text) => text.contains("needs WriteWorkspace")
                      case _              => false
                  case _ => false
                },
                finished(refusedEvents, TurnOutcome.Completed),
                !finished(refusedEvents, TurnOutcome.Cancelled),
              )
            }
          }
        }
      }
    },
    test("a write outside the workspace is rejected until that root is configured") {
      val ceiling = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      val command = "echo hi > /tmp/terret-probe.log"
      drive(shellFrames(command, allowed = false)) { (harness, seen) =>
        turn(harness, seen, ceiling).flatMap { (refusedEvents, _) =>
          ZIO.fromEither(Workspace.from("/tmp")).orDieWith(_ => new IllegalArgumentException("workspace")).flatMap {
            scratch =>
              drive(shellFrames(command, allowed = true), Chunk(scratch)) { (opened, allowedSeen) =>
                turn(opened, allowedSeen, ceiling).map { (allowed, record) =>
                  assertTrue(
                    refusedEvents.exists {
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
      drive(shellFrames("echo hi > /dev/null", allowed = true)) { (harness, seen) =>
        turn(harness, seen, run).map { (_, record) =>
          assertTrue(record.payloads.exists(option(_, "allow-once")))
        }
      }
    },
    test("a refused use_tool web_fetch names the clamp and the turn completes") {
      checkRecorded(
        "request-permission-web-fetch.json",
        "clamped by server policy",
        "web_fetch needs Network, clamped by server policy",
        "use_tool",
      )
    },
    test("a refused use_tool web_search names the clamp and the turn completes") {
      checkRecorded(
        "request-permission-web-search.json",
        "clamped by server policy",
        "web_search needs Network, clamped by server policy",
        "use_tool",
      )
    },
    test("a refused write outside the workspace names the path on the tool update") {
      checkWrite("request-permission-heredoc.json")
    },
    test("a transport failure after a permission request ends the turn") {
      fetchTurn(After.Break(HarnessFailure.Protocol(ProtocolFault.Malformed))).map { (exit, record) =>
        assertTrue(
          failed(exit, HarnessFailure.Protocol(ProtocolFault.Malformed)),
          record.payloads.exists(option(_, "reject-once")),
          record.payloads.exists(followup(_, fetchReason)),
        )
      }
    },
    test("no tool update after a permission request ends the turn") {
      fetchTurn(After.Eof).map { (exit, record) =>
        assertTrue(
          failed(exit, HarnessFailure.Protocol(ProtocolFault.Incomplete)),
          record.payloads.exists(option(_, "reject-once")),
          record.payloads.exists(followup(_, fetchReason)),
        )
      }
    },
    test("a failed permission response ends the turn") {
      fetchTurn(After.Eof, failPermission = true, full = true).map { (exit, record) =>
        assertTrue(
          failed(exit, HarnessFailure.Protocol(ProtocolFault.Rejected(-1))),
          record.payloads.exists(option(_, "reject-once")),
          record.payloads.exists(followup(_, fetchReason)),
        )
      }
    },
    test("cancel tells the agent") {
      drive(cancelFrames, after = After.Park) { (harness, seen) =>
        for
          root   <- workspace
          id     <- harness.open(root, Ceiling.all, PermissionPolicy.Inherit)
          parked <- Promise.make[Nothing, Unit]
          fiber  <- Retain.parked.locally(_ => parked.succeed(()).unit) {
            harness.prompt(id, Prompt("stay")).runCollect.fork
          }
          _      <- parked.await
          _      <- harness.cancel(id)
          got    <- fiber.join
          record <- seen.get
        yield assertTrue(
          got.exists {
            case Event.Thought("hmm") => true
            case _                    => false
          },
          got.exists {
            case Event.TurnFinished(TurnOutcome.Cancelled) => true
            case _                                         => false
          },
          record.methods.contains("session/cancel"),
        )
      }
    },
  ) @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private val recordedCeiling =
    Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))

  private val fetchReason = "web_fetch needs Network, clamped by server policy"

  private enum After:
    case Eof
    case Break(failure: HarnessFailure)
    case Park

  private def checkRecorded(resource: String, phrase: String, reason: String, avoided: String) =
    LiveBodies.json(resource).orDie.flatMap { params =>
      recorded(params, phrase, avoided).map { (got, record) =>
        refused(got, record, phrase, reason)
      }
    }

  private def checkWrite(resource: String) =
    LiveBodies.json(resource).orDie.flatMap { params =>
      filePath(params) match
        case None       => ZIO.dieMessage("recorded write frame has no file_path")
        case Some(path) =>
          val reason = s"write writes $path, outside the workspace"
          recorded(params, "outside the workspace", "write").map { (got, record) =>
            refused(got, record, "outside the workspace", reason)
          }
    }

  private def recorded(params: Json, phrase: String, avoided: String) =
    callId(params).flatMap { id =>
      drive(replayFrames(params, id, phrase, avoided)) { (harness, seen) =>
        for
          root    <- workspace
          session <- harness.open(root, recordedCeiling, PermissionPolicy.Clamp(recordedCeiling))
          got     <- harness.prompt(session, Prompt("go")).runCollect
          record  <- seen.get
        yield (got, record)
      }
    }

  private def fetchTurn(after: After, failPermission: Boolean = false, full: Boolean = false) =
    LiveBodies.json("request-permission-web-fetch.json").orDie.flatMap { params =>
      callId(params).flatMap { id =>
        val phrase = "clamped by server policy"
        val frames =
          if full then replayFrames(params, id, phrase, "use_tool")
          else opening(params, id, phrase)
        promptExit(frames, after, failPermission)
      }
    }

  private def promptExit(frames: Chunk[Json], after: After, failPermission: Boolean) =
    drive(frames, after = after, failPermission = failPermission) { (harness, seen) =>
      for
        root   <- workspace
        id     <- harness.open(root, recordedCeiling, PermissionPolicy.Clamp(recordedCeiling))
        exit   <- harness.prompt(id, Prompt("go")).runCollect.exit
        record <- seen.get
      yield (exit, record)
    }

  private def refused(got: Chunk[Event], record: Seen, phrase: String, reason: String) =
    assertTrue(
      record.payloads.exists(option(_, "reject-once")),
      record.payloads.exists(followup(_, reason)),
      refusalText(got, reason),
      thoughtHas(got, phrase),
      assistantHas(got, phrase),
      !thoughtHas(got, reason),
      !assistantHas(got, reason),
      finished(got, TurnOutcome.Completed),
      !finished(got, TurnOutcome.Cancelled),
    )

  private def failed(exit: Exit[HarnessFailure, Chunk[Event]], failure: HarnessFailure): Boolean =
    exit.causeOption.exists(_.failureOption.contains(failure))

  private def refusalText(events: Chunk[Event], needle: String): Boolean =
    events.exists {
      case Event.ToolUpdated(call) =>
        call.input match
          case Json.Str(text) => text.contains(needle)
          case _              => false
      case _ => false
    }

  private def thoughtHas(events: Chunk[Event], needle: String): Boolean =
    events.exists {
      case Event.Thought(text) => text.contains(needle)
      case _                   => false
    }

  private def assistantHas(events: Chunk[Event], needle: String): Boolean =
    events.exists {
      case Event.Assistant(text) => text.contains(needle)
      case _                     => false
    }

  private def finished(events: Chunk[Event], outcome: TurnOutcome): Boolean =
    events.exists {
      case Event.TurnFinished(found) => found == outcome
      case _                         => false
    }

  private def turn(harness: GrokHarness, seen: Ref[Seen], ceiling: Ceiling) =
    for
      root   <- workspace
      id     <- harness.open(root, ceiling, PermissionPolicy.Clamp(ceiling))
      got    <- harness.prompt(id, Prompt("go")).runCollect
      record <- seen.get
    yield (got, record)

  private def drive[A](
      frames: Chunk[Json],
      roots: Chunk[Workspace] = Chunk.empty,
      after: After = After.Eof,
      failPermission: Boolean = false,
  )(use: (GrokHarness, Ref[Seen]) => ZIO[Any, HarnessFailure, A]): ZIO[Any, HarnessFailure, A] =
    ZIO.scoped {
      (for
        pipe    <- ZIO.service[RpcPipe]
        acp     <- ZIO.service[AcpOpen]
        spawner <- ZIO.service[Spawner]
        pending <- Ref.make(frames)
        seen    <- Ref.make(Seen(Chunk.empty, Chunk.empty))
        _       <- stub[AcpOpen](
          _.open(
            any[Workspace],
            any[PermissionPolicy],
            any[Ceiling],
            any[McpPolicy],
            any[NetworkPolicy],
            any[Option[ModelKey]],
          )
        ) { (_, _, _, _, _, _) =>
          ZIO.succeed(pipe)
        }
        _ <- stub[RpcPipe](_.read)(pull(pending, after))
        _ <- stub[RpcPipe](_.write(any[Json])) { json =>
          record(seen, json) *> rejectPermission(json, failPermission)
        }
        _       <- stub[RpcPipe](_.close)(ZIO.unit)
        harness <- GrokHarness.make(GrokConfig.stdio, spawner, acp, roots)
        result  <- use(harness, seen)
      yield result).provideSome[Scope](stubbed[RpcPipe], stubbed[AcpOpen], stubbed[Spawner])
    }

  private def pull(pending: Ref[Chunk[Json]], after: After): ZIO[Any, HarnessFailure, Option[Json]] =
    pending.modify { waiting =>
      waiting.headOption match
        case Some(head) => (ZIO.succeed(Some(head)), waiting.drop(1))
        case None       => (ended(after), waiting)
    }.flatten

  private def ended(after: After): ZIO[Any, HarnessFailure, Option[Json]] =
    after match
      case After.Eof            => ZIO.succeed(None)
      case After.Break(failure) => ZIO.fail(failure)
      case After.Park           => ZIO.never

  private def record(seen: Ref[Seen], json: Json): UIO[Unit] =
    Wire.parse(json) match
      case Right(Wire.Call(_, method, _)) =>
        seen.update(found => found.copy(methods = found.methods :+ method))
      case Right(Wire.Note(method, _)) =>
        seen.update(found => found.copy(methods = found.methods :+ method))
      case Right(Wire.Ok(_, result)) =>
        seen.update(found => found.copy(payloads = found.payloads :+ result))
      case Right(Wire.Err(_, code)) =>
        seen.update(found => found.copy(payloads = found.payloads :+ Json.Obj("code" -> Json.Num(BigDecimal(code)))))
      case Left(_) =>
        ZIO.unit

  private def rejectPermission(json: Json, fail: Boolean): ZIO[Any, HarnessFailure, Unit] =
    if fail && permissionWrite(json) then ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Rejected(-1)))
    else ZIO.unit

  private def permissionWrite(json: Json): Boolean =
    Wire.parse(json) match
      case Right(Wire.Ok(_, result: Json.Obj)) =>
        JsonFields.field(result, "outcome").isDefined || hasFollowup(result)
      case _ => false

  private def hasFollowup(result: Json.Obj): Boolean =
    JsonFields
      .field(result, "_meta")
      .flatMap(JsonFields.obj)
      .flatMap(meta => JsonFields.field(meta, "followup_message"))
      .isDefined

  private def callId(params: Json): UIO[String] =
    toolObject(params).flatMap(idOf) match
      case Some(id) => ZIO.succeed(id)
      case None     => ZIO.dieMessage("recorded permission frame has no toolCallId")

  private def handshake: Chunk[Json] =
    Chunk(
      ok(1, Json.Obj("protocolVersion" -> Json.Num(BigDecimal(1)))),
      ok(2, Json.Obj("sessionId" -> Json.Str("sess-1"))),
    )

  private def closedFrames: Chunk[Json] =
    handshake ++ Chunk(
      update("agent_message_chunk", "welcome back"),
      tool,
      ok(3, Json.Obj("sessionId" -> Json.Str("sess-1"))),
    ) ++ sessionPrompt(4)

  private def inheritFrames: Chunk[Json] =
    handshake ++ sessionPrompt(3)

  private def cancelFrames: Chunk[Json] =
    handshake ++ Chunk(update("agent_thought_chunk", "hmm"))

  private def sessionPrompt(stopId: Int): Chunk[Json] =
    Chunk(
      update("agent_thought_chunk", "hmm"),
      update("agent_message_chunk", "hello"),
      Wire.request(Json.Str("ping-1"), "custom/ping", Json.Obj()),
      permission,
      toolUpdate,
      ok(stopId, Json.Obj("stopReason" -> Json.Str("end_turn"))),
    )

  private def shellFrames(command: String, allowed: Boolean): Chunk[Json] =
    val followed = if allowed then shellOutput else shellFailed
    handshake ++ Chunk(
      shellCall(command),
      shellPermission(command),
      followed,
      ok(3, Json.Obj("stopReason" -> Json.Str("end_turn"))),
    )

  private def opening(params: Json, id: String, phrase: String): Chunk[Json] =
    handshake ++ Chunk(
      update("agent_thought_chunk", phrase),
      startedCall(id, params),
      Wire.request(Json.Str("perm-recorded"), "session/request_permission", params),
    )

  private def replayFrames(params: Json, id: String, phrase: String, avoided: String): Chunk[Json] =
    opening(params, id, phrase) ++ Chunk(
      failedCall(id, avoided),
      update("agent_message_chunk", phrase),
      ok(3, Json.Obj("stopReason" -> Json.Str("end_turn"))),
    )

  private def ok(id: Int, result: Json): Json =
    Wire.ok(Json.Num(BigDecimal(id)), result)

  private def startedCall(id: String, params: Json): Json =
    val call  = toolObject(params).getOrElse(Json.Obj())
    val title = JsonFields.str(call, "title").filter(_.nonEmpty).getOrElse("tool")
    val raw   = JsonFields.field(call, "rawInput").getOrElse(Json.Obj())
    val kind  = JsonFields.str(call, "kind").filter(_.nonEmpty)
    val base  = Chunk(
      "sessionUpdate" -> Json.Str("tool_call"),
      "toolCallId"    -> Json.Str(id),
      "title"         -> Json.Str(title),
      "rawInput"      -> raw,
    )
    val fields = kind match
      case Some(value) => base :+ ("kind" -> Json.Str(value))
      case None        => base
    Wire.notify(
      "session/update",
      Json.Obj(
        "sessionId" -> Json.Str("sess-1"),
        "update"    -> Json.Obj(fields*),
      ),
    )

  private def failedCall(id: String, avoided: String): Json =
    Wire.notify(
      "session/update",
      Json.Obj(
        "sessionId" -> Json.Str("sess-1"),
        "update"    -> Json.Obj(
          "sessionUpdate" -> Json.Str("tool_call_update"),
          "toolCallId"    -> Json.Str(id),
          "status"        -> Json.Str("failed"),
          "content"       -> Json.Arr(
            Json.Obj(
              "type"    -> Json.Str("content"),
              "content" -> Json.Obj(
                "type" -> Json.Str("text"),
                "text" -> Json.Str(
                  s"The user elected to avoid running the $avoided tool. The tool was not executed. Please refer to the user's message for next steps."
                ),
              ),
            )
          ),
        ),
      ),
    )

  private def toolObject(params: Json): Option[Json.Obj] =
    params match
      case obj: Json.Obj => JsonFields.field(obj, "toolCall").flatMap(JsonFields.obj)
      case _             => None

  private def idOf(call: Json.Obj): Option[String] =
    JsonFields.str(call, "toolCallId").filter(_.nonEmpty)

  private def filePath(params: Json): Option[String] =
    toolObject(params)
      .flatMap(call => JsonFields.field(call, "rawInput"))
      .flatMap(JsonFields.obj)
      .flatMap(raw => JsonFields.str(raw, "file_path").filter(_.nonEmpty))

  private def workspace =
    ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))

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

  private val recordedOptions = Json.Arr(
    Json.Obj("optionId" -> Json.Str("always-allow"), "kind"  -> Json.Str("allow_always")),
    Json.Obj("optionId" -> Json.Str("allow-once"), "kind"    -> Json.Str("allow_once")),
    Json.Obj("optionId" -> Json.Str("reject-once"), "kind"   -> Json.Str("reject_once")),
    Json.Obj("optionId" -> Json.Str("reject-always"), "kind" -> Json.Str("reject_always")),
  )

  private val permission: Json =
    Wire.request(
      Json.Str("perm-1"),
      "session/request_permission",
      Json.Obj(
        "toolCall" -> Json
          .Obj("toolCallId" -> Json.Str("call_1"), "title" -> Json.Str("Edit"), "kind" -> Json.Str("edit")),
        "options" -> recordedOptions,
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
        "options" -> recordedOptions,
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

  private def followup(json: Json, needle: String): Boolean =
    json match
      case obj: Json.Obj =>
        JsonFields.field(obj, "_meta").flatMap(JsonFields.obj) match
          case Some(meta) => JsonFields.str(meta, "followup_message").exists(_.contains(needle))
          case None       => false
      case _ => false

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
