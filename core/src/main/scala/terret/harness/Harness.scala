package terret.harness

import terret.event.Event
import terret.model.ModelKey
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.transcript.{Entry, Page, Transcript}
import terret.workspace.Workspace
import zio.stream.ZStream
import zio.{IO, UIO}

trait Harness:
  def open(
      workspace: Workspace,
      ceiling: Ceiling,
      policy: PermissionPolicy,
      model: Option[ModelKey] = None,
  ): IO[HarnessFailure, SessionId]
  def prompt(id: SessionId, prompt: Prompt): ZStream[Any, HarnessFailure, Event]
  def resume(id: SessionId): ZStream[Any, HarnessFailure, Event]
  def transcript(id: SessionId): IO[HarnessFailure, Transcript]
  def follow(id: SessionId, after: Option[Long]): ZStream[Any, HarnessFailure, Entry]
  def page(id: SessionId, after: Option[Long]): IO[HarnessFailure, Page]

  /** Entries appended after this belong to `turn`. The id is the session epoch. */
  def markTurn(id: SessionId, turn: Long): UIO[Unit]
  def cancel(id: SessionId): IO[HarnessFailure, Unit]

  /** Release every child this harness still owns. Idempotent. */
  def close: UIO[Unit]
end Harness
