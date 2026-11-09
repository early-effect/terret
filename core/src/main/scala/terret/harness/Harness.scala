package terret.harness

import terret.event.Event
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.transcript.Transcript
import terret.workspace.Workspace
import zio.stream.ZStream
import zio.IO

trait Harness:
  def open(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): IO[HarnessFailure, SessionId]
  def prompt(id: SessionId, prompt: Prompt): ZStream[Any, HarnessFailure, Event]
  def resume(id: SessionId): ZStream[Any, HarnessFailure, Event]
  def transcript(id: SessionId): IO[HarnessFailure, Transcript]
  def cancel(id: SessionId): IO[HarnessFailure, Unit]
end Harness
