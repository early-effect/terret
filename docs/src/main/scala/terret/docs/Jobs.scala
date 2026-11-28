package terret.docs

import specular.*
import terret.Terret
import terret.app.ProcessConfig
import terret.config.TerretConfig
import terret.hooks.Hooks
import terret.mcp.{TerretMcp, TerretMcpError}
import terret.memory.{InMemory, Script}
import terret.permission.PermissionPolicy
import zio.ZIO
import zio.test.*

/** The host catalog. The names are the jobs, not a harness. */
object Jobs extends DocSpec:
  def doc = page("Jobs")(
    md"""
A host attaches to these jobs. The names are the jobs, not a harness.

| Job | Role |
| --- | --- |
| `open` | Start a session on a workspace. Optional `model` must be a `[model.<name>]` key in the grok config. A grok process terret spawns is started with `--model`. An unknown name fails and lists the known keys. The result reports the chosen model. Omit `model` to keep grok's default. |
| `run` | Accept one prompt and return while the turn is running. The turn keeps going if that call is cancelled. |
| `status` | The poll: it stays `Running` until the turn ends, and then it carries the outcome. |
| `resume` | Replay the retained log. It does not send a prompt. |
| `export` | Return that log, including whatever has been retained while the turn is still running. The Scala method is `transcript`, because `export` is a keyword. Optional `after`, `milestones`, and `latest` are described on [Events](events.html). |
| `cancel` | The only stop. |
| `find` | Search the MCP catalog `.mcp.json` defines. Hits are tools the `terret.mcp` grant allows. `withheld` names a few stronger matches that were refused, and how to enable them. An empty grant returns no hits and hint `NoGrants`. |
| `call` | Run one tool. `arguments` is a JSON object as text. A tool with no server is `NotCallable`. A missing grant is `NotGranted`. A granted server that `.mcp.json` does not define is `UnknownServer`. |

`list` is `GET /sessions` and is not an MCP tool. It returns each session this process holds, with
its state. `GET /watch` is served only on the tunnel listener and reads the feed. Steering is POST
on that listener: `POST /watch/{id}/run`, `/cancel`, `/resume`, and `POST /watch/open`. The local
listener does not serve those paths.

The gh tools run `gh` in the session workspace. In a linked worktree they set `GIT_DIR` to that
worktree's admin directory and `GIT_COMMON_DIR` to the shared git directory, so `git` sees the
repository. A git directory outside the project root is refused. `terret.gh` is empty unless set. `read` and `write`
are separate grants. `write` does not grant read. A missing grant is `GhDenied` and names that
grant, for example `pr list needs a gh read grant in terret.gh`. Merge requires a method. It is
not the default, and it does not enable auto-merge.

| Tool | Grant | Role |
| --- | --- | --- |
| `gh_pr_list` | `read` | List pull requests. |
| `gh_pr_view` | `read` | View a pull request. |
| `gh_pr_checks` | `read` | View pull request checks. |
| `gh_issue_list` | `read` | List issues. |
| `gh_issue_view` | `read` | View an issue. |
| `gh_run_view` | `read` | View a workflow run. |
| `gh_pr_create` | `write` | Create a pull request. |
| `gh_pr_comment` | `write` | Comment on a pull request. |
| `gh_pr_merge` | `write` | Merge a pull request. Merge needs `write` and is never the default. |
| `gh_issue_create` | `write` | Create an issue. |
| `gh_issue_comment` | `write` | Comment on an issue. |

`follow` is a subscription, not a job. The [Events](events.html) page covers milestones, activity,
cursors, and webhook TTL.

An empty server version is rejected before the catalog is built.
""",
    exampleZIO {
      ZIO.scoped {
        for
          memory  <- InMemory.make(Script.echo)
          session <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, None), Hooks.identity)
        yield TerretMcp
          .api(session, ProcessConfig.local.version)
          .map { api =>
            (
              api.ops.map(_.endpoint.doc.toolName).toSet,
              api.subscriptions.map(_.subscription.doc.toolName).toSet,
            )
          }
      }
    }.assert { built =>
      assertTrue(
        built == Right(
          (
            Set(
              "open",
              "list",
              "run",
              "status",
              "resume",
              "export",
              "cancel",
              "find",
              "call",
              "gh_pr_list",
              "gh_pr_view",
              "gh_pr_checks",
              "gh_issue_list",
              "gh_issue_view",
              "gh_run_view",
              "gh_pr_create",
              "gh_pr_comment",
              "gh_pr_merge",
              "gh_issue_create",
              "gh_issue_comment",
            ),
            Set("follow"),
          )
        )
      )
    },
    exampleZIO {
      ZIO.scoped {
        for
          memory  <- InMemory.make(Script.echo)
          session <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, None), Hooks.identity)
        yield TerretMcp.mcp(session, "")
      }
    }.assert { built =>
      assertTrue(built == Left(TerretMcpError.EmptyVersion))
    },
  )
