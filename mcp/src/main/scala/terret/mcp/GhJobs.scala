package terret.mcp

import heddle.*
import terret.Terret
import terret.github.*
import terret.milestone.Milestones
import terret.redact.Redact
import zio.IO
import zio.json.*

final case class GhText(text: String) derives Schema, JsonCodec

enum GhPrState derives Schema, JsonCodec:
  case Open, Closed, Merged, All
end GhPrState

enum GhIssueState derives Schema, JsonCodec:
  case Open, Closed, All
end GhIssueState

enum GhMerge derives Schema, JsonCodec:
  case Merge, Squash, Rebase
end GhMerge

final case class GhPrListJob(session: String, limit: Int, state: GhPrState) derives Schema, JsonCodec

final case class GhPrViewJob(session: String, number: Int) derives Schema, JsonCodec

final case class GhPrChecksJob(session: String, number: Int) derives Schema, JsonCodec

final case class GhIssueListJob(session: String, limit: Int, state: GhIssueState) derives Schema, JsonCodec

final case class GhIssueViewJob(session: String, number: Int) derives Schema, JsonCodec

final case class GhRunViewJob(session: String, id: Long) derives Schema, JsonCodec

final case class GhPrCreateJob(
    session: String,
    title: String,
    body: String,
    base: Option[String],
    head: Option[String],
) derives Schema,
      JsonCodec

final case class GhPrCommentJob(session: String, number: Int, body: String) derives Schema, JsonCodec

final case class GhPrMergeJob(session: String, number: Int, method: GhMerge) derives Schema, JsonCodec

final case class GhIssueCreateJob(session: String, title: String, body: String) derives Schema, JsonCodec

final case class GhIssueCommentJob(session: String, number: Int, body: String) derives Schema, JsonCodec

private[mcp] object GhJobs:
  def attach(api: Api[Milestones], terret: Terret, gh: Gh, access: GhAccess): Api[Milestones] =
    api
      .job(prList)(job => invoke(terret, gh, access, job.session, GhOp.PrList(job.limit, pull(job.state))))
      .job(prView)(job => invoke(terret, gh, access, job.session, GhOp.PrView(job.number)))
      .job(prChecks)(job => invoke(terret, gh, access, job.session, GhOp.PrChecks(job.number)))
      .job(issueList)(job => invoke(terret, gh, access, job.session, GhOp.IssueList(job.limit, issue(job.state))))
      .job(issueView)(job => invoke(terret, gh, access, job.session, GhOp.IssueView(job.number)))
      .job(runView)(job => invoke(terret, gh, access, job.session, GhOp.RunView(job.id)))
      .job(prCreate) { job =>
        invoke(terret, gh, access, job.session, GhOp.PrCreate(job.title, job.body, job.base, job.head))
      }
      .job(prComment)(job => invoke(terret, gh, access, job.session, GhOp.PrComment(job.number, job.body)))
      .job(prMerge)(job => invoke(terret, gh, access, job.session, GhOp.PrMerge(job.number, method(job.method))))
      .job(issueCreate)(job => invoke(terret, gh, access, job.session, GhOp.IssueCreate(job.title, job.body)))
      .job(issueComment)(job => invoke(terret, gh, access, job.session, GhOp.IssueComment(job.number, job.body)))

  private val read =
    "Needs a gh read grant in terret.gh. The command runs in the session workspace."

  private val write =
    "Needs a gh write grant in terret.gh. The command runs in the session workspace."

  private val prList =
    Jobs
      .fallible(Endpoint.post("gh" / "pr" / "list").in[GhPrListJob].out[GhText])
      .name("gh_pr_list")
      .summary(s"List pull requests. $read")
      .hints(Hint.ReadOnly, Hint.Idempotent)
      .mcp("gh_pr_list")

  private val prView =
    Jobs
      .fallible(Endpoint.post("gh" / "pr" / "view").in[GhPrViewJob].out[GhText])
      .name("gh_pr_view")
      .summary(s"View one pull request. $read")
      .hints(Hint.ReadOnly, Hint.Idempotent)
      .mcp("gh_pr_view")

  private val prChecks =
    Jobs
      .fallible(Endpoint.post("gh" / "pr" / "checks").in[GhPrChecksJob].out[GhText])
      .name("gh_pr_checks")
      .summary(s"View the checks on one pull request. $read")
      .hints(Hint.ReadOnly, Hint.Idempotent)
      .mcp("gh_pr_checks")

  private val issueList =
    Jobs
      .fallible(Endpoint.post("gh" / "issue" / "list").in[GhIssueListJob].out[GhText])
      .name("gh_issue_list")
      .summary(s"List issues. $read")
      .hints(Hint.ReadOnly, Hint.Idempotent)
      .mcp("gh_issue_list")

  private val issueView =
    Jobs
      .fallible(Endpoint.post("gh" / "issue" / "view").in[GhIssueViewJob].out[GhText])
      .name("gh_issue_view")
      .summary(s"View one issue. $read")
      .hints(Hint.ReadOnly, Hint.Idempotent)
      .mcp("gh_issue_view")

  private val runView =
    Jobs
      .fallible(Endpoint.post("gh" / "run" / "view").in[GhRunViewJob].out[GhText])
      .name("gh_run_view")
      .summary(s"View one workflow run. $read")
      .hints(Hint.ReadOnly, Hint.Idempotent)
      .mcp("gh_run_view")

  private val prCreate =
    Jobs
      .fallible(Endpoint.post("gh" / "pr" / "create").in[GhPrCreateJob].out[GhText])
      .name("gh_pr_create")
      .summary(s"Open a pull request. $write Does not merge.")
      .mcp("gh_pr_create")

  private val prComment =
    Jobs
      .fallible(Endpoint.post("gh" / "pr" / "comment").in[GhPrCommentJob].out[GhText])
      .name("gh_pr_comment")
      .summary(s"Comment on a pull request. $write")
      .mcp("gh_pr_comment")

  private val prMerge =
    Jobs
      .fallible(Endpoint.post("gh" / "pr" / "merge").in[GhPrMergeJob].out[GhText])
      .name("gh_pr_merge")
      .summary(
        s"Merge one pull request. $write The method is required. " +
          "This does not run unless the caller asks, and it never enables auto-merge."
      )
      .hints(Hint.Destructive)
      .mcp("gh_pr_merge")

  private val issueCreate =
    Jobs
      .fallible(Endpoint.post("gh" / "issue" / "create").in[GhIssueCreateJob].out[GhText])
      .name("gh_issue_create")
      .summary(s"Open an issue. $write")
      .mcp("gh_issue_create")

  private val issueComment =
    Jobs
      .fallible(Endpoint.post("gh" / "issue" / "comment").in[GhIssueCommentJob].out[GhText])
      .name("gh_issue_comment")
      .summary(s"Comment on an issue. $write")
      .mcp("gh_issue_comment")

  private def invoke(
      terret: Terret,
      gh: Gh,
      access: GhAccess,
      session: String,
      op: GhOp,
  ): IO[JobFailure, GhText] =
    for
      id   <- Jobs.sessionOf(session)
      held <- terret.status(id).mapError(JobFailure.from)
      text <- gh.perform(op, held.workspace, access).mapError(JobFailure.fromGh)
    yield GhText(Redact.text(text))

  private def pull(state: GhPrState): PrState =
    state match
      case GhPrState.Open   => PrState.Open
      case GhPrState.Closed => PrState.Closed
      case GhPrState.Merged => PrState.Merged
      case GhPrState.All    => PrState.All

  private def issue(state: GhIssueState): IssueState =
    state match
      case GhIssueState.Open   => IssueState.Open
      case GhIssueState.Closed => IssueState.Closed
      case GhIssueState.All    => IssueState.All

  private def method(kind: GhMerge): MergeMethod =
    kind match
      case GhMerge.Merge  => MergeMethod.Merge
      case GhMerge.Squash => MergeMethod.Squash
      case GhMerge.Rebase => MergeMethod.Rebase
end GhJobs
