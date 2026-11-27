package terret.github

enum PrState:
  case Open, Closed, Merged, All

  def wire: String = this match
    case Open   => "open"
    case Closed => "closed"
    case Merged => "merged"
    case All    => "all"
end PrState

enum IssueState:
  case Open, Closed, All

  def wire: String = this match
    case Open   => "open"
    case Closed => "closed"
    case All    => "all"
end IssueState

enum MergeMethod:
  case Merge, Squash, Rebase

  def flag: String = this match
    case Merge  => "--merge"
    case Squash => "--squash"
    case Rebase => "--rebase"
end MergeMethod

/** One gh operation. The grant is fixed by the case. Merge is a write and has no default method. */
enum GhOp:
  case PrList(limit: Int, state: PrState)
  case PrView(number: Int)
  case PrChecks(number: Int)
  case IssueList(limit: Int, state: IssueState)
  case IssueView(number: Int)
  case RunView(id: Long)
  case PrCreate(title: String, body: String, base: Option[String], head: Option[String])
  case PrComment(number: Int, body: String)
  case PrMerge(number: Int, method: MergeMethod)
  case IssueCreate(title: String, body: String)
  case IssueComment(number: Int, body: String)

  def label: String = this match
    case PrList(_, _)          => "pr list"
    case PrView(_)             => "pr view"
    case PrChecks(_)           => "pr checks"
    case IssueList(_, _)       => "issue list"
    case IssueView(_)          => "issue view"
    case RunView(_)            => "run view"
    case PrCreate(_, _, _, _)  => "pr create"
    case PrComment(_, _)       => "pr comment"
    case PrMerge(_, _)         => "pr merge"
    case IssueCreate(_, _)     => "issue create"
    case IssueComment(_, _)    => "issue comment"

  def grant: GhGrant = this match
    case PrList(_, _) | PrView(_) | PrChecks(_) | IssueList(_, _) | IssueView(_) | RunView(_) =>
      GhGrant.Read
    case PrCreate(_, _, _, _) | PrComment(_, _) | PrMerge(_, _) | IssueCreate(_, _) | IssueComment(_, _) =>
      GhGrant.Write

  /** `gh pr checks` exits 1 when a check failed and 8 while checks are pending. The JSON is still the read. */
  def accepts(code: Int): Boolean = this match
    case PrChecks(_)           => code == 0 || code == 1 || code == 8
    case PrList(_, _)          => code == 0
    case PrView(_)             => code == 0
    case IssueList(_, _)       => code == 0
    case IssueView(_)          => code == 0
    case RunView(_)            => code == 0
    case PrCreate(_, _, _, _)  => code == 0
    case PrComment(_, _)       => code == 0
    case PrMerge(_, _)         => code == 0
    case IssueCreate(_, _)     => code == 0
    case IssueComment(_, _)    => code == 0
end GhOp
