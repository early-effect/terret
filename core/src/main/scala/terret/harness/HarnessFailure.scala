package terret.harness

import terret.session.SessionId

enum HarnessFailure:
  case UnknownSession(id: SessionId)
  case Cancelled
  case Exited(code: Int)
  case Protocol(fault: ProtocolFault)
end HarnessFailure

enum ProtocolFault:
  case Malformed
  case BindMissing
  case Rejected(code: Int)
  case Incomplete
  case Refused
end ProtocolFault
