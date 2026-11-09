package terret.app

enum Listen:
  case Stdio
  case Http(host: String, port: Int)
end Listen
