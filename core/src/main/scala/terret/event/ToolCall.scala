package terret.event

import zio.json.ast.Json

final case class ToolCall(id: CallId, name: ToolName, input: Json)
