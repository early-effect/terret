package terret.find

/** A tool `find` can return.
  *
  * `server` is empty for a search fixture. Those tools are not callable. A catalog tool carries the server that listed
  * it.
  */
final case class ToolRef(
    server: Option[ServerName],
    tool: ToolId,
    description: String,
)
