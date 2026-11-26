package terret.find

import zio.{Chunk, IO}

/** A stored tool and the passage vector of its description. */
final case class Entry(
    server: Option[ServerName],
    tool: ToolId,
    description: String,
    embedding: Embedding,
):
  /** Seeds use the tool name. A catalog tool is `server/tool`, so two servers can share a tool name. */
  def key: String =
    server match
      case None       => tool.value
      case Some(name) => s"${name.value}/${tool.value}"

  def ref: ToolRef = ToolRef(server, tool, description)
end Entry

/** One ranked hit. A higher score is a closer match. */
final case class Hit(name: String, score: Double)

/** Which lexical ranker the sqlite index uses. The probe of sqlite-jdbc accepted FTS5. */
enum LexicalEngine:
  case Fts5

/** Stores tools and returns them in rank order.
  *
  * `lexical` matches the text. `nearest` matches the vector, and only against rows from the same model and width.
  */
trait Index:
  def put(entry: Entry): IO[FindError, Unit]
  def lexical(text: String, limit: Int): IO[FindError, Chunk[Hit]]
  def nearest(query: Embedding, limit: Int): IO[FindError, Chunk[Hit]]
  def lookup(name: String): IO[FindError, Option[Entry]]

/** Quote FTS5 terms and OR them. A quoted term cannot change the query into an operator. */
object FtsQuery:
  def matchOf(text: String): Option[String] =
    val terms = token.findAllIn(text.toLowerCase(java.util.Locale.ROOT)).toList.distinct.sorted
    if terms.isEmpty then None else Some(terms.map(quote).mkString(" OR "))

  def quote(term: String): String =
    "\"" + term.replace("\"", "\"\"") + "\""

  private val token = "[A-Za-z0-9]{2,}".r
end FtsQuery
