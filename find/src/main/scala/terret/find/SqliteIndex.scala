package terret.find

import saferis.*
import saferis.sqlite.given
import saferis.sqlite.jdbc.SqliteJdbc

import org.sqlite.SQLiteDataSource
import zio.{Chunk, IO, Scope, UIO, ZIO, ZLayer}

import java.nio.file.{Files, Path}
import javax.sql.DataSource

/** SQLite index. Lexical rank is FTS5 `bm25`, negated so a higher score is better. Vectors are scored in Scala. */
object SqliteIndex:
  val engine: LexicalEngine = LexicalEngine.Fts5

  def layer(file: Path): ZLayer[Any, FindError, Index] =
    ZLayer.fromZIO:
      val source = fileSource(file)
      for
        session <- ZIO.service[SqlSession].provide(ZLayer.succeed[DataSource](source) >>> SqliteJdbc.layer())
        _       <- Schema.ensure.provide(ZLayer.succeed(session)).mapError(storage)
      yield Live(session)

  /** A fresh file, removed with its WAL siblings when the scope closes. */
  def temporary: ZLayer[Scope, FindError, Index] =
    ZLayer.scoped:
      for
        file <- ZIO
          .acquireRelease(ZIO.attemptBlocking(Files.createTempFile("terret-find-", ".db")).mapError(io))(remove)
        index <- ZIO.service[Index].provide(layer(file))
      yield index

  private def fileSource(file: Path): SQLiteDataSource =
    val source = SQLiteDataSource()
    source.setUrl(s"jdbc:sqlite:$file")
    source

  private def remove(file: Path): UIO[Unit] =
    ZIO.foreachDiscard(Chunk("", "-wal", "-shm")): suffix =>
      ZIO.attemptBlocking(Files.deleteIfExists(Path.of(s"$file$suffix"))).ignore

  private def storage(error: SaferisError): FindError =
    FindError.Storage(error.message)

  private def io(thrown: Throwable): FindError =
    FindError.Storage(ModelHome.detail(thrown))

  private def positive(limit: Int): IO[FindError, Int] =
    if limit > 0 then ZIO.succeed(limit) else ZIO.fail(FindError.NonPositive(limit))

  private final class Live(session: SqlSession) extends Index:
    def put(entry: Entry): IO[FindError, Unit] =
      ZIO
        .fromEither(Stored.encode(entry))
        .flatMap: stored =>
          db:
            transact:
              for
                _ <- dml.deleteWhere[StoredTool](sql"id = ${entry.key}")
                _ <- dml.insert(stored)
              yield ()

    def lexical(text: String, limit: Int): IO[FindError, Chunk[Hit]] =
      positive(limit).flatMap: count =>
        FtsQuery.matchOf(text) match
          case None       => ZIO.succeed(Chunk.empty)
          case Some(expr) =>
            db(sql"""select tool.id as name, cast(-bm25(tool_fts) as double) as score
                     from tool
                     join tool_fts on tool.rowid = tool_fts.rowid
                     where tool_fts match $expr""".query[Ranked]).map: rows =>
              rows.map(row => Hit(row.name, row.score)).sortBy(hit => (-hit.score, hit.name)).take(count)

    def nearest(query: Embedding, limit: Int): IO[FindError, Chunk[Hit]] =
      positive(limit).flatMap: count =>
        db(sql"select * from tool".query[StoredTool]).flatMap: rows =>
          ZIO
            .foreach(rows)(stored => ZIO.fromEither(Stored.decode(stored)))
            .map: entries =>
              entries
                .foldLeft(Chunk.empty[Hit])((acc, entry) =>
                  score(query, entry) match
                    case Some(value) => acc :+ Hit(entry.key, value)
                    case None        => acc
                )
                .sortBy(hit => (-hit.score, hit.name))
                .take(count)

    def lookup(name: String): IO[FindError, Option[Entry]] =
      db(sql"select * from tool where id = $name".queryOne[StoredTool]).flatMap:
        case None         => ZIO.succeed(None)
        case Some(stored) => ZIO.fromEither(Stored.decode(stored)).map(Some(_))

    private def score(query: Embedding, entry: Entry): Option[Double] =
      if entry.embedding.model != query.model || entry.embedding.dims != query.dims then None
      else VectorMath.cosine(query.values, entry.embedding.values).toOption

    private def db[A](effect: ZIO[SqlSession, SaferisError, A]): IO[FindError, A] =
      effect.provide(ZLayer.succeed(session)).mapError(storage)
  end Live

  /** FTS5 external content. A delete is the command `'delete'`, not a SQL `DELETE` against the virtual table. */
  private object Schema:
    def ensure: ZIO[SqlSession, SaferisError, Unit] =
      for
        _ <- ddl.createTable[StoredTool]()
        _ <- fts.execute
        _ <- afterInsert.execute
        _ <- afterDelete.execute
        _ <- afterUpdate.execute
      yield ()

    private val fts =
      sql"""create virtual table if not exists tool_fts using fts5(
            id, server, tool, description,
            content='tool', content_rowid='rowid'
          )"""

    private val afterInsert =
      sql"""create trigger if not exists tool_ai after insert on tool begin
              insert into tool_fts(rowid, id, server, tool, description)
              values (new.rowid, new.id, new.server, new.tool, new.description);
            end"""

    private val afterDelete =
      sql"""create trigger if not exists tool_ad after delete on tool begin
              insert into tool_fts(tool_fts, rowid, id, server, tool, description)
              values ('delete', old.rowid, old.id, old.server, old.tool, old.description);
            end"""

    private val afterUpdate =
      sql"""create trigger if not exists tool_au after update on tool begin
              insert into tool_fts(tool_fts, rowid, id, server, tool, description)
              values ('delete', old.rowid, old.id, old.server, old.tool, old.description);
              insert into tool_fts(rowid, id, server, tool, description)
              values (new.rowid, new.id, new.server, new.tool, new.description);
            end"""
  end Schema
end SqliteIndex

@tableName("tool")
private[find] final case class StoredTool(
    @key id: String,
    server: String,
    tool: String,
    description: String,
    model: String,
    dims: Int,
    vector: zio.Chunk[Byte],
) derives Table

private[find] final case class Ranked(name: String, score: Double) derives Table

private[find] object Stored:
  def encode(entry: Entry): Either[FindError, StoredTool] =
    Right(
      StoredTool(
        entry.key,
        entry.server.fold("")(_.value),
        entry.tool.value,
        entry.description,
        entry.embedding.model.value,
        entry.embedding.dims,
        VectorBytes.encode(entry.embedding.values),
      )
    )

  def decode(stored: StoredTool): Either[FindError, Entry] =
    for
      values <- VectorBytes.decode(stored.vector)
      _      <- check(values.length, stored.dims)
      server <- serverOf(stored.server)
      tool   <- ToolId.from(stored.tool).left.map(_ => FindError.Storage("blank tool"))
    yield Entry(server, tool, stored.description, Embedding(ModelId(stored.model), values))

  private def check(length: Int, dims: Int): Either[FindError, Unit] =
    if length == dims then Right(()) else Left(FindError.DimMismatch(length, dims))

  private def serverOf(raw: String): Either[FindError, Option[ServerName]] =
    if raw.isEmpty then Right(None)
    else
      ServerName.from(raw) match
        case Left(_)     => Left(FindError.Storage("blank server"))
        case Right(name) => Right(Some(name))
end Stored
