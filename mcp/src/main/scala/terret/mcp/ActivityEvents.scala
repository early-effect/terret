package terret.mcp

import heddle.Schema
import heddle.mcp.protocol.EventName
import terret.activity.ActivityKind
import zio.json.*
import zio.Chunk

given Schema[ActivityKind]    = Schema.derived
given JsonCodec[ActivityKind] = JsonCodec.derived

final case class ActivityFilter(session: Option[String], kinds: Chunk[ActivityKind]) derives Schema, JsonCodec

final case class ActivityData(
    id: String,
    parent: String,
    session: String,
    kind: ActivityKind,
    call: Option[String],
    name: Option[String],
    status: Option[String],
    text: String,
    cursor: Option[Long],
    at: String,
) derives Schema,
      JsonCodec

object ActivityEvents:
  val name: EventName = EventName("session.activity")

  def accept(filter: ActivityFilter, data: ActivityData): Boolean =
    val session = filter.session.forall(_ == data.session)
    val kinds   = filter.kinds.isEmpty || filter.kinds.contains(data.kind)
    session && kinds
end ActivityEvents
