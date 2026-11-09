package terret.transcript

import terret.event.Event
import terret.session.SessionId
import zio.Chunk

final case class Transcript(session: SessionId, events: Chunk[Event])
