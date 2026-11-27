package terret.app.watch

import heddle.http.{Body, MediaType, Method, Request, Response, Status}
import heddle.route.{Handler, PathDsl, Routes}
import heddle.sse.Sse
import terret.Terret
import terret.app.{AccessJwt, AuthLog, AuthReason}
import terret.session.SessionId
import zio.durationInt
import zio.stream.ZStream
import zio.{Chunk, ZIO}

/** GET routes for the watch page. None of them call `open`, `run`, `cancel`, or `resume`. */
object WatchRoutes:
  def routes(terret: Terret, streams: WatchStreams): Routes[AuthLog, Nothing] =
    import PathDsl.*
    import heddle.route.string
    Routes(
      Method.GET / "watch.js"  -> Handler.succeed(script),
      Method.GET / "watch.css" -> Handler.succeed(style),
      Method.GET / "watch"     -> Handler.fromFunctionZIO(request => page(terret, None, request)),
      (Method.GET / "watch" / string("id")).handle((id, request) => page(terret, Some(id), request)),
      (Method.GET / "watch" / string("id") / "events").handle((id, request) => events(terret, streams, id, request)),
    )

  private def page(terret: Terret, id: Option[String], request: Request): ZIO[AuthLog, Nothing, Response] =
    WatchQuery.parse(request.query) match
      case Left(_)      => ZIO.succeed(WatchSecurity.seal(WatchSecurity.badQuery, hsts = true))
      case Right(query) =>
        id match
          case None      => render(terret, None, Chunk.empty, None, query)
          case Some(raw) =>
            SessionId.from(raw) match
              case Left(_)        => deny(request)
              case Right(session) =>
                terret
                  .status(session)
                  .foldZIO(
                    _ => deny(request),
                    _ =>
                      terret
                        .page(session, None)
                        .foldZIO(
                          _ => deny(request),
                          found =>
                            val rows   = WatchFeed.shape(found.events, query)
                            val stream = Some(WatchQuery.events(raw, query, found.cursor))
                            render(terret, Some(raw), rows, stream, query),
                        ),
                  )

  private def render(
      terret: Terret,
      selected: Option[String],
      feedRows: Chunk[WatchRow],
      stream: Option[String],
      query: WatchQuery,
  ): ZIO[Any, Nothing, Response] =
    for
      nonce <- Nonce.next
      rows  <- WatchPage.listed(terret)
      body  <- WatchPage.render(rows, selected, feedRows, query, stream)
    yield WatchSecurity.seal(WatchSecurity.html(WatchPage.document(body, nonce, query), nonce), hsts = true)

  private def events(
      terret: Terret,
      streams: WatchStreams,
      raw: String,
      request: Request,
  ): ZIO[AuthLog, Nothing, Response] =
    (WatchQuery.parse(request.query), cursor(request)) match
      case (Left(_), _) | (_, Left(_))  => ZIO.succeed(WatchSecurity.seal(WatchSecurity.badQuery, hsts = true))
      case (Right(query), Right(after)) =>
        SessionId.from(raw) match
          case Left(_)        => deny(request)
          case Right(session) =>
            terret
              .status(session)
              .foldZIO(
                _ => deny(request),
                _ =>
                  val viewer = request
                    .header("cf-access-jwt-assertion")
                    .filter(_.nonEmpty)
                    .map(AccessJwt.viewer)
                    .getOrElse("anonymous")
                  streams
                    .acquire(viewer)
                    .foldZIO(
                      _ => ZIO.succeed(WatchSecurity.seal(WatchSecurity.busy, hsts = true)),
                      _ => ZIO.succeed(stream(terret, streams, session, raw, query, after, viewer)),
                    ),
              )

  private def stream(
      terret: Terret,
      streams: WatchStreams,
      session: SessionId,
      raw: String,
      query: WatchQuery,
      after: Option[Long],
      viewer: String,
  ): Response =
    val url  = WatchQuery.events(raw, query, after)
    val data = terret
      .follow(session, after)
      .mapZIO { entry =>
        terret.page(session, None).map { found =>
          val rows = WatchFeed.shape(found.events, query)
          (entry.index, rows)
        }
      }
      .catchAll(_ => ZStream.empty)
      .mapZIO { (index, rows) =>
        WatchPage.feedHtml(rows, Some(raw), query, Some(url)).map { html =>
          WatchPatch.elements(html, index)
        }
      }
    val beats  = Sse.heartbeatEvery(15.seconds)
    val opened = ZStream.succeed(WatchPatch.signals("live"))
    // Heartbeats stop when follow stops. A failed follow must not leave the slot held by an idle tick.
    val events = (opened ++ data.mergeHaltLeft(beats)).ensuring(streams.release(viewer))
    WatchSecurity.seal(Response.fromServerSentEvents(events), hsts = true)

  /** `Last-Event-ID` wins over `after`. Both are exclusive. A negative or non-numeric cursor is a bad query. */
  private def cursor(request: Request): Either[WatchQueryError, Option[Long]] =
    val header = request.header("Last-Event-ID").map(_.trim).filter(_.nonEmpty)
    val query  = request.query.get("after").map(_.trim).filter(_.nonEmpty)
    header.orElse(query) match
      case None      => Right(None)
      case Some(raw) =>
        if raw.length > 32 then Left(WatchQueryError.Oversized("after"))
        else
          raw.toLongOption match
            case Some(value) if value >= 0 => Right(Some(value))
            case _                         => Left(WatchQueryError.Bad("after"))

  private def deny(request: Request): ZIO[AuthLog, Nothing, Response] =
    ZIO.serviceWithZIO[AuthLog](_.record(AuthReason.UnknownSession, request.path.render)).as {
      WatchSecurity.seal(WatchSecurity.denied, hsts = true)
    }

  private val script: Response =
    Response(Status.Ok).withBody(Body.text(WatchAssets.js, MediaType.JavascriptUtf8))

  private val style: Response =
    Response(Status.Ok).withBody(Body.text(WatchAssets.css, MediaType.CssUtf8))
end WatchRoutes
