package terret.app.watch

import heddle.http.{Body, Form, MediaType, Method, Request, Response, Status}
import heddle.route.{Handler, PathDsl, Routes}
import heddle.sse.Sse
import terret.Terret
import terret.app.{AccessActor, AuthLog, AuthReason}
import terret.session.SessionId
import zio.durationInt
import zio.stream.ZStream
import zio.{Chunk, ZIO}

/** Watch routes. Reads are GET. `run`, `cancel`, `resume`, `open`, and the queue edits are POST. */
object WatchRoutes:
  def routes(
      terret: Terret,
      streams: WatchStreams,
      steer: Steer,
      csrf: CsrfTokens,
  ): Routes[AuthLog, Nothing] =
    import PathDsl.*
    import heddle.route.string
    Routes(
      Method.GET / "watch.js"  -> Handler.succeed(script),
      Method.GET / "watch.css" -> Handler.succeed(style),
      Method.GET / "watch"     -> Handler.fromFunctionZIO(request => page(terret, steer, csrf, None, request)),
      Method.POST / "watch" / "open" -> Handler.fromFunctionZIO(request => open(steer, csrf, request)),
      (Method.GET / "watch" / string("id")).handle((id, request) => page(terret, steer, csrf, Some(id), request)),
      (Method.GET / "watch" / string("id") / "events").handle((id, request) =>
        events(terret, streams, id, request)
      ),
      (Method.POST / "watch" / string("id") / "run").handle((id, request) => run(steer, csrf, id, request)),
      (Method.POST / "watch" / string("id") / "cancel").handle((id, request) => cancel(steer, csrf, id, request)),
      (Method.POST / "watch" / string("id") / "resume").handle((id, request) => resume(steer, csrf, id, request)),
      (Method.POST / "watch" / string("id") / "queue" / string("item")).handle((id, item, request) =>
        save(steer, csrf, id, item, request)
      ),
      (Method.POST / "watch" / string("id") / "queue" / string("item") / "drop").handle((id, item, request) =>
        drop(steer, csrf, id, item, request)
      ),
    )

  private def page(
      terret: Terret,
      steer: Steer,
      csrf: CsrfTokens,
      id: Option[String],
      request: Request,
  ): ZIO[AuthLog, Nothing, Response] =
    WatchQuery.parse(request.query) match
      case Left(_)      => ZIO.succeed(WatchSecurity.seal(WatchSecurity.badQuery, hsts = true))
      case Right(query) =>
        id match
          case None      => render(terret, csrf, None, Chunk.empty, None, query, request)
          case Some(raw) =>
            SessionId.from(raw) match
              case Left(_)        => deny(request)
              case Right(session) =>
                terret
                  .status(session)
                  .foldZIO(
                    _ => deny(request),
                    _ =>
                      terret.page(session, None).foldZIO(
                        _ => deny(request),
                        found =>
                          val rows   = WatchFeed.shape(found.events, query)
                          val stream = Some(WatchQuery.events(raw, query, found.cursor))
                          steer.queue(session).flatMap { queue =>
                            render(terret, csrf, Some(raw), rows, stream, query, request, queue)
                          },
                      ),
                  )

  private def render(
      terret: Terret,
      csrf: CsrfTokens,
      selected: Option[String],
      feedRows: Chunk[WatchRow],
      stream: Option[String],
      query: WatchQuery,
      request: Request,
      queue: Chunk[Queued] = Chunk.empty,
  ): ZIO[Any, Nothing, Response] =
    for
      nonce <- Nonce.next
      token <- csrf.issue(viewer(request))
      rows  <- WatchPage.listed(terret)
      body  <- WatchPage.render(rows, selected, feedRows, query, stream, token, queue)
    yield WatchSecurity.seal(WatchSecurity.html(WatchPage.document(body, nonce, query), nonce), hsts = true)

  private def open(steer: Steer, csrf: CsrfTokens, request: Request): ZIO[AuthLog, Nothing, Response] =
    mutate(csrf, request) { form =>
      WatchForm.open(form) match
        case Left(error) => ZIO.succeed(seal(failed(error)))
        case Right(spec) =>
          steer.open(spec, viewer(request)).foldZIO(
            error => refused(request, error),
            session => ZIO.succeed(back(Some(session.id.value), WatchForm.keys(form))),
          )
    }

  private def run(steer: Steer, csrf: CsrfTokens, raw: String, request: Request): ZIO[AuthLog, Nothing, Response] =
    mutate(csrf, request) { form =>
      known(raw, request) { id =>
        WatchForm.prompt(form) match
          case Left(error) => ZIO.succeed(seal(failed(error)))
          case Right(text) =>
            val who = viewer(request)
            val act =
              if form.get("mode").contains("stop") then steer.stopAndSend(id, text, who)
              else steer.send(id, text, who)
            act.foldZIO(
              error => refused(request, error),
              _ => ZIO.succeed(back(Some(raw), WatchForm.keys(form))),
            )
      }
    }

  private def cancel(steer: Steer, csrf: CsrfTokens, raw: String, request: Request): ZIO[AuthLog, Nothing, Response] =
    mutate(csrf, request) { form =>
      known(raw, request) { id =>
        steer.cancel(id, viewer(request)).foldZIO(
          error => refused(request, error),
          _ => ZIO.succeed(back(Some(raw), WatchForm.keys(form))),
        )
      }
    }

  private def resume(steer: Steer, csrf: CsrfTokens, raw: String, request: Request): ZIO[AuthLog, Nothing, Response] =
    mutate(csrf, request) { form =>
      known(raw, request) { id =>
        steer.resume(id, viewer(request)).foldZIO(
          error => refused(request, error),
          _ => ZIO.succeed(back(Some(raw), WatchForm.keys(form))),
        )
      }
    }

  private def save(
      steer: Steer,
      csrf: CsrfTokens,
      raw: String,
      item: String,
      request: Request,
  ): ZIO[AuthLog, Nothing, Response] =
    mutate(csrf, request) { form =>
      known(raw, request) { id =>
        WatchForm.prompt(form) match
          case Left(error) => ZIO.succeed(seal(failed(error)))
          case Right(text) =>
            steer.edit(id, item, text, viewer(request)).foldZIO(
              error => refused(request, error),
              _ => ZIO.succeed(back(Some(raw), WatchForm.keys(form))),
            )
      }
    }

  private def drop(
      steer: Steer,
      csrf: CsrfTokens,
      raw: String,
      item: String,
      request: Request,
  ): ZIO[AuthLog, Nothing, Response] =
    mutate(csrf, request) { form =>
      known(raw, request) { id =>
        steer.drop(id, item, viewer(request)).foldZIO(
          error => refused(request, error),
          _ => ZIO.succeed(back(Some(raw), WatchForm.keys(form))),
        )
      }
    }

  /** Cross-site and content type first, then the synchronizer token. A missing token is the same denial as a bad session.
    */
  private def mutate(csrf: CsrfTokens, request: Request)(
      use: Form => ZIO[AuthLog, Nothing, Response]
  ): ZIO[AuthLog, Nothing, Response] =
    WatchSecurity.screen(request) match
      case Some(refused) => ZIO.succeed(seal(refused))
      case None          =>
        request.body.asForm.foldZIO(
          _ => ZIO.succeed(seal(WatchSecurity.badQuery)),
          form =>
            csrf.accept(viewer(request), tokenOf(request, form)).flatMap {
              case false => deny(request)
              case true  => use(form)
            },
        )

  private def known(raw: String, request: Request)(
      use: SessionId => ZIO[AuthLog, Nothing, Response]
  ): ZIO[AuthLog, Nothing, Response] =
    SessionId.from(raw) match
      case Left(_)       => deny(request)
      case Right(session) => use(session)

  private def refused(request: Request, error: SteerError): ZIO[AuthLog, Nothing, Response] =
    error match
      case SteerError.Unknown(_) => deny(request)
      case other                 => ZIO.succeed(seal(failed(other)))

  private def failed(error: SteerError): Response =
    error match
      case SteerError.Unknown(_)     => WatchSecurity.denied
      case SteerError.Busy(_)        => Response.text("busy", Status.Conflict)
      case SteerError.EmptyPrompt    => Response.text("empty prompt", Status.BadRequest)
      case SteerError.Oversize       => Response.text("bad prompt", Status.BadRequest)
      case SteerError.UnknownItem(_) => Response.text("unknown item", Status.BadRequest)
      case SteerError.Workspace(_)   => Response.text("bad workspace", Status.BadRequest)
      case SteerError.BadAllow(_)    => Response.text("bad allow", Status.BadRequest)
      case SteerError.Mcp(_)         => Response.text("bad mcp", Status.BadRequest)
      case SteerError.Model(_, _)    => Response.text("unknown model", Status.BadRequest)
      case SteerError.Models         => Response.text("bad model", Status.BadGateway)
      case SteerError.Harness(_)     => Response.text("bad request", Status.BadRequest)

  private def back(id: Option[String], keys: Keys): Response =
    seal(Response.redirect(WatchQuery.path(id, WatchQuery.default.copy(keys = keys)), Status.SeeOther))

  private def seal(response: Response): Response =
    WatchSecurity.seal(response, hsts = true)

  private def tokenOf(request: Request, form: Form): Option[String] =
    request.header("X-CSRF-Token").filter(_.nonEmpty).orElse(form.get("csrf").filter(_.nonEmpty))

  private def viewer(request: Request): String =
    AccessActor.of(request.headers).getOrElse("anonymous")

  private def events(
      terret: Terret,
      streams: WatchStreams,
      raw: String,
      request: Request,
  ): ZIO[AuthLog, Nothing, Response] =
    (WatchQuery.parse(request.query), cursor(request)) match
      case (Left(_), _) | (_, Left(_))  => ZIO.succeed(seal(WatchSecurity.badQuery))
      case (Right(query), Right(after)) =>
        SessionId.from(raw) match
          case Left(_)        => deny(request)
          case Right(session) =>
            terret
              .status(session)
              .foldZIO(
                _ => deny(request),
                _ =>
                  val who = viewer(request)
                  streams
                    .acquire(who)
                    .foldZIO(
                      _ => ZIO.succeed(seal(WatchSecurity.busy)),
                      _ => ZIO.succeed(stream(terret, streams, session, raw, query, after, who)),
                    ),
              )

  private def stream(
      terret: Terret,
      streams: WatchStreams,
      session: SessionId,
      raw: String,
      query: WatchQuery,
      after: Option[Long],
      viewerName: String,
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
    val events = (opened ++ data.mergeHaltLeft(beats)).ensuring(streams.release(viewerName))
    seal(Response.fromServerSentEvents(events))

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
    ZIO
      .serviceWithZIO[AuthLog](_.record(AuthReason.UnknownSession, request.path.render, AccessActor.of(request.headers)))
      .as(seal(WatchSecurity.denied))

  private val script: Response =
    Response(Status.Ok).withBody(Body.text(WatchAssets.js, MediaType.JavascriptUtf8))

  private val style: Response =
    Response(Status.Ok).withBody(Body.text(WatchAssets.css, MediaType.CssUtf8))
end WatchRoutes
