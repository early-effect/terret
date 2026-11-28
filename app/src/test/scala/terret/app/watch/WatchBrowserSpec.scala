package terret.app.watch

import heddle.Server
import heddle.route.Routes
import heddle.server.Port
import terret.Terret
import terret.app.{AccessGate, AuthLog, Ingress}
import terret.mcp.EventsKey
import terret.config.TerretConfig
import terret.event.{CallId, Event, PlanStep, ToolCall, ToolName, TurnOutcome}
import zio.json.ast.Json
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.permission.PermissionPolicy
import terret.workspace.Workspace
import zio.test.*
import zio.durationInt
import zio.{Chunk, Promise, Scope, ZIO}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** A real browser against the tunnel ingress. Histos does not copy arbitrary environment variables into the suite, so
  * the drive stays off unless `target/watch-e2e/enable` exists. Shots stay off unless
  * `target/watch-shots/enable` exists. A one-line path in `target/watch-e2e/node` selects the binary. When that file
  * is absent, the drive uses `node` on `PATH`, which is what CI does. The default run is a no-op and is not a CI
  * browser job. The listener uses `AccessGate.open` so the drive does not need a Cloudflare JWT. Production still
  * mounts `/watch` only on the tunnel, and that tunnel still requires a JWT.
  */
object WatchBrowserSpec extends ZIOSpecDefault:
  private val repo   = Path.of(sys.env.getOrElse("HISTOS_REPO", ".")).toAbsolutePath.normalize
  private val enable = repo.resolve("target/watch-e2e/enable")
  private val shots  = repo.resolve("target/watch-shots/enable")

  def spec = suite("watch browser")(
    test("the page queues, stops, cancels, resumes, switches keys, and refuses a cross-site post") {
      if Files.isRegularFile(enable) then ZIO.scoped(drive)
      else ZIO.succeed(assertTrue(true))
    } @@ TestAspect.timeout(3.minutes) @@ TestAspect.withLiveClock,
    test("the page renders a rich feed at phone and desk width in both schemes") {
      if Files.isRegularFile(shots) then ZIO.scoped(portray)
      else ZIO.succeed(assertTrue(true))
    } @@ TestAspect.timeout(3.minutes) @@ TestAspect.withLiveClock,
  )

  private def watchKey: ZIO[Any, Nothing, EventsKey] =
    ZIO
      .fromEither(EventsKey.from("watch-test-key").toRight(new IllegalArgumentException("watch key")))
      .orDie

  private def drive: ZIO[Scope, Any, TestResult] =
    for
      scratch = repo.resolve("target/watch-e2e")
      _     <- ZIO.attempt(Files.createDirectories(scratch))
      dir   <- ZIO.attempt(Files.createTempDirectory(scratch, "root"))
      root  <- ZIO.fromEither(Workspace.from(dir.toString))
      hold  <- Promise.make[Nothing, Unit]
      memory <- InMemory.make(gated(hold))
      terret <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, Some(root)), Hooks.identity)
      streams <- WatchStreams.make
      csrf    <- CsrfTokens.make
      steer   <- Steer.make(terret, Chunk(root))
      watch = WatchRoutes.routes(terret, streams, steer, csrf)
      key <- watchKey
      bound <- ZIO.fromEither(Port.make(0))
      server <- Server
        .install(
          Ingress.tunnel(Routes.empty, watch, key),
          Server.Config.default.copy(host = "127.0.0.1", port = bound, http2 = false),
        )
        .provideSome[Scope](AccessGate.open ++ AuthLog.live)
      port   <- server.port
      scriptPath = scratch.resolve("drive.cjs")
      _      <- ZIO.attempt(Files.writeString(scriptPath, Drive))
      result <- runNode(repo, scriptPath, port, dir)
      (code, output) = result
    yield assertTrue(code == 0, output.contains("WATCH_E2E ok"))

  private def portray: ZIO[Scope, Any, TestResult] =
    for
      scratch = repo.resolve("target/watch-shots")
      _       <- ZIO.attempt(Files.createDirectories(scratch))
      dir     <- ZIO.attempt(Files.createTempDirectory(scratch, "root"))
      root    <- ZIO.fromEither(Workspace.from(dir.toString))
      hold    <- Promise.make[Nothing, Unit]
      memory  <- InMemory.make(scene(hold))
      terret  <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, Some(root)), Hooks.identity)
      streams <- WatchStreams.make
      csrf    <- CsrfTokens.make
      steer   <- Steer.make(terret, Chunk(root))
      watch = WatchRoutes.routes(terret, streams, steer, csrf)
      key   <- watchKey
      bound <- ZIO.fromEither(Port.make(0))
      server <- Server
        .install(
          Ingress.tunnel(Routes.empty, watch, key),
          Server.Config.default.copy(host = "127.0.0.1", port = bound, http2 = false),
        )
        .provideSome[Scope](AccessGate.open ++ AuthLog.live)
      port       <- server.port
      scriptPath = scratch.resolve("shots.cjs")
      _          <- ZIO.attempt(Files.writeString(scriptPath, Shots))
      result     <- runNode(repo, scriptPath, port, dir)
      (code, output) = result
    yield assertTrue(
      code == 0,
      output.contains("WATCH_SHOTS ok"),
      frames.forall(name => Files.isRegularFile(scratch.resolve(name))),
    )

  private val frames: List[String] = List(
    "empty-desk-light.png",
    "empty-phone-dark.png",
    "focus-desk-light.png",
    "feed-desk-light.png",
    "feed-desk-dark.png",
    "feed-phone-light.png",
    "feed-phone-dark.png",
    "denied-desk-dark.png",
    "ended-phone-light.png",
    "filter-desk-light.png",
    "filter-phone-light.png",
  )

  /** Prompt `scene` fills the feed. Prompt `block` holds the turn so a queue can sit behind it. */
  private def scene(hold: Promise[Nothing, Unit]): Script = Script { (_, prompt) =>
    val done = zio.stream.ZStream(Event.TurnFinished(TurnOutcome.Completed))
    if prompt.text == "block" then zio.stream.ZStream.fromZIO(hold.await).drain ++ done
    else if prompt.text == "scene" then
      val events = Chunk(
        Event.Thought("Weighing the layout before I touch the feed."),
        Event.Plan(Chunk(PlanStep("Keep the headers, then restyle the page."))),
        Event.ToolStarted(
          ToolCall(
            CallId("call_read"),
            ToolName("read_file"),
            Json.Obj("path" -> Json.Str("app/src/main/scala/terret/app/watch/WatchPage.scala")),
          )
        ),
        Event.ToolUpdated(
          ToolCall(
            CallId("call_read"),
            ToolName("read_file"),
            Json.Obj(
              "stdout" -> Json.Str("brand, rows, composer\n4 tests passed\n"),
              "status" -> Json.Str("completed"),
            ),
          )
        ),
        Event.Assistant("The page can stay on one origin and still feel finished."),
        Event.Refused("call_deny", "shell", "The command is outside the workspace."),
        Event.TurnFinished(TurnOutcome.Completed),
      )
      zio.stream.ZStream.fromChunk(events)
    else zio.stream.ZStream(Event.Assistant(prompt.text)) ++ done
  }

  private def gated(hold: Promise[Nothing, Unit]): Script = Script { (_, prompt) =>
    val done = zio.stream.ZStream(Event.TurnFinished(TurnOutcome.Completed))
    if prompt.text == "block" then zio.stream.ZStream.fromZIO(hold.await).drain ++ done
    else zio.stream.ZStream(Event.Assistant(prompt.text)) ++ done
  }

  private def runNode(
      repo: Path,
      script: Path,
      port: Int,
      root: Path,
  ): ZIO[Scope, Throwable, (Int, String)] =
    val command = new ProcessBuilder(nodeCommand, script.toString)
    val env     = command.environment()
    env.put("WATCH_URL", s"http://127.0.0.1:$port")
    env.put("WATCH_ROOT", root.toString)
    env.put("NODE_PATH", "/Users/russ/projects/early-effect/chekhov/node_modules")
    command.directory(repo.toFile)
    command.redirectErrorStream(true)
    ZIO.acquireRelease(ZIO.attempt(command.start())) { process =>
      ZIO.attempt(process.destroyForcibly()).ignore
    }.flatMap { process =>
      ZIO.attemptBlockingInterrupt {
        val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
        val code   = process.waitFor()
        (code, output)
      }
    }

  /** Histos keeps a short `PATH`. A one-line absolute path in `target/watch-e2e/node` selects the binary. */
  private def nodeCommand: String =
    val noted = repo.resolve("target/watch-e2e/node")
    if Files.isRegularFile(noted) then
      val line = Files.readString(noted).trim
      if line.nonEmpty then line else "node"
    else "node"

  private val Drive: String =
    """const { chromium, request } = require("playwright");
      |const fs = require("fs");
      |const path = require("path");
      |
      |const base = process.env.WATCH_URL;
      |const root = process.env.WATCH_ROOT;
      |const shot = path.resolve("target/watch-e2e");
      |
      |(async () => {
      |  const browser = await chromium.launch({ headless: true });
      |  const page = await browser.newPage({ viewport: { width: 1400, height: 900 } });
      |  page.setDefaultTimeout(15000);
      |  const notes = [];
      |  async function step(name, body) {
      |    try {
      |      await body();
      |      notes.push(name + " ok");
      |    } catch (error) {
      |      const file = path.join(shot, name + ".png");
      |      await page.screenshot({ path: file, fullPage: true }).catch(() => {});
      |      notes.push(name + " FAIL " + error);
      |      throw error;
      |    }
      |  }
      |  try {
      |    await step("load", async () => {
      |      const response = await page.goto(base + "/watch", { waitUntil: "domcontentloaded" });
      |      if (!response || response.status() !== 200) throw new Error("status " + (response && response.status()));
      |      await page.waitForSelector("[data-composer='live']");
      |    });
      |    await step("open", async () => {
      |      await page.fill("[data-field='workspace']", root);
      |      const nav = page.waitForNavigation({ waitUntil: "domcontentloaded" });
      |      await page.click("[data-control='new-session']");
      |      const response = await nav;
      |      const status = response ? response.status() : 0;
      |      const url = page.url();
      |      const text = await page.locator("body").innerText();
      |      const path = new URL(url).pathname;
      |      if (status !== 200 || !/^\/watch\/[^/]+$/.test(path) || path === "/watch/open") {
      |        throw new Error("status " + status + " url " + url + " body " + text.slice(0, 300));
      |      }
      |      if (url.includes("keys=plain")) throw new Error(url);
      |      await page.waitForSelector("[data-prompt='send']");
      |    });
      |    await step("feed", async () => {
      |      await page.fill("[data-prompt='send']", "hello");
      |      await Promise.all([
      |        page.waitForURL(/\/watch\/[^/?#]+/),
      |        page.click("button[name='mode'][value='send']"),
      |      ]);
      |      await page.waitForFunction(() => {
      |        const feed = document.querySelector("#feed");
      |        return feed && feed.textContent.includes("hello");
      |      });
      |    });
      |    await step("queue", async () => {
      |      await page.fill("[data-prompt='send']", "block");
      |      await Promise.all([
      |        page.waitForURL(/\/watch\/[^/?#]+/),
      |        page.click("button[name='mode'][value='send']"),
      |      ]);
      |      await page.waitForFunction(() => document.body.textContent.includes("Running"));
      |      await page.fill("[data-prompt='send']", "later");
      |      await Promise.all([
      |        page.waitForURL(/\/watch\/[^/?#]+/),
      |        page.click("button[name='mode'][value='send']"),
      |      ]);
      |      await page.waitForSelector("[data-queue-item]");
      |      const queued = await page.locator("[data-queue='prompts'] textarea").inputValue();
      |      if (queued.trim() !== "later") throw new Error(queued);
      |    });
      |    await step("drop", async () => {
      |      await Promise.all([
      |        page.waitForURL(/\/watch\/[^/?#]+/),
      |        page.click("[data-control='queue-drop']"),
      |      ]);
      |      await page.waitForFunction(() => !document.querySelector("[data-queue-item]"));
      |    });
      |    await step("stop-send", async () => {
      |      await page.fill("[data-prompt='send']", "after");
      |      await Promise.all([
      |        page.waitForURL(/\/watch\/[^/?#]+/),
      |        page.click("[data-control='stop-send']"),
      |      ]);
      |      await page.waitForFunction(() => {
      |        const feed = document.querySelector("#feed");
      |        return feed && feed.textContent.includes("after");
      |      });
      |    });
      |    await step("cancel", async () => {
      |      await Promise.all([
      |        page.waitForURL(/\/watch\/[^/?#]+/),
      |        page.click("[data-control='cancel']"),
      |      ]);
      |    });
      |    await step("resume", async () => {
      |      await Promise.all([
      |        page.waitForURL(/\/watch\/[^/?#]+/),
      |        page.click("[data-control='resume']"),
      |      ]);
      |      await page.waitForSelector("#feed");
      |    });
      |    await step("keys", async () => {
      |      await Promise.all([
      |        page.waitForURL(/keys=plain/),
      |        page.click("[data-chrome='desk'] a[data-keys-choice='plain']"),
      |      ]);
      |      if (!page.url().includes("keys=plain")) throw new Error(page.url());
      |      const html = await page.locator("html").getAttribute("data-keys");
      |      if (html !== "plain") throw new Error(String(html));
      |      await Promise.all([
      |        page.waitForURL((url) => !url.toString().includes("keys=plain")),
      |        page.click("[data-chrome='desk'] a[data-keys-choice='vim']"),
      |      ]);
      |      const back = await page.locator("html").getAttribute("data-keys");
      |      if (back !== "vim") throw new Error(String(back) + " " + page.url());
      |    });
      |    await step("csrf", async () => {
      |      const api = await request.newContext();
      |      const response = await api.post(base + "/watch/open", {
      |        headers: {
      |          Origin: "https://evil.example",
      |          "Sec-Fetch-Site": "cross-site",
      |          "Content-Type": "application/x-www-form-urlencoded",
      |        },
      |        data: "workspace=%2Ftmp&csrf=nope&keys=vim",
      |      });
      |      const status = response.status();
      |      await api.dispose();
      |      if (status !== 403) throw new Error("status " + status);
      |    });
      |    console.log("WATCH_E2E ok");
      |    notes.forEach((line) => console.log(line));
      |    await browser.close();
      |  } catch (error) {
      |    console.log("WATCH_E2E fail");
      |    notes.forEach((line) => console.log(line));
      |    console.log(String(error));
      |    await browser.close().catch(() => {});
      |    process.exit(1);
      |  }
      |})().catch((error) => {
      |  console.log("WATCH_E2E fail");
      |  console.log(String(error));
      |  process.exit(1);
      |});
      |""".stripMargin

  private val Shots: String =
    """const { chromium } = require("playwright");
      |const path = require("path");
      |
      |const base = process.env.WATCH_URL;
      |const root = process.env.WATCH_ROOT;
      |const out = path.resolve("target/watch-shots");
      |
      |(async () => {
      |  const browser = await chromium.launch({ headless: true });
      |  const page = await browser.newPage({ viewport: { width: 1440, height: 900 }, colorScheme: "light" });
      |  page.setDefaultTimeout(15000);
      |  async function settle() {
      |    await page.evaluate(() => new Promise((resolve) => {
      |      requestAnimationFrame(() => requestAnimationFrame(resolve));
      |    }));
      |  }
      |  async function shot(name) {
      |    await settle();
      |    await page.screenshot({ path: path.join(out, name + ".png") });
      |  }
      |  async function scheme(name) {
      |    await page.emulateMedia({ colorScheme: name });
      |    await settle();
      |  }
      |  try {
      |    const home = await page.goto(base + "/watch", { waitUntil: "domcontentloaded" });
      |    if (!home || home.status() !== 200) throw new Error("home " + (home && home.status()));
      |    await page.waitForSelector("[data-composer='live']");
      |    await shot("empty-desk-light");
      |    await page.setViewportSize({ width: 390, height: 844 });
      |    await scheme("dark");
      |    await shot("empty-phone-dark");
      |    await page.setViewportSize({ width: 1440, height: 900 });
      |    await scheme("light");
      |    await page.fill("[data-field='workspace']", root);
      |    const nav = page.waitForNavigation({ waitUntil: "domcontentloaded" });
      |    await page.click("[data-control='new-session']");
      |    const opened = await nav;
      |    if (!opened || opened.status() !== 200) throw new Error("open " + (opened && opened.status()));
      |    await page.fill("[data-prompt='send']", "scene");
      |    await Promise.all([
      |      page.waitForNavigation({ waitUntil: "domcontentloaded" }),
      |      page.click("button[name='mode'][value='send']"),
      |    ]);
      |    let sawScene = false;
      |    for (let attempt = 0; attempt < 15; attempt++) {
      |      const feed = await page.locator("#feed").innerText();
      |      if (feed.includes("outside the workspace") && feed.includes("4 tests passed")) {
      |        sawScene = true;
      |        break;
      |      }
      |      await page.waitForTimeout(200);
      |      await page.reload({ waitUntil: "domcontentloaded" });
      |    }
      |    if (!sawScene) throw new Error("scene feed never arrived");
      |    await page.fill("[data-prompt='send']", "block");
      |    await Promise.all([
      |      page.waitForNavigation({ waitUntil: "domcontentloaded" }),
      |      page.click("button[name='mode'][value='send']"),
      |    ]);
      |    await page.waitForFunction(() => document.body.textContent.includes("Running"));
      |    await page.fill("[data-prompt='send']", "Follow up once the turn is free.");
      |    await Promise.all([
      |      page.waitForNavigation({ waitUntil: "domcontentloaded" }),
      |      page.click("button[name='mode'][value='send']"),
      |    ]);
      |    await page.waitForSelector("[data-queue-item]");
      |    await Promise.all([
      |      page.waitForNavigation({ waitUntil: "domcontentloaded" }),
      |      page.click("[data-chrome='desk'] a[data-chip='thought']"),
      |    ]);
      |    await page.waitForFunction(() => {
      |      const feed = document.querySelector("#feed");
      |      return feed && feed.textContent.includes("Weighing the layout");
      |    });
      |    async function showRich() {
      |      await page.evaluate(() => {
      |        const feed = document.querySelector("#feed");
      |        const anchor = document.querySelector("[data-kind='Thought']")
      |          || document.querySelector("[data-kind='Refused']");
      |        if (!feed || !anchor) return;
      |        const top = anchor.getBoundingClientRect().top - feed.getBoundingClientRect().top + feed.scrollTop;
      |        feed.scrollTop = Math.max(0, top - 8);
      |      });
      |      await settle();
      |    }
      |    await showRich();
      |    await page.focus("[data-prompt='send']");
      |    await shot("focus-desk-light");
      |    await page.evaluate(() => document.activeElement && document.activeElement.blur());
      |    await page.click("[data-chrome='desk'] [data-filter='events'] > summary");
      |    await page.waitForSelector("[data-chrome='desk'] [data-filter='events'][open] ~ [data-chrome-part='chips'] a[data-chip='ToolStarted']");
      |    await showRich();
      |    await shot("filter-desk-light");
      |    await page.click("[data-chrome='desk'] [data-filter='events'] > summary");
      |    await showRich();
      |    await shot("feed-desk-light");
      |    await scheme("dark");
      |    await showRich();
      |    await shot("feed-desk-dark");
      |    await page.setViewportSize({ width: 390, height: 844 });
      |    await scheme("light");
      |    await page.click("[data-chrome='phone'] [data-filter='events'] > summary");
      |    await page.waitForSelector("[data-chrome='phone'] [data-filter='events'][open] ~ [data-chrome-part='chips'] a[data-chip='TestLine']");
      |    await showRich();
      |    await shot("filter-phone-light");
      |    await page.click("[data-chrome='phone'] [data-filter='events'] > summary");
      |    await showRich();
      |    await shot("feed-phone-light");
      |    await scheme("dark");
      |    await showRich();
      |    await shot("feed-phone-dark");
      |    const deniedPage = await browser.newPage({ viewport: { width: 1440, height: 900 }, colorScheme: "dark" });
      |    deniedPage.setDefaultTimeout(15000);
      |    const denied = await deniedPage.goto(base + "/watch/not-real", { waitUntil: "domcontentloaded" });
      |    if (!denied || denied.status() !== 401) throw new Error("denied " + (denied && denied.status()));
      |    await deniedPage.screenshot({ path: path.join(out, "denied-desk-dark.png") });
      |    await deniedPage.close();
      |    await page.setViewportSize({ width: 390, height: 844 });
      |    await scheme("light");
      |    await Promise.all([
      |      page.waitForNavigation({ waitUntil: "domcontentloaded" }),
      |      page.click("[data-control='queue-drop']"),
      |    ]);
      |    await page.waitForFunction(() => !document.querySelector("[data-queue-item]"));
      |    await Promise.all([
      |      page.waitForNavigation({ waitUntil: "domcontentloaded" }),
      |      page.click("[data-control='cancel']"),
      |    ]);
      |    await page.waitForFunction(() => document.body.textContent.includes("Ended:"));
      |    await page.evaluate(() => {
      |      const feed = document.querySelector("#feed");
      |      const anchor = document.querySelector("[data-kind='Refused']");
      |      if (!feed || !anchor) return;
      |      const top = anchor.getBoundingClientRect().top - feed.getBoundingClientRect().top + feed.scrollTop;
      |      feed.scrollTop = Math.max(0, top - 8);
      |    });
      |    await settle();
      |    await shot("ended-phone-light");
      |    console.log("WATCH_SHOTS ok");
      |    await browser.close();
      |  } catch (error) {
      |    const text = await page.locator("body").innerText().catch(() => "");
      |    const html = await page.locator("#feed").innerHTML().catch(() => "");
      |    console.log("PAGE " + text.slice(0, 800));
      |    console.log("FEEDHTML " + html.slice(0, 2500));
      |    console.log("WATCH_SHOTS fail");
      |    console.log(String(error));
      |    await page.screenshot({ path: path.join(out, "failure.png") }).catch(() => {});
      |    await browser.close().catch(() => {});
      |    process.exit(1);
      |  }
      |})().catch((error) => {
      |  console.log("WATCH_SHOTS fail");
      |  console.log(String(error));
      |  process.exit(1);
      |});
      |""".stripMargin
end WatchBrowserSpec
