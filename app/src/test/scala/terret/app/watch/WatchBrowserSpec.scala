package terret.app.watch

import heddle.Server
import heddle.route.Routes
import heddle.server.Port
import terret.Terret
import terret.app.{AccessGate, AuthLog, Ingress}
import terret.config.TerretConfig
import terret.event.{Event, TurnOutcome}
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
  * the drive stays off unless `target/watch-e2e/enable` exists. The suite `PATH` often has no `node`;
  * `target/watch-e2e/node` may hold that binary's absolute path. The default run is a no-op and is not a CI browser
  * job. The listener uses `AccessGate.open` so the drive does not need a Cloudflare JWT. Production still mounts
  * `/watch` only on the tunnel, and that tunnel still requires a JWT.
  */
object WatchBrowserSpec extends ZIOSpecDefault:
  private val repo   = Path.of(sys.env.getOrElse("HISTOS_REPO", ".")).toAbsolutePath.normalize
  private val enable = repo.resolve("target/watch-e2e/enable")

  def spec = suite("watch browser")(
    test("the page queues, stops, cancels, resumes, switches keys, and refuses a cross-site post") {
      if Files.isRegularFile(enable) then ZIO.scoped(drive)
      else ZIO.succeed(assertTrue(true))
    } @@ TestAspect.timeout(3.minutes) @@ TestAspect.withLiveClock,
  )

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
      bound <- ZIO.fromEither(Port.make(0))
      server <- Server
        .install(
          Ingress.tunnel(Routes.empty, watch),
          Server.Config.default.copy(host = "127.0.0.1", port = bound, http2 = false),
        )
        .provideSome[Scope](AccessGate.open ++ AuthLog.live)
      port   <- server.port
      scriptPath = scratch.resolve("drive.cjs")
      _      <- ZIO.attempt(Files.writeString(scriptPath, Drive))
      result <- runNode(repo, scriptPath, port, dir)
      (code, output) = result
    yield assertTrue(code == 0, output.contains("WATCH_E2E ok"))

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
end WatchBrowserSpec
