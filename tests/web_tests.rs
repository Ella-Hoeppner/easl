//! Runs easl programs in headless Chrome through the web runtime (`web/`),
//! comparing what they print with golden files: the web-specific tests in
//! `data/web/`, and every program of the cpu and buffer suites, against
//! those suites' own goldens (parity with the native runtimes).
//!
//! Needs Chrome (or Chromium) with WebGPU — set `EASL_TEST_CHROME` to its
//! binary if it isn't installed in the usual place — and the wasm32 target,
//! for building the runtime. Pass a substring to run only matching tests,
//! e.g. `cargo test --features window --test web_tests web/`.

#[cfg(not(feature = "window"))]
fn main() {}

#[cfg(feature = "window")]
fn main() {
  harness::main()
}

#[cfg(feature = "window")]
mod harness {
  use std::{
    collections::HashMap,
    env, fs,
    io::{self, BufRead, BufReader, ErrorKind, Read, Write},
    net::{TcpListener, TcpStream},
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    sync::{
      Arc, Mutex,
      atomic::{AtomicUsize, Ordering},
    },
    thread,
    time::{Duration, Instant},
  };

  use easl::web_bundle::{RUNTIME_SUPPORT_FILES, bundle_program};
  use serde_json::{Value, json};
  use tungstenite::{Message, WebSocket, stream::MaybeTlsStream};
  use wasm_bindgen_cli_support::Bindgen;

  /// A step of a scripted test, run while the program runs.
  enum Step {
    /// Waits until the program prints `line`, after the lines earlier
    /// `AwaitPrint` steps matched.
    AwaitPrint(&'static str),
    KeyDown(&'static str),
    KeyUp(&'static str),
    /// A key press with an explicit `KeyboardEvent.key`, `.code`, and
    /// modifiers (`SHIFT`, or 0), like a real keyboard sends.
    KeyPress(&'static str, &'static str, u32),
    KeyRelease(&'static str, &'static str, u32),
    MouseMove(f64, f64),
    MouseDown(f64, f64),
    MouseUp(f64, f64),
    RightMouseDown(f64, f64),
    RightMouseUp(f64, f64),
    /// Sends a raw MIDI message through the runtime's `sendMidiMessage`.
    Midi(&'static [u8]),
  }
  use Step::*;

  /// The DevTools protocol's modifier bit for shift.
  const SHIFT: u32 = 8;

  /// Input scripts for `data/web/` tests. The test page's canvas is 320x240
  /// in a 400x300 viewport, so (360, 280) is outside it.
  const SCRIPTS: &[(&str, &[Step])] = &[
    (
      "input_named_keys",
      &[
        AwaitPrint("0u"),
        KeyPress("Shift", "ShiftLeft", SHIFT),
        AwaitPrint("2u"),
        KeyPress("!", "Digit1", SHIFT),
        AwaitPrint("3u"),
        // Shift released first: the release of "1" must still clear it.
        KeyRelease("Shift", "ShiftLeft", 0),
        AwaitPrint("1u"),
        KeyRelease("1", "Digit1", 0),
        AwaitPrint("0u"),
        KeyPress("Shift", "ShiftLeft", SHIFT),
        AwaitPrint("2u"),
        KeyPress("A", "KeyA", SHIFT),
        AwaitPrint("18u"),
        KeyRelease("A", "KeyA", SHIFT),
        AwaitPrint("2u"),
        KeyRelease("Shift", "ShiftLeft", 0),
        AwaitPrint("0u"),
        KeyPress(" ", "Space", 0),
        AwaitPrint("4u"),
        KeyRelease(" ", "Space", 0),
        AwaitPrint("0u"),
        KeyPress("ArrowUp", "ArrowUp", 0),
        AwaitPrint("32u"),
        KeyRelease("ArrowUp", "ArrowUp", 0),
        AwaitPrint("0u"),
        MouseMove(100., 50.),
        RightMouseDown(100., 50.),
        AwaitPrint("8u"),
        RightMouseUp(100., 50.),
        AwaitPrint("0u"),
        KeyDown("q"),
        KeyUp("q"),
      ],
    ),
    (
      "midi_input",
      &[
        AwaitPrint("(vec4f 0. 0. 0. 0.)"),
        Midi(&[0x90, 60, 127]),
        AwaitPrint("(vec4f 1. 60. 0. 0.)"),
        Midi(&[0xB0, 1, 127]),
        AwaitPrint("(vec4f 1. 60. 1. 0.)"),
        Midi(&[0xE0, 0, 0]),
        AwaitPrint("(vec4f 1. 60. 1. -1.)"),
        Midi(&[0x80, 60, 0]),
        AwaitPrint("(vec4f 0. 0. 1. -1.)"),
        KeyDown("q"),
        KeyUp("q"),
      ],
    ),
    (
      "audio_closure_midi",
      &[AwaitPrint("\"started\""), Midi(&[0xB0, 7, 127])],
    ),
    (
      "input_just_pressed",
      &[
        AwaitPrint("\"ready\""),
        KeyDown("a"),
        KeyUp("a"),
        AwaitPrint("\"a pressed\""),
        MouseMove(100., 50.),
        MouseDown(100., 50.),
        MouseUp(100., 50.),
        AwaitPrint("(vec2u 320u 240u)"),
        KeyDown("q"),
        KeyUp("q"),
      ],
    ),
    (
      "input_held",
      &[
        AwaitPrint("(vec4u 0u 0u 0u 0u)"),
        MouseMove(100., 50.),
        AwaitPrint("(vec4u 1u 0u 0u 0u)"),
        MouseDown(100., 50.),
        AwaitPrint("(vec4u 1u 1u 0u 0u)"),
        MouseUp(100., 50.),
        AwaitPrint("(vec4u 1u 0u 0u 0u)"),
        KeyDown("a"),
        AwaitPrint("(vec4u 1u 0u 1u 0u)"),
        KeyDown("b"),
        AwaitPrint("(vec4u 1u 0u 1u 1u)"),
        KeyUp("a"),
        AwaitPrint("(vec4u 1u 0u 0u 1u)"),
        KeyUp("b"),
        AwaitPrint("(vec4u 1u 0u 0u 0u)"),
        MouseMove(360., 280.),
        AwaitPrint("(vec4u 0u 0u 0u 0u)"),
        KeyDown("q"),
        KeyUp("q"),
      ],
    ),
  ];

  /// Cpu- and buffer-suite programs that can't run on the web, and why.
  const PARITY_SKIPS: &[(&str, &str)] = &[
    ("buffer/load_red_pixel", "reads a file"),
    ("buffer/save_png_roundtrip", "reads and writes files"),
    ("buffer/save_png_render_target", "writes a file"),
    ("cpu/load_wav_local_binding", "reads a file"),
    ("cpu/load_wav_raw", "reads a file"),
    ("cpu/wav_sample_rate", "reads a file"),
    ("cpu/save_wav_roundtrip", "writes a file"),
    (
      "buffer/struct_array_buffer",
      "reads storage-write data in a vertex shader, which browsers don't \
       support (web/vertex_storage_write_unsupported pins the error)",
    ),
    (
      "buffer/bidirectional_transfer_render",
      "writes storage from a vertex shader, which browsers don't support \
       (web/vertex_storage_write_unsupported pins the error)",
    ),
    (
      "cpu/audio_closure_entry",
      "its window never closes (the native suite stops it after its \
       simulated frames); web/audio_closure_hofs covers its audio graph",
    ),
    (
      "cpu/audio_closure_entry_hofs",
      "its window never closes (the native suite stops it after its \
       simulated frames); web/audio_closure_hofs covers it",
    ),
    (
      "cpu/audio_time_through_hof_chain",
      "its window never closes (the native suite stops it after its \
       simulated frames)",
    ),
    (
      "cpu/midi_queries",
      "needs the spoofed MIDI state its own test in cpu_tests injects",
    ),
    (
      "cpu/hof_closure_factory_arg",
      "its output depends on StringIO's 8 Hz sample rate and simulated \
       frames",
    ),
  ];

  /// How long one test may run.
  const TEST_TIMEOUT: Duration = Duration::from_secs(60);

  const TEST_PAGE: &str = r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8" />
    <style>
      html, body { margin: 0; background: #000; }
      canvas { display: block; width: 320px; height: 240px; }
    </style>
  </head>
  <body>
    <canvas id="easl-canvas"></canvas>
    <script type="module">
      window.runProgram = async () => {
        const { startEaslProgram } = await import("./easl-program.js");
        await startEaslProgram(document.getElementById("easl-canvas"));
      };
    </script>
  </body>
</html>
"#;

  struct Case {
    /// `<suite>/<test>`, e.g. `web/readback`.
    name: String,
    source: PathBuf,
    expected: Expected,
    script: &'static [Step],
  }

  enum Expected {
    /// What the program prints, from `<test>.txt`.
    Output(String),
    /// Text the program's rejection must contain, from `<test>.error`
    /// (`data/web/` only).
    Error(String),
    /// What the program prints, and the canvas's top-left pixel (RGBA8,
    /// space-separated) once it has finished, from a buffer suite
    /// `<test>.screen.txt`: its print lines, then a `frame <i>: <pixel>`
    /// line per frame, of which the canvas shows the last.
    Screen { output: String, pixel: String },
  }

  pub fn main() {
    let filter = env::args().skip(1).find(|arg| !arg.starts_with('-'));
    let cases: Vec<Case> = collect_cases()
      .into_iter()
      .filter(|case| filter.as_ref().is_none_or(|f| case.name.contains(f)))
      .collect();
    if cases.is_empty() {
      println!("\nrunning 0 tests\n");
      return;
    }

    let scratch = Path::new(env!("CARGO_TARGET_TMPDIR")).join("web_tests");
    let runtime_dir = build_runtime(&scratch);
    let server = Server::start(&runtime_dir);
    let chrome = Chrome::launch(&scratch.join("chrome-profile"));
    chrome.check_webgpu(&server);

    println!("\nrunning {} tests", cases.len());
    let started = Instant::now();
    let next_case = AtomicUsize::new(0);
    let failures = Mutex::new(vec![]);
    let workers = thread::available_parallelism()
      .map_or(4, |n| n.get())
      .min(8);
    thread::scope(|scope| {
      for _ in 0..workers {
        scope.spawn(|| {
          loop {
            let index = next_case.fetch_add(1, Ordering::Relaxed);
            let Some(case) = cases.get(index) else {
              break;
            };
            match run_case(case, &chrome, &server) {
              Ok(()) => println!("test {} ... ok", case.name),
              Err(failure) => {
                println!("test {} ... FAILED", case.name);
                failures.lock().unwrap().push((case.name.clone(), failure));
              }
            }
          }
        });
      }
    });

    let mut failures = failures.into_inner().unwrap();
    failures.sort();
    for (name, failure) in &failures {
      println!("\n---- {name} ----\n{failure}");
    }
    println!(
      "\ntest result: {}. {} passed; {} failed; finished in {:.2}s\n",
      if failures.is_empty() { "ok" } else { "FAILED" },
      cases.len() - failures.len(),
      failures.len(),
      started.elapsed().as_secs_f64()
    );
    drop(chrome);
    if !failures.is_empty() {
      std::process::exit(1);
    }
  }

  fn collect_cases() -> Vec<Case> {
    let mut cases = vec![];
    for suite in ["web", "cpu", "buffer"] {
      let mut sources: Vec<PathBuf> = fs::read_dir(format!("./data/{suite}"))
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "easl"))
        .collect();
      sources.sort();
      for source in sources {
        let test = source.file_stem().unwrap().to_string_lossy().into_owned();
        let name = format!("{suite}/{test}");
        if PARITY_SKIPS.iter().any(|(skipped, _)| *skipped == name) {
          continue;
        }
        let expected = if let Ok(output) =
          fs::read_to_string(source.with_extension("txt"))
        {
          Expected::Output(output)
        } else if let Ok(golden) =
          fs::read_to_string(source.with_extension("screen.txt"))
        {
          let (frames, prints): (Vec<&str>, Vec<&str>) =
            golden.lines().partition(|line| line.starts_with("frame "));
          let pixel = frames
            .last()
            .and_then(|line| line.split_once(": "))
            .unwrap_or_else(|| panic!("{name} has no frame lines"))
            .1
            .to_string();
          Expected::Screen {
            output: prints.iter().map(|line| format!("{line}\n")).collect(),
            pixel,
          }
        } else if suite == "web" {
          let error = fs::read_to_string(source.with_extension("error"))
            .unwrap_or_else(|_| panic!("{name} has no .txt or .error file"));
          Expected::Error(error.trim().to_string())
        } else {
          // Programs the suite's own hand-written tests drive.
          continue;
        };
        let script = SCRIPTS
          .iter()
          .find(|(scripted, _)| suite == "web" && *scripted == test)
          .map_or(&[][..], |(_, script)| *script);
        cases.push(Case {
          name,
          source,
          expected,
          script,
        });
      }
    }
    cases
  }

  /// Builds the web runtime into `scratch/runtime`, returning that
  /// directory: `easl_web.js` and `easl_web_bg.wasm`.
  fn build_runtime(scratch: &Path) -> PathBuf {
    let web_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("web");
    let target_dir = scratch.join("runtime-target");
    // The runtime is its own build, configured by `web/.cargo/config.toml`;
    // none of the environment cargo gives this test applies to it.
    let mut cargo = Command::new(env::var("CARGO").unwrap_or("cargo".into()));
    cargo
      .current_dir(&web_dir)
      .args(["build", "--release", "--target", "wasm32-unknown-unknown"])
      .arg("--target-dir")
      .arg(&target_dir);
    for (name, _) in env::vars_os() {
      let name = name.to_string_lossy();
      if name.starts_with("CARGO_") && name != "CARGO_HOME" {
        cargo.env_remove(&*name);
      }
    }
    let status = cargo.status().expect("failed to run cargo");
    assert!(
      status.success(),
      "building the web runtime failed; it needs the wasm32 target \
       (`rustup target add wasm32-unknown-unknown`)"
    );
    let runtime_dir = scratch.join("runtime");
    Bindgen::new()
      .input_path(
        target_dir.join("wasm32-unknown-unknown/release/easl_web.wasm"),
      )
      .web(true)
      .unwrap()
      .typescript(false)
      .omit_default_module_path(false)
      .generate(&runtime_dir)
      .expect("generating the web runtime's JS bindings failed");
    runtime_dir
  }

  /// Serves the runtime and each test's page and program over HTTP. Test
  /// `id` lives at `/t/<id>/`, and every test shares the runtime files at
  /// `/runtime/`, so the browser compiles the wasm once.
  struct Server {
    port: u16,
    programs: Arc<Mutex<HashMap<usize, String>>>,
    next_id: AtomicUsize,
  }

  impl Server {
    fn start(runtime_dir: &Path) -> Self {
      let listener = TcpListener::bind("127.0.0.1:0").unwrap();
      let port = listener.local_addr().unwrap().port();
      let runtime_js = fs::read(runtime_dir.join("easl_web.js")).unwrap();
      let runtime_wasm =
        fs::read(runtime_dir.join("easl_web_bg.wasm")).unwrap();
      let programs = Arc::new(Mutex::new(HashMap::<usize, String>::new()));
      let served_programs = Arc::clone(&programs);
      let files = Arc::new((runtime_js, runtime_wasm));
      thread::spawn(move || {
        for stream in listener.incoming() {
          let Ok(stream) = stream else { continue };
          let programs = Arc::clone(&served_programs);
          let files = Arc::clone(&files);
          thread::spawn(move || {
            let _ = serve(stream, &programs, &files.0, &files.1);
          });
        }
      });
      Self {
        port,
        programs,
        next_id: AtomicUsize::new(0),
      }
    }

    /// Serves `program_js` as a new test's program, returning the URL of its
    /// page.
    fn add_program(&self, program_js: String) -> (usize, String) {
      let id = self.next_id.fetch_add(1, Ordering::Relaxed);
      self.programs.lock().unwrap().insert(
        id,
        // Every runtime file the program refers to (`./easl_web.js`, the
        // wasm, the audio worklet) is served once, from `/runtime/`.
        program_js.replace("\"./", "\"/runtime/"),
      );
      (
        id,
        format!("http://127.0.0.1:{}/t/{id}/index.html", self.port),
      )
    }

    fn remove_program(&self, id: usize) {
      self.programs.lock().unwrap().remove(&id);
    }
  }

  fn serve(
    stream: TcpStream,
    programs: &Mutex<HashMap<usize, String>>,
    runtime_js: &[u8],
    runtime_wasm: &[u8],
  ) -> io::Result<()> {
    let mut reader = BufReader::new(stream.try_clone()?);
    let mut request_line = String::new();
    reader.read_line(&mut request_line)?;
    let path = request_line.split_whitespace().nth(1).unwrap_or("");
    let program_body;
    let (content_type, body): (&str, &[u8]) = match path {
      "/runtime/easl_web.js" => ("text/javascript", runtime_js),
      "/runtime/easl_web_bg.wasm" => ("application/wasm", runtime_wasm),
      _ if let Some((_, contents)) = RUNTIME_SUPPORT_FILES
        .iter()
        .find(|(name, _)| path.strip_prefix("/runtime/") == Some(*name)) =>
      {
        ("text/javascript", contents.as_bytes())
      }
      _ => {
        let mut parts = path.trim_start_matches("/t/").splitn(2, '/');
        let id: Option<usize> = parts.next().and_then(|id| id.parse().ok());
        match (id, parts.next()) {
          (Some(_), Some("index.html")) => ("text/html", TEST_PAGE.as_bytes()),
          (Some(id), Some("easl-program.js")) => {
            program_body = programs.lock().unwrap().get(&id).cloned();
            match &program_body {
              Some(program) => ("text/javascript", program.as_bytes()),
              None => ("", &[]),
            }
          }
          _ => ("", &[]),
        }
      }
    };
    let mut stream = stream;
    if content_type.is_empty() {
      stream.write_all(
        b"HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\
          Connection: close\r\n\r\n",
      )
    } else {
      write!(
        stream,
        "HTTP/1.1 200 OK\r\nContent-Type: {content_type}\r\n\
         Content-Length: {}\r\nCache-Control: no-store\r\n\
         Connection: close\r\n\r\n",
        body.len()
      )?;
      stream.write_all(body)
    }
  }

  /// A headless Chrome, killed when dropped.
  struct Chrome {
    process: Child,
    port: u16,
  }

  impl Chrome {
    fn launch(profile_dir: &Path) -> Self {
      let _ = fs::remove_dir_all(profile_dir);
      fs::create_dir_all(profile_dir).unwrap();
      let binary = chrome_binary();
      let process = Command::new(&binary)
        .args([
          "--headless=new",
          "--enable-unsafe-webgpu",
          "--no-first-run",
          "--no-default-browser-check",
          "--disable-background-timer-throttling",
          "--disable-backgrounding-occluded-windows",
          "--disable-renderer-backgrounding",
          "--autoplay-policy=no-user-gesture-required",
          // Audio still renders (the tests observe it through shared
          // variables) but never reaches the speakers.
          "--mute-audio",
          "--remote-debugging-port=0",
        ])
        .arg(format!("--user-data-dir={}", profile_dir.display()))
        .arg("about:blank")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .unwrap_or_else(|e| {
          panic!("failed to launch Chrome at {}: {e}", binary.display())
        });
      // With port 0, Chrome picks a free port and writes it here.
      let port_file = profile_dir.join("DevToolsActivePort");
      let deadline = Instant::now() + Duration::from_secs(20);
      let port = loop {
        if let Ok(contents) = fs::read_to_string(&port_file)
          && let Some(Ok(port)) = contents.lines().next().map(str::parse)
        {
          break port;
        }
        assert!(Instant::now() < deadline, "Chrome didn't start");
        thread::sleep(Duration::from_millis(50));
      };
      Self { process, port }
    }

    /// Fails the run with a clear message if this Chrome has no WebGPU.
    fn check_webgpu(&self, server: &Server) {
      let (id, url) = server.add_program(String::new());
      let mut tab = Tab::open(self, &url);
      let result = tab.session.evaluate(
        "(async () => !!(navigator.gpu && \
         await navigator.gpu.requestAdapter()))()",
      );
      server.remove_program(id);
      assert!(
        result == Ok(json!(true)),
        "this Chrome has no WebGPU adapter ({result:?}); the web tests \
         need a Chrome with working WebGPU"
      );
    }

    /// Opens a page in a window of its own, returning its target id. Tabs
    /// sharing a window would be throttled: only the front one gets
    /// animation frames.
    fn open_window(&self) -> String {
      let version: Value =
        serde_json::from_str(&self.http("GET", "/json/version")).unwrap();
      let (mut browser, _) =
        tungstenite::connect(version["webSocketDebuggerUrl"].as_str().unwrap())
          .unwrap();
      let request = json!({
        "id": 1, "method": "Target.createTarget",
        "params": { "url": "about:blank", "newWindow": true }
      });
      browser.send(Message::text(request.to_string())).unwrap();
      loop {
        let Message::Text(text) = browser.read().unwrap() else {
          continue;
        };
        let response: Value = serde_json::from_str(&text).unwrap();
        if response["id"] == 1 {
          let _ = browser.close(None);
          return response["result"]["targetId"]
            .as_str()
            .expect("DevTools didn't open a window")
            .to_string();
        }
      }
    }

    /// Sends a request to the DevTools HTTP endpoint, returning the body.
    fn http(&self, method: &str, path: &str) -> String {
      let mut stream = TcpStream::connect(("127.0.0.1", self.port)).unwrap();
      write!(
        stream,
        "{method} {path} HTTP/1.1\r\nHost: 127.0.0.1:{}\r\n\
         Connection: close\r\n\r\n",
        self.port
      )
      .unwrap();
      // DevTools may keep the connection open, so read exactly the body's
      // declared length.
      let mut reader = BufReader::new(stream);
      let mut content_length = 0;
      loop {
        let mut header = String::new();
        reader.read_line(&mut header).unwrap();
        let header = header.trim_end();
        if header.is_empty() {
          break;
        }
        if let Some((name, value)) = header.split_once(':')
          && name.eq_ignore_ascii_case("content-length")
        {
          content_length = value.trim().parse().unwrap();
        }
      }
      let mut body = vec![0; content_length];
      reader.read_exact(&mut body).unwrap();
      String::from_utf8(body).unwrap()
    }
  }

  impl Drop for Chrome {
    fn drop(&mut self) {
      let _ = self.process.kill();
      let _ = self.process.wait();
    }
  }

  fn chrome_binary() -> PathBuf {
    if let Some(path) = env::var_os("EASL_TEST_CHROME") {
      return path.into();
    }
    let installed = [
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Chromium.app/Contents/MacOS/Chromium",
      "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe",
      "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe",
    ];
    if let Some(path) = installed.iter().find(|path| Path::new(path).exists()) {
      return path.into();
    }
    let on_path = [
      "google-chrome",
      "google-chrome-stable",
      "chromium",
      "chromium-browser",
    ];
    for name in on_path {
      if let Some(dirs) = env::var_os("PATH") {
        for dir in env::split_paths(&dirs) {
          if dir.join(name).exists() {
            return dir.join(name);
          }
        }
      }
    }
    panic!(
      "couldn't find Chrome; install it or set EASL_TEST_CHROME to its \
       binary"
    )
  }

  /// A browser page and its DevTools session, closed when dropped.
  struct Tab<'a> {
    chrome: &'a Chrome,
    id: String,
    session: Session,
  }

  impl<'a> Tab<'a> {
    /// Opens a tab showing `url`, returning once the page has loaded.
    fn open(chrome: &'a Chrome, url: &str) -> Self {
      let id = chrome.open_window();
      let (socket, _) = tungstenite::connect(format!(
        "ws://127.0.0.1:{}/devtools/page/{id}",
        chrome.port
      ))
      .unwrap();
      if let MaybeTlsStream::Plain(stream) = socket.get_ref() {
        stream
          .set_read_timeout(Some(Duration::from_millis(100)))
          .unwrap();
      }
      let mut session = Session {
        socket,
        next_id: 0,
        responses: HashMap::new(),
        prints: vec![],
        errors: vec![],
        loaded: false,
      };
      session.call("Runtime.enable", json!({})).unwrap();
      session.call("Page.enable", json!({})).unwrap();
      session
        .call(
          "Emulation.setDeviceMetricsOverride",
          json!({
            "width": 400, "height": 300, "deviceScaleFactor": 1,
            "mobile": false
          }),
        )
        .unwrap();
      session
        .call("Page.navigate", json!({ "url": url }))
        .unwrap();
      let deadline = Instant::now() + TEST_TIMEOUT;
      while !session.loaded && Instant::now() < deadline {
        session.pump();
      }
      Tab {
        chrome,
        id,
        session,
      }
    }
  }

  impl Drop for Tab<'_> {
    fn drop(&mut self) {
      self.chrome.http("GET", &format!("/json/close/{}", self.id));
    }
  }

  struct Session {
    socket: WebSocket<MaybeTlsStream<TcpStream>>,
    next_id: u64,
    responses: HashMap<u64, Value>,
    /// Each `console.log` call's text, which is how the runtime prints.
    prints: Vec<String>,
    /// Console errors and uncaught exceptions.
    errors: Vec<String>,
    loaded: bool,
  }

  impl Session {
    fn send(&mut self, method: &str, params: Value) -> u64 {
      self.next_id += 1;
      let message =
        json!({ "id": self.next_id, "method": method, "params": params });
      self
        .socket
        .send(Message::text(message.to_string()))
        .unwrap();
      self.next_id
    }

    /// Sends a command and waits for its result.
    fn call(&mut self, method: &str, params: Value) -> Result<Value, String> {
      let id = self.send(method, params);
      let deadline = Instant::now() + TEST_TIMEOUT;
      loop {
        if let Some(response) = self.responses.remove(&id) {
          return match response.get("error") {
            Some(error) => Err(error.to_string()),
            None => Ok(response["result"].clone()),
          };
        }
        if Instant::now() > deadline {
          return Err(format!("{method} timed out"));
        }
        self.pump();
      }
    }

    /// Evaluates a JS expression, awaiting it if it's a promise.
    fn evaluate(&mut self, expression: &str) -> Result<Value, String> {
      let result = self.call(
        "Runtime.evaluate",
        json!({
          "expression": expression, "awaitPromise": true,
          "returnByValue": true
        }),
      )?;
      match result.get("exceptionDetails") {
        Some(details) => Err(exception_text(details)),
        None => Ok(result["result"]["value"].clone()),
      }
    }

    /// Handles one incoming message, if one arrives within the socket's
    /// read timeout.
    fn pump(&mut self) {
      let message = match self.socket.read() {
        Ok(Message::Text(text)) => text,
        Ok(_) => return,
        Err(tungstenite::Error::Io(e))
          if matches!(
            e.kind(),
            ErrorKind::WouldBlock | ErrorKind::TimedOut
          ) =>
        {
          return;
        }
        Err(e) => panic!("DevTools connection failed: {e}"),
      };
      let message: Value = serde_json::from_str(&message).unwrap();
      if let Some(id) = message["id"].as_u64() {
        self.responses.insert(id, message);
        return;
      }
      let params = &message["params"];
      match message["method"].as_str() {
        Some("Runtime.consoleAPICalled") => {
          let text = params["args"]
            .as_array()
            .unwrap()
            .iter()
            .map(|arg| match &arg["value"] {
              Value::String(s) => s.clone(),
              Value::Null => arg["description"].as_str().unwrap_or("").into(),
              other => other.to_string(),
            })
            .collect::<Vec<_>>()
            .join(" ");
          match params["type"].as_str() {
            Some("log") => self.prints.push(text),
            Some("error") => self.errors.push(text),
            _ => {}
          }
        }
        Some("Runtime.exceptionThrown") => self
          .errors
          .push(exception_text(&params["exceptionDetails"])),
        Some("Page.loadEventFired") => self.loaded = true,
        _ => {}
      }
    }

    /// Whether the program panicked. A panic aborts the wasm instance, so
    /// the program's promise never settles.
    fn panicked(&self) -> bool {
      self
        .errors
        .iter()
        .any(|error| error.contains("panicked at"))
    }
  }

  fn exception_text(details: &Value) -> String {
    details["exception"]["description"]
      .as_str()
      .or(details["text"].as_str())
      .unwrap_or("unknown exception")
      .to_string()
  }

  /// Runs a test, returning a description of how it failed.
  fn run_case(
    case: &Case,
    chrome: &Chrome,
    server: &Server,
  ) -> Result<(), String> {
    let program_js = bundle_program(&case.source)
      .map_err(|e| format!("compile error:\n{e}"))?
      .program_js;
    let (id, url) = server.add_program(program_js);
    let mut tab = Tab::open(chrome, &url);
    let result = run_program(&mut tab.session, case.script);
    let pixel = match &case.expected {
      Expected::Screen { .. } => Some(canvas_pixel(&mut tab.session)),
      _ => None,
    };
    server.remove_program(id);
    let session = &tab.session;
    let output: String = session
      .prints
      .iter()
      .map(|line| format!("{line}\n"))
      .collect();
    let mut failure = String::new();
    match (&case.expected, result) {
      (Expected::Output(_) | Expected::Screen { .. }, Err(error)) => {
        failure += &format!("{error}\n")
      }
      (Expected::Error(expected), Ok(())) => {
        failure +=
          &format!("ran successfully; expected an error {expected:?}\n")
      }
      (Expected::Error(expected), Err(error)) if !error.contains(expected) => {
        failure += &format!("expected an error {expected:?}, got: {error}\n")
      }
      _ => {}
    }
    for error in &session.errors {
      failure += &format!("console error: {error}\n");
    }
    let expected_output = match &case.expected {
      Expected::Output(output) | Expected::Screen { output, .. } => {
        output.as_str()
      }
      Expected::Error(_) => "",
    };
    if let (
      Expected::Screen {
        pixel: expected, ..
      },
      Some(actual),
    ) = (&case.expected, &pixel)
    {
      match actual {
        Ok(actual) if actual == expected => {}
        Ok(actual) => {
          failure += &format!(
            "screen mismatch\n  expected: {expected:?}\n    actual: \
             {actual:?}\n"
          )
        }
        Err(error) => {
          failure += &format!("couldn't read the canvas: {error}\n")
        }
      }
    }
    if output != expected_output {
      failure += &format!(
        "output mismatch\n  expected: {expected_output:?}\n    actual: \
         {output:?}\n"
      );
    }
    if failure.is_empty() {
      Ok(())
    } else {
      Err(failure)
    }
  }

  /// The canvas's top-left pixel as space-separated RGBA8 channels, read
  /// from a screenshot: what the page shows, which is the last presented
  /// frame.
  fn canvas_pixel(session: &mut Session) -> Result<String, String> {
    let result = session.call(
      "Page.captureScreenshot",
      json!({
        "format": "png",
        "clip": { "x": 0, "y": 0, "width": 1, "height": 1, "scale": 1 }
      }),
    )?;
    let data = result["data"]
      .as_str()
      .ok_or_else(|| format!("unexpected result {result}"))?;
    let png = decode_base64(data)?;
    let image = image::load_from_memory(&png)
      .map_err(|e| e.to_string())?
      .to_rgba8();
    let channels: Vec<String> = image
      .get_pixel(0, 0)
      .0
      .iter()
      .map(|c| c.to_string())
      .collect();
    Ok(channels.join(" "))
  }

  fn decode_base64(text: &str) -> Result<Vec<u8>, String> {
    let mut bytes = vec![];
    let mut buffer = 0u32;
    let mut bits = 0;
    for c in text.bytes().filter(|&c| c != b'=') {
      let value = match c {
        b'A'..=b'Z' => c - b'A',
        b'a'..=b'z' => c - b'a' + 26,
        b'0'..=b'9' => c - b'0' + 52,
        b'+' => 62,
        b'/' => 63,
        _ => return Err(format!("invalid base64 character {c}")),
      };
      buffer = (buffer << 6) | value as u32;
      bits += 6;
      if bits >= 8 {
        bits -= 8;
        bytes.push((buffer >> bits) as u8);
      }
    }
    Ok(bytes)
  }

  /// Runs the page's program to completion, performing `script` meanwhile.
  fn run_program(session: &mut Session, script: &[Step]) -> Result<(), String> {
    let run = session.send(
      "Runtime.evaluate",
      json!({ "expression": "runProgram()", "awaitPromise": true }),
    );
    let deadline = Instant::now() + TEST_TIMEOUT;
    let mut matched = 0;
    for step in script {
      match *step {
        AwaitPrint(line) => loop {
          if let Some(offset) = session.prints[matched..]
            .iter()
            .position(|print| print == line)
          {
            matched += offset + 1;
            break;
          }
          if session.panicked() || Instant::now() > deadline {
            return Err(format!("never printed {line:?}"));
          }
          session.pump();
        },
        KeyDown(key) => key_event(session, "keyDown", key)?,
        KeyUp(key) => key_event(session, "keyUp", key)?,
        KeyPress(key, code, modifiers) => {
          coded_key_event(session, "keyDown", key, code, modifiers)?
        }
        KeyRelease(key, code, modifiers) => {
          coded_key_event(session, "keyUp", key, code, modifiers)?
        }
        MouseMove(x, y) => mouse_event(session, "mouseMoved", "left", x, y)?,
        MouseDown(x, y) => mouse_event(session, "mousePressed", "left", x, y)?,
        MouseUp(x, y) => mouse_event(session, "mouseReleased", "left", x, y)?,
        RightMouseDown(x, y) => {
          mouse_event(session, "mousePressed", "right", x, y)?
        }
        RightMouseUp(x, y) => {
          mouse_event(session, "mouseReleased", "right", x, y)?
        }
        Midi(bytes) => {
          session.evaluate(&format!(
            "import(\"./easl-program.js\").then((program) => \
             program.sendMidiMessage(new Uint8Array({bytes:?})))"
          ))?;
        }
      }
    }
    loop {
      if let Some(response) = session.responses.remove(&run) {
        return match response["result"].get("exceptionDetails") {
          Some(details) => Err(format!("error: {}", exception_text(details))),
          None => Ok(()),
        };
      }
      if session.panicked() {
        return Err("the runtime panicked".into());
      }
      if Instant::now() > deadline {
        return Err("timed out".into());
      }
      session.pump();
    }
  }

  fn key_event(
    session: &mut Session,
    kind: &str,
    key: &str,
  ) -> Result<(), String> {
    let params = if kind == "keyDown" {
      json!({ "type": kind, "key": key, "text": key })
    } else {
      json!({ "type": kind, "key": key })
    };
    session.call("Input.dispatchKeyEvent", params).map(|_| ())
  }

  /// A key event with an explicit code and modifiers. Only a character key
  /// press carries text; named keys are sent as raw key downs.
  fn coded_key_event(
    session: &mut Session,
    kind: &str,
    key: &str,
    code: &str,
    modifiers: u32,
  ) -> Result<(), String> {
    let character = key.chars().count() == 1;
    let kind = match kind {
      "keyDown" if !character => "rawKeyDown",
      kind => kind,
    };
    let mut params = json!({
      "type": kind, "key": key, "code": code, "modifiers": modifiers
    });
    if kind == "keyDown" {
      params["text"] = json!(key);
    }
    session.call("Input.dispatchKeyEvent", params).map(|_| ())
  }

  fn mouse_event(
    session: &mut Session,
    kind: &str,
    button: &str,
    x: f64,
    y: f64,
  ) -> Result<(), String> {
    session
      .call(
        "Input.dispatchMouseEvent",
        json!({
          "type": kind, "x": x, "y": y, "button": button, "clickCount": 1
        }),
      )
      .map(|_| ())
  }
}
