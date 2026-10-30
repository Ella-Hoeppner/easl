//! The front page of the easl site.
//!
//! The landing page is self-verifying: every easl snippet displayed on it
//! is compiled with the real easl compiler when the site is built (a
//! snippet that stops compiling fails the build), and the animated hero
//! background is a whole easl program — `pages/main_page_example.easl`,
//! `@cpu` entry and all — run in the browser by the easl web runtime
//! (`web/`), exactly as `easl compile --web` would package it.

use std::{
  fs,
  path::{Path, PathBuf},
  process::Command,
};

use easl::{
  compile_easl_file_to_wgsl,
  web_bundle::{RUNTIME_SUPPORT_FILES, bundle_program},
};

use crate::highlight;

/// The hero background program, relative to the generator's manifest
/// directory. It's validated at build time and shipped as source: the web
/// runtime compiles and runs it in the browser.
const HERO_PROGRAM_PATH: &str = "../pages/main_page_example.easl";

/// The easl web runtime crate, relative to the generator's manifest
/// directory.
const WEB_RUNTIME_PATH: &str = "../../web";

/// Where the hero program and the web runtime go, relative to the site root.
const HERO_RUNTIME_DIR: &str = "easl-web";

struct Showcase {
  id: &'static str,
  kicker: &'static str,
  title: &'static str,
  prose: &'static str,
  source: &'static str,
  caption: &'static str,
}

const SHOWCASES: &[Showcase] = &[
  Showcase {
    id: "abstractions",
    kicker: "the language",
    title: "Abstractions shader languages never had",
    prose: "Generics, sum types with exhaustive <code>match</code>, \
            closures, and functions that take functions — all \
            monomorphized and inlined at compile time, so the WGSL that \
            comes out is exactly what you'd have written by hand. \
            <code>gradient</code> below takes <em>any</em> scalar field \
            and differentiates it; the enum makes \"did the ray hit?\" a \
            type, not a convention.",
    source: r#"(defn sphere [p: vec3f]: f32
  (- (length p) 1.))

(defn gradient [f: (Fn [vec3f] f32)
                p: vec3f]: vec3f
  (let [e 0.001]
    (normalize
     (vec3f (- (f (+ p (vec3f e 0. 0.))) (f p))
            (- (f (+ p (vec3f 0. e 0.))) (f p))
            (- (f (+ p (vec3f 0. 0. e))) (f p))))))

(enum Trace
  (Surface vec3f)
  Sky)

(defn shade [hit: Trace]: vec3f
  (match hit
    (Trace/Surface p) (* (vec3f 0.5)
                         (+ (vec3f 1.)
                            (gradient sphere p)))
    Trace/Sky (vec3f 0.02 0.02 0.04)))"#,
    caption: "Higher-order functions cost nothing: the compiler inlines \
              them, so this is as fast as writing the derivative out by \
              hand.",
  },
  Showcase {
    id: "one-file",
    kicker: "the runtime",
    title: "The whole program in one file",
    prose: "Easl isn't just a shader compiler — it's a runtime. \
            <code>@cpu</code> code opens windows, dispatches GPU work, and \
            reads the results back <em>in the same frame</em>, with no \
            sync calls, no buffer mapping, no host language. Declare a \
            variable, touch it from both processors, and the runtime \
            ships exactly the data you used — an unused variable costs \
            nothing.",
    source: r#"(var wave: [256: f32])

@{workgroup-size 64}
@compute
(defn fill [@{builtin global-invocation-id} id: vec3u]
  (= (wave id.x)
     (sin (+ (window-time)
             (* 0.02 (f32 id.x))))))

@cpu
(defn main []
  (spawn-window
   (fn []
       (dispatch-compute-shader fill (vec3u 4u 1u 1u))
       (print (wave 0u)))))"#,
    caption: "The GPU fills the buffer; the CPU prints an element of it \
              on the next line. The runtime notices the read and syncs — \
              exactly once, exactly what you touched.",
  },
  Showcase {
    id: "audio",
    kicker: "the instrument",
    title: "It plays music, too",
    prose: "Hand a closure to <code>start-audio</code> and it runs once \
            per sample on a real-time audio thread. Captured state — \
            oscillator phases, envelopes, whole delay buffers — moves \
            with it, and stays visible to the rest of the program through \
            the same automatic syncing as everything else. Synth graphs \
            are just higher-order functions.",
    source: r#"(defn create-osc [freq: f32]: (Fn [] f32)
  (let [@var phase 0.]
    (fn []
        (= phase
           (% (+ phase (/ freq (sample-rate)))
              1.))
        (sin (* 6.28318 phase)))))

@cpu
(defn main []
  (start-audio (create-osc 220.))
  (spawn-window (fn [] ())))"#,
    caption: "A sine at 220 Hz: the closure keeps its phase between \
              samples, at whatever rate the audio device runs.",
  },
];

/// Compile a snippet with the real compiler, panicking with the
/// compiler's own error output if it fails — a landing page snippet that
/// doesn't compile fails the site build.
fn compile_check(label: &str, source: &str) -> String {
  match easl::compile_easl_source_to_wgsl(source) {
    Ok(Ok(wgsl)) => wgsl,
    Ok(Err((documents, errors))) => panic!(
      "landing page snippet `{label}` no longer compiles:\n{}",
      errors.describe(&documents)
    ),
    Err(_) => panic!("landing page snippet `{label}` failed to parse"),
  }
}

fn naga_validate(label: &str, wgsl: &str) {
  let module = naga::front::wgsl::parse_str(wgsl).unwrap_or_else(|e| {
    panic!("{label}: naga failed to parse generated WGSL:\n{e}\n\n{wgsl}")
  });
  naga::valid::Validator::new(
    naga::valid::ValidationFlags::all(),
    naga::valid::Capabilities::all(),
  )
  .validate(&module)
  .unwrap_or_else(|e| {
    panic!("{label}: naga validation failed on generated WGSL:\n{e:?}")
  });
}

/// Builds the web runtime (`web/build.sh`) so the page runs the program
/// with the same compiler that validated it, and returns its output
/// directory. The nested cargo build gets none of this process's cargo
/// environment: `cargo run` sets variables describing the generator's own
/// build, which would leak into the runtime's build scripts.
fn build_web_runtime(web_dir: &Path) -> PathBuf {
  let mut build = Command::new(web_dir.join("build.sh"));
  for (name, _) in std::env::vars_os() {
    let name = name.to_string_lossy();
    if (name.starts_with("CARGO") && name != "CARGO_HOME") || name == "TARGET"
    {
      build.env_remove(name.as_ref());
    }
  }
  let status = build
    .status()
    .unwrap_or_else(|e| panic!("couldn't run web/build.sh: {e}"));
  assert!(status.success(), "web/build.sh failed (see its output above)");
  web_dir.join("dist")
}

/// Writes the hero program and everything the web runtime needs to run it
/// into `out_dir`, failing the build if the program doesn't compile.
fn package_hero_program(program_path: &Path, web_dir: &Path, out_dir: &Path) {
  let bundle = bundle_program(program_path).unwrap_or_else(|errors| {
    panic!(
      "the landing page program {} doesn't compile:\n{errors}",
      program_path.display()
    )
  });
  match compile_easl_file_to_wgsl(program_path) {
    Ok(Ok(Ok(wgsl))) => naga_validate("landing page program", &wgsl),
    _ => panic!("the landing page program validated but didn't compile to WGSL"),
  }
  let dist = build_web_runtime(web_dir);
  fs::create_dir_all(out_dir).unwrap();
  fs::write(out_dir.join("easl-program.js"), bundle.program_js).unwrap();
  for (name, contents) in RUNTIME_SUPPORT_FILES {
    fs::write(out_dir.join(name), contents).unwrap();
  }
  for name in ["easl_web.js", "easl_web_bg.wasm"] {
    fs::copy(dist.join(name), out_dir.join(name)).unwrap_or_else(|e| {
      panic!("couldn't copy {name} from {}: {e}", dist.display())
    });
  }
}

pub fn generate(site_dir: &Path) {
  for showcase in SHOWCASES {
    compile_check(showcase.id, showcase.source);
  }

  let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let hero_path = manifest_dir.join(HERO_PROGRAM_PATH);
  let hero_source = fs::read_to_string(&hero_path).unwrap_or_else(|e| {
    panic!("couldn't read {}: {e}", hero_path.display())
  });
  package_hero_program(
    &hero_path,
    &manifest_dir.join(WEB_RUNTIME_PATH),
    &site_dir.join(HERO_RUNTIME_DIR),
  );

  let mut showcases_html = String::new();
  for (index, showcase) in SHOWCASES.iter().enumerate() {
    let flip = if index % 2 == 1 { " flip" } else { "" };
    showcases_html.push_str(&format!(
      r#"<section class="showcase{flip}" id="{id}">
<div class="showcase-text">
<span class="kicker">{kicker}</span>
<h2>{title}</h2>
<p>{prose}</p>
</div>
<figure class="showcase-code">
<pre class="code easl"><code>{code}</code></pre>
<figcaption>{caption}</figcaption>
</figure>
</section>
"#,
      id = showcase.id,
      kicker = showcase.kicker,
      title = showcase.title,
      prose = showcase.prose,
      code = highlight::highlight_easl(showcase.source),
      caption = showcase.caption,
    ));
  }

  let hero_highlighted = highlight::highlight_easl(&hero_source);
  let html = landing_html(&hero_highlighted, &showcases_html);
  fs::write(site_dir.join("index.html"), html).unwrap();
  fs::write(
    site_dir.join("landing.css"),
    include_str!("../assets/landing.css"),
  )
  .unwrap();
}

fn landing_html(hero_highlighted: &str, showcases: &str) -> String {
  format!(
    r##"<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Easl — a lispy graphics programming language</title>
<meta name="description" content="Easl is an expressive graphics programming language: one lispy language for GPU shaders, CPU logic, and real-time audio, with generics, closures, sum types, and state that syncs itself.">
<link rel="stylesheet" href="landing.css">
</head>
<body>
<header class="top">
<a class="wordmark" href="index.html">easl</a>
<nav>
<a href="guide.html">Guide</a>
<a href="reference/overview.html">Reference</a>
<a href="functions.html">Functions</a>
<a href="https://github.com/Ella-Hoeppner/easl">GitHub</a>
</nav>
</header>

<div class="hero">
<canvas id="hero-canvas" aria-hidden="true"></canvas>
<div class="hero-scrim" aria-hidden="true"></div>
<div class="hero-inner">
<h1>easl</h1>
<p class="tagline">A lispy graphics programming language.</p>
<p class="subline">One language for GPU shaders, CPU logic, and real-time
audio — with generics, closures, sum types, and state that syncs
itself.</p>
<div class="cta">
<a class="button primary" href="#get-started">Get started</a>
<a class="button" href="guide.html">Read the guide</a>
</div>
<p class="hero-note" id="hero-note">the background of this page is an
easl program, compiled and run in your browser by the easl runtime —
<a href="#hero-source">view its source</a></p>
<p class="hero-note fallback-note">this page's background is normally a
live easl program — your browser doesn't support WebGPU, so it's taking
the night off</p>
</div>
</div>

<main>

<section class="pillars">
<div class="pillar">
<h3>GPU</h3>
<p>Compiles to clean WGSL: vertex, fragment, and compute entry points,
every WGSL builtin, and abstractions that vanish at compile time.</p>
</div>
<div class="pillar">
<h3>CPU</h3>
<p>The same language runs whole applications — windowing, input,
dispatch — on a fast bytecode VM. No host language required.</p>
</div>
<div class="pillar">
<h3>Audio</h3>
<p>Closures run per-sample on a real-time audio thread, and their state
shares itself with the rest of the program automatically.</p>
</div>
</section>

{showcases}

<section class="showcase" id="hero-source">
<div class="showcase-text">
<span class="kicker">show, don't tell</span>
<h2>The page you're looking at</h2>
<p>This is the program painting the background above. Your browser is
running it with the easl web runtime — the same compiler and bytecode VM
as <code>easl run</code>, compiled to WebAssembly — and the site's build
step checks it with the real easl compiler, so if it ever stops
compiling, the site stops building.</p>
</div>
<figure class="showcase-code">
<pre class="code easl tall"><code>{hero_highlighted}</code></pre>
<figcaption>fract, palette, glow: four octaves of folded space in a
couple dozen lines.</figcaption>
</figure>
</section>

<section class="get-started" id="get-started">
<h2>Get started</h2>
<div class="steps">
<div class="step">
<span class="step-n">1</span>
<p>Clone and install the CLI (needs a <a
href="https://rustup.rs">Rust toolchain</a>):</p>
<pre class="code"><code>git clone https://github.com/Ella-Hoeppner/easl_cli
cd easl_cli &amp;&amp; cargo install --path .</code></pre>
</div>
<div class="step">
<span class="step-n">2</span>
<p>Run an example — a window opens and a shader runs:</p>
<pre class="code"><code>easl run examples/simple.easl</code></pre>
</div>
<div class="step">
<span class="step-n">3</span>
<p>Learn the language:</p>
<p class="doc-links"><a href="guide.html">Overview</a> ·
<a href="language.html">Language guide</a> ·
<a href="shaders.html">Writing shaders</a> ·
<a href="cpu.html">The CPU runtime</a> ·
<a href="functions.html">Function index</a></p>
</div>
</div>
</section>

</main>

<footer>
<p><a href="https://github.com/Ella-Hoeppner/easl">easl</a> ·
<a href="https://github.com/Ella-Hoeppner/easl_cli">easl_cli</a> ·
<a href="guide.html">documentation</a></p>
<p class="fine">Easl is a work in progress; expect breaking changes.</p>
</footer>

<script type="module">
import {{ startEaslProgram }} from "./{HERO_RUNTIME_DIR}/easl-program.js";
if (navigator.gpu) {{
  startEaslProgram(document.getElementById("hero-canvas")).catch((error) => {{
    console.error(error);
    document.body.classList.add("no-webgpu");
  }});
}} else {{
  document.body.classList.add("no-webgpu");
}}
</script>
</body>
</html>
"##,
    showcases = showcases,
    hero_highlighted = hero_highlighted,
  )
}
