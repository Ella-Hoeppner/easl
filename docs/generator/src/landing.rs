//! The front page of the easl site.
//!
//! The landing page is self-verifying: every easl snippet displayed on it
//! is compiled with the real easl compiler when the site is built (a
//! snippet that stops compiling fails the build), and the animated hero
//! background is an actual easl program — `assets/demo.easl` — whose
//! compiled WGSL is naga-validated and embedded into the page for WebGPU
//! to run in the browser.

use crate::highlight;

/// The hero background program. Compiled to WGSL at build time; the page
/// runs the output with WebGPU, writing `[time, width, height, 0]` into
/// the `@[uniform 0 0]` binding each frame.
const DEMO_SOURCE: &str = include_str!("../assets/demo.easl");

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
    (Surface p) (* (vec3f 0.5)
                   (+ (vec3f 1.)
                      (gradient sphere p)))
    Sky (vec3f 0.02 0.02 0.04)))"#,
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

pub fn generate(site_dir: &std::path::Path) {
  // compile-check every snippet, and compile + validate the hero demo
  for showcase in SHOWCASES {
    compile_check(showcase.id, showcase.source);
  }
  let demo_wgsl = compile_check("hero demo", DEMO_SOURCE);
  naga_validate("hero demo", &demo_wgsl);

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

  let demo_highlighted = highlight::highlight_easl(DEMO_SOURCE);
  let html = landing_html(&demo_wgsl, &demo_highlighted, &showcases_html);
  std::fs::write(site_dir.join("index.html"), html).unwrap();
  std::fs::write(
    site_dir.join("landing.css"),
    include_str!("../assets/landing.css"),
  )
  .unwrap();
}

fn landing_html(
  demo_wgsl: &str,
  demo_highlighted: &str,
  showcases: &str,
) -> String {
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
easl program, compiled by the easl compiler when this site was built —
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
<p>This is the program painting the background above. The site's build
step compiles it with the real easl compiler, validates the WGSL, and
embeds the result for WebGPU — if it ever stops compiling, the site
stops building.</p>
</div>
<figure class="showcase-code">
<pre class="code easl tall"><code>{demo_highlighted}</code></pre>
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

<script>
const WGSL = `{wgsl}`;
async function boot() {{
  if (!navigator.gpu) throw new Error("no webgpu");
  const adapter = await navigator.gpu.requestAdapter();
  if (!adapter) throw new Error("no adapter");
  const device = await adapter.requestDevice();
  const canvas = document.getElementById("hero-canvas");
  const ctx = canvas.getContext("webgpu");
  const format = navigator.gpu.getPreferredCanvasFormat();
  ctx.configure({{ device, format, alphaMode: "opaque" }});
  const module = device.createShaderModule({{ code: WGSL }});
  const pipeline = device.createRenderPipeline({{
    layout: "auto",
    vertex: {{ module, entryPoint: "vert" }},
    fragment: {{ module, entryPoint: "frag", targets: [{{ format }}] }},
    primitive: {{ topology: "triangle-list" }},
  }});
  const ubuf = device.createBuffer({{
    size: 16,
    usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
  }});
  const bind = device.createBindGroup({{
    layout: pipeline.getBindGroupLayout(0),
    entries: [{{ binding: 0, resource: {{ buffer: ubuf }} }}],
  }});
  const start = performance.now();
  function frame() {{
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    const w = Math.floor(canvas.clientWidth * dpr);
    const h = Math.floor(canvas.clientHeight * dpr);
    if (w && h && (canvas.width !== w || canvas.height !== h)) {{
      canvas.width = w;
      canvas.height = h;
    }}
    device.queue.writeBuffer(
      ubuf,
      0,
      new Float32Array([
        (performance.now() - start) / 1000,
        canvas.width,
        canvas.height,
        0,
      ])
    );
    const encoder = device.createCommandEncoder();
    const pass = encoder.beginRenderPass({{
      colorAttachments: [
        {{
          view: ctx.getCurrentTexture().createView(),
          loadOp: "clear",
          clearValue: {{ r: 0.05, g: 0.05, b: 0.07, a: 1 }},
          storeOp: "store",
        }},
      ],
    }});
    pass.setPipeline(pipeline);
    pass.setBindGroup(0, bind);
    pass.draw(3);
    pass.end();
    device.queue.submit([encoder.finish()]);
    requestAnimationFrame(frame);
  }}
  requestAnimationFrame(frame);
}}
boot().catch(() => document.body.classList.add("no-webgpu"));
</script>
</body>
</html>
"##,
    showcases = showcases,
    demo_highlighted = demo_highlighted,
    wgsl = demo_wgsl.replace('\\', "\\\\").replace('`', "\\`").replace("${", "\\${"),
  )
}
