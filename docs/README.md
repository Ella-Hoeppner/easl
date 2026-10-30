# Easl documentation & site

The documentation lives as plain Markdown in [`pages/`](pages/), readable directly on GitHub or in any Markdown viewer. A static site generator in [`generator/`](generator/) turns those pages into a styled HTML site, and also builds the **landing page** — a front page with a live, WebGPU-rendered easl program as its hero background.

## Building the site

```
cd docs/generator
cargo run
```

This writes the site to `docs/site/`. Serve it with any static file server (e.g. `python3 -m http.server` in `docs/site/`) — the landing page loads the web runtime as an ES module and a `.wasm` file, which browsers won't do from `file://`. The hero animation needs a WebGPU-capable browser (current Chrome/Edge/Safari) and degrades to a static gradient elsewhere.

The build also builds the easl web runtime (`web/build.sh`), so it needs the `wasm32-unknown-unknown` target and `wasm-bindgen-cli` at the version pinned in `web/Cargo.toml` (see `web/build.sh`).

For a live preview while editing, rebuild on every change (refresh the browser to see it):

```
cd docs/generator
cargo watch -w ../pages -w assets -w src -x run
```

## The landing page is self-verifying

The generator depends on the easl compiler itself (`easl = { path = "../.." }`):

- Every easl snippet shown on the landing page (`generator/src/landing.rs`) is **compiled with the real compiler at site-build time** — a snippet that stops compiling fails the build, so the front page can't silently rot.
- The hero background is a whole easl program, [`pages/main_page_example.easl`](pages/main_page_example.easl), `@cpu` entry included. The build validates it (and naga-validates its WGSL), then packages it the way `easl compile --web` does: its source plus the easl web runtime go into `site/easl-web/`, and the page runs it on the hero canvas with the same compiler and bytecode VM as `easl run`, compiled to WebAssembly. The hero canvas is the program's window, so `window-resolution` is the hero's size in physical pixels.

## Layout

- `pages/guide.md`, `pages/language.md`, `pages/shaders.md`, `pages/cpu.md` — the guide pages
- `pages/reference/*.md` — the builtin function reference, one page per category
- `generator/` — the site generator: parses the Markdown with pulldown-cmark, applies a hand-rolled easl syntax highlighter to ```` ```easl ```` code fences, and wraps everything in a template with sidebar navigation and per-page tables of contents; `generator/src/landing.rs` builds the front page
- `generator/assets/` — the docs stylesheet and the landing stylesheet
- `site/` — the generated output (disposable; regenerate any time); `site/index.html` is the landing, `site/guide.html` is the docs entry point, `site/easl-web/` the hero program and the web runtime

## Conventions the generator relies on

- The nav order and page titles are declared in the `PAGES` table at the top of `generator/src/main.rs`; add new pages there.
- Every `###` heading on a reference page (other than `reference/overview.md`) is treated as a function entry: each backticked name in the heading becomes an entry in the generated, filterable **function index** page, linking to that heading.
- When a heading abbreviates a family of names (e.g. `` `vec2f`…`vec4b` ``), an HTML comment of the form `<!-- index: vec2f vec3f ... -->` placed after the heading adds the elided names to the function index.
- Relative links between `.md` files are rewritten to `.html` in the generated site, so cross-page links work both on GitHub and in the site.
- Heading anchors use GitHub's slug style, so `#section-name` fragment links also work in both places. Operator headings (whose names are all symbols) get readable generated anchors like `#op-plus-eq`.
