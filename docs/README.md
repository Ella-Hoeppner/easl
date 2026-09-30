# Easl documentation & site

The documentation lives as plain Markdown in [`pages/`](pages/), readable directly on GitHub or in any Markdown viewer. A static site generator in [`generator/`](generator/) turns those pages into a styled HTML site, and also builds the **landing page** — a front page with a live, WebGPU-rendered easl program as its hero background.

## Building the site

```
cd docs/generator
cargo run
```

This writes the site to `docs/site/` — open `docs/site/index.html` in a browser (no server needed; the hero animation needs a WebGPU-capable browser — current Chrome/Edge/Safari — and degrades to a static gradient elsewhere).

## The landing page is self-verifying

The generator depends on the easl compiler itself (`easl = { path = "../.." }`):

- Every easl snippet shown on the landing page (`generator/src/landing.rs`) is **compiled with the real compiler at site-build time** — a snippet that stops compiling fails the build, so the front page can't silently rot.
- The hero background is an actual easl program, [`generator/assets/demo.easl`](generator/assets/demo.easl). Its compiled WGSL is naga-validated and embedded into the page, where a small inline WebGPU harness runs it (fullscreen triangle, one `@[uniform 0 0]` vec4 carrying `[time, width, height, 0]`).

To preview the hero art without a browser, append an offscreen-render `@cpu` main to `demo.easl` (render to a `blank-texture` target, `save-png`) and `easl run` it — that's how the shader was tuned.

## Layout

- `pages/guide.md`, `pages/language.md`, `pages/shaders.md`, `pages/cpu.md` — the guide pages
- `pages/reference/*.md` — the builtin function reference, one page per category
- `generator/` — the site generator: parses the Markdown with pulldown-cmark, applies a hand-rolled easl syntax highlighter to ```` ```easl ```` code fences, and wraps everything in a template with sidebar navigation and per-page tables of contents; `generator/src/landing.rs` builds the front page
- `generator/assets/` — the docs stylesheet, the landing stylesheet, and the hero demo program
- `site/` — the generated output (disposable; regenerate any time); `site/index.html` is the landing, `site/guide.html` is the docs entry point

## Conventions the generator relies on

- The nav order and page titles are declared in the `PAGES` table at the top of `generator/src/main.rs`; add new pages there.
- Every `###` heading on a reference page (other than `reference/overview.md`) is treated as a function entry: each backticked name in the heading becomes an entry in the generated, filterable **function index** page, linking to that heading.
- When a heading abbreviates a family of names (e.g. `` `vec2f`…`vec4b` ``), an HTML comment of the form `<!-- index: vec2f vec3f ... -->` placed after the heading adds the elided names to the function index.
- Relative links between `.md` files are rewritten to `.html` in the generated site, so cross-page links work both on GitHub and in the site.
- Heading anchors use GitHub's slug style, so `#section-name` fragment links also work in both places. Operator headings (whose names are all symbols) get readable generated anchors like `#op-plus-eq`.
