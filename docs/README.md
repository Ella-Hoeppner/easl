# Easl documentation & site

The documentation lives as plain Markdown in [`pages/`](pages/), readable directly on GitHub or in any Markdown viewer. A static site generator in [`generator/`](generator/) turns those pages into a styled HTML site, and also builds the **landing page**, a single screen with a live, WebGPU-rendered easl program as its background.

## Building the site

```
cd docs/generator
cargo run
```

This writes the site to `docs/site/`. Serve it with any static file server (e.g. `python3 -m http.server` in `docs/site/`): the landing page loads the web runtime as an ES module and a `.wasm` file, which browsers won't do from `file://`. The hero animation needs a WebGPU-capable browser (current Chrome/Edge/Safari) and degrades to a static gradient elsewhere.

The build also builds the easl web runtime (`web/build.sh`), so it needs the `wasm32-unknown-unknown` target and `wasm-bindgen-cli` at the version pinned in `web/Cargo.toml` (see `web/build.sh`).

For a live preview while editing, rebuild on every change (refresh the browser to see it):

```
cd docs/generator
cargo watch -w ../pages -w assets -w src -x run
```

## The landing page

Everything on the landing page (`generator/src/landing.rs`) comes from `pages/`, read on every build, so editing it means editing those files:

- `pages/landing/*.md` holds all of its text: `nav.md` (the links at the top), `hero.md`, `getting-started.md`, `program-note.md` (the caption under the code), and `no-webgpu.md` (the caption shown instead when the browser has no WebGPU).
- `hero.md` has to start with the name as a heading, then the tagline, then a description. Those three also make the page's `<title>` and meta description. Every link in it renders as a button.
- `pages/main_page_example.easl` is the program. The same file is both displayed on the page and run as its background, with the easl web runtime: the build validates it (and naga-validates its WGSL), then packages it the way `easl compile --web` does into `site/easl-web/`. If it stops compiling, the site stops building. The page's canvas is the program's window, so `window-resolution` is the canvas's size in physical pixels.

The page's markup is `generator/assets/landing.html`, a `format!` template, so literal braces in it are doubled. The code panel's text scales with the window so that every line fits on screen, down to a readable minimum, and the panel's transparency is set by `--code-panel-opacity` and `--code-panel-opacity-hover` at the top of `generator/assets/landing.css`.

## Live previews

A page can run an easl program on a canvas inline with an HTML comment, which GitHub and other Markdown viewers hide:

```
<!-- live-preview: raymarch/sphere.easl -->
<!-- live-preview: raymarch/cpu-raymarch.easl console -->
```

The path is relative to the page's own Markdown file, and `console` adds a box under the canvas showing the program's `print` output. The build fails if a program doesn't compile, and packages each one into `site/easl-web/` the way `easl compile --web` does; `generator/assets/live-previews.js` starts each preview the first time it nears the screen, so a page's previews don't all start at once, and pauses it whenever it's off screen.

The ray marching walkthrough's programs live in `pages/raymarch/`, each one the walkthrough's program as it stands at its preview. They're separate copies of the code shown in the page, so a change to the walkthrough's code blocks needs the matching change there.

## Layout

- `pages/guide.md`, `pages/language.md`, `pages/shaders.md`, `pages/cpu.md` — the guide pages
- `pages/raymarch.md` — the ray marching walkthrough, and `pages/raymarch/` the programs its live previews run
- `pages/landing/`, `pages/main_page_example.easl` — the landing page's text and program
- `pages/reference/*.md` — the builtin function reference, one page per category
- `generator/` — the site generator: parses the Markdown with pulldown-cmark, applies a hand-rolled easl syntax highlighter to ```` ```easl ```` code fences, and wraps everything in a template with sidebar navigation and per-page tables of contents; `generator/src/landing.rs` builds the landing page
- `generator/assets/` — the docs stylesheet, and the landing page's template and stylesheet
- `site/` — the generated output (disposable; regenerate any time); `site/index.html` is the landing, `site/guide.html` is the docs entry point, `site/easl-web/` the hero program and the web runtime

## Conventions the generator relies on

- The nav order and page titles are declared in the `PAGES` table at the top of `generator/src/main.rs`; add new pages there.
- Every `###` heading on a reference page (other than `reference/overview.md`) is treated as a function entry: each backticked name in the heading becomes an entry in the generated, filterable **function index** page, linking to that heading.
- When a heading abbreviates a family of names (e.g. `` `vec2f`…`vec4b` ``), an HTML comment of the form `<!-- index: vec2f vec3f ... -->` placed after the heading adds the elided names to the function index.
- Relative links between `.md` files are rewritten to `.html` in the generated site, so cross-page links work both on GitHub and in the site.
- Heading anchors use GitHub's slug style, so `#section-name` fragment links also work in both places. Operator headings (whose names are all symbols) get readable generated anchors like `#op-plus-eq`.
