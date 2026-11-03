//! The front page of the easl site: a single screen with a live easl
//! program as its background and that program's source on top of it.
//!
//! Everything on the page comes from `pages/`, read on every build. The
//! prose is Markdown in `pages/landing/`. The program is
//! `pages/main_page_example.easl`, `@cpu` entry and all: the build fails if
//! it doesn't compile, and the page both displays that file and runs it
//! with the easl web runtime (`web/`), packaged as `easl compile --web`
//! would package it.

use std::{
  fs,
  path::{Path, PathBuf},
  process::Command,
};

use easl::{
  compile_easl_file_to_wgsl,
  web_bundle::{bundle_program, RUNTIME_SUPPORT_FILES},
};
use pulldown_cmark::{html::push_html, Event, Parser, Tag, TagEnd};

use crate::{escape_html, highlight, rewrite_md_link};

/// The page's prose, relative to the generator's manifest directory.
const LANDING_PAGES_PATH: &str = "../pages/landing";

/// The background program, relative to the generator's manifest directory.
const HERO_PROGRAM_PATH: &str = "../pages/main_page_example.easl";

/// The easl web runtime crate, relative to the generator's manifest
/// directory.
const WEB_RUNTIME_PATH: &str = "../../web";

/// Where the web runtime and the programs it runs (the background program
/// and every page's live previews) go, relative to the site root.
pub const RUNTIME_DIR: &str = "easl-web";

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
    if (name.starts_with("CARGO") && name != "CARGO_HOME") || name == "TARGET" {
      build.env_remove(name.as_ref());
    }
  }
  let status = build
    .status()
    .unwrap_or_else(|e| panic!("couldn't run web/build.sh: {e}"));
  assert!(
    status.success(),
    "web/build.sh failed (see its output above)"
  );
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
    _ => {
      panic!("the landing page program validated but didn't compile to WGSL")
    }
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

fn read_source(path: &Path) -> String {
  fs::read_to_string(path)
    .unwrap_or_else(|e| panic!("couldn't read {}: {e}", path.display()))
}

/// `source` as HTML, with links between Markdown files pointing at the
/// generated pages.
fn render_markdown(source: &str) -> String {
  let events = Parser::new(source).map(|event| match event {
    Event::Start(Tag::Link {
      link_type,
      dest_url,
      title,
      id,
    }) => Event::Start(Tag::Link {
      link_type,
      dest_url: rewrite_md_link(&dest_url),
      title,
      id,
    }),
    event => event,
  });
  let mut html = String::new();
  push_html(&mut html, events);
  html
}

/// The plain text of each heading and paragraph in `source`.
fn text_blocks(source: &str) -> Vec<String> {
  let mut blocks = Vec::new();
  let mut block = String::new();
  for event in Parser::new(source) {
    match event {
      Event::Text(text) | Event::Code(text) => block.push_str(&text),
      Event::SoftBreak | Event::HardBreak => block.push(' '),
      Event::End(TagEnd::Heading(_) | TagEnd::Paragraph) => {
        blocks.push(std::mem::take(&mut block));
      }
      _ => {}
    }
  }
  blocks
}

pub fn generate(site_dir: &Path) {
  let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let landing_dir = manifest_dir.join(LANDING_PAGES_PATH);
  let prose = |name: &str| read_source(&landing_dir.join(name));

  let hero_path = manifest_dir.join(HERO_PROGRAM_PATH);
  package_hero_program(
    &hero_path,
    &manifest_dir.join(WEB_RUNTIME_PATH),
    &site_dir.join(RUNTIME_DIR),
  );

  let hero = prose("hero.md");
  let [name, tagline, description, ..] = &text_blocks(&hero)[..] else {
    panic!("hero.md must start with a name, a tagline, and a description");
  };
  let hero_source = read_source(&hero_path);
  let hero_source = hero_source.trim_end();

  let html = format!(
    include_str!("../assets/landing.html"),
    title = escape_html(&format!("{name} — {}", tagline.trim_end_matches('.'))),
    description = escape_html(description),
    nav = render_markdown(&prose("nav.md")),
    hero = render_markdown(&hero),
    getting_started = render_markdown(&prose("getting-started.md")),
    program_lines = hero_source.lines().count(),
    program = highlight::highlight_easl(hero_source),
    program_note = render_markdown(&prose("program-note.md")),
    no_webgpu = render_markdown(&prose("no-webgpu.md")),
    runtime_dir = RUNTIME_DIR,
  );
  fs::write(site_dir.join("index.html"), html).unwrap();
  fs::write(
    site_dir.join("landing.css"),
    include_str!("../assets/landing.css"),
  )
  .unwrap();
}
