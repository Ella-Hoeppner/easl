//! Live previews: easl programs running on canvases inside docs pages.
//!
//! A page places one with an HTML comment naming the program, relative to
//! the page's own Markdown file:
//!
//! ```text
//! <!-- live-preview: raymarch/sphere.easl -->
//! <!-- live-preview: raymarch/cpu-raymarch.easl console -->
//! ```
//!
//! Markdown viewers like GitHub hide the comment. `console` shows the
//! program's `print` output under the canvas. The build fails if a program
//! doesn't compile, and precompiles each one as `easl compile --web` would;
//! `assets/live-previews.js` starts each preview when it first nears the
//! screen.

use std::{
  fs,
  path::{Path, PathBuf},
};

use easl::web_bundle::bundle_program;

const MARKER: &str = "<!-- live-preview:";

pub struct LivePreview {
  /// The program's main file, relative to `pages/`.
  source: PathBuf,
  /// The packaged program's module's file name in the runtime directory.
  module: String,
  /// The precompiled program's file name in the runtime directory, which
  /// the module loads.
  data: String,
  /// Whether the program's `print` output is shown under the canvas.
  console: bool,
}

impl LivePreview {
  /// The preview a block of HTML places, if it's a live-preview marker on a
  /// page whose Markdown file is in `page_dir` (relative to `pages/`).
  pub fn from_marker(html: &str, page_dir: &Path) -> Option<LivePreview> {
    let inner = html.trim().strip_prefix(MARKER)?.strip_suffix("-->")?;
    let mut words = inner.split_whitespace();
    let path = words
      .next()
      .unwrap_or_else(|| panic!("a live-preview marker must name a program"));
    let mut console = false;
    for option in words {
      match option {
        "console" => console = true,
        other => panic!("unknown live-preview option `{other}` for {path}"),
      }
    }
    let source = page_dir.join(path);
    let name = format!(
      "preview-{}",
      source
        .with_extension("")
        .to_string_lossy()
        .replace('/', "-")
    );
    Some(LivePreview {
      source,
      module: format!("{name}.js"),
      data: format!("{name}.bin"),
      console,
    })
  }

  /// The preview's markup, for a page `prefix` away from the site root.
  pub fn html(&self, prefix: &str) -> String {
    let console = if self.console {
      "<pre class=\"live-console\" data-placeholder=\"Output from print \
       appears here.\"></pre>\n"
    } else {
      ""
    };
    format!(
      "<figure class=\"live-preview\" data-module=\"{prefix}{runtime}/{module}\">\n\
       <canvas></canvas>\n\
       {console}\
       <p class=\"live-preview-note no-webgpu-note\">This live preview needs \
       a browser with WebGPU.</p>\n\
       <p class=\"live-preview-note failed-note\">This live preview couldn't \
       start; the browser console has the details.</p>\n\
       </figure>\n",
      runtime = crate::landing::RUNTIME_DIR,
      module = self.module,
    )
  }

  /// Validates the program and writes it, packaged for the web runtime,
  /// into `runtime_dir`.
  pub fn package(&self, pages_dir: &Path, runtime_dir: &Path) {
    let path = pages_dir.join(&self.source);
    let bundle = bundle_program(&path).unwrap_or_else(|errors| {
      panic!(
        "the live preview program {} doesn't compile:\n{errors}",
        path.display()
      )
    });
    fs::write(
      runtime_dir.join(&self.module),
      bundle.program_js(&self.data),
    )
    .unwrap();
    fs::write(runtime_dir.join(&self.data), bundle.program_data).unwrap();
  }
}
