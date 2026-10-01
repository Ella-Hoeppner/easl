//! Packaging a program for the web runtime (`web/`): the page and the
//! module that hand the program's sources to the prebuilt runtime, which
//! compiles and runs them in the browser.

use std::path::{Path, PathBuf};

use crate::{
  compiler::{core::load_easl_program_from_file, program::CompilerTarget},
  parse::EaslMultiDocument,
};

/// The page that runs the program on a full-window canvas.
pub const INDEX_HTML: &str = include_str!("../web/templates/index.html");

/// Files every web build includes alongside the runtime's `easl_web.js` and
/// `easl_web_bg.wasm`, the same for every program: the audio worklet that
/// runs a program's audio thread, and the text-encoding polyfill it needs.
pub const RUNTIME_SUPPORT_FILES: &[(&str, &str)] = &[
  (
    "easl-audio-worklet.js",
    include_str!("../web/templates/easl-audio-worklet.js"),
  ),
  (
    "easl-text-polyfill.js",
    include_str!("../web/templates/easl-text-polyfill.js"),
  ),
];

const PROGRAM_JS_TEMPLATE: &str =
  include_str!("../web/templates/easl-program.js");

/// The program-specific files of a web build. Alongside them go the
/// runtime's `easl_web.js` and `easl_web_bg.wasm` and the
/// [`RUNTIME_SUPPORT_FILES`], which are the same for every program.
pub struct WebBundle {
  /// `index.html`: see [`INDEX_HTML`].
  pub index_html: &'static str,
  /// `easl-program.js`: the program's sources, and the `startEaslProgram`
  /// export that runs them.
  pub program_js: String,
}

/// Validates the program whose `@cpu` entry is in `main_path` and packages
/// it and every file it imports. Errors describe why the program doesn't
/// compile.
pub fn bundle_program(main_path: &Path) -> Result<WebBundle, String> {
  let documents = match load_easl_program_from_file(main_path)
    .map_err(|e| format!("IO error reading {}: {e}", main_path.display()))?
  {
    Ok((documents, Ok(mut program))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      if !errors.is_empty() {
        return Err(errors.describe(&documents));
      }
      documents
    }
    Ok((documents, Err(errors))) => return Err(errors.describe(&documents)),
    Err(failed_documents) => {
      return Err(failed_documents.describe_parse_failures());
    }
  };
  let (main_path, files) = relative_sources(&documents);
  let files_json = files
    .iter()
    .map(|(path, source)| {
      format!("\n  {}: {}", json_string(path), json_string(source))
    })
    .collect::<Vec<_>>()
    .join(",");
  Ok(WebBundle {
    index_html: INDEX_HTML,
    program_js: PROGRAM_JS_TEMPLATE
      .replace("__EASL_MAIN_PATH__", &json_string(&main_path))
      .replace("__EASL_FILES__", &format!("{{{files_json}\n}}")),
  })
}

/// The documents' sources keyed by their paths relative to the directory
/// containing them all, so a bundle never exposes where the program lives
/// on disk. The first document is the main file.
fn relative_sources(
  documents: &EaslMultiDocument,
) -> (String, Vec<(String, String)>) {
  let paths: Vec<PathBuf> = documents
    .sources
    .iter()
    .map(|(_, path, _)| PathBuf::from(path))
    .collect();
  let mut root = paths[0].parent().unwrap_or(Path::new("")).to_path_buf();
  while !paths.iter().all(|path| path.starts_with(&root)) {
    root.pop();
  }
  let relative = |path: &Path| {
    path
      .strip_prefix(&root)
      .unwrap()
      .to_string_lossy()
      .into_owned()
  };
  let files = paths
    .iter()
    .zip(&documents.sources)
    .map(|(path, (_, _, source))| (relative(path), source.clone()))
    .collect();
  (relative(&paths[0]), files)
}

/// `s` as a JSON string literal, which is also a valid JavaScript one.
fn json_string(s: &str) -> String {
  let mut quoted = String::with_capacity(s.len() + 2);
  quoted.push('"');
  for c in s.chars() {
    match c {
      '"' => quoted.push_str("\\\""),
      '\\' => quoted.push_str("\\\\"),
      '\n' => quoted.push_str("\\n"),
      '\r' => quoted.push_str("\\r"),
      '\t' => quoted.push_str("\\t"),
      // Control characters, and the line separators JavaScript string
      // literals can't contain unescaped in older engines.
      c if (c as u32) < 0x20 || c == '\u{2028}' || c == '\u{2029}' => {
        quoted.push_str(&format!("\\u{:04x}", c as u32))
      }
      c => quoted.push(c),
    }
  }
  quoted.push('"');
  quoted
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn json_string_escapes() {
    assert_eq!(
      json_string("a\"b\\c\nd\u{1}\u{2028}é"),
      "\"a\\\"b\\\\c\\nd\\u0001\\u2028é\""
    );
  }
}
