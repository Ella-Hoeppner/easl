//! Packaging a program for the web runtime (`web/`): the page, the
//! program precompiled (see `crate::precompiled`), and the module that
//! hands it to the prebuilt runtime, which runs it in the browser. A
//! program can also be packaged as its sources, for the runtime built with
//! the compiler, which compiles them in the browser.

use std::path::{Path, PathBuf};

use crate::{
  compiler::{
    core::load_easl_program_from_file,
    program::{CompilerTarget, Program},
  },
  parse::EaslMultiDocument,
  precompiled::PrecompiledProgram,
};

/// The page that runs the program on a full-window canvas.
pub const INDEX_HTML: &str = include_str!("web_templates/index.html");

/// The runtime that runs precompiled programs (`web/build.sh` builds it):
/// its JS bindings and its wasm module.
pub const RUNTIME_FILES: [&str; 2] = ["easl_web.js", "easl_web_bg.wasm"];

/// The runtime built with the compiler, which compiles programs from their
/// sources: its JS bindings and its wasm module. Pages using it need the
/// [`RUNTIME_FILES`] too, which the audio worklet runs.
pub const COMPILER_RUNTIME_FILES: [&str; 2] =
  ["easl_web_compiler.js", "easl_web_compiler_bg.wasm"];

/// Files every web build includes alongside the runtime, the same for every
/// program: the audio worklet that runs a program's audio thread, and the
/// text-encoding polyfill it needs.
pub const RUNTIME_SUPPORT_FILES: &[(&str, &str)] = &[
  (
    "easl-audio-worklet.js",
    include_str!("web_templates/easl-audio-worklet.js"),
  ),
  (
    "easl-text-polyfill.js",
    include_str!("web_templates/easl-text-polyfill.js"),
  ),
];

const PROGRAM_JS_TEMPLATE: &str = include_str!("web_templates/easl-program.js");

const PROGRAM_FROM_SOURCES_JS_TEMPLATE: &str =
  include_str!("web_templates/easl-program-from-sources.js");

/// The name `index.html` loads the program's module by.
pub const PROGRAM_JS_FILE: &str = "easl-program.js";

/// The name `easl compile --web` gives the precompiled program.
pub const PROGRAM_DATA_FILE: &str = "easl-program.bin";

/// The program-specific files of a web build of a precompiled program.
/// Alongside them go the [`RUNTIME_FILES`] and the
/// [`RUNTIME_SUPPORT_FILES`], which are the same for every program.
pub struct WebBundle {
  /// `index.html`: see [`INDEX_HTML`].
  pub index_html: &'static str,
  /// The precompiled program, which the program's module loads from the
  /// file named when making it ([`Self::program_js`]).
  pub program_data: Vec<u8>,
}

impl WebBundle {
  /// The program's module (`easl-program.js`, for `index.html`), with the
  /// `startEaslProgram` export that runs the program, loading it from
  /// `data_file`, a URL relative to the module.
  pub fn program_js(&self, data_file: &str) -> String {
    PROGRAM_JS_TEMPLATE
      .replace("__EASL_PROGRAM_DATA__", &json_string(data_file))
  }
}

/// The program-specific files of a web build that compiles the program in
/// the browser. Alongside them go the [`COMPILER_RUNTIME_FILES`], the
/// [`RUNTIME_FILES`], and the [`RUNTIME_SUPPORT_FILES`].
pub struct SourcesWebBundle {
  /// `index.html`: see [`INDEX_HTML`].
  pub index_html: &'static str,
  /// `easl-program.js`: the program's sources, and the `startEaslProgram`
  /// export that compiles and runs them.
  pub program_js: String,
}

/// Validates and precompiles the program whose `@cpu` entry is in
/// `main_path`. Errors describe why the program doesn't compile.
pub fn bundle_program(main_path: &Path) -> Result<WebBundle, String> {
  let (_, program) = validated_program(main_path)?;
  let program = PrecompiledProgram::new(program).map_err(|e| e.to_string())?;
  Ok(WebBundle {
    index_html: INDEX_HTML,
    program_data: program.to_bytes(),
  })
}

/// Validates the program whose `@cpu` entry is in `main_path` and packages
/// it and every file it imports as sources, for the runtime built with the
/// compiler. Errors describe why the program doesn't compile.
pub fn bundle_program_sources(
  main_path: &Path,
) -> Result<SourcesWebBundle, String> {
  let (documents, _) = validated_program(main_path)?;
  let (main_path, files) = relative_sources(&documents);
  let files_json = files
    .iter()
    .map(|(path, source)| {
      format!("\n  {}: {}", json_string(path), json_string(source))
    })
    .collect::<Vec<_>>()
    .join(",");
  Ok(SourcesWebBundle {
    index_html: INDEX_HTML,
    program_js: PROGRAM_FROM_SOURCES_JS_TEMPLATE
      .replace("__EASL_MAIN_PATH__", &json_string(&main_path))
      .replace("__EASL_FILES__", &format!("{{{files_json}\n}}")),
  })
}

/// Loads and validates the program whose `@cpu` entry is in `main_path`.
fn validated_program(
  main_path: &Path,
) -> Result<(EaslMultiDocument, Program), String> {
  match load_easl_program_from_file(main_path)
    .map_err(|e| format!("IO error reading {}: {e}", main_path.display()))?
  {
    Ok((documents, Ok(mut program))) => {
      let errors = program.validate_raw_program(CompilerTarget::WGSL);
      if errors.is_empty() {
        Ok((documents, program))
      } else {
        Err(errors.describe(&documents))
      }
    }
    Ok((documents, Err(errors))) => Err(errors.describe(&documents)),
    Err(failed_documents) => Err(failed_documents.describe_parse_failures()),
  }
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
