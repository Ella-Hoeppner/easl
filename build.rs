//! With the `web-runtime` feature, provides the web runtimes `web_bundle`
//! embeds (`WEB_RUNTIME`, `COMPILER_WEB_RUNTIME`) in `OUT_DIR`: the one that
//! runs precompiled programs, and the one built with the compiler, which
//! compiles programs from their sources in the browser. In a checkout of the
//! repository, they're built from the `web/` crate. The crate published to
//! crates.io has no `web/` (it's a package of its own), so it carries them
//! prebuilt in `web_runtime/`, which `publish.sh` builds.

fn main() {
  #[cfg(feature = "web-runtime")]
  web::provide_runtime();
}

#[cfg(feature = "web-runtime")]
mod web {
  use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
  };

  use wasm_bindgen_cli_support::Bindgen;

  /// The files `web_bundle` embeds: the runtime that runs precompiled
  /// programs (`RUNTIME_FILES` there), then the one built with the compiler
  /// (`COMPILER_RUNTIME_FILES`).
  const RUNTIME_FILES: [&str; 4] = [
    "easl_web.js",
    "easl_web_bg.wasm",
    "easl_web_compiler.js",
    "easl_web_compiler_bg.wasm",
  ];

  /// Variables cargo sets for build scripts, besides the `CARGO_*` ones.
  const BUILD_SCRIPT_VARS: &[&str] = &[
    "TARGET",
    "HOST",
    "OUT_DIR",
    "OPT_LEVEL",
    "PROFILE",
    "DEBUG",
    "NUM_JOBS",
    "RUSTC",
    "RUSTDOC",
    "RUSTC_LINKER",
    "RUSTFLAGS",
  ];

  pub fn provide_runtime() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let web_dir = manifest_dir.join("web");
    if web_dir.join("Cargo.toml").exists() {
      for path in [
        manifest_dir.join("src"),
        manifest_dir.join("Cargo.toml"),
        web_dir.join("src"),
        web_dir.join("Cargo.toml"),
        web_dir.join("Cargo.lock"),
        web_dir.join(".cargo"),
      ] {
        println!("cargo:rerun-if-changed={}", path.display());
      }
      build_runtime(&web_dir, &out_dir);
    } else {
      let prebuilt_dir = manifest_dir.join("web_runtime");
      println!("cargo:rerun-if-changed={}", prebuilt_dir.display());
      for name in RUNTIME_FILES {
        let prebuilt = prebuilt_dir.join(name);
        fs::copy(&prebuilt, out_dir.join(name)).unwrap_or_else(|e| {
          panic!(
            "the prebuilt easl web runtime is missing {} ({e}): a package \
             without the `web/` crate carries it prebuilt (see `publish.sh`)",
            prebuilt.display()
          )
        });
      }
    }
  }

  /// Builds the runtimes from the `web/` crate into `out_dir`.
  fn build_runtime(web_dir: &Path, out_dir: &Path) {
    let target_dir = out_dir.join("web-target");
    for (profile, features, out_name) in [
      ("release", "", "easl_web"),
      ("release-compiler", "compiler", "easl_web_compiler"),
    ] {
      // The runtime is its own build, configured by
      // `web/.cargo/config.toml` (target and linker flags) — none of this
      // build's settings apply. Cargo describes this build script's target
      // in environment variables (`CARGO_CFG_UNIX`, `TARGET`, ...), which
      // the nested build would otherwise pass on to its own build scripts.
      let mut cargo = Command::new(env::var("CARGO").unwrap());
      cargo
        .current_dir(web_dir)
        .args(["build", "--profile", profile, "--features", features])
        .args(["--target", "wasm32-unknown-unknown", "--target-dir"])
        .arg(&target_dir);
      for (name, _) in env::vars_os() {
        let name = name.to_string_lossy();
        if (name.starts_with("CARGO_") && name != "CARGO_HOME")
          || BUILD_SCRIPT_VARS.contains(&&*name)
        {
          cargo.env_remove(&*name);
        }
      }
      let status = cargo
        .status()
        .expect("failed to run cargo to build the easl web runtime");
      if !status.success() {
        panic!(
          "building the easl web runtime failed. Building it from the \
           repository's `web/` crate needs the wasm32 target \
           (`rustup target add wasm32-unknown-unknown`)."
        );
      }

      Bindgen::new()
        .input_path(
          target_dir
            .join(format!("wasm32-unknown-unknown/{profile}/easl_web.wasm")),
        )
        .out_name(out_name)
        .web(true)
        .unwrap()
        .typescript(false)
        .omit_default_module_path(false)
        .generate(out_dir)
        .expect("failed to generate the easl web runtime's JS bindings");
    }
  }
}
