#!/bin/sh
# Builds the easl web runtimes into web/dist: easl_web.js and
# easl_web_bg.wasm, which run precompiled programs (what every compiled web
# program loads), and easl_web_compiler.js and easl_web_compiler_bg.wasm,
# which also compile programs from their sources in the browser. Needs the
# wasm32-unknown-unknown target
# (`rustup target add wasm32-unknown-unknown`) and wasm-bindgen-cli at the
# version pinned in Cargo.toml
# (`cargo install wasm-bindgen-cli --version 0.2.129`).
set -e
cd "$(dirname "$0")"
cargo build --release --target wasm32-unknown-unknown
wasm-bindgen --target web --no-typescript --out-dir dist \
  target/wasm32-unknown-unknown/release/easl_web.wasm
cargo build --profile release-compiler --features compiler \
  --target wasm32-unknown-unknown
wasm-bindgen --target web --no-typescript --out-dir dist \
  --out-name easl_web_compiler \
  target/wasm32-unknown-unknown/release-compiler/easl_web.wasm
