#!/bin/sh
# Builds the easl web runtime into web/dist: easl_web.js and
# easl_web_bg.wasm, the files every compiled web program loads. Needs the
# wasm32-unknown-unknown target (`rustup target add wasm32-unknown-unknown`)
# and wasm-bindgen-cli at the version pinned in Cargo.toml
# (`cargo install wasm-bindgen-cli --version 0.2.129`).
set -e
cd "$(dirname "$0")"
cargo build --release
wasm-bindgen --target web --no-typescript --out-dir dist \
  target/wasm32-unknown-unknown/release/easl_web.wasm
