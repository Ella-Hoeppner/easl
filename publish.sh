#!/bin/sh
# Publishes easl to crates.io with its web runtime prebuilt in
# `web_runtime/` (which git ignores): the published crate has no `web/` crate
# to build it from (see `build.rs`). Run it on a clean checkout of the commit
# to publish. Its arguments go to `cargo publish` (`./publish.sh --dry-run`
# checks everything without publishing). Building the runtime needs what
# `web/build.sh` needs.
set -e
cd "$(dirname "$0")"
if [ -n "$(git status --porcelain)" ]; then
  echo "publish.sh: the working tree has uncommitted changes" >&2
  exit 1
fi
rm -rf web_runtime
trap 'rm -rf web_runtime' EXIT
web/build.sh
mkdir web_runtime
cp web/dist/easl_web.js web/dist/easl_web_bg.wasm \
  web/dist/easl_web_compiler.js web/dist/easl_web_compiler_bg.wasm \
  web_runtime/
# Cargo counts the ignored `web_runtime/` as uncommitted (it's packaged), so
# `--allow-dirty`: the check above covers everything else. Verifying the
# package with `web-runtime` builds it the way a user of the crate does.
cargo publish --allow-dirty --features web-runtime "$@"
