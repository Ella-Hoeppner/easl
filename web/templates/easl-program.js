import init, { runEaslProgram } from "./easl_web.js";

// The program's sources, keyed by path. Filled in by the easl compiler.
const MAIN_PATH = __EASL_MAIN_PATH__;
const FILES = __EASL_FILES__;

let initialized = null;

/**
 * Compiles and runs the program, rendering into `canvas`. Resolves once the
 * program finishes (a program with a window runs until it calls
 * `close-window`); rejects with a description of any compile or runtime
 * error.
 */
export async function startEaslProgram(canvas) {
  initialized ??= init();
  await initialized;
  await runEaslProgram(
    canvas,
    MAIN_PATH,
    Object.keys(FILES),
    Object.values(FILES),
  );
}
