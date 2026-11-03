import init, { startProgram, sendMidiMessage } from "./easl_web.js";

// The program's sources, keyed by path. Filled in by the easl compiler.
const MAIN_PATH = __EASL_MAIN_PATH__;
const FILES = __EASL_FILES__;

let initialized = null;

/**
 * Loads the runtime, once. The compiled module is kept for the audio
 * worklet, which instantiates its own copy of the runtime.
 */
async function initialize() {
  const response = await fetch(new URL("./easl_web_bg.wasm", import.meta.url));
  const module = await WebAssembly.compile(await response.arrayBuffer());
  await init({ module_or_path: module });
  return module;
}

/**
 * Compiles and runs the program, rendering into `canvas`, and returns it at
 * once, to control and wait on:
 *
 * - `pause()` stops it before its next frame: its canvas keeps showing the
 *   last frame, its audio is suspended, and its clock stops, so
 *   `window-time` continues where it left off when it's resumed.
 * - `resume()` resumes it.
 * - `paused` is whether it's paused.
 * - `finished` is a promise that resolves once the program finishes (a
 *   program with a window runs until it calls `close-window`), and rejects
 *   with a description of any compile or runtime error.
 *
 * It can be paused before it has started; it then starts paused.
 *
 * `options.onPrint`, if given, is called with each line the program prints,
 * in place of the browser console.
 */
export function startEaslProgram(canvas, options = {}) {
  let program = null;
  let paused = false;
  const finished = (async () => {
    initialized ??= initialize();
    const module = await initialized;
    program = startProgram(
      canvas,
      MAIN_PATH,
      Object.keys(FILES),
      Object.values(FILES),
      {
        // The audio worklet instantiates its own copy of the runtime.
        module,
        workletUrl: new URL("./easl-audio-worklet.js", import.meta.url).href,
        startMidi: listenToMidi,
        onPrint: options.onPrint,
      },
    );
    if (paused) program.pause();
    await program.finished;
  })();
  return {
    pause() {
      paused = true;
      program?.pause();
    },
    resume() {
      paused = false;
      program?.resume();
    },
    get paused() {
      return paused;
    },
    finished,
  };
}

/**
 * Feeds a raw MIDI message (an array of bytes, status byte first) to the
 * program, as if a MIDI device sent it.
 */
export { sendMidiMessage };

/** Forwards every MIDI input device's messages to the program. */
async function listenToMidi() {
  if (!navigator.requestMIDIAccess) {
    console.warn("easl: this browser has no Web MIDI support");
    return;
  }
  try {
    const access = await navigator.requestMIDIAccess();
    const listen = (input) => {
      input.onmidimessage = (event) => sendMidiMessage(event.data);
    };
    access.inputs.forEach(listen);
    access.onstatechange = (event) => {
      if (event.port.type === "input" && event.port.state === "connected") {
        listen(event.port);
      }
    };
  } catch (error) {
    console.warn("easl: MIDI unavailable:", error);
  }
}
