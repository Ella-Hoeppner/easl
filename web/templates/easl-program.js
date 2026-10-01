import init, { runEaslProgram, sendMidiMessage } from "./easl_web.js";

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
 * Compiles and runs the program, rendering into `canvas`. Resolves once the
 * program finishes (a program with a window runs until it calls
 * `close-window`); rejects with a description of any compile or runtime
 * error.
 */
export async function startEaslProgram(canvas) {
  initialized ??= initialize();
  const module = await initialized;
  await runEaslProgram(
    canvas,
    MAIN_PATH,
    Object.keys(FILES),
    Object.values(FILES),
    {
      // The audio worklet instantiates its own copy of the runtime.
      module,
      workletUrl: new URL("./easl-audio-worklet.js", import.meta.url).href,
      startMidi: listenToMidi,
    },
  );
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
