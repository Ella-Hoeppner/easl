import init, { startProgram, sendMidiMessage } from "./easl_web.js";

// The precompiled program, in a file beside this module. Filled in by the
// easl compiler.
const PROGRAM_URL = new URL(__EASL_PROGRAM_DATA__, import.meta.url);

let initialized = null;

/**
 * Loads the runtime and the program, once. The compiled runtime module is
 * kept for the audio worklet, which instantiates its own copy.
 */
async function initialize() {
  const [module, program] = await Promise.all([
    fetch(new URL("./easl_web_bg.wasm", import.meta.url))
      .then((response) => response.arrayBuffer())
      .then((bytes) => WebAssembly.compile(bytes)),
    fetch(PROGRAM_URL).then((response) => {
      if (!response.ok) {
        throw new Error(`easl: couldn't load the program: ${response.status}`);
      }
      return response.arrayBuffer();
    }),
  ]);
  await init({ module_or_path: module });
  return { module, program: new Uint8Array(program) };
}

/**
 * Runs the program, rendering into `canvas`, and returns it at once, to
 * control and wait on:
 *
 * - `pause()` stops it before its next frame: its canvas keeps showing the
 *   last frame, its audio is suspended, and its clock stops, so
 *   `window-time` continues where it left off when it's resumed.
 * - `resume()` resumes it.
 * - `paused` is whether it's paused.
 * - `finished` is a promise that resolves once the program finishes (a
 *   program with a window runs until it calls `close-window`), and rejects
 *   with a description of any runtime error.
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
    const { module, program: bytes } = await initialized;
    program = startProgram(canvas, bytes, {
      // The audio worklet instantiates its own copy of the runtime.
      module,
      workletUrl: new URL("./easl-audio-worklet.js", import.meta.url).href,
      startMidi: listenToMidi,
      onPrint: options.onPrint,
    });
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
