// The audio thread of an easl program on the web: an AudioWorklet
// processor holding its own instance of the easl runtime, which runs the
// audio program the page compiled one render quantum at a time.
// Shared variables and MIDI arrive from the page as messages, and shared
// variables this thread publishes go back the same way.

import "./easl-text-polyfill.js";
import { initSync, AudioEngine } from "./easl_web.js";

class EaslAudioProcessor extends AudioWorkletProcessor {
  constructor(options) {
    super();
    const { module, program, functionNames } = options.processorOptions;
    this.engine = null;
    this.running = false;
    this.port.onmessage = (event) => this.receive(event.data);
    try {
      initSync({ module });
      this.engine = new AudioEngine(program, functionNames);
    } catch (error) {
      this.fail(error);
    }
  }

  fail(error) {
    this.running = false;
    this.port.postMessage({ type: "error", message: String(error) });
  }

  receive(message) {
    if (!this.engine) return;
    try {
      switch (message.type) {
        case "start":
          this.engine.start(message.entry);
          this.running = true;
          break;
        case "switch":
          this.engine.switchEntry(message.entry);
          break;
        case "snapshot":
          this.engine.installSnapshot(message.index, message.words);
          break;
        case "midi":
          this.engine.midiMessage(message.bytes);
          break;
      }
    } catch (error) {
      this.fail(error);
    }
  }

  process(_inputs, outputs) {
    const output = outputs[0];
    if (!this.running || output.length === 0) return true;
    try {
      const channel = output[0];
      const published = this.engine.render(channel, sampleRate);
      for (let c = 1; c < output.length; c++) output[c].set(channel);
      for (const index of published) {
        this.port.postMessage({
          type: "snapshot",
          index,
          words: this.engine.snapshot(index),
        });
      }
    } catch (error) {
      this.fail(error);
    }
    return true;
  }
}

registerProcessor("easl-audio", EaslAudioProcessor);
