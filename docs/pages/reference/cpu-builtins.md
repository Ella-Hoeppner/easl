# CPU builtins

Builtins of easl's [CPU runtime](../cpu.md): printing, strings, windowing, GPU dispatch, input, textures, audio, MIDI, and video. Most are CPU-only; each entry notes where else it works.

## Printing

### `print`

```easl
(print x: T): ()
```

Prints any value. `u32`s print as `1u`, `i32`s bare (`1`), whole `f32`s with a trailing dot (`2.`), strings in double quotes, and arrays space-separated in square brackets.

## Strings

`String` values exist only in CPU code. Indices count characters.

### `string`

```easl
(string x: T): String
```

Formats any value as `print` would, without quoting strings: `(string 5u)` is `"5u"`.

### `concat`

```easl
(concat a: String b: String): String
(concat a: [T] b: [T]): [T]
```

Joins strings, or runtime-sized arrays. **Associative.**

### `substr`

```easl
(substr s: String start: u32 end: u32): String
```

The characters from `start` up to (not including) `end`. Out-of-range indices clamp, so this never fails.

`(length s)` gives a string's character count, and `==` / `!=` compare contents.

## Windowing

### `spawn-window`

```easl
(spawn-window frame-fn: (Fn [] ())): ()
```

Opens a window and calls `frame-fn` once per frame until it closes. See [the frame loop](../cpu.md#the-frame-loop).

### `close-window`

```easl
(close-window): ()
```

Closes the window at the end of the current frame, ending the `spawn-window` loop.

## GPU dispatch

### `dispatch-render-shaders`

```easl
(dispatch-render-shaders vert: V frag: F vert-count: u32): ()
(dispatch-render-shaders vert: V frag: F vert-count: u32 additive-blend: bool): ()
```

Draws `vert-count` vertices with the vertex shader `vert` and fragment shader `frag`, which can be named functions or inline `fn`s. `additive-blend` switches from alpha blending to additive. Draws go to the window unless `set-render-target` redirects them.

### `dispatch-compute-shader`

```easl
(dispatch-compute-shader entry: F workgroup-count: vec3u): ()
```

Runs the compute shader `entry` over `workgroup-count` workgroups (the shader's `@{workgroup-size ...}` sets the threads in each). See [CPU and GPU writes](../cpu.md#cpu-and-gpu-see-each-others-writes).

## Window and input queries

These work in both CPU and GPU code, and return the same value for the whole frame.

### `window-resolution`

```easl
(window-resolution): vec2u
```

The window's current size in pixels.

### `window-time`

```easl
(window-time): f32
```

Seconds since the window opened.

### `window-delta-time`

```easl
(window-delta-time): f32
```

Seconds elapsed between the previous frame and this one.

### `window-frame-index`

```easl
(window-frame-index): u32
```

The current frame number, counting from 0.

### `mouse-coords`

```easl
(mouse-coords): vec2u
```

The cursor position in pixels, relative to the window's top-left corner.

### `mouse-present?`

```easl
(mouse-present?): bool
```

Whether the cursor is currently over the window.

### `mouse-down?`

```easl
(mouse-down?): bool
```

Whether the primary mouse button is held.

### `mouse-just-down?`

```easl
(mouse-just-down?): bool
```

Whether the primary mouse button was pressed this frame.

### `key-down?`

```easl
(key-down? key: String): bool
```

Whether the named key is held, e.g. `(key-down? "a")`: a lowercase character key (arrows, space, and modifiers aren't tracked yet). Shader code needs a string literal; CPU code can compute the name, e.g. `(key-down? (string i))`.

### `key-just-down?`

```easl
(key-just-down? key: String): bool
```

Whether the named key was pressed this frame. Same key-naming rules as `key-down?`.

## Textures and render targets

### `load-image`

```easl
(load-image path: String): (Texture2D f32)
```

Loads a PNG or JPEG into a texture. The path must be a string literal, relative to the source file.

### `blank-texture`

```easl
(blank-texture width: u32 height: u32): (Texture2D f32)
(blank-texture size: vec2u): (Texture2D f32)
```

An empty texture of the given size, e.g. to render into.

### `set-render-target`

```easl
(set-render-target t: (Texture2D f32)): ()
```

Sends later `dispatch-render-shaders` calls this frame to `t` instead of the window. Later dispatches can sample what was drawn; see [render targets](../cpu.md#render-targets).

### `clear-render-target`

```easl
(clear-render-target): ()
```

Sends later render dispatches back to the window.

### `save-png`

```easl
(save-png t: (Texture2D f32) path: String): ()
```

Saves a texture as a PNG. The path must be a string literal, relative to the source file; missing directories are created. It works on textures the GPU just rendered into:

```easl
(set-render-target tex)
(dispatch-render-shaders vertex fragment 3)
(clear-render-target)
(save-png tex "frame.png")
```

## Audio

See [audio](../cpu.md#audio) in the runtime guide.

### `start-audio`

```easl
(start-audio f: (Fn [] f32)): ()
(start-audio f: (Fn [f32] f32)): ()
```

Starts (or switches) the audio output stream. `f` is called once per sample, optionally with the time in seconds, and returns a sample in `[-1, 1]`. Usually a closure, whose captured state persists between samples. Each call hands off the function it's given, so call it once or when the sound should change, not every frame.

### `sample-rate`

```easl
(sample-rate): f32
```

The audio stream's sample rate in Hz. Works in any code, including closure factories that run before the stream starts.

### `audio-time`

```easl
(audio-time): f32
```

The stream's clock in seconds. **Audio code only.**

### `audio-input`

```easl
(audio-input): f32
```

The current sample from the input device chosen with `start-listening`, mono and roughly in `[-1, 1]`; `0.` when nothing is listening. **Audio code only.**

### `start-listening`

```easl
(start-listening): ()
(start-listening-from device: String): ()
```

Starts capturing audio input from the default input device, or from the first device whose name contains `device`. There's no resampling, so the input and output devices should share a sample rate.
<!-- index: start-listening-from -->

### `listenable-sources`

```easl
(listenable-sources): [String]
```

The available input device names, sorted.

### `load-wav`

```easl
(load-wav path: String): [f32]
(load-wav-raw path: String): [i32]
```

Loads a `.wav` file as mono samples (multi-channel files are averaged). `load-wav` gives floats in `[-1, 1]`; `load-wav-raw` gives the raw integers (16-bit range). The samples are at the file's own rate — see `get-wav-sample-rate`. Paths are resolved relative to the source file.
<!-- index: load-wav-raw -->

```easl
(var kick: [f32])

@cpu
(defn main []
  (= kick (load-wav "kick.wav"))
  (start-audio (fn [t: f32]
                 (let [i (u32 (* t (sample-rate)))]
                   (if (< i (array-length kick)) (kick i) 0.))))
  (spawn-window (fn [] ())))
```

### `get-wav-sample-rate`

```easl
(get-wav-sample-rate path: String): f32
```

A `.wav` file's sample rate, without loading its samples.

### `save-wav`

```easl
(save-wav path: String samples: [f32] rate: f32): ()
```

Writes mono samples to a 16-bit PCM `.wav` file; values outside `[-1, 1]` clip.

## MIDI

MIDI input from every connected device, merged. These work in CPU, audio, **and shader** code; the CPU and GPU see a per-frame snapshot, the audio thread a per-buffer one. Held notes are `MidiNote` structs with fields `note: u32`, `velocity: f32`, and `aftertouch: f32` (all `0.`–`1.`).

### `down-midi-notes`

```easl
(down-midi-notes): [MidiNote]
```

The currently held notes, in the order they were pressed.

### `get-midi-note`

```easl
(get-midi-note note: u32): (Option MidiNote)
```

Note number `note` if it's held (`Some`), else `None`.

### `midi-cc`

```easl
(midi-cc controller: u32): f32
```

The last value of MIDI controller `controller` (`0`–`127`), in `0.`–`1.`.

### `midi-aftertouch`

```easl
(midi-aftertouch): f32
```

Channel pressure, in `0.`–`1.`.

### `midi-pitch-bend`

```easl
(midi-pitch-bend): f32
```

The pitch bend, from `-1.` to `1.`, centered at `0.`.

## Video

Requires building easl with the `video` feature and an `ffmpeg` executable on the path. A `Video` is a small value — which file, and which frame — so copying one and scrubbing the copy leaves the original alone.

### `load-video`

```easl
(load-video path: String): Video
```

Opens a video file, positioned at frame 0.

### `progress-video-frame`

```easl
(progress-video-frame v: Video): ()
(jump-to-video-frame v: Video frame: u32): ()
```

Advances `v` to its next frame, or to frame `frame`. `v` must be a mutable place.
<!-- index: jump-to-video-frame -->

### `get-current-frame-index`

```easl
(get-current-frame-index v: Video): u32
(get-video-length v: Video): u32
```

The current frame index, and the number of frames.
<!-- index: get-video-length -->

### `get-video-frame-texture`

```easl
(get-video-frame-texture v: Video): (Texture2D f32)
```

Decodes the current frame. Only assignable directly to a texture variable: `(= tex (get-video-frame-texture v))`.

## See also

- [Arrays](arrays.md): `zeroed-array` and `into-dynamic-array` size and fill runtime-sized storage buffers; `push`, `insert`, `remove`, and `reverse` build new arrays.
