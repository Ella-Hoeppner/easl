# CPU builtins

Builtins of easl's [CPU runtime](../cpu.md): printing, strings, windowing, GPU dispatch, input, textures, audio, MIDI, and video. Most are CPU-only; each entry notes where else it works.

## Printing

### `print`

```easl
(print x: T): ()
```

Prints any value. `u32`s print as `1u`, `i32`s bare (`1`), whole `f32`s with a trailing dot (`2.`), strings in double quotes, and arrays space-separated in square brackets.

## Strings

`String` values exist only in CPU code. Indices count characters. String literals take the escapes `\n`, `\t`, `\r`, `\"`, `\\`, and `\` at the end of a line (see [strings](../cpu.md#runtime-sized-arrays-and-strings)).

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

## Files

The builtins that read or write files (`load-image`, `save-png`, `load-wav`, `load-wav-raw`, `get-wav-sample-rate`, `save-wav`, `load-video`) take a path that, when relative, is relative to the file the call is written in, so a library loads its assets from its own directory wherever it's imported from.

### `resolve-path`

```easl
(resolve-path path: String): String
(resolve-path directory: String path: String): String
```

With one argument, the absolute path of `path`, relative to the file the call is written in: a library can hand its callers a path to one of its own files. With two, `path` relative to `directory`. Either way an absolute `path` comes back unchanged. File builtins treat their path argument as if it were wrapped in `(resolve-path …)`.

The one-argument form only works when called directly, since it takes its directory from where the call is written; to pass `resolve-path` to a higher-order function, use the two-argument form.

### `current-directory`

```easl
(current-directory): String
```

The absolute directory of the file the call is written in. A helper that wraps a file builtin, in a library, resolves paths against its own directory; to let callers pass paths relative to *their* files, have it take a directory too, and call it as `(save-filtered-png (current-directory) "frame.png")`, resolving with `(resolve-path directory path)` inside.

Like `resolve-path`'s one-argument form, it has to be called directly: it's an error to use it as a value.

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
```

Draws `vert-count` vertices with the vertex shader `vert` and fragment shader `frag`, which can be named functions or inline `fn`s. The draw uses the [render settings](#render-settings) current when it's dispatched, and goes to the window unless `set-render-target` redirects it.

### `dispatch-compute-shader`

```easl
(dispatch-compute-shader entry: F workgroup-count: vec3u): ()
(dispatch-compute-shader entry: F workgroup-count: vec3u workgroup-size: vec3u): ()
```

Runs the compute shader `entry` over `workgroup-count` workgroups. Each workgroup runs `workgroup-size` threads: the size given here, or else the shader's `@{workgroup-size N}` annotation, or 1. A size given here must be a compile-time constant (a `vec3u` of literals, or a `def` holding one), since WGSL fixes it in the shader; easl compiles a version of `entry` for each size it's dispatched at, so the same function or closure can be dispatched at several sizes, including from inside a higher-order helper. A size can have at most 256 threads in all, with x and y at most 256 and z at most 64.

```easl
(defn compute-foreach-n [n: u32 f: (Fn [u32] ())]
  (dispatch-compute-shader
    (fn []
      (let [i (.x (global-invocation-id))]
        (when (< i n)
          (f i))))
    (vec3u (/ (+ n 63u) 64u) 1u 1u)
    (vec3u 64u 1u 1u)))
```

 Each component can be at most the GPU's `maxComputeWorkgroupsPerDimension` (65535 on most devices); a larger count is a runtime error. See [CPU and GPU writes](../cpu.md#cpu-and-gpu-see-each-others-writes).

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

### `mouse-right-down?`

```easl
(mouse-right-down?): bool
(mouse-right-just-down?): bool
```

Whether the right (secondary) mouse button is held, or was pressed this frame. In the browser, right clicks on the canvas don't open the context menu.
<!-- index: mouse-right-just-down? -->

### `mouse-delta`

```easl
(mouse-delta): vec2f
```

The raw mouse motion since the previous frame, in device units, with +y pointing down. Unlike `mouse-coords`, it keeps reporting motion while the cursor is captured and pinned in place, which makes it the right input for first-person camera controls.

### `mouse-scroll-delta`

```easl
(mouse-scroll-delta): vec2f
```

How far the mouse wheel or trackpad scrolled since the previous frame, in pixels (the units of `mouse-coords`), with +y scrolling down (toward the end of a document) and +x scrolling right. Trackpads report pixels directly; a wheel that scrolls by lines counts 40 logical pixels per line. In the browser, scrolling over the canvas doesn't scroll the page.

### `capture-mouse`

```easl
(capture-mouse): ()
(release-mouse): ()
```

`capture-mouse` hides the cursor and locks it to the window (pointer lock), so the mouse can be moved indefinitely in any direction while `mouse-delta` reports the motion; `release-mouse` gives it back. Requests take effect at the end of the frame that makes them. Pressing Escape, or the window losing focus, always releases a captured cursor, so a program can never trap it; check `mouse-captured?` to see whether capture is currently active, e.g. to re-capture on the next click. In the browser, capture is only granted shortly after a click or key press, so request it in response to one. **CPU code only.**
<!-- index: release-mouse -->

```easl
@cpu
(defn main []
  (let [@var yaw 0.]
    (spawn-window (fn []
                    (when (and (mouse-just-down?) (not (mouse-captured?)))
                      (capture-mouse))
                    (when (mouse-captured?)
                      (+= yaw (* 0.002 (.x (mouse-delta)))))))))
```

### `mouse-captured?`

```easl
(mouse-captured?): bool
```

Whether `capture-mouse` currently holds the cursor. Like the other input queries, it reads the state at the start of the frame, so it doesn't change in the frame that calls `capture-mouse` or `release-mouse` — it reflects the request from the next frame on.

### `key-down?`

```easl
(key-down? key: String): bool
```

Whether the named key is held, e.g. `(key-down? "a")`. Character keys are named by their lowercase character, ignoring modifiers (shift+1 is still `"1"`). Other keys use these names: `" "` (or `"space"`), `"shift"`, `"ctrl"`, `"alt"`, `"super"` (cmd / the Windows key), `"escape"`, `"enter"`, `"tab"`, `"backspace"`, `"delete"`, `"up"`, `"down"`, `"left"`, and `"right"`. Shader code needs a string literal; CPU code can compute the name, e.g. `(key-down? (string i))`.

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

Loads a PNG or JPEG into a texture, decoding its colors to [linear values](textures.md#color-space). A relative path is relative to the file the call is written in.

### `blank-texture`

```easl
(blank-texture width: u32 height: u32): (Texture2D f32)
(blank-texture size: vec2u): (Texture2D f32)
```

An empty texture of the given size, e.g. to render into.

### `Sampler`

```easl
(Sampler filter: FilterMode address: AddressMode): Sampler
```

Constructs a sampler, to assign to a `Sampler` var. `filter` is how texture samples between texel centers are read:

- `FilterMode/Nearest`: the nearest texel, for crisp, blocky scaling.
- `FilterMode/Linear`: a blend of the surrounding texels, for smooth scaling.

`address` is how coordinates outside `[0, 1]` are read, on both axes:

- `AddressMode/ClampToEdge`: the texel at the nearest edge.
- `AddressMode/Repeat`: the texture tiles.
- `AddressMode/MirrorRepeat`: the texture tiles, flipping every other copy.

A `Sampler` var that's never assigned is `(Sampler FilterMode/Nearest AddressMode/ClampToEdge)`.

### `set-render-target`

```easl
(set-render-target t: (Texture2D f32)): ()
```

Sends later `dispatch-render-shaders` calls (and clears) to `t` instead of the window, until `reset-render-target`, across frames too. Later dispatches can sample what was drawn; see [render targets](../cpu.md#render-targets).

### `reset-render-target`

```easl
(reset-render-target): ()
```

Sends later render dispatches back to the window.

### Render settings

How a draw is rasterized and how its output combines with its target. Each setter changes one setting for every later draw, until it's changed again, across frames too; a draw uses the settings current when `dispatch-render-shaders` is called. Settings are set from CPU code only.

```easl
(set-blend src: BlendFactor dst: BlendFactor op: BlendOperation): ()
(set-blend src: BlendFactor dst: BlendFactor op: BlendOperation
           alpha-src: BlendFactor alpha-dst: BlendFactor
           alpha-op: BlendOperation): ()
(set-blend-constant color: vec4f): ()
(set-depth-test test: DepthTest): ()
(set-depth-write enabled: bool): ()
(set-cull-mode mode: CullMode): ()
(set-front-face face: FrontFace): ()
(set-topology topology: Topology): ()
(reset-render-settings): ()
```

`reset-render-settings` restores every setting's default (the render target is separate: see `reset-render-target`).

- `set-blend`: how a fragment's color (the source) combines with what the target holds (the destination). Each channel becomes `op(src-color * src, dst-color * dst)`. The three-argument form treats alpha like the color channels; the six-argument form gives alpha its own factors and operation. These are WebGPU's blend settings, all of them:
  - `BlendFactor/Zero`, `One`, `Src`, `OneMinusSrc`, `SrcAlpha`, `OneMinusSrcAlpha`, `Dst`, `OneMinusDst`, `DstAlpha`, `OneMinusDstAlpha`, `SrcAlphaSaturated`, `Constant`, `OneMinusConstant`. `Src` and `Dst` weigh each channel by the matching channel, `Constant` by the blend constant, and `SrcAlphaSaturated` by `min(src.a, 1 - dst.a)` (by `1` for alpha).
  - `BlendOperation/Add`, `Subtract` (source minus destination), `ReverseSubtract`, `Min`, `Max`. `Min` and `Max` compare the unweighted colors, ignoring the factors.
  - The default is no blending: a fragment overwrites the target, alpha included (`BlendFactor/One BlendFactor/Zero BlendOperation/Add`). Blending is opt-in because it costs fill rate, and on tile-based GPUs (Apple's) it also turns off hidden-surface removal.
  - Straight alpha, where a fragment `(vec4f color alpha)` covers the target by `alpha`: `(set-blend BlendFactor/SrcAlpha BlendFactor/OneMinusSrcAlpha BlendOperation/Add BlendFactor/One BlendFactor/OneMinusSrcAlpha BlendOperation/Add)`. Drawing this way into a transparent texture leaves premultiplied contents, so composite such a texture with premultiplied blending, `BlendFactor/One BlendFactor/OneMinusSrcAlpha`. `BlendFactor/One BlendFactor/One BlendOperation/Add` is additive.
- `set-blend-constant`: the color the `Constant` factors use (default `(vec4f 0.)`).
- `DepthTest/Off` (the default), `Less`, `LessEqual`, `Greater`, `GreaterEqual`, `Equal`, `Always`: whether a fragment is tested against the target's depth buffer, and the comparison its depth must pass against the stored one. A fragment's depth is its position's `z`, from `0.` near to `1.` far, unless the fragment shader returns a struct with a `@{builtin frag-depth}` field; with `DepthTest/Off`, that output is ignored. `Off` neither tests nor writes depth; `Always` passes every fragment but still writes depth.
- `set-depth-write`: whether a fragment that passes the depth test stores its depth (default `true`).
- `CullMode/Off` (the default), `Back`, `Front`: which triangles to discard by the way they face.
- `FrontFace/CounterClockwise` (the default), `Clockwise`: the winding of a front-facing triangle (also what `front-facing` reports).
- `Topology/TriangleList` (the default), `TriangleStrip`, `LineList`, `LineStrip`, `PointList`: how the vertices form primitives.

Every render target (the window, and each texture) has its own depth buffer, made the first time a draw into it tests depth. The window's starts each frame cleared to `1.`; a texture's keeps its contents, like its colors.

### `clear-target`

```easl
(clear-target color: vec4f): ()
```

Clears the current render target to `color`, and its depth buffer to `1.`, in order with the frame's draws. A texture keeps what's drawn into it until it's cleared; the window starts each frame cleared to opaque black, so to use another background, clear it before the frame's first draw.

### `clear-depth`

```easl
(clear-depth): ()
(clear-depth value: f32): ()
```

Clears the current render target's depth buffer to `1.` (or `value`), leaving its colors.

### `save-png`

```easl
(save-png t: (Texture2D f32) path: String): ()
```

Saves a texture as a PNG, encoding its [linear colors](textures.md#color-space) as sRGB, so the file looks like the texture drawn to the window. A relative path is relative to the file the call is written in; missing directories are created. It works on textures the GPU just rendered into:

```easl
(set-render-target tex)
(dispatch-render-shaders vertex fragment 3)
(reset-render-target)
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

Loads a `.wav` file as mono samples (multi-channel files are averaged). `load-wav` gives floats in `[-1, 1]`; `load-wav-raw` gives the raw integers (16-bit range). The samples are at the file's own rate — see `get-wav-sample-rate`. A relative path is relative to the file the call is written in.
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
(length v: Video): u32
```

The current frame index, and the number of frames (`length` is the same as `get-video-length`).
<!-- index: get-video-length -->

### `get-video-frame-texture`

```easl
(get-video-frame-texture v: Video): (Texture2D f32)
```

Decodes the current frame, with [linear colors](textures.md#color-space) like `load-image`. Only assignable directly to a texture variable: `(= tex (get-video-frame-texture v))`.

## See also

- [Arrays](arrays.md): `zeroed-array` and `into-dynamic-array` size and fill runtime-sized storage buffers; `push`, `insert`, `remove`, and `reverse` build new arrays.
