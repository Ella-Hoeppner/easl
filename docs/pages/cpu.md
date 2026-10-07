# The CPU runtime

While easl is designed primarily to be used a shader language, it also has a CPU runtime that can do window management, dispatch a file's shaders via simple higher-order functions, take user input from the mouse and keyboard, and even play audio. Things like buffers, pipelines, and data synchronization, which often need hundreds or thousands of lines of code in traditional graphics programming frameworks, are all handled automatically and seamlessly by easl's CPU runtime, so you never need to worry about them or write any code to make them work.

The following minimal example program shows how to spawn a window, dispatch shaders, and read info about the window like the resolution and time, all of which will be explained in more detail later on in this page:

```easl
@cpu
(defn main []
  (spawn-window
    (fn []
      (dispatch-render-shaders
        (fn [] (match (vertex-index)
                 0 (vec4f -1. -1. 0. 1.)
                 1 (vec4f -1. 3. 0. 1.)
                 _ (vec4f 3. -1. 0. 1.)))
        (fn [] (vec4f (/ (.xy (position)) (vec2f (window-resolution)))
                      (+ 0.5 (* 0.5 (sin (window-time))))
                      1.))
        3))))
```

Run it with the [easl CLI](https://github.com/Ella-Hoeppner/easl_cli): `easl run program.easl`.

## The frame loop

`spawn-window` opens a new window when called. `spawn-window` accepts a function as an argument, and calls that function once per frame to act as an update loop. Inside that function you do whatever you want to do: set GPU-bound variables, dispatch render and compute shaders, read user input, read GPU results, and end the loop with `close-window`. The function is a closure, so it can capture variables from the surrounding code, and changes to captured `@var`s persist between frames.

## CPU and GPU see each other's writes

**Writes on either side are visible to the other immediately**, with no synchronization calls. A compute shader can write a variable that the next line prints:

```easl
(var results: [64: f32])

@compute
(defn simulate []
  (= (results (.x (global-invocation-id))) 1.))

@cpu
(defn main []
  (dispatch-compute-shader simulate (vec3u 64u))
  (print (results 0u))) ; 1.
```

The runtime tracks which side last wrote each variable. CPU writes are uploaded before the next dispatch that uses them, and a CPU read of GPU-written data runs the pending GPU work, then reads it back. You'll never need to worry about whether your data is properly synchronized when writing easl, everything evaluates in program-order, and is available wherever it's needed.

## Input

Easl has several functions to get input about the state of the window, and for gathering user input, such as : `window-resolution`, `window-time`, `window-delta-time`, `window-frame-index`, `mouse-coords`, `mouse-delta`, `mouse-present?`, `mouse-down?`, `mouse-just-down?`, `mouse-right-down?`, `mouse-right-just-down?`, `mouse-captured?`, `key-down?`, and `key-just-down?`. `capture-mouse` and `release-mouse` hide and lock the cursor for first-person-style controls. These functions can be called either on the CPU or GPU and work the same in both cases. See the [reference](reference/cpu-builtins.md#window-and-input-queries) for more details on these functions.

## Textures and Render targets

Render dispatches draw to the window unless `set-render-target` points them at a texture. After you'ves set a target, you can also call `clear-render-target` to clear the target, such that subsequent calls will go back to the window itself. Textures come from `blank-texture` or `load-image`, later dispatches in the same frame can sample what was drawn, and [`save-png`](reference/cpu-builtins.md#save-png) saves any texture, including one just rendered.

```easl
(var canvas: (Texture2D f32))

@cpu
(defn main []
  (= canvas (blank-texture (vec2u 512u)))
  (spawn-window (fn []
                  (set-render-target canvas)
                  (dispatch-render-shaders vertex draw 3)
                  (clear-render-target)
                  (dispatch-render-shaders vertex show-canvas 3))))
```

## Audio

`start-audio` calls a function once per sample on a real-time audio thread. The function takes no arguments, or the time in seconds, and returns a sample between `-1.` and `1.`. It's usually a closure, whose variables carry state from one sample to the next:

```easl
(defn sine [freq: f32]: (Fn [] f32)
  (let [@var phase 0.]
    (fn []
      (= phase (% (+ phase (/ freq (sample-rate))) 1.))
      (sin (* 6.283185 phase)))))

@cpu
(defn main []
  (start-audio (sine 220.))
  (spawn-window (fn [] ())))
```

`(sample-rate)` works anywhere. In audio code, `(audio-time)` is the stream's clock and `(audio-input)` the latest [microphone or line-in](reference/cpu-builtins.md#start-listening) sample. [MIDI input](reference/cpu-builtins.md#midi) works in audio, CPU, and shader code.

Each call to `start-audio` hands over the function it's given, so a later call can switch to a different sound. Call it once, or when the sound should change, rather than every frame: calling it again with a freshly made closure starts that closure over.

### Variables shared with audio

A global used by both the audio function and the rest of the program is shared between them. Each thread works on its own copy, and they trade changes at the end of each window frame and each audio buffer. Within one frame, or one buffer, a shared variable doesn't change underneath you. When audio starts, it receives the current value of every shared variable, so a [`load-wav`](reference/cpu-builtins.md#load-wav)ed sample assigned beforehand is ready to play.

Sharing works both ways: a frequency set by the frame loop reaches the audio at its next buffer, and a level meter written by the audio reaches the frame loop at its next frame. Shaders take part through the frame loop.

- Changes arrive at the next boundary, not instantly — typically within a few milliseconds.
- If both sides write a variable between two exchanges, the later write replaces the whole value; writes to different elements of an array aren't merged. Where possible, write each variable from one side.

Variables only one side uses aren't shared, and avoid all overhead associated with synchronization.

## Runtime-sized arrays and strings

In CPU and audio code, runtime-sized arrays (`[f32]`, `[Material]`, `[[f32]]`) are ordinary values: you can bind them, pass and return them, and store them in structs and enums. Copying one is cheap, and changing a copy never affects the original. [`push`, `insert`, `remove`, `concat`, and `reverse`](reference/arrays.md#push) return new arrays, so growing one is `(= arr (push arr x))`. Shaders can use runtime-sized storage buffers, but can't create, pass, or return runtime-sized values.

`String`s work in CPU code only. `(string x)` turns any value into one, and [`concat`, `substr`, and `length`](reference/cpu-builtins.md#strings) work on them.

`(print x)` prints any value: `1u` for a `u32`, `1` for an `i32`, `2.` for a whole `f32`, and strings in double quotes.

## Embedding: `@external` variables

(This section is mainly for rust developers interested in integrating easl into a larger project, you can safely ignore it if you're just using easl as an indepdent language)

A program hosted inside another application, such as a live-coding editor, can mark globals `@external` for the host to read and write while it runs:

```easl
@external (var gain: f32)
```

The host creates an `easl::external::ExternalVars` handle from the compiled program, passes it to the runner, and calls `read_external_var` and `write_external_var` (or the `_index` and `_raw` variants) from any thread. The host is one more participant in the sharing above: its writes arrive at the program's next frame or audio buffer, including on the GPU. An `@external` variable has no initial value; the host sets it before the program starts.

[Easl Studio](https://github.com/Ella-Hoeppner/easl-studio)'s sliders work this way: each slider is an element of an `@external` array.
