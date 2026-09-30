# Easl

**Easl** (Enhanced Abstraction Shader Language) is a Lisp-syntax shader language that compiles to [WGSL](https://www.w3.org/TR/WGSL/), the WebGPU shading language. It keeps everything WGSL offers and adds generics, sum types, closures, higher-order functions, and expression-based control flow. It can also be used a standalone language, running on the CPU, so you can create windows and run shaders without a separate host language, with data and functions passing seamlessly between the GPU and CPU.

```easl
(defn sphere [p: vec3f]: f32
  (- (length p) 1.))

(defn gradient [f: (Fn [vec3f] f32) p: vec3f]: vec3f
  (- (vec3f (f (+ p (vec3f 0.001 0. 0.)))
            (f (+ p (vec3f 0. 0.001 0.)))
            (f (+ p (vec3f 0. 0. 0.001))))
     (f p)))

(defn surface-normal [p: vec3f]: vec3f
  (normalize (gradient sphere p)))
```

`gradient` takes any scalar field and differentiates it numerically. The compiler inlines `sphere` into it, so the WGSL it produces is what you'd have written by hand.

Everything in WGSL has a direct easl equivalent, with names in Lisp style: `textureSample` is `texture-sample`, `texture_2d<f32>` is `(Texture2D f32)`.

## What easl adds

- **Expressions everywhere.** `let`, `if`, and `match` produce values and can appear wherever a value can.
- **Type inference** inside function bodies; top-level functions declare their signatures.
- **Generics** for functions, structs, and enums, specialized at compile time.
- **Sum types**: enums whose variants carry data, with exhaustive `match`.
- **Closures and higher-order functions.** Functions passed as arguments are inlined; functions stored in arrays or structs are dispatched at runtime.
- **A runtime.** A `@cpu` function can open a window, dispatch shaders, read GPU results back, play audio, and take audio and MIDI input. Buffers and CPU↔GPU synchronization are handled automatically.

## These docs

- **[Language guide](language.md)**: the language itself.
- **[Writing shaders](shaders.md)**: entry points, GPU bindings, and annotations.
- **[The CPU runtime](cpu.md)**: running whole programs.
- **[Builtin reference](reference/overview.md)** and the searchable **[function index](functions.md)**.

## Tools

- [easl](https://github.com/Ella-Hoeppner/easl): the compiler, as a Rust crate.
- [easl_cli](https://github.com/Ella-Hoeppner/easl_cli): `easl run program.easl` compiles and runs a program.
- [easl_lsp](https://github.com/Ella-Hoeppner/easl_lsp): a work-in-progress language server, with a VS Code client.

Easl is a work in progress; expect breaking changes.
