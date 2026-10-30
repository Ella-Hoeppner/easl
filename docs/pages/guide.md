# Easl

**Easl** (Enhanced Abstraction Shader Language) is a Lisp-syntax shader language that compiles to [WGSL](https://www.w3.org/TR/WGSL/), the WebGPU shading language. It keeps everything WGSL offers and adds many features that have previously been unavailable in shader languages:

- **Expression-based syntax** `let`, `if`, and `match` produce values and can appear wherever a value can.
- **Sum types**: enums whose variants carry data, with exhaustive `match`.
- **Closures and higher-order functions.** Functions passed as arguments are inlined; functions stored in arrays or structs are dispatched at runtime.
- **Generics** for functions, structs, and enums, specialized at compile time.

Easl can also be run on the CPU, and the CPU runtime can open windows, dispatch shaders, and generate real-time audio. When using the CPU runtime, memory is seamlessly shared between the CPU and GPU without the need for any explicit synchronization code.

## Using these docs

- **[Language guide](language.md)** describes the basic language features like syntax, the type system, control flow, and modules
- **[Writing shaders](shaders.md)** describes shader-specific functionality
- **[The CPU runtime](cpu.md)** demonstrates the CPU side of the language, including the process of spawning windows, dispatching shaders, responding to user input, and generating audio.
- **[Builtin reference](reference/overview.md)** and the searchable **[function index](functions.md)** are useful for seeing all of the builtin functions available in easl.
- **[Worked example: Raymarching](raymarch.md)** is a walkthrough that uses easl to build a raymarcher. It shows off a lot of easl's unique features and is a great way to get started with the language.

## Tooling

- [easl](https://github.com/Ella-Hoeppner/easl): the compiler, as a Rust crate.
- [easl_cli](https://github.com/Ella-Hoeppner/easl_cli): `easl run program.easl` compiles and runs a program.
- [easl_lsp](https://github.com/Ella-Hoeppner/easl_lsp): a work-in-progress language server, with a VS Code client.

Easl is a work in progress; expect breaking changes.
