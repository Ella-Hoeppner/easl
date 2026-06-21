# Easl Compiler

Easl (Enhanced Abstraction Shader Language) is a Lisp-like shader language that compiles to WGSL. It uses S-expression syntax (parenthesized prefix notation) and provides generics, sum types (enums), higher-order functions, and expression-based control flow on top of what WGSL offers.

## Build & Test

```bash
cargo test --features window                        # run all tests (ALWAYS use this flag)
cargo test --features window <test_name>            # run a specific test
cargo run                                           # runs benchmark (compiles all .easl files in data/gpu/)
```

> **IMPORTANT**: Always pass `--features window` when running tests. Without it, the interpreter, wgpu GPU execution, and windowing code are all compiled out, causing most buffer/cpu/window tests to silently produce wrong results or be skipped. The `window` feature is required for any test that involves the interpreter, GPU compute/render, or the `IOManager` trait.

## Project Structure

- `src/lib.rs` — public API: `compile_easl_source_to_wgsl`, `get_easl_program_info`, `format_easl_source`; also re-exports `pub mod window` and `pub mod audio` when the `window` feature is enabled
- `src/parse.rs` — S-expression parser (uses the `fsexp` crate)
- `src/format.rs` — source formatter
- `src/interpreter.rs` — CPU-side tree-walking interpreter; also defines the `IOManager` trait, `WindowEvent`/`IOEvent` enums, `GpuBufferKind`, `EvaluationEnvironment`, and `Value::to_uniform_bytes`
- `src/window.rs` — wgpu-based GPU renderer; `GpuCore` struct (public) manages device/surface/pipelines/buffers and is the central GPU resource type; `StdoutIO` opens a real winit window; also exports `create_headless_gpu_core` for surfaceless GPU use
- `src/audio.rs` — audio runtime invoked by the `start-audio` builtin (see "Audio runtime" below). `AudioBackend { VM, C }` enum + `AudioSource` for what gets handed to the audio thread; `start_audio_thread_vm` drives the cpal callback through `BytecodeProgram::execute`; the C path lives behind the `c_audio` feature
- `src/main.rs` — CLI entry point, currently just runs a compilation benchmark. The full CLI lives in the separate `easl_cli` crate (e.g. `../easl_cli` if cloned alongside)
- `src/compiler/` — the compiler:
  - `core.rs` — top-level compilation entry point
  - `program.rs` — `Program` struct and the main compilation pipeline (`validate_raw_program`). This is the largest and most important file. Also home to `compile_to_bytecode_program`
  - `expression.rs` — `TypedExp` (typed expression tree) and all expression-level transformations (monomorphization, inlining, type inference, etc.)
  - `functions.rs` — `AbstractFunctionSignature`, `FunctionSignature`, monomorphization and higher-order argument inlining for functions
  - `types.rs` — type system: `Type`, `AbstractType`, `TypeState`, `ExpTypeInfo`, type inference, unification, constraints. Note: `Type::data_size_in_u32s` special-cases matNxM struct names to return `cols*rows*element_size` (see "Bytecode VM" below)
  - `structs.rs` — `AbstractStruct`, struct monomorphization
  - `enums.rs` — `AbstractEnum`, enum monomorphization
  - `builtins.rs` — all built-in function/struct/macro definitions
  - `effects.rs` — effect types (fragment-exclusive functions, print, window/spawn-window, etc.). `CPUExclusiveFunction(_)` and `CPUExclusiveType(_)` are used to filter what gets compiled for non-CPU targets (WGSL, C, and now also VM)
  - `entry.rs` — entry point kinds (`Cpu`, `Vertex`, `Fragment`, `Compute`, `Audio`) and per-target filtering via `should_compile_to_target(target)`
  - `error.rs` — `CompileErrorKind` enum and error reporting
  - `vars.rs` — top-level variables and address spaces
  - `wgsl.rs` — WGSL code generation (final output)
  - `annotation.rs` — `@annotation` parsing
  - `macros.rs` — macro expansion
  - `info.rs` — program info extraction
  - `util.rs` — utilities
- `src/vm/` — bytecode VM, a faster alternative interpreter (see "Bytecode VM" below):
  - `bytecode.rs` — VM only: `Op` enum, `Instruction`, `Function`, `Code`, `BytecodeProgram`, and the `execute()` dispatch loop
  - `compile.rs` — bytecode *compiler*: `IntermediateBytecodeFunction`, `BytecodeCompilationState` (struct + impl with all `emit_*` helpers, `compile_builtin`, etc.), free utility functions, and `impl TypedExp { compile_to_bytecode }`. Keep new bytecode-compile logic here, not in `expression.rs`

## Compilation Pipeline

The main pipeline lives in `Program::validate_raw_program` (program.rs). The major phases, in order:

1. **Name validation** — checks for reserved/invalid names
2. **Mutable arg wrapping** — wraps `@var` function args
3. **Deshadowing** — renames shadowed local bindings to unique names
4. **Type inference** (`fully_infer_types`) — bidirectional type inference with unification
5. **Control flow validation** — checks for expressions after `break`/`return`, validates match exhaustiveness
6. **Associative expansion** — expands `(+ a b c)` into `(+ (+ a b) c)`
7. **Deexpressionification** — lifts expression-position let/match/if blocks into statements
8. **Monomorphization** — replaces generic functions/structs/enums with concrete versions
9. **Inner function extraction** — extracts closures/lambdas as top-level functions
10. **Overloaded function separation** — renames overloaded functions with type suffixes
11. **Higher-order argument inlining** — specializes HoF calls by inlining the function argument
12. **Entry point & effect validation** — checks shader stage constraints
13. **Ownership validation** — checks reference mutability rules
14. **Reference address space monomorphization** — final pass

## Key Concepts

### Naming conventions
- Easl uses **kebab-case** for functions/variables (`make-two-of`) which compiles to **snake_case** in WGSL (`make_two_of`)
- Type names use **PascalCase** (`TwoOf`, `Option`)
- Monomorphized names get type suffixes: `map` with `T=f32` becomes `map_f32`

### Generics & Monomorphization
- Generic functions, structs, and enums use type parameters: `(defn (map T U) [...])`
- Monomorphization creates concrete copies: `map_f32_TwoOf_f32`
- The monomorphizer lives in `TypedExp::monomorphize` (expression.rs) and `AbstractFunctionSignature::generate_monomorphized` (functions.rs)
- It handles: struct constructors, enum constructors, composite function calls, and generic function references passed as HoF arguments
- `AbstractStruct::opaque: true` marks WGSL built-in types (`atomic`, `texture_2d`, `sampler`) that must never be emitted as struct definitions in WGSL output. The `compile_to_wgsl` filter in `program.rs` skips structs where `s.opaque == true`. Any new built-in type that is a WGSL primitive (not a user-definable struct) should have `opaque: true` set in `builtins.rs`.

### Enums (Sum Types)
- Compiled to WGSL as a struct with a `discriminant: u32` and `data: array<u32, N>` where N fits the largest variant
- Constructors and match blocks use `bitcast` to convert to/from the u32 array representation
- Unit variants become constants, data variants become constructor functions

### Higher-Order Functions
- All function arguments must be compile-time inlinable (no dynamic dispatch)
- Pipeline: monomorphize -> extract inner functions -> inline HoF arguments
- `inline_all_higher_order_arguments` creates specialized versions like `map_f32_TwoOf_f32_make_two_of_f32`

### `AbstractFunctionSignature`
- Central type representing function signatures throughout compilation
- Has a `Default` impl — use `..Default::default()` to omit fields with default values
- Defaults: `implementation: Builtin { empty effect }`, `associative: false`, `captured_scope: None`, `generic_args: vec![]`
- Keep `implementation` explicit when it has a non-empty effect (e.g. `FragmentExclusiveFunction`, `Print`)
- Keep `associative: true` when the function is associative (e.g. `+`, `*`, `&&`)

### Writing Easl Code

- Integer literals like `0` or `5` are ambiguous — the type checker can't tell if they're `i32` or `u32`. Use `0i` / `5i` for signed or `0u` / `5u` for unsigned to help type inference.
- `(if cond then else)` requires both branches and they must have compatible types. For side-effect-only conditionals (e.g. `(when cond (break))`), use the `when` macro instead, which handles the unit-typed else branch automatically.
- Float literals should include a decimal point: `5.` not `5`. An `f` suffix is also valid: `5f`.
- When printing values, the type is included in the output: `u32` prints as `1u`, `i32` as `1i`, `f32` as `1` (no suffix). This matters for `.txt` expected-output files.

### Entry point annotations

Easl programs can have multiple annotated sections compiled/run separately:
- `@cpu` — marks the CPU entry point, compiled with `CompilerTarget::WGSL` and run by the interpreter
- `@vertex`, `@fragment`, `@compute` — mark GPU shader entry points
- `@{workgroup-size N}` — optional on `@compute` functions; sets threads per workgroup (defaults to 1 if omitted; can also be `@{workgroup-size X Y Z}`)
- `@{builtin vertex-index}`, `@{builtin global-invocation-id}`, `@{builtin position}`, etc. — bind WGSL builtins to function arguments or return values. Note: some builtins (e.g. `global-invocation-id`) are also available as zero-argument helper functions that can be called anywhere in a shader without needing an annotated parameter.
- `@{location N}` — binds vertex inputs / fragment outputs

### GPU-bound top-level variables

`(var name: type)` with an address-space annotation creates a GPU-accessible variable visible to shaders and tracked by the interpreter's `binding_vars`:
```
@{address uniform        group 0  binding 0}  (var frame-index: u32)
@{address storage-read   group 0  binding 1}  (var read-only-buf: [N: vec4f])
@{address storage-write  group 0  binding 2}  (var rw-buf: [N: vec4f])
```
There is also a shorthand `@[address group binding]` array annotation that specifies the three values positionally:
```
@[uniform        0 0]  (var frame-index: u32)
@[storage-read   0 1]  (var read-only-buf: [N: vec4f])
@[storage-write  0 2]  (var rw-buf: [N: vec4f])
```
- `uniform` — maps to `GpuBufferKind::Uniform` (read-only from shader, writable from CPU)
- `storage-read` — maps to `GpuBufferKind::StorageReadOnly`
- `storage-write` — maps to `GpuBufferKind::StorageReadWrite` (GPU can write; vertex shaders cannot access)
- Unsized arrays (`[vec4f]`) are valid for storage bindings; the buffer is sized at runtime

### Windowing builtins

- `(spawn-window (fn [] ...))` — open a GPU window; the lambda body is the per-frame callback
- `(dispatch-render-shaders vert-fn frag-fn vert-count)` — queue a render pass for this frame
- `(dispatch-compute-shader compute-fn (vec3u X Y Z))` — queue a compute dispatch for this frame
- `(into-dynamic-array arr)` — convert a fixed-size array to a dynamically-sized `[T]`

Compute dispatches are always submitted before the render pass within a frame.

## Interpreter & Window System

The interpreter evaluates `@cpu`-annotated easl code on the CPU, driving GPU work through the `IOManager` trait.

### `IOManager` trait
Three implementations:
- `StdoutIO` — real windowing via wgpu; opens a winit window, runs a real render loop
- `StringIO` — test/debug, no GPU; simulates N frames (default 10), records all events to `events: Vec<IOEvent>`
- `CaptureIO` — wraps `StdoutIO`, additionally captures `println` output to `prints: Vec<String>`; used by `run_program_capturing_output`

Key methods:
- `println` — print output
- `record_draw(vert, frag, vert_count)` — called by `dispatch-render-shaders`
- `record_compute(entry, workgroup_count)` — called by `dispatch-compute-shader`
- `take_frame_draw_calls() -> Vec<WindowEvent>` — drains the current frame's GPU commands
- `run_spawn_window(body, env)` — called by `spawn-window`; `StdoutIO` opens a real window, `StringIO` simulates N frames (default 10)

### Key types in `interpreter.rs`
- **`BufferUpload`** — payload for a single binding upload:
  - `Data(Vec<u8>)` — upload the given bytes
  - `Clear { byte_count: u64 }` — zero the buffer on the GPU via `encoder.clear_buffer` (no CPU allocation)
- **`WindowEvent`** — frame-level GPU command passed from interpreter to `window.rs`:
  - `RenderShaders { vert: String, frag: String, vert_count: u32, pre_upload: Vec<((u8,u8), BufferUpload)> }`
  - `ComputeShader { entry: String, workgroup_count: (u32, u32, u32), pre_upload: Vec<((u8,u8), BufferUpload)> }`
- **`IOEvent`** — unified ordered log used by `StringIO` for testing:
  - `Print(String)`, `SpawnWindow`, `DispatchShaders { vert, frag, vert_count }`, `DispatchComputeShader { entry, workgroup_count }`
- **`GpuBufferKind`** — `Uniform | StorageReadOnly | StorageReadWrite`; exposed from `interpreter.rs` so `window.rs` doesn't need to reach into the compiler
- **`EvaluationEnvironment`** — holds `binding_vars: Vec<(GroupAndBinding, Arc<str>, Type, VariableAddressSpace)>` for all GPU-bound top-level variables (Uniform + StorageRead + StorageReadWrite)
  - `binding_infos() -> Vec<((u8,u8), GpuBufferKind, u64)>` — size is 0 for unsized/dynamic arrays
  - `binding_buffer_data() -> Vec<((u8,u8), BufferUpload)>` — returns current interpreter values as upload payloads, padded to 16 bytes
- **`Value::ZeroedArray { length: usize, zero_element: Box<Value> }`** — lazily-materialized zeroed array created by `zeroed-array`. Avoids allocating a huge `Vec`. Converted to `BufferUpload::Clear` on upload; expanded to `Value::Array` only if a CPU write to an individual element is needed.
- **`Value::to_uniform_bytes(&self, ty: &Type) -> Vec<u8>`** — serializes a value to GPU bytes; uses `ty` for struct field ordering (walks `s.fields` in declaration order)

### Interpreter implementation notes

**`Function::Scoped` — closures with captured scope:**
`extract_inner_functions` transforms a lambda that captures outer `let` bindings into a top-level function whose first argument is a scope struct. At the call site the lambda is replaced with `Application(Name("inner_fn_scope"), captured_vars)`. The interpreter evaluates this as a `Function::Scoped { inner: Box<Function>, scope: Box<Value> }` — where `inner` is the extracted composite function (taking scope as first arg) and `scope` is the evaluated scope struct. `spawn-window` and general function calls both handle this variant.

**`env.structs` is keyed by base name, not monomorphized name:**
`env.structs` (a `HashMap<Arc<str>, AbstractStruct>`) is populated from `typedefs.structs` using `s.name.0` as the key. After monomorphization `s.name.0` is still the base name (e.g. `"Atomic"`), NOT the monomorphized name (e.g. `"Atomic_u32"`). In `Function::from_abstract_signature` for `StructConstructor`, look up `f.name` (the abstract function's base name), not the `name` parameter (the monomorphized function name).

**Getting function names from dispatch-style builtins:**
When a builtin receives a function as an argument (e.g. `dispatch-render-shaders`, `dispatch-compute-shader`), extract the original pre-monomorphization name via `abstract_ancestor`:
```rust
let (_, Type::Function(f)) = &args[0] else { panic!() };
let name = f.abstract_ancestor.as_ref().unwrap().borrow().name.clone();
```

**Buffer sizing — use `data_size_in_u32s`, not serialization:**
`ty.data_size_in_u32s(&SourceTrace::empty())` is the canonical way to compute how many u32s (×4 = bytes) a type occupies on the GPU. Do **not** use `value.to_uniform_bytes(ty).len()` for this — if the value is `Uninitialized` (common at startup for storage arrays), it returns 0 and produces the wrong buffer size.

**Unsized arrays and `Value::zeroed()`:**
`Value::zeroed()` returns `Err(CantCreateZeroedUnsizedArray)` for unsized array types. When initializing `binding_vars` for a variable that might be unsized, use `.unwrap_or(Value::Uninitialized)`.

**`GpuBufferKind` ↔ `VariableAddressSpace` mapping:**
- `VariableAddressSpace::Uniform` → `GpuBufferKind::Uniform`
- `VariableAddressSpace::Storage(AccessMode::Read)` → `GpuBufferKind::StorageReadOnly`
- `VariableAddressSpace::Storage(AccessMode::ReadWrite)` → `GpuBufferKind::StorageReadWrite`

### ⚠️ Synchronous GPU↔CPU semantics — DO NOT BREAK

**This is a hard language design requirement.** Easl programs must be able to write to a variable from the GPU (via `dispatch-compute-shader`) and then immediately read it back from the CPU (e.g. `print`) in the same frame body, without any explicit sync call, and get the updated value. Similarly, CPU writes must be visible to subsequent GPU dispatches. This is an intentional, load-bearing design constraint of the language — not an implementation detail to be optimised away.

**How it works:** `check_cpu_readable` (called before any function reads a global variable) calls `io.flush_queued_compute()` if any of the variables it needs are `CPUOutOfDate`. This flushes all pending compute dispatches as a single batched encoder + submit + poll (`GpuCore::execute_compute_batch`) before the readback. Render shader events stay deferred to end-of-frame (they target the framebuffer, not CPU-readable storage).

**What must NOT change:**
- `StdoutIO::flush_queued_compute` must execute queued compute synchronously (one batched submit + blocking poll), not defer it
- `CaptureIO::run_spawn_window` must set `windowed = true` before running frames, so that `record_compute` queues work (same as the real winit loop) rather than executing inline — this keeps test behaviour identical to production
- `check_cpu_readable` must call `flush_queued_compute` before attempting GPU→CPU readback
- Do NOT collapse all frame work into a single deferred submit; compute must be flushable mid-frame on demand

**Performance note:** `flush_queued_compute` uses `execute_compute_batch` to run all queued dispatches in one encoder/submit/poll rather than N separate ones. If you need to improve GPU throughput, batch *within* a flush, but do not remove the flush or make it async.

### `window.rs`
- One `wgpu::ShaderModule` and one `wgpu::PipelineLayout` shared by all render and compute pipelines
- `bind_group_layouts` stored as a field on `RenderState` (not dropped after creation) so `rebuild_bind_groups` can recreate bind groups without invalidating the pipeline layout
- `upload_bindings`: detects when a buffer's byte size changes → recreates the buffer → calls `rebuild_bind_groups`; handles `BufferUpload::Clear` via `encoder.clear_buffer` (efficient GPU-side zero-fill, no CPU→GPU data copy)
- `render`: pre-creates all pipelines, dispatches all `ComputeShader` calls first (each in its own compute pass), then does one render pass for all `RenderShaders` calls
- winit `EventLoop` is stored in a thread-local and reused across multiple `spawn-window` calls via `run_app_on_demand`
- **Known limitation**: `binding_buffer_data` / `collect_dirty_uploads` serialize and upload *all* dirty bindings every frame, including large GPU-written storage buffers the CPU never touches. A dirty-flag system is planned. `ZeroedArray` bindings are exempt — they become `BufferUpload::Clear` with no CPU allocation.

**Public API (accessible via `easl::window` with the `window` feature):**
- `GpuCore` — the central GPU resource type; holds device, queue, shader module, pipeline layout, bind groups, and per-binding buffers
- `GpuCore::new_from_parts(device, queue, wgsl, binding_infos) -> Arc<RwLock<GpuCore>>` — creates a headless `GpuCore` from an existing `wgpu::Device` and `wgpu::Queue`; intended for embedders (e.g. easl_studio) that want to share a device with their own renderer instead of creating a second one
- `create_headless_gpu_core(wgsl, binding_infos) -> Arc<RwLock<GpuCore>>` — creates a `GpuCore` with a freshly-created device and no surface; useful for pure compute / offscreen rendering
- `GpuCore::execute_render_batch_to_view(calls, view: &wgpu::TextureView, format: wgpu::TextureFormat)` — runs a batch of screen-targeted render calls against an external `TextureView` instead of acquiring one from an internal surface; used by easl_studio to render into its own offscreen RT

## Bytecode VM

A second, faster interpreter built around a register-style bytecode, intended for performance-critical use (e.g. DSP) where the tree-walking `interpreter.rs` is way too slow. Currently the primary user is the audio runtime — `start-audio` compiles the program to bytecode and the cpal callback runs `execute()` once per sample.

The conformance test suite runs the VM as a target alongside the interpreter and the C backend; passing rate is currently 158/158.

### Layout

The two halves only meet through `Code` / `BytecodeProgram`:

- **VM (`src/vm/bytecode.rs`)**: `BytecodeProgram { code, stack: Vec<u32>, call_stack: Vec<Range<u32>> }`. The `stack` is a flat array of `u32`s; values are raw bits reinterpreted per-op (`f32::from_bits`, etc.). Instructions are `{ op, arg_positions: [u16; 3], return_position: u16 }`, where positions are **absolute** indices into the shared `stack` (static addressing — no per-call frame base). Execution is a single dispatch loop; `InvokeFunction` pushes the caller's remaining instruction range onto `call_stack` and jumps to the callee; running off the end of a function's instructions pops `call_stack` to return. `call_stack` holds the whole continuation (designed to later support pause/resume for algebraic effects). Heavy use of `unsafe` (`get_unchecked`, `ptr::copy`) — correctness relies on the compiler emitting in-bounds indices.
- **Compiler (`src/vm/compile.rs`)**: holds `BytecodeCompilationState` (struct + impl with all `emit_*` methods, `compile_builtin`, etc.), free utility helpers (`vec_kind`, `mat_kind`, `arithmetic_op_for`, etc.), and the `impl TypedExp { compile_to_bytecode }` block. The top-level entry point `Program::compile_to_bytecode_program` lives in `program.rs` for symmetry with `compile_to_target`.
  - Walks the fully-lowered `TypedExp` (after `validate_raw_program`) into instructions.
  - Functions are emitted in `composite_functions_in_usage_order()` (topological, callees first — no recursion in easl, so the call graph is a DAG).
  - Each function gets a fixed disjoint region of the stack; slots are bump-allocated (`take_stack_slot`) with no reuse yet (register allocation / stack coloring are deferred to future bytecode passes).

### Running a compiled function

```rust
let (mut program, names) = compiled_program.compile_to_bytecode_program();
let f_index = names.iter().position(|n| &**n == "f").unwrap();
// For functions with args: write args directly into the function's arg
// slots (start at `program.get_function_return_position(f_index)`),
// then run.
program.prepare_to_run_function(f_index);
program.execute();
let slot = program.get_function_return_position(f_index);
let result = f32::from_bits(program.stack[slot as usize]); // reinterpret per return type
```

### Key design decisions / invariants

- **Static absolute addressing.** No per-call frame base. Every function is assigned a fixed, disjoint region of the single shared `stack` at compile time, and instruction operands are absolute slot indices. Globals and locals are one uniform address space (a global is just a low slot). Valid because easl has **no recursion** (the call graph is a DAG). Trade-off: rules out multi-shot continuations, but single-shot suspend/resume (the efficiently-implementable subset of algebraic effects we care about) still works.
- **Top-level vars with initializers run via a synthetic `$init_globals` function.** Globals get slots at the bottom of the stack. If any have initializer expressions, `compile_to_bytecode_program` emits a synthetic function (name `"$init_globals"`, the `$` makes it unspeakable in easl) that compiles each initializer and `Move`s the result into the global slot. `Code::init_function_index: Option<usize>` records its index; `BytecodeProgram::from_code` runs it once on construction so globals are live before any user code executes. Don't break this contract — it's how `(def TAU: f32 6.283185)` and friends actually have their values at runtime.
- **Matrix storage is flat N\*M scalars.** WGSL `matNxM` is nominally a one-field opaque struct in `builtins.rs`, but `Type::data_size_in_u32s` in `types.rs` special-cases the `matNxM` name and returns `cols*rows*element_size`. This is the only place outside the VM that touches matrix sizing differently than other structs — keep it.
- **`compile_to_bytecode_program` filters functions like the C backend does in `TopLevelFunction::compile`:**
  - Skip entry points whose `entry_point.should_compile_to_target(CompilerTarget::VM)` returns false (currently only `EntryPoint::Audio` returns true; `Cpu`/`Vertex`/`Fragment`/`Compute` are all skipped).
  - Skip any function whose effects include a `CPUExclusiveFunction(_)` or `CPUExclusiveType(_)` — these are helpers transitively called only from `@cpu` code; without this filter the compiler would hit `todo!()` on `spawn-window` / `window-frame-index` / `start-audio` / etc.
  - Effects are transitive and `composite_functions_in_usage_order` is callees-first, so if a function gets skipped, anything that depends on it also gets skipped — no dangling references to worry about.
- **Vector / matrix ops are fan-out, not new opcodes.** `vec3f + vec3f` compiles to three `PlusF32`s over three consecutive slots; `dot`, `length`, `cross`, etc. are written in terms of scalar primitives. Matrix multiplication uses `emit_mat_mul` (naive triple loop, all scalar). No SIMD or vec-width-generic opcodes yet — explicit non-goal for the MVP.
- **The `compile_to_bytecode_program` precondition is "program is already validated".** `validate_raw_program` is **not idempotent** — a late pass converts `Ownership::Reference` → `Ownership::Pointer(_)`, and an earlier pass on a re-run panics on the resulting Pointer. Callers must validate exactly once before calling `compile_to_bytecode_program`, then pass the validated `Program` in. Do not re-validate a clone inside the VM-compile path — that was a real bug we hit once already.

### `BytecodeCompilationState` emit helpers

When emitting bytecode, prefer the methods on `BytecodeCompilationState` over raw `push_instruction` calls. They handle slot allocation and keep the patterns consistent:
- `emit_unary` / `emit_binary` / `emit_ternary` — single-slot scalar ops.
- `emit_fanout_unary` / `_binary` / `_ternary` — N-slot fan-out for vec/mat element-wise ops.
- `emit_fanout_binary_scalar_lhs` / `_rhs` — broadcast a single scalar across one side of a fan-out (e.g. `vec * scalar`).
- `emit_elementwise_unary` / `_binary` / `_ternary` / `_binary_inplace` — scalar-or-vec dispatch wrapped on top of the above, takes a "what op for this element type" closure. This is what most builtin arms use.
- `emit_dot` / `emit_mat_mul` / `emit_determinant` / `emit_det3` — bigger primitives used by specific builtins.
- `emit_u32_constant(value)` / `emit_f32_constant(value)` — allocate a fresh slot and write a `Constant`. Use these instead of inline `take_stack_slot + push_instruction(Constant)`.

When you need to identify a vec or matrix type, use the free helpers `vec_kind(t)` / `mat_kind(t)` in `compile.rs`. `vec_kind` returns `Some((count, element_type))` for `vec2`/`vec3`/`vec4` **and** matrices (flat count = `cols*rows`); `mat_kind` returns `Some((cols, rows, element_type))` only for matrices.

### Adding a new scalar builtin (recipe)

1. Add the variant to `Op` in `src/vm/bytecode.rs` (in the right section — there's a comment grouping).
2. Add its `execute` arm. For simple per-element math, use `f32_unary` / `f32_binary` / `i32_binary` / `u32_binary` / `f32_cmp` / etc. helpers.
3. If its operands aren't all slot indices (`Constant`'s halves, `Jump`'s target), add a custom `max_touched_index` arm — otherwise the `_` arm will overcount and inflate the stack.
4. In `compile_builtin` (in `compile.rs`), add a dispatch arm keyed on the builtin name. For overloaded ops, dispatch on **operand type** via the `arg_types[i]` slice, not on the return type (that breaks for e.g. comparisons that always return bool regardless of operand type).

### Known issues / things future work will need to fix

These came up during the build-out and are deferred but real:

- **Mutable reference args don't work.** Functions with `@var @ref state: T` arguments compile, but the bytecode `Composite`-call path just `Move`-copies the arg into the callee's slot, so mutations inside the function never propagate back to the caller. The audio2 example silently produces zero because its `hooke-attract` spring takes a `@var @ref state: vec2f` and the state never actually updates. **Suggested fix:** detect reference-typed args at the call site, write the *slot index* into the callee's arg slot instead of the value bytes, and add two new opcodes — `LoadIndirect { dest, addr_slot, size }` and `StoreIndirect { addr_slot, src, size }` — for reads and writes through that slot inside the function. The frontend already lowers `@var @ref` to `Ownership::Pointer(address_space)` after `monomorphize_reference_address_spaces` (program.rs:4070); the bytecode compile path can key off that.
- **Assignment into struct fields and array elements doesn't work.** `(= arr[i] x)` and `(= obj.field x)` go through the `=` builtin which currently just emits a fixed-destination `Move`. The array-store case needs a runtime-destination store op (mirror of the existing `ArrayLookup`); the field-store case needs a known-offset partial store (could be a Move with the computed offset, similar in shape to the Field-read path). This and the mutable-references item largely overlap and are likely best tackled together.
- **No optimization passes.** Slot allocation is bump-only with no reuse, so the stack ends up bigger than necessary; the `Function`-arm closing `Move` is emitted even when the body wrote the return slot directly; matrix ops allocate a lot of fresh temp slots. The WIP-notes plan was to add a redundant-`Move` elimination pass and a liveness-based slot allocator over the emitted bytecode — both untouched. Bytecode correctness is the oracle for these (the conformance tests, vm_tests, audio examples).
- **No construction-time bounds check.** `from_code` does `get_unchecked` everywhere; safety is "the compiler emits in-bounds indices." A `debug_assert` pass over `Code` validating every `arg_positions`/`return_position` against the inferred stack size would be a cheap safety net for catching emitter bugs.
- **VM-backed audio doesn't hot-swap.** `start_audio_thread_vm` only does the cpal setup on the first call; subsequent `start-audio` calls during the spawn-window frame loop are no-ops. The C audio path does hot-swap by atomically replacing a function pointer; the VM equivalent would be putting the `BytecodeProgram` behind an `Arc<Mutex<...>>` (or a single-writer / multiple-reader scheme that doesn't add per-sample lock overhead) and swapping it in subsequent calls. Audio2.easl-style live-coding will want this once mutable references are working.
- **Many builtins still `todo!()`.** Texture-sample / texture-load / `dxdy` (no sensible CPU semantics) — these will probably never be implemented for the VM; they're shader-only. `atomic-*` is also out of scope. But anything that has scalar math semantics (e.g. `pack-*`/`unpack-*`, more comparison vec lifts) is fair game.

## Audio runtime (`src/audio.rs`)

When an easl program calls `(start-audio audio-fn)`, the runtime starts a cpal output stream that calls `audio-fn(t: f32, rate: f32) -> f32` once per sample. There are two backends, selected via `AudioBackend`:

- `AudioBackend::VM` (default, always available): compile the program to bytecode and have the cpal callback run `BytecodeProgram::execute` once per sample. Portable, no external dependencies.
- `AudioBackend::C` (gated on the `c_audio` feature): JIT-compile the program's C-backend output to a dylib via `clang`, dlopen it via `libloading`, and have the cpal callback call the loaded function pointer. Faster but requires `clang` on the host. Selecting `AudioBackend::C` without `c_audio` enabled panics at audio-compile time.

The runner functions in `interpreter.rs` (`run_program_entry_from_path`, `run_program_entry_with_io_from_path`) use `AudioBackend::default()` (= `VM`). To opt into the C backend explicitly, use `run_program_entry_with_io_and_audio_backend_from_path` (gated on `feature = "window"`).

### Public surface
- `AudioBackend { VM, C }` — caller-selected backend choice (`Default = VM`).
- `AudioSource` — what the interpreter hands to the IO manager: `Bytecode { program, function_names }` or `C(String)`.
- `start_audio_thread_vm(entry_name, program, function_names)` — VM-backed driver.
- `start_audio_thread_c(entry_name, c_source)` — C-backed driver; `panic!`s without `c_audio` feature.
- `is_audio_thread_started() -> bool` — used by IO managers to distinguish "repeated start-audio call after first one already set things up" (no-op) from "run was started without audio support" (error).

### Things the `start-audio` builtin handles, that you shouldn't break

- **`(start-audio ...)` is typically called every frame** from inside a `spawn-window` callback, so the builtin gets dispatched repeatedly. The first call moves the `AudioSource` out of the env and into the IO manager; subsequent calls pass `None` and the IO manager treats that as a no-op if a stream is running. `StringIO` (the test manager) records *every* start-audio event regardless.
- **The audio source is compiled from a clone of the (already-validated) `Program`.** Do **not** re-validate the clone — see the "compile_to_bytecode_program precondition" note in the Bytecode VM section.
- **VM audio hot-swap is not implemented** — second-and-later calls just log a note and return. C audio does hot-swap via an `AtomicPtr` of the loaded function.

## Test Structure

There are five test suites:

### GPU/compiler tests (`tests/shader_tests.rs`, sources in `data/gpu/`)
```rust
success_test!(test_name);  // compiles data/gpu/test_name.easl, validates WGSL output with naga
error_test!(test_name, CompileErrorKind::SomeError);  // expects specific compile errors
```
- `assert_compiles` validates the WGSL output through naga's parser and validator
- `assert_errors` checks that exact error kinds match (uses `PartialEq`, not discriminant comparison)
- Compiled WGSL is written to `out/` for inspection

### CPU interpreter tests (`tests/cpu_tests.rs`, sources in `data/cpu/`)
```rust
cpu_test!(test_name);  // runs data/cpu/test_name.easl, compares stdout to data/cpu/test_name.txt
```
- Compiles with `CompilerTarget::WGSL`, runs via `run_program_capturing_output`
- Asserts that captured `(print ...)` output matches the `.txt` file exactly

### Window/interpreter tests (`tests/window_tests.rs`, sources in `data/window/`)
```rust
window_test!(test_name);  // runs data/window/test_name.easl, compares IOEvent log to data/window/test_name.txt
```
- Runs the program with `StringIO` (no real GPU), which simulates 10 window frames
- Asserts that `io.events` matches the events parsed from the `.txt` file
- **`.txt` event format** (one event per line):
  - `spawn-window`
  - `print: <message>`
  - `dispatch-render-shaders <vert_fn> <frag_fn> <vert_count>`
  - `dispatch-compute-shader <entry_fn> (vec3u <x>u <y>u <z>u)`

### Conformance tests (`tests/conformance_tests.rs`, sources in `data/conformance/`)
```rust
conformance_test!(test_name);          // exact match across all backends
conformance_test!(test_name, 0.001);   // match within tolerance (for irrational results)
```
- Each file in `data/conformance/` defines a single function `f` that returns `f32`. The test harness runs `f` through **three** backends and checks they all agree (mod tolerance):
  1. **Interpreter + WGSL**: harness appends boilerplate that calls `f` on the CPU (printing the result), dispatches a compute shader that writes `f()` into a storage variable, then reads it back and prints it again. Asserts the two prints agree.
  2. **C backend**: compiles to C via `compile_to_target(CompilerTarget::C)`, appends a `main()` that prints `f()`, runs through `clang`, compares to the interpreter result. Slow (the clang invocation dominates wall time).
  3. **Bytecode VM**: compiles to bytecode via `compile_to_bytecode_program`, runs `f` via `prepare_to_run_function` + `execute`, compares to the interpreter result.
- With no tolerance argument the comparison is exact (string for interpreter/GPU, `f64` equality for C and VM); with a tolerance the values are parsed as `f64` and the test passes if all pairs are within tolerance of each other.
- Tests cover arithmetic, rounding, trigonometry (including hyperbolic), exponentials/logarithms, sqrt/pow, min/max/clamp, mix/smoothstep/fma/ldexp, vector ops (dot, cross, length, normalize, distance), integer arithmetic, type conversions, bitcast, bit manipulation, matrices (constructors, mul, transpose, determinant), enums, swizzles, control flow, and reads of `(def …)` and `(var … expr)` globals.
- The test runner uses `load_easl_program_from_file_with_lookup_function` to inject the boilerplate into the source string rather than requiring it in each file, so test files can stay minimal.
- **Dev tip**: the C stage is the slow part (~0.5-1s per test for the clang invocation). When iterating on the VM backend, temporarily wrap the C section in `if false { … }` and re-parse `cpu_result` immediately before the VM section to skip it — but restore before committing. (The C stage is a regression test on the *interpreter*/*WGSL*/*C* triple; new VM bugs almost never manifest there.)

### Bytecode VM tests (`tests/vm_tests.rs`, sources in `data/vm/`)
```rust
vm_test!(test_name);  // compiles+runs data/vm/test_name.easl through the bytecode VM
```
- Each `.easl` file must define a zero-arg function `f` returning `f32`. The harness validates the program, compiles it with `compile_to_bytecode_program`, runs `f` via `prepare_to_run_function`/`execute`, reads the return slot, reinterprets it as `f32`, and compares against the single float in `data/vm/test_name.txt` within a `0.0001` tolerance.
- Use this suite for things that are awkward to express as a single `f` returning a meaningful float — e.g. tests of `let`, control flow, mutation, struct/array access, etc. The conformance suite is broader but each test has to be expressible as "produce one f32 that equals f's interpreter/WGSL value".

### Shared notes
- `#_` reader macro in `.easl` files comments out the next form — useful for disabling parts of test files
- Target a specific suite: `cargo test --test shader_tests`, `--test cpu_tests`, `--test window_tests`, `--test conformance_tests`, `--test vm_tests`

## Style Notes

- Rust 2024 edition — uses `let` chains in `if let` expressions freely
- The codebase uses `take_mut::take` for in-place mutation of `&mut self`
- `Rc<RefCell<...>>` is used extensively for shared mutable state in the AST
- `ExpTypeInfo` wraps `TypeState` and implements `Deref<Target = TypeState>`
- `unwrap_known()` on `ExpTypeInfo` returns a cloned `Type` (panics if not `Known`)
