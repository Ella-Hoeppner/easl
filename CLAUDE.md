# Easl Compiler

Easl (Enhanced Abstraction Shader Language) is a Lisp-like shader language that compiles to WGSL. It uses S-expression syntax (parenthesized prefix notation) and provides generics, sum types (enums), higher-order functions, and expression-based control flow on top of what WGSL offers.

## Build & Test

```bash
cargo test                        # run all tests
cargo test <test_name>            # run a specific test
cargo run                         # runs benchmark (compiles all .easl files in data/gpu/)
```

## Project Structure

- `src/lib.rs` — public API: `compile_easl_source_to_wgsl`, `get_easl_program_info`, `format_easl_source`
- `src/parse.rs` — S-expression parser (uses the `fsexp` crate)
- `src/format.rs` — source formatter
- `src/interpreter.rs` — CPU-side interpreter; also defines the `IOManager` trait, `WindowEvent`/`IOEvent` enums, `GpuBufferKind`, `EvaluationEnvironment`, and `Value::to_uniform_bytes`
- `src/window.rs` — wgpu-based GPU renderer; creates the winit window, manages wgpu device/surface/pipelines, uploads CPU bindings each frame, dispatches compute and render passes
- `src/main.rs` — CLI entry point, currently just runs a compilation benchmark
- `src/compiler/` — the compiler:
  - `core.rs` — top-level compilation entry point
  - `program.rs` — `Program` struct and the main compilation pipeline (`validate_raw_program`). This is the largest and most important file
  - `expression.rs` — `TypedExp` (typed expression tree) and all expression-level transformations (monomorphization, inlining, type inference, etc.)
  - `functions.rs` — `AbstractFunctionSignature`, `FunctionSignature`, monomorphization and higher-order argument inlining for functions
  - `types.rs` — type system: `Type`, `AbstractType`, `TypeState`, `ExpTypeInfo`, type inference, unification, constraints
  - `structs.rs` — `AbstractStruct`, struct monomorphization
  - `enums.rs` — `AbstractEnum`, enum monomorphization
  - `builtins.rs` — all built-in function/struct/macro definitions
  - `effects.rs` — effect types (fragment-exclusive functions, print, window/spawn-window, etc.)
  - `entry.rs` — shader entry point validation (vertex/fragment/compute)
  - `error.rs` — `CompileErrorKind` enum and error reporting
  - `vars.rs` — top-level variables and address spaces
  - `wgsl.rs` — WGSL code generation (final output)
  - `annotation.rs` — `@annotation` parsing
  - `macros.rs` — macro expansion
  - `info.rs` — program info extraction
  - `util.rs` — utilities

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
- `@cpu` — marks the CPU entry point, compiled with `CompilerTarget::CPU` and run by the interpreter
- `@vertex`, `@fragment`, `@compute` — mark GPU shader entry points
- `@{workgroup-size N}` — required on `@compute` functions; sets threads per workgroup (can also be `@{workgroup-size X Y Z}`)
- `@{builtin vertex-index}`, `@{builtin global-invocation-id}`, `@{builtin position}`, etc. — bind WGSL builtins to function arguments or return values
- `@{location N}` — binds vertex inputs / fragment outputs

### GPU-bound top-level variables

`(var name: type)` with an address-space annotation creates a GPU-accessible variable visible to shaders and tracked by the interpreter's `binding_vars`:
```
@{address uniform        group 0  binding 0}  (var frame-index: u32)
@{address storage-read   group 0  binding 1}  (var read-only-buf: [N: vec4f])
@{address storage-write  group 0  binding 2}  (var rw-buf: [N: vec4f])
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
Two implementations: `StdoutIO` (real windowing via wgpu) and `StringIO` (test/debug, no GPU).
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
- **`EvaluationEnvironment`** — holds `binding_vars: Vec<(GroupAndBinding, Rc<str>, Type, VariableAddressSpace)>` for all GPU-bound top-level variables (Uniform + StorageRead + StorageReadWrite)
  - `binding_infos() -> Vec<((u8,u8), GpuBufferKind, u64)>` — size is 0 for unsized/dynamic arrays
  - `binding_buffer_data() -> Vec<((u8,u8), BufferUpload)>` — returns current interpreter values as upload payloads, padded to 16 bytes
- **`Value::ZeroedArray { length: usize, zero_element: Box<Value> }`** — lazily-materialized zeroed array created by `zeroed-array`. Avoids allocating a huge `Vec`. Converted to `BufferUpload::Clear` on upload; expanded to `Value::Array` only if a CPU write to an individual element is needed.
- **`Value::to_uniform_bytes(&self, ty: &Type) -> Vec<u8>`** — serializes a value to GPU bytes; uses `ty` for struct field ordering (walks `s.fields` in declaration order)

### Interpreter implementation notes

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

### `window.rs`
- One `wgpu::ShaderModule` and one `wgpu::PipelineLayout` shared by all render and compute pipelines
- `bind_group_layouts` stored as a field on `RenderState` (not dropped after creation) so `rebuild_bind_groups` can recreate bind groups without invalidating the pipeline layout
- `upload_bindings`: detects when a buffer's byte size changes → recreates the buffer → calls `rebuild_bind_groups`; handles `BufferUpload::Clear` via `encoder.clear_buffer` (efficient GPU-side zero-fill, no CPU→GPU data copy)
- `render`: pre-creates all pipelines, dispatches all `ComputeShader` calls first (each in its own compute pass), then does one render pass for all `RenderShaders` calls
- winit `EventLoop` is stored in a thread-local and reused across multiple `spawn-window` calls via `run_app_on_demand`
- **Known limitation**: `binding_buffer_data` / `collect_dirty_uploads` serialize and upload *all* dirty bindings every frame, including large GPU-written storage buffers the CPU never touches. A dirty-flag system is planned. `ZeroedArray` bindings are exempt — they become `BufferUpload::Clear` with no CPU allocation.

## Test Structure

There are three test suites:

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
- Compiles with `CompilerTarget::CPU`, runs via `run_program_capturing_output`
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

### Shared notes
- `#_` reader macro in `.easl` files comments out the next form — useful for disabling parts of test files
- Target a specific suite: `cargo test --test shader_tests`, `--test cpu_tests`, `--test window_tests`

## Style Notes

- Rust 2024 edition — uses `let` chains in `if let` expressions freely
- The codebase uses `take_mut::take` for in-place mutation of `&mut self`
- `Rc<RefCell<...>>` is used extensively for shared mutable state in the AST
- `ExpTypeInfo` wraps `TypeState` and implements `Deref<Target = TypeState>`
- `unwrap_known()` on `ExpTypeInfo` returns a cloned `Type` (panics if not `Known`)
