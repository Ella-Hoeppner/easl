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
- `src/audio.rs` — audio runtime invoked by the `start-audio` builtin (see "Audio runtime" below). `AudioBackend { VM, C }` enum + `AudioSource` for what gets handed to the audio thread; `VmAudioDriver` runs the bytecode replica one callback-batch at a time (adopt → samples → publish); the C path lives behind the `c_audio` feature
- `src/thread_sync.rs` — the cross-thread shared-variable primitive: `SharedVarSlot` (lock-free version-stamped snapshot publication via `arc_swap`), `ThreadSharedTable`, and the `participant` bit constants (see "Cross-thread shared variables" below)
- `src/external.rs` — `ExternalVars`, the embedder-facing handle for `@external` globals: `read/write_external_var[_index]` + `_raw` word-level variants; created from a validated `Program` and passed into the `*_with_external_*` runners
- `src/main.rs` — CLI entry point, currently just runs a compilation benchmark. The full CLI lives in the separate `easl_cli` crate (e.g. `../easl_cli` if cloned alongside)
- `src/compiler/` — the compiler:
  - `core.rs` — top-level compilation entry point
  - `program.rs` — `Program` struct and the main compilation pipeline (`validate_raw_program`). This is the largest and most important file. Also home to `compile_to_bytecode_program` and `thread_shared_globals` (the static shared-variable analysis)
  - `expression.rs` — `TypedExp` (typed expression tree) and all expression-level transformations (monomorphization, inlining, type inference, etc.)
  - `functions.rs` — `AbstractFunctionSignature`, `FunctionSignature`, monomorphization and higher-order argument inlining for functions
  - `types.rs` — type system: `Type`, `AbstractType`, `TypeState`, `ExpTypeInfo`, type inference, unification, constraints. Note: `Type::data_size_in_u32s` special-cases matNxM struct names to return `cols*rows*element_size` (see "Bytecode VM" below)
  - `structs.rs` — `AbstractStruct`, struct monomorphization
  - `enums.rs` — `AbstractEnum`, enum monomorphization
  - `builtins.rs` — all built-in function/struct/macro definitions
  - `effects.rs` — effect types (fragment-exclusive functions, print, window/spawn-window, etc.). `CPUExclusiveFunction(_)` and `CPUExclusiveType(_)` are used to filter what gets compiled for non-CPU targets (WGSL, C, and now also VM). ⚠️ `is_side_effect_free` treats `CPUExclusiveFunction` as pure, and non-final block statements that pass it are **pruned as dead code** — so a statement-position builtin must also carry an externally-observable effect (`Window`, `Print`, `FileWrite`) or its calls silently vanish (this is why `set-render-target`/`start-audio` carry `Window`, and `save-png` carries `FileWrite`). `ReadsArrayLength(_)` is a length-only read of an array variable (emitted for direct `Name` arguments of `array-length`): it's excluded from `read_and_written_globals()` (the GPU→CPU readback set) but included in `gpu_read_and_written_globals()` (the dispatch pre-upload set) — the GPU can never resize a buffer, so lengths never need a readback, but WGSL's `arrayLength()` derives from buffer size so uploads still count it
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
  - `shared_sync.rs` — the VM side of cross-thread sharing: `publish_shared`/`adopt_shared` over a program's stack + dynamic memory, plus the `BytecodeProgram::publish_shared`/`adopt_shared` wrappers

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
- When printing values: `u32` prints with its suffix (`1u`), `i32` prints bare (`1`), `f32` prints with a trailing decimal point when whole (`1.`), bools print as `true`/`false`. This matters for `.txt` expected-output files.

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
- `storage` (alias `storage-read`) — maps to `GpuBufferKind::StorageReadOnly`
- `storage-write` — maps to `GpuBufferKind::StorageReadWrite` (GPU can write; vertex shaders cannot access)
- Unsized arrays (`[vec4f]`) are valid for storage bindings; the buffer is sized at runtime

**Binding-number elision**: the group/binding numbers may be omitted entirely — `@[uniform]`, `@[storage]`, `@{address handle}` — and the compiler assigns free numbers at the end of validation (`assign_elided_bindings`: declaration order, lowest free slot, filling gaps around explicitly-numbered bindings, group 0 first). Elision is all-or-nothing (`@[uniform 0]` is an error, pre-existing `GroupMissingBinding`/`BindingMissingGroup`). In the AST this is `BindingSpec::{Specified, Elided}` (vars.rs) on `TopLevelVariableKind::Var`; no `Elided` survives validation, so post-validation consumers always see concrete numbers via `BindingSpec::specified()`. Every compiler-created binding (window-info uniforms, dispatched-closure captures) is emitted `Elided` and numbered by the same central pass — passes never allocate numbers in place. Explicit numbers remain an interface contract the compiler never touches (`catch_bind_group_collisions` checks only those for duplicates); when coordinating with an external host that reads the emitted WGSL, specify numbers explicitly — elided bindings get numbers the host would have to discover from the output. Pinned by `elided_bindings` in both the shader suite (naga-validates gap-filling) and the buffer suite (full CPU↔GPU round trip on both runtimes).

### `@external` variables

`@external` on a top-level `var` marks it as readable/writable by an embedding host program through an `ExternalVars` handle (see "Cross-thread shared variables" below). It stacks with binding annotations in either order (`@external @[uniform 0 0] (var sliders: [2: vec4f])`) or stands alone on a plain private var (`@external (var gain: f32 0.)`). Compile errors: `@external` on a texture (`ExternalTextureVar`), `@external` on a var whose type contains a String (`ExternalStringVar` — a string's words are a heap id, meaningless outside its own runtime), and any annotation on a `def` (pre-existing `ConstantMayNotHaveAnnotation`).

### Windowing builtins

- `(spawn-window (fn [] ...))` — open a GPU window; the lambda body is the per-frame callback
- `(dispatch-render-shaders vert-fn frag-fn vert-count)` — queue a render pass for this frame
- `(dispatch-compute-shader compute-fn (vec3u X Y Z))` — queue a compute dispatch for this frame
- `(into-dynamic-array arr)` — convert a fixed-size array to a dynamically-sized `[T]`

GPU work executes in **program order** within a frame: compute dispatches and texture-targeted render passes observe each other's writes in the order they were dispatched (a hard language requirement; pinned by the `offscreen_render_compute_order` buffer test). Only screen-targeted draws are deferred to the end of the frame — nothing on the GPU can read the surface, so that's unobservable.

### Window-info queries

The window/input query builtins — `window-resolution`, `window-time`, `window-delta-time`, `window-frame-index`, `mouse-coords`, `mouse-present?`, `mouse-down?`, `mouse-just-down?`, `key-down?`, `key-just-down?` — are callable from both CPU and GPU code, and **always read a per-frame snapshot**: the `extract_gpu_window_info` pass (which runs before effect validation) unconditionally rewrites every query into a read of an implicit elided-number uniform binding, one per distinct query (key queries get one binding per distinct compile-time key string; bools become `u32` bindings read as `(!= b 0u)`, since bools aren't host-shareable in WGSL uniforms). The runtime refreshes these bindings from the IO manager at the start of every frame (`refresh_window_info_bindings` / `refresh_vm_window_info`, driven by `Program::window_info_bindings` — which must be carried through the registry-rebuilding passes' `take()` calls, like `top_level_vars`) and marks them CPU-written, so the normal dirty-upload machinery ships them before dispatches. Rewriting *unconditionally* — CPU uses too, not just GPU-reachable ones — is a deliberate semantic choice: every query in a frame sees the same value, and whether some other call site dispatches a helper to the GPU never non-locally changes what the helper's CPU calls observe. (On the real winit path this freezing is behaviorally a no-op anyway: `gpu.window_time` etc. are only updated once per frame.)

The builtins carry `Effect::WindowInfo(WindowInfoKind)` (not `CPUExclusiveFunction`; still excluded from the C and audio targets via the same filter sites). Key queries require compile-time string literals; a non-literal key can't currently be written in easl source (`String` isn't a nameable type), but `validate_gpu_window_info` defensively rejects one reaching GPU code. `SpoofedWindowInfo` on `CaptureIO` (the test/capture wrapper — kept off `StdoutIO` so production accessors stay branch-free) lets tests inject deterministic values end-to-end (see `gpu_window_info_spoofed` in buffer_tests).

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
  - `binding_infos() -> Vec<GpuBindingInfo>` — per binding: group/binding, source-level name, kind, byte size (0 for unsized/dynamic arrays), and `BindingStages` (which shader stages actually reference it, derived from entry-point effects at env construction). `window.rs` uses the stages for usage-derived bind-group-layout visibility — critical because Metal caps the vertex stage at 16 buffer slots (`MAX_VERTEX_BUFFERS` in wgpu-hal, one of which wgpu reserves), counting every vertex-visible layout binding whether used or not. `validate_binding_limits` pre-flights the binding table against device limits + the Metal vertex rule before any wgpu object creation (clear error naming the offending variables), and `install_gpu_error_handler` converts any remaining uncaptured wgpu error into an easl-framed panic instead of a raw wgpu one
  - `binding_buffer_data() -> Vec<((u8,u8), BufferUpload)>` — returns current interpreter values as upload payloads, padded to 16 bytes
- **`Value::ZeroedArray { length: usize }`** — lazily-materialized zeroed array created by `zeroed-array`. Avoids allocating a huge `Vec`. Converted to `BufferUpload::Clear` on upload; expanded to `Value::Array` only if a CPU write to an individual element is needed. (The VM runtime has the same laziness in flat form: `DynMemory::Zeroed`.)
- **`Value::to_uniform_bytes(&self, ty: &Type) -> Vec<u8>`** — serializes a value to GPU bytes; uses `ty` for struct field ordering (walks `s.fields` in declaration order)

### Interpreter implementation notes

**`Function::Scoped` — closures with captured scope:**
`extract_inner_functions` transforms a lambda that captures outer `let` bindings into a top-level function whose first argument is a scope struct. At the call site the lambda is replaced with `Application(Name("inner_fn_scope"), captured_vars)`, whose callee carries the scope struct's constructor as an explicit `StructConstructor` abstract ancestor (the closure-ness lives in the expression's own type: a function type whose ancestor is the extracted inner fn). Backends recognize scope constructions positively — callee ancestor is a `StructConstructor` AND the expression type is function-typed — never by ancestor absence: every fully-lowered application callee must carry an ancestor, and compiling one without it is a panic-worthy compiler bug (the enum match-arm payload reconstructions in `bitcasted_from_enum_data_inner` also attach explicit `StructConstructor` ancestors for this reason). The interpreter evaluates this as a `Function::Scoped { inner: Box<Function>, scope: Box<Value> }` — where `inner` is the extracted composite function (taking scope as first arg) and `scope` is the evaluated scope struct. `spawn-window` and general function calls both handle this variant. When a `Scoped` value is bound to a composite-call parameter whose static type expects the scope struct itself (the trailing scope arg HoF inlining appends), the bare struct is bound instead, and `write_back_through_lhs` re-wraps the mutated struct into the `Scoped` wrapper at the source binding — this is what makes mutation through a directly-called closure persist (pinned by the `closure_scope_write_back` conformance test).

**GPU-dispatched closures — one binding per capture:**
When a scoped closure is dispatched to the GPU (`dispatch-compute-shader` / `dispatch-render-shaders`), `extract_dispatched_closure_scopes` (program.rs) drops the entry's trailing scope arg and lifts **each captured var to its own implicit elided-number read-only-storage global** named `<scope-struct>_data_<capture>`, rewriting the body's `scope.field` accesses into reads of the per-field global. Per-capture bindings (rather than one scope-struct binding) are what make runtime-sized captures work: each capture is then an ordinary global of its own type, so a dyn-array capture is just a storage-bound runtime-sized array — including several per closure, which a single struct binding could never express (WGSL allows at most one runtime-sized member, last position only). The scope struct itself lives on in the typedefs but is no longer referenced by the entry; struct declarations with runtime-sized or String fields are skipped from emission entirely, with function-typed fields checked recursively through their representative scope structs (`type_makes_struct_cpu_only`) — validation guarantees GPU code never references them. Captured *closures* recurse (`lift_scope_captures` / `gpuify_captured_closure`): the closure's own captures are lifted to bindings the same way (keyed by its own scope struct, so entries capturing the same closure share them), and it gets a memoized **GPU clone** — trailing scope param dropped, body reading the lifted globals, call sites in the capturing body repointed to the clone with the forwarding arg removed — while the original definition stays intact for direct CPU calls. This reaches arbitrary depth, so a dispatched closure can transitively capture runtime-sized arrays through any chain of closures (the everyday HoF-wrapper-over-a-local-array pattern). A capture whose type embeds a runtime-sized array without being one (e.g. a captured dyn-field struct value) is rejected at lift time with `RuntimeSizedFieldInBinding` — `validate_gpu_runtime_sized_use` runs before this pass and can't see the implicit bindings. Closures defined *inline inside* a dispatched body keep their scope-construction calling convention unchanged. At dispatch time each runtime writes the captured values into the per-capture bindings and marks them CPU-written so the normal dirty-upload machinery ships them, recursing into captured closures' scopes: tree-walker — `upload_dispatched_closure_scope` / `write_scope_capture_bindings` (interpreter.rs) walks the scope `Value` (a captured closure's value is its own scope data); VM — `resolve_dispatched_fn` / `emit_scope_capture_writes` (vm/compile.rs) emits a `Move` into slot-backed bindings or `RegionFromHeap` into dyn-region bindings, walking the closure value's flat slot layout by field offsets (construction args align with scope fields by order). Pinned by `dispatch_closure_captures_dynamic_array` / `_two_dynamic_arrays` / `_closure_with_dynamic_array` / `_closure_three_deep`, `hof_dispatch_captures_local_dynamic_array`, and the `<closure-scope>` sync goldens (one upload line per *leaf* capture per dispatch).

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

**How it works:** `check_cpu_readable` (called before any function reads a global variable) calls `io.flush_queued_compute()` if any of the variables it needs are `CPUOutOfDate`. This executes all queued texture-targeted work — compute dispatches and render-to-texture passes — in program order through `GpuCore::execute_frame_gpu_work` (the same implementation the end-of-frame path uses), then blocks until completion before the readback. Screen-targeted render events execute afterwards into the acquired surface (saved as `pending_present` so end-of-frame just presents; they target the framebuffer, not CPU-readable storage). One deliberate exception: `(array-length arr)` on a GPU-dirty array does **not** sync — lengths are CPU-authoritative (the GPU can never resize a buffer), so it reads the possibly-stale CPU value's length directly (see `Effect::ReadsArrayLength`; the `array_length_no_readback` sync test pins this).

**What must NOT change:**
- `StdoutIO::flush_queued_compute` must execute queued compute synchronously (one batched submit + blocking poll), not defer it
- `CaptureIO::run_spawn_window` must execute each frame's queued events through the shared `GpuCore::execute_frame_gpu_work` / `execute_frame_screen_renders` methods — the same code the real winit loop's `render` uses — skipping only screen-targeted draws (no surface in headless mode). Do NOT give the test harness its own parallel frame-execution path: that divergence once hid a real upload-ordering bug in `render` for months (see the `windowed_scope_upload_ordering` sync test)
- `check_cpu_readable` must call `flush_queued_compute` before attempting GPU→CPU readback
- Do NOT collapse all frame work into a single deferred submit; compute must be flushable mid-frame on demand

**Performance note:** `flush_queued_compute` uses `execute_compute_batch` to run all queued dispatches in one encoder/submit/poll rather than N separate ones. If you need to improve GPU throughput, batch *within* a flush, but do not remove the flush or make it async.

### `window.rs`
- One `wgpu::ShaderModule` shared by all pipelines; **bind group layouts, bind groups, and pipeline layouts are per-pipeline** (`PipelineBindings` on each `CachedComputePipeline`/`CachedRenderPipeline`). Each pipeline's layouts cover exactly the buffer bindings its entry points' effects reference (plus every texture binding — texture use isn't effect-tracked yet), with per-stage visibility from actual usage. Buffers themselves stay global and shared across all pipelines — only descriptor objects multiply. This is what makes per-stage binding budgets per-pipeline: a program is valid as long as no *single* pipeline exceeds the device's limits (Metal caps vertex-stage buffers at 16 incl. one wgpu-reserved sizes slot; checked in `validate_pipeline_bindings` at pipeline creation with an easl-voiced error naming the bindings; `validate_binding_limits` checks the program-wide properties like bind-group count at GpuCore creation; `install_gpu_error_handler` converts any uncaptured wgpu error into an easl-framed panic)
- **Dispatch events carry dense entry ids, not names**: `WindowEvent` references entry points by `u16` index into the env's GPU entry table (`GpuEntryInfo`: compiled name + used bindings, built at env construction from entry-point effects, sorted by name for deterministic ids; `EvaluationEnvironment::gpu_entry_id` resolves source names at dispatch-record time). The GPU frame loop does no string work: compute pipelines are a dense `Vec` indexed by entry id, render pipelines a small linear-scanned vec keyed by `(vert_id, frag_id, additive, format)`
- `upload_bindings`: detects when a buffer's byte size changes → recreates the buffer → rebuilds the bind groups of only the cached pipelines referencing it (`rebuild_bind_groups_for`; bind groups are immutable snapshots of buffer references, so this is forced by the API — layouts and pipelines survive); handles `BufferUpload::Clear` via `encoder.clear_buffer` (efficient GPU-side zero-fill, no CPU→GPU data copy)
- The frame path is split into two shared `GpuCore` methods so the winit loop, the headless test loop, and the mid-frame flush all run identical code: `execute_frame_gpu_work` (pipeline pre-pass, screen-render pre_uploads upfront, then all texture-targeted work — compute dispatches and render-to-texture passes — executed strictly in program order, batching consecutive runs of each kind; compute runs go through the conflict-splitting encoder, which splits the submit when an upload would overwrite a binding an already-encoded dispatch depends on, e.g. the same entry dispatched twice in one frame with different captured-scope values) and `execute_frame_screen_renders` (the screen-targeted draws, one pass, after surface acquisition). The winit `render` wraps them with surface acquisition/present; `flush_queued_compute` runs `execute_frame_gpu_work` + a blocking wait, then screen draws via `execute_render_batch`
- winit `EventLoop` is stored in a thread-local and reused across multiple `spawn-window` calls via `run_app_on_demand`
- **Known limitation**: `binding_buffer_data` / `collect_dirty_uploads` serialize and upload *all* dirty bindings every frame, including large GPU-written storage buffers the CPU never touches. A dirty-flag system is planned. `ZeroedArray` bindings are exempt — they become `BufferUpload::Clear` with no CPU allocation.

**Public API (accessible via `easl::window` with the `window` feature):**
- `GpuCore` — the central GPU resource type; holds device, queue, shader module, pipeline layout, bind groups, and per-binding buffers
- `GpuCore::new_from_parts(device, queue, wgsl, binding_infos: &[GpuBindingInfo], gpu_entries: &[GpuEntryInfo]) -> Arc<RwLock<GpuCore>>` — creates a headless `GpuCore` from an existing `wgpu::Device` and `wgpu::Queue`; intended for embedders (e.g. easl_studio) that want to share a device with their own renderer instead of creating a second one. Runs the program-wide binding pre-flight (the Metal vertex rule is per-pipeline and applies only when the backend is known — skipped here) and does not install an error handler; the embedder owns its device
- `create_headless_gpu_core(wgsl, binding_infos: &[GpuBindingInfo], gpu_entries: &[GpuEntryInfo]) -> Arc<RwLock<GpuCore>>` — creates a `GpuCore` with a freshly-created device and no surface; useful for pure compute / offscreen rendering
- `GpuCore::execute_render_batch_to_view(calls, view: &wgpu::TextureView, format: wgpu::TextureFormat)` — runs a batch of screen-targeted render calls against an external `TextureView` instead of acquiring one from an internal surface; used by easl_studio to render into its own offscreen RT. `calls` reference entries by dense id (`u16`), matching `WindowEvent`

## Bytecode VM

A second, faster interpreter built around a register-style bytecode. Originally built for performance-critical DSP (the audio runtime: `start-audio` compiles the program to bytecode and the cpal callback runs `execute()` once per sample), it is now also the **default runtime for `@cpu` code** (see "VM CPU runtime" below). The tree-walking interpreter remains fully supported as a reference implementation and debugging tool, selectable via `CpuRuntime::TreeWalking`.

The conformance test suite runs the VM as a target alongside the interpreter and the C backend; passing rate is currently 158/158.

### Layout

The two halves only meet through `Code` / `BytecodeProgram`:

- **VM (`src/vm/bytecode.rs`)**: `BytecodeProgram { code, stack: Vec<u32>, call_stack: Vec<Range<u32>>, dyn_memory: Vec<DynMemory>, heap: Vec<Option<Arc<HeapCell>>> + heap_free, shared_dirty/shared_adopted/shared_scratch }` (`heap` backs first-class runtime-sized array *values* — see "First-class runtime-sized arrays" below; the shared_* fields are the thread-sharing replica state — see "Cross-thread shared variables"). The `stack` is a flat array of `u32`s; values are raw bits reinterpreted per-op (`f32::from_bits`, etc.). Instructions are `{ op, arg_positions: [u16; 3], return_position: u16 }`, where positions are **absolute** indices into the shared `stack` (static addressing — no per-call frame base). Execution is a single dispatch loop; `InvokeFunction` pushes the caller's remaining instruction range onto `call_stack` and jumps to the callee; running off the end of a function's instructions pops `call_stack` to return. `call_stack` holds the whole continuation (designed to later support pause/resume for algebraic effects). Heavy use of `unsafe` (`get_unchecked`, `ptr::copy`) — correctness relies on the compiler emitting in-bounds indices.
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
- **Global slot locations are part of the public artifact.** `Code::globals: Vec<(Arc<str>, u16, u16)>` records each top-level var's name, base slot, and size in u32 slots (declaration order); `BytecodeProgram::get_global_slot(name)` / `write_global(name, &[u32])` look up and overwrite them. Globals persist across `execute` calls, so external hosts can stream values into a running program — easl_studio uses this to mirror live slider values into the audio VM between buffers.
- **Matrix storage is flat N\*M scalars.** WGSL `matNxM` is nominally a one-field opaque struct in `builtins.rs`, but `Type::data_size_in_u32s` in `types.rs` special-cases the `matNxM` name and returns `cols*rows*element_size`. This is the only place outside the VM that touches matrix sizing differently than other structs — keep it.
- **`compile_to_bytecode_program` filters functions like the C backend does in `TopLevelFunction::compile`:**
  - Skip entry points whose `entry_point.should_compile_to_target(CompilerTarget::VM)` returns false (currently only `EntryPoint::Audio` returns true; `Cpu`/`Vertex`/`Fragment`/`Compute` are all skipped).
  - Skip any function whose effects include a `CPUExclusiveFunction(_)`, `CPUExclusiveType(_)`, `WindowInfo(_)`, or `Print` — these are helpers transitively called only from `@cpu` code; without this filter the compiler would hit `todo!()` on `spawn-window` / `window-frame-index` / `print` / etc. (`Print` matters for frame closures that never call a CPU-exclusive builtin — printing is the only thing marking them CPU-side.)
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

- **Mutable reference args are zero-overhead via per-usage specialization** (not a gap — documented here because the mechanism is easy to miss). A function with any non-Owned arg — user `@ref` params, and the trailing captured-scope param `extract_inner_functions` adds to closures — is never compiled standalone: it's stashed in `ref_arg_functions`, each call site records a `PendingRefFnUsage`, and after the calling function finishes, the callee is compiled once per usage with its ref params bound *directly to the caller's slots* (the `ref_arg_positions` parameter of `TopLevelFunction::compile_to_bytecode`) — no copy in, no copy out, true aliasing via static addressing. Detection keys off **signature-level ownership** (`Type::Function(sig)` arg ownership), NOT `arg_annotations` — annotations only reflect user-written `@ref` and miss lowering-created params. Remaining gap: reference args whose source location is runtime-computed (dynamic-array elements) aren't supported and would need indirect-addressing opcodes.
- **No optimization passes.** Slot allocation is bump-only with no reuse, so the stack ends up bigger than necessary; the `Function`-arm closing `Move` is emitted even when the body wrote the return slot directly; matrix ops allocate a lot of fresh temp slots. The WIP-notes plan was to add a redundant-`Move` elimination pass and a liveness-based slot allocator over the emitted bytecode — both untouched. Bytecode correctness is the oracle for these (the conformance tests, vm_tests, audio examples).
- **No construction-time bounds check.** `from_code` does `get_unchecked` everywhere; safety is "the compiler emits in-bounds indices." A `debug_assert` pass over `Code` validating every `arg_positions`/`return_position` against the inferred stack size would be a cheap safety net for catching emitter bugs.
- **VM-backed audio doesn't hot-swap.** `start_audio_thread_vm` only does the cpal setup on the first call; subsequent `start-audio` calls during the spawn-window frame loop are no-ops. The C audio path does hot-swap by atomically replacing a function pointer; the VM equivalent would be putting the `BytecodeProgram` behind an `Arc<Mutex<...>>` (or a single-writer / multiple-reader scheme that doesn't add per-sample lock overhead) and swapping it in subsequent calls. Audio2.easl-style live-coding will want this.
- **Many builtins still `todo!()`.** Texture-sample / texture-load / `dxdy` (no sensible CPU semantics) — these will probably never be implemented for the VM; they're shader-only. `atomic-*` is also out of scope. But anything that has scalar math semantics is fair game (`pack-*`/`unpack-*` were implemented this way).

### VM CPU runtime

`@cpu` entry points run on the VM by default (`CpuRuntime::{TreeWalking, BytecodeVm}`, default `BytecodeVm`; every `run_program_*` entry point has a `*_with_runtime` variant). Architecture:

- **One `Op::HostCall` opcode + cold metadata tables on `Code`** (`host_ops`, `host_types`, `host_strings`, `host_bindings`, `host_dispatches`). All CPU orchestration (print, GPU dispatch, sync checks, dynamic arrays, window queries) goes through it; the dispatch loop is otherwise untouched and the audio path (`execute()` = `execute_with_host(NoopHost)`, monomorphized) pays nothing.
- **`compile_to_bytecode_program_cpu`** (cpu_mode in `vm/compile.rs`): compiles `@cpu` entries + transitive callees, lowers CPU-exclusive builtins to host ops via `try_compile_cpu_builtin`, and emits explicit sync instructions from effect analysis — `CheckGpuToCpu` before an application whose read-set touches GPU-bound globals, `MarkCpuWritten` after one whose write-set does, exactly mirroring the tree-walker's `check_cpu_readable`/`mark_cpu_written` placement. All names resolve to table indices at compile time; nothing does runtime name lookups.
- **`VmCpuRuntime` (interpreter.rs) wraps a real `EvaluationEnvironment`**, reusing its sync-state, upload/readback, `format_for_print`, audio, and render-target machinery unchanged. Fixed-size GPU-bound globals live authoritatively in VM stack slots and are mirrored into the env's `Value`s lazily (`slots_dirty`, refreshed only when an upload needs serialization). **Runtime-sized array globals live in `BytecodeProgram::dyn_memory` as flat words** (`DynMemory`: lazily-`Zeroed` — preserving the no-alloc `BufferUpload::Clear` upload — or materialized `Words(Vec<u32>)`), outside the u16-addressed stack, in *both* CPU and audio modes; element/length accesses compile to the direct bounds-checked `DynLen`/`DynLoad`/`DynStore`/`DynResize`/`DynAssignFromSlots` opcodes (region id compile-time, element index runtime), and element ref-args/compound assignment work via the same pending-write-back pattern as `ArrayLookup`/`ArrayStore`. `Value`s are built from the words only at boundaries: printing, upload serialization (`refresh_dirty_slots` mirrors dirty regions into the env before `collect_dirty_uploads`), and GPU readback (mirrored back into words by `CheckGpuToCpu`). Textures (`HostBindingStorage::Dynamic`) still live in the env as `Value`s behind host ops; audio-mode compilation skips texture globals entirely (audio code can never touch them) rather than panicking on their unknowable slot size.
- **`Value::from_vm_words`/`to_vm_words`** convert between the VM's flat layout and tree-walker `Value`s (matrices are arrays of column vectors, matching the tree-walker), so printing and GPU serialization are byte-identical across runtimes.
- **`spawn-window`/`close-window` suspend the VM** (`RunResult::Suspended`; the continuation stays in `call_stack`, is stashed while the frame loop runs, and resumes after). A scoped frame closure's scope is materialized into slots at the spawn-window site, and the frame fn is compiled as a per-usage specialization whose scope param aliases those slots (`PendingFrameFnUsage` patches the `HostOp::SpawnWindow` index after the deferred compile) — slots persist, so mutation across frames matches `Function::Scoped` semantics. The frame loops themselves are shared with the tree-walker via the `FrameDriver` trait (one implementation per IO manager, driven by either backend).
- **Closures are represented as their captured scope data** (`vm_type_size`: function-typed values occupy their scope struct's slots; scope-less closures are zero-size). Scope constructions compile like struct constructions (recognized by their `StructConstructor` callee ancestor plus function-typed expression type; they keep a separate arm from ordinary struct construction because scope layout uses `vm_type_size` — captured fields can themselves be closures). Closures reachable only through scope constructions are discovered via `composite_functions_in_usage_order_with_discovery(true)` (the reference-address-space rebuild drops them from the registry; only the VM CPU compile uses discovery — WGSL/C/audio emission must not see them).
- **Known gaps** (panic with clear messages): nested `spawn-window` is rejected. (Whole dynamic arrays passing by value / array-to-array assignment now work — see "First-class runtime-sized arrays"; runtime strings work — see "Runtime strings".)

### First-class runtime-sized arrays (Arc/COW heap handles)

Runtime-sized arrays are ordinary *values* on the CPU: locals, function args and returns, struct fields, enum payloads, and nesting (`[[f32]]`) all work, with **value semantics everywhere** (binding/assigning/passing copies; mutating one copy never shows through another — pinned by `dynamic_array_copy_semantics`). Architecture:

- **A dyn value is one stack word: a heap id** (`heap_index + 1`; 0 = null/empty) into `BytecodeProgram.heap`'s `Arc<HeapCell>` cells (`HeapCell { memory: DynMemory, stride }`). `vm_type_size(unsized array) = 1`, which is what makes dyn values sizable in every aggregate/signature position. Dynamic **globals** keep the region system (`dyn_memory`), bridged by `HeapFromRegion` (deep copy on read-as-value) and `RegionFromHeap` (whole-assignment to a global); all existing region machinery (host bindings, GPU sync, shared-sync publishing, `load-wav`) is untouched.
- **Containers of heap values own their elements as Rust `Arc`s** (`DynMemory::Cells(Vec<Option<Arc<HeapCell>>>)`, `None` = the null id): when the element type is itself heap-backed (`[[f32]]`, `[String]`), cells and regions store `Arc` children instead of flat words, so element lifetimes are Rust's job — storing an element (`HeapStore`/`DynStore` cell arms) clones an owned share, dropping or COW-cloning a container releases/shares children automatically, and reading an element (`HeapLoad`/`DynLoad`) allocates a fresh table id around a cloned child. Which variant a container uses is fixed at compile time by its element type (`is_heap_value_type` in compile.rs): construction emits `HeapFromSlotsCells`/`HeapZeroedCells` instead of the flat ops, and the cpu-mode whole-global fast paths (`DynResize`/`DynAssignFromSlots`) apply only to flat elements — heap-backed elements go through the generic value-then-`RegionFromHeap` path, whose `memory.clone()` is `Arc`-correct. Flat-element containers never touch the `Cells` arm (it's one extra never-taken variant in the existing `Zeroed`/`Words` match, so the audio hot path is unaffected); `Zeroed` occurs only for flat elements. Pinned by `nested_dynamic_array_element_store`/`_copy_on_write`/`_global` and `string_array_element_store`. Decoding embedded children needs no heap table (`value_from_heap_cell` — children are self-contained); thread-shared publishing of `Cells` regions panics with a clear message (element references can't cross heaps).
- **Copies are `Arc::clone`-cheap, mutation is COW**: `HeapCopy` takes a fresh id sharing the payload; `HeapStore` mutates through `Arc::make_mut`, cloning only when shared. This is the Perceus-style share-until-mutate design agreed for the language.
- **Reclamation is drop-on-overwrite only**: every op that writes a heap id into its statically-fixed destination slot releases the previous occupant first. A site retains at most one live cell (freed/overwritten the next time that code path runs); there are no scope-exit drops and no liveness analysis (that's the planned v2). Sound for the same reason slot allocation is: no recursion ⇒ one live activation per final function.
- **Ownership rule**: a heap id is *owned* by the slot a `Heap*`-creating op wrote it to, and container elements are owned by their container (the `Cells` bullet above). Heap-id words that plain `Move`s copy into struct/enum/scope data or out of function returns are **borrows** — valid until the owning site re-executes. Call-boundary copies (`copy_op_for`: heap-typed args and returns use `HeapCopy` instead of `Move`) and let-bindings of variable-storage initializers (`aliases_variable_storage`) promote borrows to owned ids at every binding point the pinned tests exercise. ⚠️ Known v1 gap: a borrow held across the owning site's re-execution dangles — storing a dyn-payload enum or dyn-field struct somewhere that outlives the activation that built it is unsupported (the committed pins all stay within one activation).
- **CPU-only emission filtering**: `TopLevelFunction::compile`'s `allowed_on_gpu` gate covers both ways a composite function can be CPU-only — effects (CPU-exclusive calls/types, window queries) and now *types* (a signature involving runtime-sized values, via `Type::involves_runtime_sized_array`); its `fn_string` closure is lazy, so skipped bodies are never lowered. Enum *constructors* are synthesized definitions emitted directly in `compile_to_target` (they never pass through `TopLevelFunction::compile`), so that loop carries its own signature gate; enums with runtime-sized payloads are likewise skipped from WGSL struct emission.
- **The GPU boundary is validated** (`validate_gpu_runtime_sized_use`, runs before `validate_top_level_fn_effects`): functions reachable from GPU entry points may not return/accept runtime-sized arrays (`GpuFunctionReturnsRuntimeSizedArray`/`...Accepts...`), locally bind them (`RuntimeSizedLocalInGpuCode`), or use struct/enum *values* containing them (`RuntimeSizedFieldOnGpu`, no exceptions); a binding may involve a runtime-sized array only by *being* one — nested runtime-sized arrays are rejected (`NestedRuntimeSizedArrayBinding`) and so is any struct/enum/fixed-array binding type containing one (`RuntimeSizedFieldInBinding` — deliberately stricter than WGSL's trailing-member allowance; bind the array separately instead). When this pass fires, the redundant `CPUExclusiveFunctionInGPUEntryPoint` complaint for the array constructors is suppressed (the shader error tests assert exact error sets).
- Printing works for every runtime-sized shape on both runtimes: dyn locals/results, struct fields, enum payloads, and nested arrays decode through `value_from_vm_words_heap` (the heap-aware sibling of `Value::from_vm_words` — the `Print` host op receives the heap via `VmHost::host_call`); flat types delegate to `from_vm_words`, keeping output byte-identical to the tree-walker.

### Runtime strings

Strings are first-class runtime CPU values built on the same heap: a `String`-typed value is one stack word — a heap id whose cell holds **one char codepoint per word, stride 1** (UTF-32-style, so `length`/`substr` are character-based and can never split a codepoint; encoding/decoding happens only at Rust boundaries via `string_to_words`/`words_to_string` in `bytecode.rs`). `HeapCopy`, drop-on-overwrite, and `copy_op_for` (which routes `Type::String` args/returns through `HeapCopy`) all apply to strings unchanged. `String` is a nameable type in easl source (`Type::from_name`), usable in user function signatures and locals.

- **The API** (`string_functions()` in `builtins.rs`): `(string x)` — generic over any `T`, returns the value's exact `print` representation; `(concat a b ...)` — associative, so n-ary like `+`; `(length s)` → `u32` char count (an overload of vector `length`); `(substr s start end)` — char indices, **exclusive end**, out-of-range indices clamp (start ≥ end or start past the string ⇒ empty string; never errors); `==`/`!=` overloads for content equality. Pinned by the `string_*` cpu tests, which run on both runtimes.
- **VM**: `Str*` opcodes in `bytecode.rs` — `StrConst` (materialize `host_strings[i]`; the generic `StringLiteral` compile arm emits this, replacing the old "can't handle strings" panic — literal consumers like print fast paths/key queries/file paths still intercept before it), `StrConcat`, `StrSubstr`, `StrEq`; `length` compiles to the existing `HeapLen`. `(string x)` is the `Stringify` host op: it decodes via `value_from_vm_words_heap`, formats with the same `format_for_print` as `Print`, and writes a fresh string cell — which is why `VmHost::host_call` takes `heap`/`heap_free` mutably (host-side cell writes go through the shared `release_heap_id`/`alloc_heap_cell` helpers, the same functions the dispatch loop's `release_cell!`/`alloc_cell!` macros delegate to).
- **CPU-only, automatically**: every `String`-typed expression merges `Effect::CPUExclusiveType("String")` (expression.rs), which excludes string-using functions from WGSL/C/audio emission through the existing filters; `allowed_on_gpu`'s signature gate also checks `Type::involves_string` (covering the degenerate case of a `String`-typed parameter the body never touches), and `value_from_vm_words_heap`'s flat-type fast path is gated on both `involves_runtime_sized_array` and `involves_string`.
- Gaps, same shape as dyn arrays: strings never cross the thread-sync or GPU boundaries (`@external` String-containing vars are rejected, `ExternalStringVar`), and struct/enum-embedded string ids are borrows under the v1 ownership rule. `[String]` locals and globals are fine — string elements are owned `Cells` children like any heap-backed element type.

## Cross-thread shared variables (`src/thread_sync.rs`, `src/vm/shared_sync.rs`)

How the main `@cpu` thread and the `start-audio` thread share global variables. The model is **boundary-batched replicated coherence**: every thread holds its own replica of each *shared* global; writes touch only the local replica and set a per-variable dirty flag; at each thread's **iteration boundary** — a window frame on main, a cpal callback batch on audio — the thread *publishes* version-stamped snapshots of the shared variables it wrote and *adopts* newer snapshots published by the other. Within an iteration every read is stable. Concurrent writers resolve by **whole-variable last-writer-wins**; two threads interleaving element writes into one array is a documented footgun, not an error (multi-writer is deliberately allowed — CPU+GPU both writing one var is a headline feature of the language, and the audio thread inherits the same stance).

- **The shared set is static**: `Program::thread_shared_globals` (program.rs) computes reachability from the `@cpu` entry roots and the `@audio` entry roots separately — following function-valued references through `Known(Type::Function)` ancestors, but **cutting the argument edge of `(start-audio ...)`** so the audio fn's body doesn't count as main-reachable. A var is shared when both sides can touch it, **or when it's marked `@external`** (embedder access is invisible to static analysis, so the annotation forces membership; `@external` on a texture is a compile error, `ExternalTextureVar`). Each shared var carries an **audience bitmask** (`thread_sync::participant`: `MAIN`/`AUDIO`/`EXTERNAL` — which participants' code can touch it). The result is sorted by name, and that order index-aligns everything: `Code::shared_vars` in every compiled artifact (main-CPU-mode and audio-mode bytecode both), the env's `shared_globals`, `ExternalVars` handles, and the `ThreadSharedTable`'s slots. Vars touched by only one thread never publish, ever — the `only_genuinely_shared_vars_sync` golden pins this.
- **The primitive** (`thread_sync.rs`, no `unsafe`): `SharedVarSlot` holds an `ArcSwapOption<SharedSnapshot>` (version + VM-layout words, immutable once published) and an atomic version counter. `publish(words) -> (version, Option<Vec<u32>>)` hands back the previous snapshot's buffer when no adopter holds it — steady-state publication allocates nothing (the VM keeps per-var `shared_scratch` buffers). `adopt_if_newer(last_adopted)` is a lock-free load. Neither side ever blocks the other, which is what makes adoption safe inside the real-time audio callback. Real-thread tearing/convergence stress tests live in this file's unit tests.
- **Audience × live-participant gating replaces the old all-or-nothing `active` flag**: the table holds an atomic word of live-participant bits (`MAIN` always; `start-audio` joins `AUDIO`; creating an `ExternalVars` handle joins `EXTERNAL` — monotonic, nobody leaves). A participant publishes var *i* only when `audience[i] & live_others(self) != 0` (someone *else* who cares actually exists) and adopts only vars in its **own** audience — so an audio↔embedder var flows between those two directly without main ever copying it (`external_to_audio_direct` pins the absence of main-adopt lines), and programs with no second participant pay one atomic load per boundary. Bootstraps generalize as "a new listener joined → fill the gaps it can't otherwise see, never overwrite": a forced (bootstrap) publish applies only to vars in the **publisher's own audience** (its replica of anything else is never authoritative) and only when the slot has **never been published** (`SharedVarSlot::has_published`) — the newcomer's first adopt takes an existing snapshot anyway, and overwriting one would clobber another participant's state (shipped once as studio sliders silent-until-drag; pinned by `external_seed_survives_start_audio`). `start-audio` force-publishes with `force_mask = AUDIO`; the **entry-start bootstrap** (`bootstrap_external_globals` / `vm_bootstrap_external`, run right before the `@cpu` entry body) first adopts anything the embedder pre-seeded, then dirty-marks-and-publishes the program-computed initial value of any unpublished `@external` var (this one deliberately includes vars outside main's audience — at entry start main's replica holds the same `$init_globals` values as everyone's, and the embedder has no other way to observe initializers). Publishing **sets the publisher's own `adopted` version** — otherwise a thread would waste a copy re-adopting its own snapshot at the next boundary.
- **Dirty marking**: tree-walker — `mark_cpu_written` (which already receives every application's write set) also sets `shared_dirty`. VM — `emit_write_marks` emits an `Op::MarkSharedDirty` instruction after any application whose write set touches a shared var, in **both** cpu and audio modes (the `MarkCpuWritten` host op stays cpu-mode-only). Per-sample cost on the audio thread is one instruction per shared-var write.
- **Boundary placement**: both `FrameDriver::run_frame` impls adopt at frame start (before window-info refresh) and publish at frame end — including frames that end via `close-window` (the frame's writes are real; only genuine errors skip the publish). `VmAudioDriver::run_batch` adopts before the first sample and publishes after the last.
- **Snapshot format is VM words**: `SharedVarInfo.storage` says where a replica keeps the value (`Slots { position, size }` or `DynMemory { region, stride }` — each artifact records its *own* layout; indices align by name). The tree-walker converts through `value_to_shared_words`/`shared_words_to_value`; an `Uninitialized` unsized array publishes as empty words, matching the VM (whose dyn regions have no uninitialized state) — traces must be runtime-identical.
- **The GPU is a full participant, proxied by main.** It has no boundary loop of its own, so main acts on its behalf in both directions. *Inbound*: adopting a GPU-bound global sets its buffer state to `GPUOutOfDate` **directly — NOT via `mark_cpu_written`**, which would re-dirty the shared flag and ping-pong the value back at the next boundary (the VM frame driver also sets `slots_dirty` so the env mirror re-serializes); the normal dirty-upload machinery then ships the adopted value before the next dispatch. *Outbound*: at every frame-end publish **and** the `start-audio` bootstrap, any shared binding whose newest value lives on the GPU (buffer `CPUOutOfDate`) is read back — through the production `check_cpu_readable` path, which flushes the frame's queued GPU work first — and force-published, which is what makes GPU writes visible to the audio thread (tree-walker: inside `publish_shared_globals`; VM: the GPU-proxy pass in `VmFrameDriver::publish_shared` and the `HostOp::StartAudio` arm, both via the shared `readback_binding_into_vm` helper, which `CheckGpuToCpu` also uses). The readback is a real per-writing-frame cost, paid only for genuinely-shared GPU-written vars and only while another thread is live — the `gpu_write_audio_read`, `gpu_write_no_audio_no_readback`, `gpu_write_before_start_audio`, and `audio_and_gpu_write_cycle` goldens pin all sides of this.
- **`ExternalVars` (`src/external.rs`) is the embedder participant**: created from the *validated* `Program` (must be the same one the runner receives — `table_for_env` asserts the var counts agree), passed into `run_program_entry_with_io_runtime_and_external_from_path` (or `VmCpuRuntime::new_with_external` / `EaslRuntimeHandle`-style embedders), `Send + Sync`, each read/write call is its own boundary (reads adopt-then-copy, writes publish). Index writes are **read-modify-write on the whole variable**. The `_raw` methods speak flat VM-layout words (a documented public contract); the `Value` methods wrap them. easl-studio's slider system is the reference consumer: it seeds the handle from the AST-derived values before spawning the runtime and writes `extracted_sliders` on drags — no direct GPU/interpreter/audio pokes anywhere.
- **Trace hooks are zero-cost**: `record_shared_publish`/`record_shared_adopt` on `IOManager` (default no-ops) and the `impl FnMut(u16)` hook params on the VM/audio paths exist for the thread-sync test suite; production passes `|_| {}`, which monomorphizes away.
- **The bootstrap consumes dirty flags**: `start-audio`'s force-publish clears them, so a var written earlier in that same frame is *not* re-published at the frame boundary (pinned by the `dynamic_array_shared` golden).

## Audio runtime (`src/audio.rs`)

When an easl program calls `(start-audio audio-fn)`, the runtime starts a cpal output stream that calls `audio-fn(t: f32, rate: f32) -> f32` once per sample. There are two backends, selected via `AudioBackend`:

- `AudioBackend::VM` (default, always available): compile the program to bytecode and have the cpal callback run `BytecodeProgram::execute` once per sample. Portable, no external dependencies.
- `AudioBackend::C` (gated on the `c_audio` feature): JIT-compile the program's C-backend output to a dylib via `clang`, dlopen it via `libloading`, and have the cpal callback call the loaded function pointer. Faster but requires `clang` on the host. Selecting `AudioBackend::C` without `c_audio` enabled panics at audio-compile time.

The runner functions in `interpreter.rs` (`run_program_entry_from_path`, `run_program_entry_with_io_from_path`) use `AudioBackend::default()` (= `VM`). To opt into the C backend explicitly, use `run_program_entry_with_io_and_audio_backend_from_path` (gated on `feature = "window"`).

### Public surface
- `AudioBackend { VM, C }` — caller-selected backend choice (`Default = VM`).
- `AudioSource` — what the interpreter hands to the IO manager: `Bytecode { program, function_names, shared_table }` or `C(String)`. The `shared_table` (an `Option<Arc<ThreadSharedTable>>`) is attached by the `start-audio` builtin after the bootstrap publish; the C variant predates the sharing system and keeps its compile-time-baked globals.
- `VmAudioDriver` — one audio thread's iteration engine: bytecode replica + shared-table plumbing. `run_batch(frames, rate, emit, on_adopt, on_publish)` adopts, runs the audio fn once per sample (emitting clamped samples), and publishes; the hook closures monomorphize away when passed `|_| {}` (production) and drive the thread-sync test harness's trace otherwise.
- `start_audio_thread_vm(entry_name, program, function_names, shared_table)` — VM-backed cpal driver; each cpal callback is one `run_batch`.
- `start_audio_thread_c(entry_name, c_source)` — C-backed driver; `panic!`s without `c_audio` feature.
- `is_audio_thread_started() -> bool` — used by IO managers to distinguish "repeated start-audio call after first one already set things up" (no-op) from "run was started without audio support" (error).

### Things the `start-audio` builtin handles, that you shouldn't break

- **`(start-audio ...)` is typically called every frame** from inside a `spawn-window` callback, so the builtin gets dispatched repeatedly. The first call moves the `AudioSource` out of the env and into the IO manager; subsequent calls pass `None` and the IO manager treats that as a no-op if a stream is running. `StringIO` (the test manager) records *every* start-audio event regardless.
- **The audio source is compiled from a clone of the (already-validated) `Program`.** Do **not** re-validate the clone — see the "compile_to_bytecode_program precondition" note in the Bytecode VM section.
- **VM audio hot-swap is not implemented** — second-and-later calls just log a note and return. C audio does hot-swap via an `AtomicPtr` of the loaded function.
- **`start-audio` bootstraps the shared-variable table** (see "Cross-thread shared variables" below): both handlers (tree-walker builtin arm and `HostOp::StartAudio`), on the first call — the one that still holds a `Bytecode` source — activate the env's `ThreadSharedTable`, force-publish *every* thread-shared global from the main replica, and attach the table to the `AudioSource` before handing it to the IO manager. The audio replica's first batch-boundary adopt then sees the current state of everything — this is what makes the `load-wav`-into-global-then-`start-audio` sample pattern work — and from there on both threads publish/adopt at their iteration boundaries, so later writes on either side propagate (this replaced the old one-time `copy_globals_into_audio_program_*` snapshot). The `start_audio_bootstrap_publishes_current_globals` audio test drives the bootstrap publish/adopt pair directly (never through cpal).
- **`(load-wav "path")`** (CPU-exclusive) reads a `.wav` as mono f32 samples at its native rate (multi-channel mixed down by averaging; `hound` crate), typically assigned to a `[f32]` global. Tree-walker: `Value::Array`; VM: `HostOp::AssignDynFromWav` writing `DynMemory::Words` directly.

## Test Structure

The test suites:

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
- Compiles with `CompilerTarget::WGSL`, runs via `run_program_capturing_output_with_runtime`
- Asserts that captured `(print ...)` output matches the `.txt` file exactly
- **Runs every test on both CPU runtimes** (tree-walking and bytecode VM) and asserts identical output for each — as do the buffer, sync, and window suites

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

### Sync tests (`tests/sync_tests.rs`, sources in `data/sync/`)
```rust
sync_test!(test_name);  // runs data/sync/test_name.easl, golden-matches the GPU↔CPU transfer trace
```
- Runs the program on the real GPU via `run_program_capturing_io_from_path` and compares `CaptureIO::sync_trace` — the ordered log of implicit CPU→GPU uploads, GPU→CPU readbacks, and prints — against `data/sync/test_name.txt` (`upload: <var>` / `readback: <var>` / `print: <text>`, one per line).
- The trace is recorded through the `record_cpu_to_gpu_sync` / `record_gpu_to_cpu_sync` `IOManager` hooks (default no-ops in production IO managers; `CaptureIO` overrides them).
- Use this suite to assert exactly *when* implicit syncs happen: both that spurious syncs don't occur (e.g. GPU-only dataflow must never read back) and that genuine ones still do (CPU reads of GPU-written data must sync exactly once per dirty→read transition). Blocking readbacks are the most expensive implicit operation in the runtime, so regressions here are performance bugs even when output is correct.
- Implicit dispatched-closure capture bindings (`<scope>_data_<capture>`, one per captured var) have gensym'd scope names whose numbering isn't stable across runs; the harness normalizes any name containing `_scope_data` to `<closure-scope>` in the trace — so one dispatch produces one `<closure-scope>` upload line per capture.

### Audio tests (`tests/audio_tests.rs`, sources in `data/audio/`)
```rust
audio_test!(test_name);  // runs data/audio/test_name.easl via the real from-path entry point, compares prints
```
- The only suite that exercises eager audio-source compilation (`try_compile_audio_source`, which runs whenever the program has an `@audio` entry point — before `start-audio` is ever called): the runners behind the cpu/buffer/window suites take a source path but use it only for the source *dir*, so they never compile the audio source at all. Anything about the audio runtime worth pinning belongs here.
- Runs each test on both CPU runtimes via `run_program_entry_with_io_and_runtime_from_path` with `CaptureIO`, comparing captured `(print ...)` output to the `.txt` file.

### Thread-sync tests (`tests/thread_sync_tests.rs`, sources in `data/thread_sync/`)
```rust
thread_sync_test!(test_name, [Frame, AudioBatch(3), Frame, AudioBatch(3)]);  // scripted schedule
```
- Tests the cross-thread shared-variable semantics deterministically on one thread: `ThreadSyncIO` (defined in the test file) walks the given schedule — `Frame` runs one main-thread frame through the production `FrameDriver::run_frame` path, `AudioBatch(n)` runs `n` samples through the production `VmAudioDriver::run_batch` (stashed by its `start_audio` override instead of opening a cpal stream), and `ExternalWrite`/`ExternalWriteIndex`/`ExternalRead` drive a real `ExternalVars` handle (created only when the schedule has external steps; external steps *before* the first `Frame`/`AudioBatch` run before the program starts — the embedder's seed-then-run pattern). The batch size is a per-test harness parameter. Because the sharing semantics are boundary-batched, iteration-granularity scheduling is a *complete* model of real-thread interleavings; the true-concurrency invariants (no tearing, version monotonicity) are stress-tested separately on the primitive in `thread_sync.rs`'s unit tests.
- **Dispatches are real**: `ThreadSyncIO` wraps `StdoutIO` the same way `CaptureIO` does (lazy headless `GpuCore`; each `Frame` step executes the queued events through the shared `execute_frame_gpu_work` path), so the GPU↔thread-sync interplay is exercised genuinely — GPU-written shared vars really are read back and published at frame boundaries, and adopted values really are uploaded before dispatches.
- Golden-matches the ordered trace against `data/thread_sync/<name>.txt`: `frame <i>` / `audio-batch <i> x<n>` schedule markers, `main-publish:`/`main-adopt:`/`audio-publish:`/`audio-adopt: <var>` sync events, `upload:`/`readback: <var>` GPU transfers, `dispatch-compute-shader`/`dispatch-render-shaders` events, `print: <text>`, `samples: <s0> <s1> …` (the batch's clamped output), plus `spawn-window`/`start-audio: <entry>`/`close-window`. The goldens pin exactly *when* cross-thread syncs happen — a spurious publish/adopt/readback is a silent performance bug, a missing one a correctness bug — and the bootstrap publish-all at `start-audio` enumerates precisely which variables the static analysis classified as shared.
- Every test runs on **both** main-thread runtimes against the same golden. The harness sample rate is 8 Hz so `t` values are exactly representable and goldens stay float-noise-free.

### Shared notes
- `#_` reader macro in `.easl` files comments out the next form — useful for disabling parts of test files
- Target a specific suite: `cargo test --test shader_tests`, `--test cpu_tests`, `--test window_tests`, `--test conformance_tests`, `--test vm_tests`, `--test sync_tests`, `--test audio_tests`, `--test thread_sync_tests`

## Style Notes

- Rust 2024 edition — uses `let` chains in `if let` expressions freely
- The codebase uses `take_mut::take` for in-place mutation of `&mut self`
- `Rc<RefCell<...>>` is used extensively for shared mutable state in the AST
- `ExpTypeInfo` wraps `TypeState` and implements `Deref<Target = TypeState>`
- `unwrap_known()` on `ExpTypeInfo` returns a cloned `Type` (panics if not `Known`)
