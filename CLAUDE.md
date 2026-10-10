# Easl Compiler

Easl (Enhanced Abstraction Shader Language) is a Lisp-like shader language that compiles to WGSL. It uses S-expression syntax and adds generics, sum types (enums), higher-order functions, first-class function values, and expression-based control flow on top of WGSL. The same program also runs on the CPU (a tree-walking interpreter and a bytecode VM), on an audio thread, and in the browser.

User-facing semantics are documented in `docs/pages/language.md`; this file is about the implementation.

## Build & Test

```bash
cargo test --features window                        # run all tests (ALWAYS use this flag)
cargo test --features window <test_name>            # run a specific test
cargo test --features window --test cpu_tests       # one suite (see Test Structure for names)
cargo run                                           # compilation benchmark (all .easl in data/gpu/)
```

> **IMPORTANT**: Always pass `--features window`. Without it the interpreter, GPU execution, and `IOManager` code are compiled out and most suites silently skip or misbehave.

Run `cargo fmt` before handing changes back (it reformats `tests/format_tests.rs` — revert that file). Don't commit unless asked.

## Project Structure

- `src/lib.rs` — public API (`compile_easl_source_to_wgsl`, `get_easl_program_info`, `format_easl_source`); exports `window`/`audio` modules under the `window` feature
- `src/parse.rs` — S-expression parser (`fsexp` crate); `src/format.rs` — source formatter
- `src/interpreter.rs` — tree-walking interpreter, the `IOManager` trait and its implementations, `EvaluationEnvironment`, `VmCpuRuntime`, value (de)serialization
- `src/window.rs` — wgpu renderer: `GpuCore` (device/pipelines/buffers), `StdoutIO` (real winit window), `create_headless_gpu_core`
- `src/audio.rs` — audio runtime: `AudioBackend { VM, C }`, `AudioSource`, `VmAudioDriver` (one callback batch: adopt → samples → publish); C path behind `c_audio`
- `src/thread_sync.rs` — `SharedVarSlot` / `ThreadSharedTable` (lock-free snapshot publication) and `participant` bits
- `src/external.rs` — `ExternalVars`, the embedder handle for `@external` globals
- `src/video.rs` — video decoding (`ffmpeg` subprocess) behind the `video` feature
- `src/midi.rs` — native MIDI listener (`midir`); `src/midi_web.rs` — the web equivalent
- `src/web_bundle.rs` — packaging a program for the web runtime (`web/` crate)
- `src/main.rs` — benchmark only; the CLI is the separate `easl_cli` crate
- `src/compiler/`:
  - `program.rs` — `Program` and the pipeline (`validate_raw_program`); emission (`compile_to_target`), `compile_to_bytecode_program`, `thread_shared_globals`. The largest file
  - `expression.rs` — `TypedExp` and expression-level passes (inference, monomorphization, inlining, deexpressionification, …)
  - `functions.rs` — `AbstractFunctionSignature`, `FunctionSignature`, `TopLevelFunction`, specialization
  - `types.rs` — `Type`, `AbstractType`, `TypeState`, `ExpTypeInfo`, unification. `Type::flat_data_size_in_u32s` is the flat/GPU size (errors on heap-involving types; special-cases `matNxM` to `cols*rows*elem`); the VM's own sizing is `vm_stack_size` (vm/compile.rs), where a heap value is one id word
  - `function_values.rs` — boxing and defunctionalization of runtime-chosen function values
  - `structs.rs`, `enums.rs` — struct/enum definitions and monomorphization
  - `builtins.rs` — all builtin functions, structs, enums, macros
  - `effects.rs` — effect types (see below)
  - `entry.rs` — entry point kinds and `should_compile_to_target`
  - `exp_builder.rs` — `ExpBuilder` for building typed expressions in post-inference passes
  - `modules.rs` — module resolution (`resolve_modules`): flattens files and `mod` forms into one namespace of internal names (see "Modules")
  - `error.rs`, `vars.rs`, `wgsl.rs`, `annotation.rs`, `macros.rs`, `info.rs`, `util.rs`, `core.rs`
- `src/vm/`: `bytecode.rs` (the VM: `Op`, `Code`, `BytecodeProgram`, `execute`), `compile.rs` (bytecode compiler — keep bytecode-compile logic here, not in `expression.rs`), `shared_sync.rs` (VM side of cross-thread sharing)

### Downstream consumers (changes here are breaking changes for another repo)
- **easl_cli** — owns `easl compile --web`; its `build.rs` builds `web/` in a nested cargo invocation, stripping the outer build's `CARGO_*`/`TARGET` env (otherwise host cfgs leak into the nested build scripts). Its `wasm-bindgen-cli-support` pin (`=0.2.129`) must exactly match `web/Cargo.toml`'s `wasm-bindgen`
- **easl_lsp** — uses `MainFileIndex`, `info::definition_signatures`, and `TypeDescription`'s display (generic instances print as `(Pair f32 u32)`); analyzes each open document as the main file of its own program via `load_and_parse_easl_multidocument_with_lookup_function` (open buffers overlaid on disk), `Program::from_easl_documents` + `validate_raw_program`, and `gather_type_annotations` for hover. It relies on `EaslMultiDocument::imports` being recorded even when loading stops at an error (to place an imported file's errors on the importing `import` form), on a missing import being an `ImportNotFound` error at the form rather than an IO error, and on `Program::main_file_index` for completion, hover, and go-to-definition
- **easl-studio** — uses `ExternalVars` for its sliders (seeds the handle from AST values before spawning the runtime, writes on drags), `GpuCore::new_from_parts` (shares its own wgpu device), `GpuCore::execute_render_batch_to_view` (renders into its own offscreen target), and has used `BytecodeProgram::write_global` to stream values into the audio VM

## Cross-cutting rules

**Effects (`effects.rs`)**:
- ⚠️ `is_side_effect_free` treats `CPUExclusiveFunction` as pure, and non-final block statements that pass it are **pruned as dead code**. A statement-position builtin must also carry an observable effect (`Window`, `Print`, `FileWrite`) or its calls vanish (why `start-audio`/`set-render-target` carry `Window`, `save-png`/`save-wav` carry `FileWrite`). Builtins mutating through a `@var @ref` arg need nothing extra: that arg derives `ModifiesLocalVar`/`ModifiesGlobalVar`
- `ReadsArrayLength` (direct `array-length` of a variable) is excluded from the GPU→CPU readback set (the GPU can't resize a buffer) but included in the upload set (WGSL `arrayLength` derives from buffer size) and in thread-sharing (another *thread* can resize)
- A lambda's effects exclude reads/writes of its own parameters and `Return`; `break`/`continue` can't cross a lambda (`BreakOutsideLoop`). Every other effect propagates out
- A `MutableReference` (or `Pointer(_, Mutable)`) argument derives `ModifiesLocalVar`/`ModifiesGlobalVar` at the call site

**`ExpBuilder`**: use it (not hand-built `TypedExp`/`FunctionSignature`) for anything a pass synthesizes after inference. `with_data` keeps an existing node's `ExpTypeInfo` (ownership, `is_globally_bound`) — use it wherever a node stands in for an existing expression; `typed`/`name` create fresh info. Fresh nodes are marked `subtree_fully_typed`, safe only because inference runs once, before every synthesizing pass.

**`is_globally_bound`**: any pass that rewrites a name into a reference to a global (storage-ref inlining, the GPU and audio capture lifts) must propagate `is_globally_bound` up the accessor/lookup chain above it. Mutable-ref effect derivation reads the *outer* node's flag; a stale flag silently drops writes from GPU sync and shared-dirty marking.

**Program side tables** (`top_level_vars`, `window_info_bindings`, `lifted_audio_captures`, `lifted_gpu_captures`; `overload_groups` is consumed right after inference) must survive every pass that rebuilds the function registry (the `take()` closures).

**Top-level initializers are bodies too**: a pass that lowers or rewrites function bodies must also cover `top_level_vars`' initializers (the VM runs them in `$init_globals`). Associative expansion and pseudo-application normalization once skipped them (n-ary `+` in a `def` silently kept two arguments; indexing in a `def` crashed the VM).

**Assignment builtins**: every assignment-like builtin (`=`, the arithmetic and bitwise compound forms) must be in `ASSIGNMENT_OPS` — it drives WGSL emission as an assignment statement, mutability validation, and the tree-walker's write-back — and must declare its target `MutableReference`, or calls are pruned as side-effect-free.

**References and addressability**: only parameters are ever references. `Program::mark_value_names_owned` (the last validation pass) marks every name bound by a `let`, `match` payload, or `for` loop as owned, so a temporary built from a value read through a `@ref` (whose data carries the reference ownership) is never dereferenced by emission. A local passed to a `@ref` parameter is made addressable at its binding, never copied per call (deexpressionification, classified by `TypedExp::local_bindings`): a `let` becomes a `var`, an owned parameter (of a function or a lambda) is rebound as a `var` at the top of its body, a `match` payload is always emitted as a `var`; any other binding is a compiler-bug panic. Parameters lowering creates later are handled by `make_referenced_params_addressable`.

**Hash order**: hash seeds are intentionally random — never fix them to paper over order-dependence; fix the order-dependence (dedupe by identity, not generated names; register before rewriting; sort address-keyed maps before iterating).

## Compilation Pipeline

Before it, `Program::from_easl_documents` macroexpands each document and runs **module resolution** (`resolve_modules`, modules.rs), which hands the rest of the compiler one flat list of definitions with every name rewritten to an internal name. Then `Program::validate_raw_program` (program.rs), in order:

1. **Name validation** — reserved/invalid names (incl. the compiler's reserved `easl_*` names)
2. **Mutable arg wrapping** — `@var` args
3. **Deshadowing** — every local name is bound at most once per function (shadowing *and* sibling scopes); later passes key variables by name
4. **Type inference** (`fully_infer_types`; overload groups' duplicate signatures are caught just before it), then `resolve_overload_groups` (group references become the chosen member's name), then `rewrite_aliased_builtin_calls` (pure aliases like `into`/`length` become their targets), then `box_function_values`
5. **Control flow validation** — code after `break`/`return`, `match` exhaustiveness and matchable types
6. **Associative expansion** — `(+ a b c)` → `(+ (+ a b) c)`
7. **Deexpressionification** — lifts expression-position `let`/`match`/blocks into statements. Arguments keep left-to-right order: before a later argument is lifted, every earlier argument with effects is bound to a temporary (by-reference and function-valued ones stay in place); an effectful unit-typed argument is bound the same way, because unit values are erased later. A `for` update needing statements stays in the update slot so `continue` still runs it (WGSL: `loop { if !(cond) { break; } body continuing { update } }`; C: a `({ ... })` statement expression in the header)
8. **Monomorphization**
9. **Inner function extraction** — closures become top-level functions taking a scope struct
10. **Overload separation** — type-suffixed names, then `canonicalize_function_references` repoints every function type's `abstract_ancestor` at the registered signature sharing its implementation. After this, reading a name off any ancestor is safe (signature *copies* otherwise keep stale names)
11. **Higher-order argument inlining** (looped with extraction), then `defunctionalize_boxed_functions` right after that loop
12. **Entry point & effect validation**, context exclusivity
13. **Ownership validation**
14. **Reference address space monomorphization** — final pass; also inlines whole-global storage refs

Ordering constraints worth knowing:
- `extract_audio_closure_scopes` runs **before** reference monomorphization, which drops scoped closures (non-owned trailing scope param) from the registry
- `validate_context_exclusivity` runs **after** implicit entry-point marking and **before** `extract_audio_info` erases the audio-info calls
- `validate_gpu_runtime_sized_use` runs **before** storage-ref inlining, so its "no runtime-sized params" check applies only to *owned* params (a `@ref [T]` param is fine — whole storage globals always inline). It can't see implicit dispatched-capture bindings, so the lift itself rejects `RuntimeSizedFieldInBinding`

The pipeline is **not idempotent** (a late pass turns `Reference` into `Pointer`): validate a program exactly once, then compile the validated program. Never re-validate a clone.

## Key Concepts

### Naming
- Easl uses kebab-case (`make-two-of` → WGSL `make_two_of`), PascalCase for types
- Monomorphized names get type suffixes (`map_f32`); higher-order specializations append the inlined function's name (gensym'd on collision)

### Modules (`modules.rs`)
- Every file is a module (one per canonical path, however often imported); `(mod name …)` nests one. Forms: `(import "p")` (every public name, unqualified), `(import alias "p")` (namespace), `(use path)` / `(use path [a b])` (a module or an enum), `@private` (visible to the module and its nested `mod`s), `@unpack` on enums. User docs: `docs/pages/language.md` ("Modules")
- **Internal names**: each definition is its module's prefix + its name. The main file's prefix is empty (single-file programs are unchanged); an imported file's is its stem (deduplicated against other files and the main file's top-level `mod`s, e.g. `geometry/`); a `mod`'s is its parent's + `name/`. Variants are `<enum>/<variant>` (`Shape/Circle`) unless `@unpack`. A main-file `defn` sharing a builtin's name gets the `root/` prefix so it overloads the builtin only where it's visible. `compile_word` turns `/` into `_`; printing shows a struct's or variant's last segment (`display_name`), as do `UnboundName`/`CantShadowTopLevelBinding`
- **Scopes**: a module's own definitions + (for a `mod`) its enclosing scope + names from `import`/`use`, which act exactly as if defined there. Functions with the same bare name overload; any other repeat is `NameCollision`; the same definition reached twice is fine. No re-export: a module's *members* are only its own definitions
- **Privacy is per overload**: a module's member table holds every overload of a name with its own privacy flag. When a name has both private and public overloads in one module, the private ones get the internal name `<name>/private` (a separate registry bucket), so importers reach only the public bucket while the module's own scope groups both. A name whose overloads are all private (or all public) keeps the plain internal name — entry points keep their names
- **Overload groups**: wherever several functions share a bare name in a scope (or one shares a builtin's name), references resolve to a group name (`describe@0`) registered in `Program::overload_groups` → member registry buckets. Inference sees the union of the members' signatures (`concrete_signatures`, `names_functions`); `resolve_overload_groups` rewrites each reference to its ancestor's name right after inference. Groups are created eagerly per scope so `catch_duplicate_overload_group_signatures` sees conflicts even when nothing calls them
- **Positions**: the resolver rewrites leaves by syntactic position — type positions (right of `:`, enum payloads, struct field types) skip function origins (so a `vec4f` constructor overload never hijacks the type `vec4f`); struct field names, annotation contents, comments, and strings are untouched; only the head of `a.b.c` is resolved; a definition's generic parameters are never resolved; `~x` becomes an explicit `(group x)` application when `into` resolves to a group
- **File paths**: a relative path is relative to the file the call is written in, through one mechanism in the resolver: `resolve_path_arguments` expands a direct `(resolve-path path)` into `(resolve-path (current-directory) path)` and wraps every file builtin's path argument (`PATH_BUILTIN_ARGUMENTS`: `load-image`, `save-png`, the wav builtins, `load-video`) the same way, in every file; `expand_current_directory` then turns every `(current-directory)` call — written or inserted — into the file's absolute directory as a string literal. `current-directory` (and one-argument `resolve-path`) exist only as direct calls: any other use of `current-directory` is `CurrentDirectoryNotCalled` (`check_current_directory_calls`). The runtimes therefore always receive absolute paths and take no source directory. Only direct calls are rewritten (a head the module rebinds isn't the builtin). Pinned by `module_relative_paths` and `current_directory_value_failure`
- **File isolation**: in non-main files, a leaf matching one of the main file's unqualified names (which those files can't see) is renamed to `<module prefix><name>`, so a local there can't collide with, or resolve to, the main file's global
- The loader records which document each `import` form loads in `EaslMultiDocument::imports` (keyed by the form's position) — also when loading stops early at an error. An import naming a file that doesn't exist is `ImportNotFound` at the form. The resolver also records `Program::main_file_index` (`MainFileIndex`, for editor tooling): every bare name and path the main file can refer to with its `NameKind`, every main-file name referring to top-level definitions with those definitions' name positions (all overloads), the main file's own definition names, and `written_names` (how the main file writes each reachable definition whose internal name differs: `geometry/Point` imported as `geo` → `geo/Point`). `info::local_references` links each local-variable reference to its binding (parameters, `let`, lambda parameters, `match` bindings, `for` variables), and `info::definition_signatures` renders each user definition as easl source (`(defn (swap T U) [p: (Pair T U)]: (Pair U T))`), keyed by name position — call both before validation, which renames bindings and rewrites definitions. Pinned by the `module_*` cpu tests (`data/cpu/modules/` holds their library files) and the import suite

### Generics
- `(defn (map T U) [...])`; monomorphized by `TypedExp::monomorphize` and `AbstractFunctionSignature::generate_monomorphized`
- `AbstractStruct::opaque: true` marks WGSL-native types (`Atomic`, `Texture2D`, `Sampler`, `Video`) that must never be emitted as struct definitions. New WGSL-primitive builtin types need it. `Texture2D`, `Sampler`, and `Video` are also in `ABNORMAL_CONSTRUCTOR_STRUCTS` (no field constructor; `Atomic` keeps its `(Atomic x)`). (`MidiNote` is deliberately *not* opaque or skipped — it has no native WGSL form, so it emits like a user struct)
- Every declared generic must appear in the definition's signature (`validate_generic_usage` → `UnusedGeneric`): an unused one can never be inferred. The check collects names everywhere a generic can hide (function-typed args, const array sizes, nested skolems); over-collecting is safe, missing a name is not
- Generic instances named only in declarations (a top-level var's type, a signature) are registered for emission too, so declarations never name undefined types

### Enums
- WGSL: a struct `{ discriminant: u32, data: array<u32, N> }`. Payload words are **flat** (leaves in declaration order, no padding; a closure contributes its scope's words), so host uploads/readbacks serialize payloads through `Value::to_vm_words`/`from_vm_words`, never the payload's padded WGSL layout
- Constructors/matches `bitcast` to/from the words — except `bool` chunks (packed `u32(x)`, unpacked `x != 0u`; C stores `? 1u : 0u`) and matrices (packed column-major, unpacked through the scalar `matCxR(...)` constructor; matrix column indexing is `m[i]` in WGSL and the prelude's `index_matNxM` in C). String/runtime-sized payloads are CPU-only
- Variant names are qualified by their enum (`Shape/Circle`, see "Modules"); `@unpack` makes them plain module members (the builtin `Option`'s `Some`/`None` behave so)
- Unit variants become constants, data variants constructor functions. Enum constructors are synthesized directly in `compile_to_target` (they never pass through `TopLevelFunction::compile`), so that loop has its own target gate — a new "CPU-only type" condition must be added in both places
- Builtin enums: `Option` (unpacked variants `Some`/`None`), and `FilterMode`/`AddressMode` (qualified variants, e.g. `FilterMode/Linear`, for the `Sampler` constructor). The resolver passes a builtin enum's qualified variant through when the program doesn't bind its head (`builtin_variants`); `(use FilterMode)` isn't supported
- `Option` is a builtin enum, always registered. Defining a struct or enum with any builtin type's name (primitives, builtin structs/enums, type aliases — `Option`, `vec4f`, `f32`, …) is `BuiltinTypeRedefinition`, in every module (checked by the resolver; `PRIMITIVE_TYPE_NAMES` lists the names that aren't typedefs). Enum emission skips still-generic enums (`flat_data_size_in_u32s` panics on a generic payload)
- `match` works on numbers, bools, enums, and vectors (vector-literal patterns); anything else is `CantMatchOnType`

### Higher-Order Functions
- A function passed directly as an argument is inlined at compile time (`inline_all_higher_order_arguments`), creating specializations like `map_f32_make_two_of_f32`
- **Specializations are identified by origin, never by name**: each specialization's signature records a `SpecializationOrigin` (callee implementation, argument index, inlined function), and call sites reuse the registered signature with a matching origin. Names can collide (overload separation drops function-typed args from names). Each pass registers every surviving function before rewriting any body. The origin is on the *signature* because a pass write-locks the implementation it's rewriting
- A closure scope argument passed by reference that isn't already a place is bound to a `scope_arg` variable at the **nearest statement boundary** (`extract_non_bound_mutable_references`), so it's built exactly where the argument was — never hoisted above earlier statements or out of a loop/arm
- When HoF inlining appends a trailing scope argument to a call, the callee's function-type view gets a matching `MutableReference` param (every append site must do this; effect analysis and write-back read it)
- `extract_inner_functions` (a) stamps a captured variable's function-type `abstract_ancestor` onto the rewritten scope-field reference — ancestor propagation has no `Access` arm, so without it calls through captured closures are never specialized; (b) splits a nested lambda's capture of the enclosing *scope parameter* into the individual fields it reads — the lifts can rewrite `scope.field` but not a whole-scope capture

### First-class function values (`function_values.rs`)

Functions and closures (including stateful ones) can be stored in arrays/structs/enums/globals/`@var`s or chosen by `if`/`match`. Everything else in the compiler assumes a `Type::Function` has one static ancestor, so runtime-chosen values go through two passes:

- **`box_function_values`** (right after `rewrite_aliased_builtin_calls`) retypes every function value in a *dynamic* position as `Type::BoxedFunction`, opaque data to every later pass. Static values flowing in are wrapped in `$fnbox-make`; calls become `$fnbox-apply` (value passed by mutable reference); a static closure local passed to a boxed parameter is lent as `$fnbox-borrow` (lowered to the place itself for a singleton set, otherwise a union temporary copied back after the call, `lower_copy_back`). Functions taking/returning functions are stored through **boxed-parameter clones** (`boxed_parameter_clone`, keyed by `CloneKey`). Generic functions instantiated at function types box that generic at the call site. A strict no-op for programs without function values.
- **`defunctionalize_boxed_functions`** runs a union-find flow analysis over member sets per function *instance* (functions with boxed signatures are re-analyzed per call site, so sets are per-usage). Callees resolve to registered signatures by implementation identity (`resolve_in_registry`; a miss is a compiler bug). Each position lowers to: nothing (zero/one scopeless function), the closure's scope struct (one closure — a direct call, zero overhead), or a `FnUnion` enum with a generated `apply_*` dispatcher (two or more). Structs/enums holding function values are specialized per representation.
- **Lowering details**: a closure whose scope lowers to nothing gets a payload-less union variant (the dispatcher passes an empty scope); `remove_unitlike_values` also strips unitlike params from *registry* signatures (or reference monomorphization would drop a closure whose stripped scope still looked like a reference); a boxed param whose representation is unit is passed by value; a dispatcher called on a value that is itself a ref param takes it by reference; owned params a lowered body passes by reference get a `@var` copy (`make_referenced_params_addressable` — WGSL params aren't addressable; e.g. a by-value twin's scope lent to a `@ref` helper); a call through a provably-empty set lowers to `zero_value`; deexpressionification keeps a pure-place scrutinee holding function values in place (`is_pure_place`) so it can be aliased; the tree-walker represents a closure used as its scope struct by that scope (`closure_as_scope`).
- **Semantics**: calling a stored closure mutates it in place (scope by mutable reference). A closure that provably never mutates its scope, called from a place that may be GPU storage, goes through a by-value twin (`by_value_twin`). "Mutates its scope" is structural (`body_mutates_scope` / `scope_writes_in`): a direct write, or passing the scope to a call of a closure that itself mutates its scope. A `match` payload binding on a place scrutinee aliases the stored payload (`alias_payload_binding` → generated `call_through_payload` helpers).
- **Copying a stateful closure that stays in use is a compile error** (`CopiedStatefulClosure`, `validate_copied_stateful_closures`). The agreed semantics are that closures capture *variables* (every copy shares them) — not implemented yet; v1 rejects exactly the programs where copy and shared semantics would differ, so later work only accepts more programs. Stateful = the closure's own body writes a captured variable, or it captures a stateful value (calling a captured closure isn't itself a write). Copy sites: `let` values, `=` RHS, array literals, by-value args, captures, returns; passing by reference lends unless the callee copies the parameter out (escape summary). Places are tracked by **field path**, so `(= voices (push voices …))` on a captured `voices` (a scope field) ends the old value like it does for a local.
- **Other validation**: `validate_closure_state_aliasing` (`AliasedRefArgs` for overlapping mutating closures in one call), `validate_ref_captures`. The earlier capture check (`catch_duplicate_closures_capturing_mutable_variables`) doesn't treat a `$fnbox-apply` argument as a write — stateful members are judged later by the copy check. **Deliberately unsupported shapes** (clean errors, not bugs): `StoredFunctionToHigherOrderParameter`, `UnsupportedFunctionValueSignature` (a let-bound higher-order lambda can't be stored — its definition is shared by every use), `ClosureMutationThroughImmutableRef`, `RecursiveFunctionValue`, `DynamicFunctionValueNotAllowedHere` (host builtins need static functions; a match-over-members rewrite is the planned follow-up).
- **Invariants**: a lowered closure's scope param is declared `AbstractType::AbstractStruct`; implicit entry marking (`mark_implicit_entry_point`) marks by implementation identity, never by re-locking registry entries by name.
- **Open question (deferred)**: function-typed parameters act as an implicit `@var @ref` (lending) — the one exception to value semantics. An explicit annotation would be the principled alternative; treat the current behavior as unsettled.
- **Rejected designs — don't retry**: (1) one union enum per structural `Fn` signature (couples unrelated code, never zero-overhead, self-referential merges); (2) reifying before closure extraction (no closure identity yet; unhandled shapes miscompile silently) — hence box early, defunctionalize late; (3) snapshot semantics for stored closures (a stored oscillator would never advance).

### Storage-ref inlining (`monomorphize_reference_address_spaces`)
Passing a storage/uniform global **as a whole** (a bare name) to a `@ref`/`@var @ref` param is legal everywhere, GPU included: it produces a per-(function, global) variant with the global inlined in place of the param (removed from every arg list), body names rewritten with `is_globally_bound` propagated. Unconditional on every target. Accessor-chain refs (element/field of such a global) keep a pointer param — CPU/audio only (`PassedReferenceFromInvalidAddressSpace` on GPU); that pointer's ownership `Pointer(space, Mutable)` counts as a write for effects.

### `AbstractFunctionSignature`
- Has `Default` — use `..Default::default()`. Defaults: builtin with empty effect, not associative, no captured scope, no generics, no specialization origin
- Keep `implementation` explicit when it has a non-empty effect; keep `associative: true` where it applies

### Writing Easl Code
- Integer literals are ambiguous: use `0i`/`5u`. Floats need a decimal point (`5.`) or `f` suffix
- `(if c a b)` needs both branches; use `when` for side-effect-only conditionals
- `~x` is `(into x)`, an ordinary overloadable function. Builtin `into`/`length` overloads are pure aliases of their targets (rewritten right after inference, so no backend sees `into`); `~5u: f32`-style ascriptions pick an overload via `constrain_fn_by_return_type`, an `into` with no expected type is an ambiguity error, and the alias swap uses a shape-aware matcher (`abstract_type_shallow_matches`) since arity can't tell `array-length`'s sized and unsized signatures apart
- Many common words are builtins and can't be used as local names (`step`, `log`, …): `CantShadowTopLevelBinding`
- `print` formatting: `1u`, `1` (i32), `1.` (whole f32), `true`, and Strings quoted (`"foo"`, also inside arrays/structs). `(string x)` produces unquoted text; an unquoted `print-str` is planned. Matters for `.txt` goldens

### Entry points and annotations
- `@cpu` (run by the interpreter/VM), `@vertex`, `@fragment`, `@compute` (with optional `@{workgroup-size X [Y Z]}`), audio entries via `start-audio`
- `@{builtin vertex-index}` etc. bind WGSL builtins (some also exist as zero-arg helper functions); `@{location N}` binds vertex inputs / fragment outputs

### Context exclusivity (`validate_context_exclusivity`)
The single pass for "only usable in X code" rules. It computes each function's possible execution contexts (cpu/vertex/fragment/compute/audio) as a fixpoint over **call** edges (never function-value references), seeded from entry points plus host edges (`spawn-window`'s function runs on cpu, `start-audio`'s on audio). Following call edges rather than syntactic position is what keeps it correct when deexpressionification relocates argument evaluation out of `start-audio`. Rules: fragment-exclusive builtins/`discard` → fragment; CPU-exclusive builtins → cpu (dynamic-array constructors and `push`/`insert`/`remove`/`concat`/`reverse` are also allowed in audio — VM-native heap ops); builtin attribute lookups per stage; `audio-time`/`audio-input` → audio; accessor-chain storage refs → cpu/audio. Only builtin callees are matched, so each violation is reported once, at the call, with the call chain as secondary positions.

### Closure-based emission & `directly_user_written`
`compile_to_target` emits exactly the transitive call-graph closure of a root set: the target's entry points plus every directly-user-written function that is transitively valid for the target (`TopLevelFunction::is_valid_for_target`). Dangling references are structurally impossible. Contract: **a user-written function emits iff it could be called from the target's code**; compiler-generated functions emit only when something emitted needs them.
- For WGSL, `is_valid_for_target` also rejects a function that *reads* a GPU-space global with no WGSL binding layout (`Type::is_valid_wgsl_binding_layout`) — a storage-ref-inlined variant can have a clean signature and an un-emittable body
- `Program::type_makes_struct_cpu_only` decides which structs are skipped from emission (runtime-sized/String/`Video` fields, and closure types whose scope structs are CPU-only, recursively); functions whose signatures reference such structs must be skipped by the same predicate or the output dangles

⚠️ `TopLevelFunction.directly_user_written` is true only at parse time. Every pass that derives a *new* function must use `TopLevelFunction::derived_from` (clears the flag), never `.clone()`. Overload separation and reference-address-space monomorphization are rename-families and keep the flag (ref-mono drops the original function from the registry, so its function-space variant *is* the user's function).

### GPU-bound top-level variables
- An unannotated `(var name: type)` is GPU-shared `storage-write` with elided binding numbers — CPU, GPU, and audio thread see one variable. `@local` (WGSL `private`) is per-execution-context, never shared, and the only initializable space
- Address spaces: `uniform`, `storage` (read), `storage-write`, `local`, `handle` (textures). Terse `@storage-write`, positional `@[storage-write 0 2]`, or longhand `@{address storage-write group 0 binding 2}`. Unsized arrays are valid storage bindings
- **Sync obligations are usage-derived**: only vars some GPU entry actually touches (`gpu_used_globals`, plus all textures) become runtime bindings with uploads/readbacks. A user-declared unused one still appears in emitted WGSL; a compiler-generated one emits only if an *emitted* function references it (`wgsl_referenced_globals`, built from `emitted_function_closure` — not merely target-valid functions, which would pull in vars from never-emitted audio clones). Host-shareability (`bool`/`String` not allowed) is checked only for GPU-used vars
- Binding-number elision is all-or-nothing per var; `assign_elided_bindings` numbers them at the end of validation (no `Elided` survives). Compiler-created bindings are always elided. Explicit numbers are an interface contract the compiler never changes

### `@external` variables
Readable/writable by an embedding host through `ExternalVars`. Must be in a GPU space (so no initializer — the embedder seeds through the handle before running; the entry-start bootstrap adopts those seeds so pre-frame code and frame 0's dispatches see them). Errors: `@external @local`, on textures, on String-containing types.
- The handle must be created from the *same* validated `Program` the runner receives (`table_for_env` asserts the counts agree); creating it joins the `EXTERNAL` participant
- `Send + Sync`; each read/write call is its own boundary (reads adopt-then-copy, writes publish); index writes are read-modify-write on the whole variable
- The `_raw` methods speak flat VM-layout words — a documented public contract

### Windowing builtins
`(spawn-window (fn [] ...))` (per-frame callback), `(dispatch-render-shaders vert frag count)`, `(dispatch-compute-shader f (vec3u x y z))`, `(close-window)`. GPU work runs in **program order** within a frame: compute, render-to-texture, and screen draws see each other's writes, and each sees exactly the CPU writes made before it (pinned by the `screen_draw_*` buffer tests).

### Window-info queries
`window-resolution`, `window-time`, `mouse-*`, `key-down?`, etc. work in CPU and GPU code and always read a **per-frame snapshot**: `extract_gpu_window_info` rewrites every query into a read of an implicit uniform binding (bools as `u32`), refreshed at frame start. The rewrite is unconditional — CPU uses too — **by design**: every query in a frame sees one value, and whether some other call site dispatches a helper to the GPU never changes what its CPU calls observe. Key queries with literal strings get bindings; runtime-computed key strings stay CPU-only live queries.

### Textures
A texture value is a `TextureValue`: a counted reference to a GPU texture (id, size, and how it starts — `TextureInit`: decoded pixels, blank, or a copy of another texture — consumed when the GPU creates it). Pixels live only on the GPU — there's no CPU copy to keep in sync, so textures carry no sync state, and `save-png` reads back through `read_texture` (which sets up the GPU first: queued writes may be pending). The GPU's own references — a binding (`GpuCore::slot_textures`), a queued upload (`BufferUpload::Texture`), a pending copy's source (`TextureInit::CopyOf`), a queued `WindowEvent::WriteTexture` — are uncounted `TextureHandle`s: GPU work queued earlier can never observe a later change, so they never need to block one. `GpuCore` stores textures by id (`texture_store`, freed by `prune_textures` once no reference is left; `textures_created` counts creations, for tests).
- **Value semantics by refcount** (`TextureValue::is_unique`: exactly one value holds it), like the VM's heap values. Assigning a texture shares it. Rendering into a texture other values hold first gives the target a `CopyOf` texture (`prepare_render_target`, shared by both runtimes' draw recording). Assigning new pixels (`load-image`, video frames) of the same size over a texture no other value holds rewrites it in place as a queued `WriteTexture` (`assign_texture`; an IO manager that can't queue it — `IOManager::record_texture_write`'s default — gets a new texture instead). Variables, parameters, and temporaries all count as holders; in the tree-walker, transient Rust clones can only over-count (an unneeded copy, never a missed one)
- An unassigned texture var holds its own blank 1×1 texture. `set-render-target` names a texture variable (resolved by name in both runtimes). The VM holds textures only in globals (host-side values): a texture parameter or any other non-global texture value panics there; the tree-walker handles them
- Pinned by `texture_value_semantics`, `texture_in_place_writes`, `texture_parameter_holder` (tree-walker), and `texture_reassigned_each_frame` (one texture created across 20 frames)
- ⚠️ **Invariant the copy-on-write relies on: every change to a texture's contents happens in GPU program order.** A `CopyOf` handle is realized when its draw's uploads run, not when it's recorded, so it's correct only because the one way to change contents (rendering) is queued in the same order. Any future CPU-side texture write (e.g. a `set-pixel`, or pixel-array → texture conversion writing into an existing texture) must go through the queued GPU work too — e.g. as an upload attached to the next dispatch, applied in order — and must itself do copy-on-write when another variable shares the texture. Writing to the GPU texture immediately would leak into a pending copy recorded earlier (a copy made after the write it should precede). Replacing a variable's whole texture (assignment) is safe: it swaps handles and never touches contents.

### Samplers
`Sampler` vars live in the `Handle` space like textures and are host-side on the CPU (`Value::Sampler(SamplerSettings)`, VM `dynamic_globals`). The `(Sampler filter address)` constructor is CPU-exclusive and only assigned to a `Sampler` var (VM: `HostOp::AssignSampler`; `CopyHostGlobal` copies one host-side global into another); an unassigned one holds `SamplerSettings::default()` (nearest, clamp-to-edge). They upload as `BufferUpload::Sampler`, and `GpuCore` keeps one `wgpu::Sampler` per `GpuBufferKind::Sampler` binding, recreated (with its bind groups) when the settings change. Pinned by `sampler_modes` and `sampler_usage` (expected values avoid GPU-dependent filtering precision: linear samples sit exactly between texels).

### Video input (`video` feature)
`Video` is a builtin **value** struct (`_source`, `_frame`, `_length` u32s) with no constructor — produced only by `load-video`. Opacity is by convention (the `_` fields are technically accessible — a documented trade-off). The heavy decoder lives host-side in `VideoRegistry` as a transparent cache; decoding is a pure function of (source, frame). The frame count is exact: `open` decodes the video once, counting frames with `-fps_mode passthrough` like the decoder (so neither duplicates nor drops frames), and a decoder that runs out before a counted frame is an error. Scrub/query ops are pure slot arithmetic; `load-video` and `get-video-frame-texture` are host ops (the latter only as the RHS of assignment to a texture global). All ops are CPU-exclusive; any type embedding a `Video` is CPU-only for emission (`involves_video`).

## Interpreter & Window System

### `IOManager` implementations
- `StdoutIO` — real winit window and audio. Its `(sample-rate)` queries the device with the same config selection the stream builder uses (`preferred_output_device_and_config`)
- `StringIO` — tests; simulates N frames (default 10), records `IOEvent`s
- `CaptureIO` — wraps `StdoutIO` (real GPU, headless) and captures prints; used by most test suites. Its `start-audio` **never opens an output stream** (tests must be silent); audio is tested by driving `VmAudioDriver` directly
- `midi_state`, `start_audio_input`, `listenable_sources` default to silence/no-op/empty; only `StdoutIO` touches real devices. `listenable-sources` returns names sorted (indices stable within a session)

**Test spoofing hooks**: `SpoofedWindowInfo` on `CaptureIO` (time, resolution, mouse, keys — kept off `StdoutIO` so production accessors stay branch-free); `spoofed_midi` on `StringIO`/`CaptureIO`/`ThreadSyncIO`; `VmAudioDriver::midi_override` and `audio_input_override` (a `VecDeque<f32>`) on the audio side. Pinned sample rates: `StringIO`/`ThreadSyncIO` 8 Hz, `CaptureIO` 44100.

### Key types (`interpreter.rs`)
- `WindowEvent` (`RenderShaders`/`ComputeShader`, referencing entries by dense `u16` id, with `pre_upload`s), `IOEvent` (the `StringIO` log), `BufferUpload::{Data, Clear}`
- `EvaluationEnvironment` — `binding_vars` for GPU-bound globals; `binding_infos()` (incl. per-stage usage, used for bind-group visibility — Metal caps the vertex stage at 16 buffers)
- `Value::Texture(TextureHandle)` — a handle to a GPU texture (see "Textures")
- `Value::ZeroedArray` — lazily zeroed array (uploads as `BufferUpload::Clear`); the VM equivalent is `DynMemory::Zeroed`
- Buffer sizes come from `flat_data_size_in_u32s`, never from serializing a value (an `Uninitialized` value serializes to 0 bytes). `Value::zeroed()` errors on unsized arrays — use `.unwrap_or(Value::Uninitialized)`
- `GpuBufferKind`: `Uniform` / `StorageReadOnly` / `StorageReadWrite` map from the corresponding address spaces
- Known limitation: `collect_dirty_uploads` uploads every dirty binding each frame, including large GPU-written buffers the CPU never touches (a finer dirty-flag scheme is planned)

### Interpreter notes
- **Closures**: extraction turns a capturing lambda into a top-level function whose last param is a scope struct; the construction site becomes an application of the scope struct's constructor (callee ancestor = `StructConstructor`, expression type = a function type). Backends recognize scope constructions positively by that pair — every lowered callee must carry an ancestor. The tree-walker's `Function::Scoped { inner, scope }` binds the bare scope struct to a scope parameter and writes mutations back (`write_back_through_lhs`); write-back ownerships come from the implementation's own function type
- **GPU-dispatched closures**: `extract_dispatched_closure_scopes` lifts **each capture to its own implicit read-only storage global** (`<scope>_data_<capture>`, names recorded in `Program::lifted_gpu_captures` — never re-derived by string convention), so runtime-sized captures work. Captured closures recurse through **clone families** (`cloneify_captured_closure_family`, shared with the audio lift via `ClosureLiftTarget`): the original *and* its HoF specializations are cloned against the lifted globals, call sites repointed by actual callee name. A captured closure passed onward as a HoF argument works through receiver clones (`cloneify_closure_receiver`). `check_dispatched_closure_scope_mutations` recurses into a function-typed argument's closure body rather than treating the pass as a mutation (the `MutableReference` on HoF closure params is a CPU write-back convention). Storing a captured closure in a data structure is `CapturedClosureUsedAsValue`. At dispatch, each runtime writes captured values into the bindings (tree-walker `write_scope_capture_bindings`, VM `emit_scope_capture_writes`), skipping captures no shader reads (they have no binding)
- `env.structs` is keyed by **base** struct name, not monomorphized name
- To get a dispatched function's source name, read `abstract_ancestor` off its function type

### ⚠️ Synchronous GPU↔CPU semantics — DO NOT BREAK
**Hard language requirement**: a program can write a variable on the GPU (`dispatch-compute-shader`) and immediately read it on the CPU in the same frame, with no explicit sync, and CPU writes are visible to later dispatches.

CPU writes are marked **where they happen**: an application marks only the globals it writes itself, through its mutable-reference arguments (`TypedExp::argument_writes`), never its callee's whole static write set — that would make a callee's CPU copy authoritative again after GPU work the callee dispatched later (pinned by `cpu_write_in_callee_keeps_gpu_writes`). A write to *part* of a GPU-bound global (an element or field, including an element `@var @ref`'s write-back at return) is a read-modify-write, so it syncs the global right before it lands (`partially_written_globals`; tree-walker `eval_assignment_op`/`write_back_through_lhs`; pinned by `ref_element_write_back_keeps_gpu_writes`).

How: a sync check guards every CPU read of a GPU-bound global, **at the read itself** — never for a call's whole static read set, which would block on the GPU for reads on untaken branches (pinned by the `untaken_branch_read_no_readback` sync test). Tree-walker: the `Name` arm. VM: `Op::CheckGpuRead` (`emit_read_check`), a flag check that reaches the host only when `Op::MarkGpuNewer` set it after a dispatch; a clean check costs ~3.5 ns, and a planned bytecode pass will drop provably redundant ones. An element passed by `@ref` is read (and synced) as the argument is evaluated — the callee holds a snapshot. ⚠️ Any VM path that reads a global without compiling its `Name` (e.g. `PrintBinding`) must emit the check itself. `check_cpu_readable` calls `io.flush_queued_compute()` when needed, which runs all queued GPU work in program order via `GpuCore::execute_frame_gpu_work`, blocks, then reads back. `array-length` never syncs (lengths are CPU-authoritative).

Must not change:
- `StdoutIO::flush_queued_compute` executes synchronously (one batched submit + blocking poll)
- `CaptureIO::run_spawn_window` executes frames through the *same* `render_frame` (`execute_frame_gpu_work` + `finish_frame`) as the real loop — no parallel test-only frame path
- `check_cpu_readable` flushes before reading back
- Compute must stay flushable mid-frame; don't collapse frame work into one deferred submit

### Tree-walker stack usage
Tests run on 2MB stacks and debug builds reserve stack for every match arm at once. Keep `eval` a thin dispatcher (substantial arms live in helpers like `eval_application`, `eval_let`, …) and keep `Value`/`EvalException` small (56 bytes each, `Result<Value, EvalException>` 64 — box large payloads). Adding locals or arms to `eval`/`apply_builtin_fn` can overflow deeply recursive tests; extract helpers instead. Measure a frame with the prologue's `sub sp` in `objdump -d --disassemble-symbols=<symbol>` on a test binary.

### `window.rs`
- One shader module; bind group layouts/bind groups/pipeline layouts are **per pipeline**, covering only the bindings its entries use (with per-stage visibility). Buffers are global. Limits are checked per pipeline (`validate_pipeline_bindings`) and program-wide (`validate_binding_limits`); `install_gpu_error_handler` turns uncaptured wgpu errors into easl-framed panics
- Dispatch events reference entries by dense id: compute pipelines are a `Vec` by entry id, render pipelines a small linear-scanned vec keyed by `(vert_id, frag_id, additive, format)` — no string work in the frame loop
- `upload_bindings` recreates a buffer when its size changes and rebuilds only the bind groups referencing it
- The frame path is two shared methods used by the winit loop, the headless test loop, the web runtime, and mid-frame flushes: `execute_frame_gpu_work` (all queued work in program order, batching runs; the compute encoder splits a submit when an upload would overwrite a binding an encoded dispatch uses, and after `MAX_PASSES_PER_SUBMIT` passes — wgpu records a pass as several backend command buffers, and Metal treats more than 4096 created-but-unsubmitted ones as a lost device, which made every later resource silently invalid; a draw with uploads starts a new render run, and texture draws always carry their target's bind, so render runs stay short) and `finish_frame` (presents)
- **Screen draws** (`ScreenTarget`): the frame's first screen pass opens the frame's screen image (`begin_screen_pass`) and clears it; later passes, including ones after a mid-frame flush, load it; `finish_frame` presents. `Surface` (native windows) draws straight into the surface image, so vsync's block moves to the frame's first screen draw — no intermediate texture, by design (a full-screen copy costs ~0.2–0.4 ms/frame at 4K). When the surface has no image (occluded/lost), and under `StandIn` (headless), draws run into a 1×1 stand-in so their storage writes still happen. `Texture` draws into a window-sized texture copied to the surface at frame end (the web; and `CaptureIO::screen_size`, whose pixels tests read back). `Embedder` (`new_from_parts`) skips screen draws — easl-studio renders them itself with `execute_render_batch_to_view`. A surface is added with `attach_surface`; `resize_surface`/`reconfigure_surface` drop a held frame image first (configure panics while one is live). The frame that calls `close-window` isn't rendered
- ⚠️ **No frame loop may run unpaced.** The winit loop's only implicit throttle is vsync, which exists only on presented frames. On unpresented frames (occluded window, compute-only programs) `RedrawRequested` applies backpressure (`wait_idle`) and cadence (`ControlFlow::WaitUntil` at the display rate, with the next redraw requested from `new_events(ResumeTimeReached)` — never at schedule time, which defeats the wait on macOS). Without both, the loop floods the Metal queue and can crash the system. Background frames keep running at full cadence (they may drive audio)
- winit's `EventLoop` is reused across `spawn-window` calls via `run_app_on_demand`
- Embedder API: `GpuCore::new_from_parts` (runs the program-wide binding pre-flight but skips the Metal vertex rule since the backend is unknown, and deliberately installs **no** error handler — the embedder owns its device), `create_headless_gpu_core`, `GpuCore::execute_render_batch_to_view`

## Bytecode VM

A register-style bytecode interpreter; the **default runtime for `@cpu` code** and the audio-thread runtime. The tree-walker remains a supported reference implementation (`CpuRuntime::TreeWalking`).

### Layout
- `bytecode.rs`: `BytecodeProgram { code, stack: Vec<u32>, call_stack, dyn_memory, heap, shared_* }`. Instructions are `{ op, arg_positions: [u16; 3], return_position }` with **absolute** stack positions. One dispatch loop; heavy `unsafe` indexing — correctness relies on the compiler emitting in-bounds positions
- `compile.rs`: `BytecodeCompilationState` and its `emit_*` helpers, `compile_builtin`, `TypedExp::compile_to_bytecode`. Functions are compiled callees-first; each gets a fixed disjoint stack region (bump allocation, no reuse yet)

### Running a function
```rust
let (mut program, names) = validated_program.compile_to_bytecode_program();
let f = names.iter().position(|n| &**n == "f").unwrap();
// args go at program.code.functions[f].first_arg_position (after the return slot);
// Function::arg_words says how many words a function takes (the audio driver uses it to pass `t` or not)
program.prepare_to_run_function(f);
program.execute();
let result = f32::from_bits(program.stack[program.get_function_return_position(f) as usize]);
```

### Invariants
- **Static absolute addressing** — valid because easl has no recursion
- **Every function's return slot is reserved separately from args and temps**: heap-id slots release on overwrite, so a scalar written over one would be misread as a heap id. Args start at `first_arg_position`
- **`$init_globals`** (synthetic, unspeakable name) runs initializers once in `from_code`
- **Global slot locations are public** (`Code::globals`, `get_global_slot`, `write_global`) — embedders stream values in between runs
- **Only touched globals get slots**: a user-declared var that no compiled (reachable) function names — nor the initializer of a var that has slots — and that isn't shared or `@external` gets no VM slots (e.g. a large buffer only shaders use); a GPU binding of one gets a host binding with `HostBindingStorage::Dynamic`. Compiler-generated vars always get slots: the runtime writes window-info, MIDI, audio-info, and capture vars into them. Slot sizes are checked (`vm_stack_size` errors past 65535 words rather than truncating), and running out names the global or function (pinned by `gpu_only_global_no_vm_slots`)
- Matrices are flat `cols*rows` scalars
- Audio/CPU filtering mirrors emission: skip entries not for the target and functions with CPU-exclusive effects/types, window queries, or `Print` (except audio-allowed heap ops). Effects are transitive and callees come first, so skips never dangle
- Vector/matrix ops are scalar fan-out, not new opcodes
- **Audio-mode `Code` is serializable** (`Code::to_bytes`/`from_bytes`; serde + postcard) — the web runtime hands it to its audio worklet. Shared vars carry a `ValueLayout` (word sizes + heap-id positions) instead of a compiler `Type`; CPU-mode host tables are skipped and `to_bytes` asserts they're empty
- Heap-element global regions start as empty `Cells` (`Code::cell_regions`): an audio replica's region is never seeded before its code runs, and a `Zeroed` start would make `push` build flat words around an unowned child id

### Emit helpers and adding builtins
Prefer `emit_unary`/`_binary`/`_ternary`, `emit_fanout_*`, `emit_elementwise_*`, `emit_dot`/`emit_mat_mul`/…, `emit_u32_constant`/`emit_f32_constant` over raw `push_instruction`. `vec_kind`/`mat_kind` identify vector/matrix types. New scalar builtin: add the `Op` variant, its `execute` arm (use the `f32_binary` etc. helpers), a custom `max_touched_index` arm if its operands aren't all slots, and a `compile_builtin` arm dispatching on **operand** types.

### Notable mechanisms and gaps
- **Reference args are zero-overhead**: a function with any non-owned param is compiled once *per call site* with its ref params bound directly to the caller's storage (`RefArgBinding::Slot` / `DynRegion`). Placeholder copies are emitted only for owned args. Detect refs from signature-level ownership, not `arg_annotations`. Aliasing is a compile error (`validate_ref_arg_aliasing`); capturing a `@var @ref` param is an error, a bare `@ref` capture may not escape (`validate_ref_captures`)
- **Every `start-audio` execution is a fresh handoff**: it re-seeds the closure's lifted captures. Calling it unconditionally every frame with a closure resets state every frame — Ella's explicit call, with no guard machinery compromising the semantics (call it once, or behind a condition; an LSP lint for every-frame calls is planned). Don't add a "seed only once" guard
- Known gaps: no optimization passes (no slot reuse, redundant moves); no construction-time bounds check; nested `spawn-window` is rejected; texture sampling and atomics are shader-only; bounded known leaks (ref-fn placeholder groups skip destination hygiene; element release for containers embedded as *fields* of overwritten aggregates isn't walked — both belong to the planned last-use analysis)

### VM CPU runtime
- One `Op::HostCall` opcode with cold metadata tables on `Code` (`host_ops`, `host_types`, `host_strings`, `host_bindings`, `host_dispatches`) handles all CPU orchestration; the audio path pays nothing
- `compile_to_bytecode_program_cpu` lowers CPU builtins to host ops and emits sync instructions (`CheckGpuRead`, `MarkCpuWritten`) mirroring the tree-walker
- `VmCpuRuntime` wraps a real `EvaluationEnvironment`, reusing its sync, upload, print, and audio machinery. Fixed-size GPU globals live in VM slots, mirrored into the env lazily; runtime-sized globals live in `dyn_memory` regions with direct `Dyn*` opcodes. `Value`s are built only at boundaries (print, upload, readback) via `Value::from_vm_words`/`to_vm_words` and the heap-aware `value_from_vm_words_heap`
- `spawn-window`/`close-window` suspend the VM; a scoped frame closure's scope persists in slots across frames. The stepped API (`start`/`run_frame`/`complete_readback`) is also what the web runtime drives
- Closures are represented as their scope data; closures reachable only through scope constructions are found by discovery (`composite_functions_in_usage_order_with_discovery`)

### First-class runtime-sized arrays
Runtime-sized arrays are CPU values with **value semantics everywhere** (locals, args, returns, fields, payloads, nesting). `push`/`insert`/`remove`/`concat`/`reverse` return fresh arrays (`(= arr (push arr x))` is the idiom) and are audio-callable.
- A dyn value is one stack word: a heap id (`index + 1`, 0 = empty) into `BytecodeProgram.heap`'s `Arc<HeapCell>`s. Copies are `Arc::clone`; mutation is copy-on-write. Reclamation is drop-on-overwrite only (no scope-exit drops yet)
- Containers whose elements are heap values store owned child `Arc`s (`DynMemory::Cells`). Containers whose elements *embed* heap ids (`[Packet]` with a `[f32]` field) keep flat words that own their ids, maintained by compile-time-emitted fixups (`emit_reown_container_elements`, `emit_ensure_unique_cell` guarded by `Op::HeapUnique`) — no runtime layout metadata
- Every copy that moves embedded heap ids goes through `emit_value_copy` / `HeapCopyPlan` (release old ids, move, promote new ones; enum payloads dispatch on the discriminant). Copy plans come from expression types, not call-site signature views
- **Element ref args have strict left-to-right snapshot semantics** (a soundness contract): a mutable-position element lookup (`@ref` element arg, compound-assignment target) is snapshotted when evaluated, and its temp takes *owned* shares of the element's heap ids — a later argument could mutate the container and free a borrowed id. The write-back (`emit_owned_element_write_back`; `PendingWriteBack::Embedding`/`FixedArray`/`ReleaseTemp`) runs ensure-unique *after* all argument evaluation
- Element mutation through nested chains propagates `CompilePosition::{Value, Ref, MutRef}` so each level registers a write-back. Immutable `@ref` element args never write back
- GPU boundary: GPU-reachable code may not take, return, or locally bind runtime-sized values or values containing them (`validate_gpu_runtime_sized_use`); a binding may involve one only by *being* one

### Runtime strings
String literals: the parser's string context has `\` as its escape character (so `\"` doesn't end a literal) and leaves escapes in the leaf's raw text; `parse::unescape_string_literal` gives the value (`\n`, `\t`, `\r`, `\"`, `\\`, and `\`-newline, which drops the newline and following whitespace; anything else is `InvalidStringEscape`), and `escape_string_literal` is its inverse — use it for any literal a pass synthesizes (the resolver's `current-directory` expansion does). The formatter prints a literal's raw text verbatim (`Block::StringLiteral`) except the whitespace after `\`-newline, which it re-indents to line up after the opening quote. A `String` is one heap id whose cell holds one codepoint per word. API: `(string x)` (unquoted display text), `concat` (n-ary), `length` (chars), `substr` (char indices, exclusive end, clamping), `==`/`!=`. String-typed expressions carry `CPUExclusiveType("String")`, keeping them off GPU/C/audio. Strings never cross thread or GPU boundaries.

## Cross-thread shared variables (`thread_sync.rs`, `vm/shared_sync.rs`)

**Boundary-batched replicated coherence**: each thread (main, audio, embedder) holds its own replica of each shared global; writes set a dirty flag; at each iteration boundary (a frame on main, a callback batch on audio) a thread publishes snapshots of what it wrote and adopts newer ones. Reads within an iteration are stable; concurrent writers resolve last-writer-wins per variable, by version: `SharedVarSlot::publish` installs a snapshot only if no newer version is installed (a lock-free conditional swap), so a publisher that loses a race is dropped and adopts the winner at its next boundary — replicas always converge (pinned by `older_version_never_replaces_newer`, a 200k-round two-thread stress test, and `older_publish_is_dropped`).
- **The shared set is static** (`Program::thread_shared_globals`): vars touchable from both main and audio code (reachability cuts at `start-audio`'s argument; GPU dispatches count as main), plus every `@external`. `@local` is never shared. Seed writes and length reads count as touches. The sorted list index-aligns every compiled artifact, the env, `ExternalVars`, and the table. Each var has an audience bitmask (`MAIN`/`AUDIO`/`EXTERNAL`)
- A participant publishes only vars whose audience includes another *live* participant, and adopts only vars in its own audience. A forced (bootstrap) publish covers only vars in the **publisher's own audience** and only never-published slots (`SharedVarSlot::has_published`) — never clobber another participant's state. Publishing records the publisher's own adopted version
- **The `start-audio` bootstrap consumes dirty flags**: a var written earlier in the same frame isn't re-published at that frame's end
- The entry-start bootstrap (`bootstrap_external_globals` / `vm_bootstrap_external`) *adopts* embedder pre-seeds before the `@cpu` entry body runs
- Dirty marking: tree-walker `mark_cpu_written`; VM `Op::MarkSharedDirty` after writes (both modes)
- Snapshots are VM words (`Slots` storage size is the `vm_stack_size` count, not the GPU size); heap-involving vars use a serialized **wire encoding** (count-prefixed contents, ids dereferenced on publish and freshly minted on adopt — no id ever crosses heaps), walked via `ValueLayout`. The tree-walker's byte-identical siblings are `value_to_shared_words`/`shared_words_to_value` and `encode_value_wire`/`decode_value_wire`; an `Uninitialized` unsized array publishes as empty words, matching the VM — traces must be runtime-identical
- **The GPU is a participant proxied by main**: adopting a GPU-bound var marks its buffer `GPUOutOfDate` directly (not via `mark_cpu_written`, which would ping-pong); at each frame-end publish and `start-audio` bootstrap, GPU-newer shared bindings are read back and published
- Trace hooks (`record_shared_publish`/`record_shared_adopt`, `FnMut(u16)` params) are no-ops in production and drive the thread-sync goldens

## Audio runtime (`audio.rs`)

`(start-audio f)` runs `f` once per sample on the audio thread. Entries take `()` or `(t: f32)`.
- **Audio-info functions** compile to reads of reserved `@local` vars (`easl_audio_time`, `easl_sample_rate`, `easl_audio_input`; `extract_audio_info`): `(sample-rate)` anywhere (main side seeded from `IOManager::sample_rate()`), `(audio-time)` and `(audio-input)` audio-only (`AudioInfoOutsideAudio`). The names are fixed (the C wrapper relies on them) and reserved (`EaslReservedName`)
- **Audio input**: `(start-listening)`, `(start-listening-from name)`, `(listenable-sources)`; a process-global capture ring drained per batch; mono, no resampling
- **MIDI** (`(down-midi-notes)`, `(midi-cc i)`, `(midi-aftertouch)`, `(midi-pitch-bend)`, `(get-midi-note n)`): rewritten into reads of reserved `easl_midi_*` storage-read vars, refreshed per frame on main and per batch on audio from a process-global listener snapshot. Usable in cpu, audio, and shader code. These vars are **excluded from thread sharing** by their name prefix: sharing would overwrite the audio replica's fresher per-batch values with main's per-frame ones, and exclusion keeps windowless audio programs MIDI-live. The tree-walker and VM keep **separate** generation caches (env construction refreshes the tree-walker bindings unconditionally; a shared cache would mark the VM's never-written slots fresh). The `Option<MidiNote>` table layout (discriminant 0 = `Some`, 1 = `None`) must match across both runtimes and the GPU (`MidiState::note_table_words` / `note_table_value`)
- **Backends**: `AudioBackend::VM` (default; bytecode per sample) and `AudioBackend::C` (`c_audio` feature; clang-compiled dylib). The C backend is accepted as unsupported for closure entries, heap-involving shared vars, MIDI refresh, and audio input — the planned bytecode→C backend supersedes it
- **Stateful closure entries** (`(start-audio (create-osc 440.))`): `extract_audio_closure_scopes` lifts each capture to a storage-write global named by `NameContext::gensym` and recorded in `Program::lifted_audio_captures` (`LiftedCaptures`, per capture *path*, so multiple instances of one constructor get separate state). Captured closures recurse through clone families; the entry gets a scope-less clone named `{original}_audio`. Lifted captures share through the normal rules: the seed writes are `Effect::SeedsGlobalVar`, which is invisible to `read_and_written_globals`, so the `start-audio` builtin's own marking is the single source of truth for them. Main seeds them on every `start-audio` execution. String-containing captures are rejected (`UnshareableAudioCapture`) because audio-mode compilation filters String-using functions; supporting them means splitting that filter by operation (`(string x)` is a host op, the rest are VM-native). Audio clones never emit to WGSL
- **`start-audio` handling**: repeated calls are fine (same entry: no-op; different entry: live switch). The first call bootstraps the shared table and hands the source to the IO manager; later reloads hot-swap the whole driver (`VmSwapMessage` mailbox, never blocking the real-time thread). The audio source is compiled from a clone of the already-validated program — don't re-validate
- **WAV builtins** (CPU-exclusive, file I/O every call): `load-wav` → `[f32]`, `load-wav-raw` → `[i32]`, `get-wav-sample-rate`, `save-wav` (16-bit PCM, `FileWrite`)

## Web runtime (`web/`)

A separate `easl_web` crate (wasm32) that embeds the compiler and the VM and runs any program: sources are compiled in the browser (`load_easl_program_from_sources`), rendering through WebGPU into a canvas. `web_bundle::bundle_program` produces `index.html` + `easl-program.js`; `easl compile --web` (easl_cli) writes them with the runtime files and `RUNTIME_SUPPORT_FILES`.
- Frames come from `requestAnimationFrame` via the stepped `VmCpuRuntime` API. A browser can't block on the GPU, so `CheckGpuToCpu` suspends the VM (`HostSuspendReason::GpuReadback`); the runtime flushes, awaits the readback, and resumes. Textures never suspend. Screen draws go to a `ScreenTarget::Texture` copied into the canvas at frame end: yielding for a readback presents whatever the canvas holds, so drawing into it directly would show partial frames (the canvas is configured with `COPY_DST`). The web suite checks `.screen.txt` goldens' last frame against the canvas
- Programs whose vertex shaders use storage-write vars are rejected up front (no `VERTEX_WRITABLE_STORAGE` in browsers)
- **Audio** runs in an AudioWorklet with a second runtime instance. The page sends its compiled audio `Code` serialized — one compilation. Recompiling in the worklet is a rejected design: generated names can differ between instances (random hash seeds). Shared variables cross by index over `postMessage` using the native boundary machinery. The AudioContext is created at start and resumed on the first user gesture
- **MIDI** comes from Web MIDI through `sendMidiMessage` → `easl::midi::handle_message`, forwarded to the worklet
- Gaps: audio input, file I/O builtins, `@external`

## Test Structure

All suites run with `--features window`. Most CPU-side suites run every test on **both** CPU runtimes and require identical output. Suites: `shader_tests`, `cpu_tests`, `buffer_tests`, `window_tests`, `conformance_tests`, `vm_tests`, `sync_tests`, `audio_tests`, `thread_sync_tests`, `web_tests`, `full_tests`, `c_tests`, `import_tests`, `format_tests`, `video_tests`.

- **Every suite that runs `.easl` programs also checks their WGSL**: after validating a program, the cpu, buffer, vm, sync, thread-sync, audio, window, and video harnesses call `common::assert_valid_wgsl` (`tests/common/mod.rs`), which emits it to WGSL and validates the output with naga. Every GPU-valid user function is emitted whether or not anything runs it on the GPU, so this is what catches WGSL emission bugs in CPU-oriented code. New harnesses should call it too
- **Shader** (`data/gpu/`): `success_test!(name)` validates emitted WGSL with naga (written to `out/` for inspection); `error_test!(name, CompileErrorKind::X(...))` asserts the exact error set with `PartialEq` — payloads must match
- **CPU** (`data/cpu/`): `cpu_test!(name)` compares printed output to `name.txt`. Files under `data/cpu/modules/` are libraries imported by the `module_*` tests
- **Import** (`data/import/<name>/main.easl` + its imported files): `import_test!` compiles to WGSL and naga-validates; `import_error_test!(name, errors…)` asserts the exact (deduplicated) error set
- **Buffer** (`data/buffer/`): real GPU dispatch round trips, compared to `.txt`. `screen_test!` programs render to a 1×1 screen texture and compare against `.screen.txt`: prints, then `frame <i>: r g b a` per frame
- **Window** (`data/window/`): `StringIO` event logs — lines `spawn-window`, `print: <msg>`, `dispatch-render-shaders <vert> <frag> <count>`, `dispatch-compute-shader <entry> (vec3u Xu Yu Zu)`
- **Conformance** (`data/conformance/`): each file defines `f(): f32` (the harness injects the CPU/GPU boilerplate via `load_easl_program_from_file_with_lookup_function`); the interpreter+GPU, C (via clang — the slow part, ~0.5–1 s per test; while iterating on the VM you can temporarily wrap the C section in `if false {…}`, restoring it before committing), and VM must **agree** (optionally within a tolerance). It checks agreement, not correctness — pin correctness elsewhere too
- **VM** (`data/vm/`): runs `f` on the VM, compares to the single float in `.txt` within `0.0001`
- **Sync** (`data/sync/`): golden traces of implicit transfers — lines `upload: <var>`, `readback: <var>`, `print: <text>`. A spurious readback is a performance bug. Names containing `_scope_data` normalize to `<closure-scope>` (one upload line per *leaf* capture per dispatch)
- **Audio** (`data/audio/`): the only suite exercising eager audio-source compilation; also direct `VmAudioDriver` tests
- **Thread-sync** (`data/thread_sync/`): `thread_sync_test!(name, [Frame, AudioBatch(n), …])` drives real frame code (incl. real GPU dispatches through `execute_frame_gpu_work`) and real audio batches on one thread with a scripted schedule; `ExternalWrite`/`ExternalWriteIndex`/`ExternalRead` steps drive an `ExternalVars` handle (steps before the first `Frame`/`AudioBatch` run before the program starts). Golden lines: `frame <i>`, `audio-batch <i> x<n>`, `main-publish:`/`main-adopt:`/`audio-publish:`/`audio-adopt: <var>`, `upload:`/`readback: <var>`, `dispatch-*`, `print:`, `samples: <s0> <s1> …`, `spawn-window`, `start-audio: <entry>`, `close-window`. Names normalize: `_scope_audio_data_` → `<audio-scope>_<capture>`, `start-audio: …_audio` → `<audio-closure>`
- **Web** (`data/web/`): custom harness driving headless Chrome (muted) over DevTools. Needs Chrome with WebGPU and the `wasm32-unknown-unknown` target; **fails loudly, never skips** when missing (`EASL_TEST_CHROME` overrides the binary). One browser *window* per test (tabs sharing a window get no animation frames). A `<test>.error` file holds text the program's rejection must contain; `SCRIPTS` (`AwaitPrint`, key/mouse events, `Midi`) drive input. Parity mode runs every cpu/buffer program with a `.txt` (`PARITY_SKIPS` lists exceptions with reasons). Takes a substring filter: `cargo test --features window --test web_tests web/`
- **Full** (`data/full/`): real window via `StdoutIO`
- **Video** (`data/video/`, gated `#![cfg(feature = "video")]`): `.mp4` fixtures, both runtimes
- `#_` comments out the next form in `.easl` files

Practices: pin required semantics with a test that fails before a fix; prefer one fixture covering several shapes; for nondeterministic compiler bugs, compile on many fresh threads (each gets new hash seeds).

## Style Notes
- Rust 2024 edition; `let` chains are used freely
- `take_mut::take` for in-place mutation of `&mut self`; `Rc<RefCell<…>>`/`Arc<RwLock<…>>` for shared AST state
- `ExpTypeInfo` derefs to `TypeState`; `unwrap_known()` clones the `Type` (panics if unknown)
- Imports go at the file header (no inline fully-qualified paths); comments describe current behavior, not history
