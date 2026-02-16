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
- `src/interpreter.rs` — CPU-side interpreter (WIP)
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
  - `effects.rs` — effect types (fragment-exclusive functions, print, etc.)
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

## Test Structure

GPU tests live in `tests/shader_tests.rs` with source files in `data/gpu/`. CPU interpreter tests live in `tests/cpu_tests.rs` with source and expected-output files in `data/cpu/`.

```rust
// GPU tests (shader_tests.rs):
success_test!(test_name);  // compiles data/gpu/test_name.easl, validates output with naga
error_test!(test_name, CompileErrorKind::SomeError);  // expects specific compile errors

// CPU tests (cpu_tests.rs):
cpu_test!(test_name);  // runs data/cpu/test_name.easl, compares stdout to data/cpu/test_name.txt
```

- `assert_compiles` compiles the .easl file and validates the WGSL output through naga's parser and validator
- `assert_errors` compiles and checks that exact error kinds match (uses `PartialEq`, not discriminant comparison)
- CPU tests compile with `CompilerTarget::CPU`, run via `run_program_capturing_output`, and assert the captured print output matches the `.txt` file exactly
- Compiled WGSL output is written to `out/` for inspection
- The `#_` reader macro in .easl files comments out the next form — useful for disabling parts of test files
- Run `cargo test --test shader_tests` or `cargo test --test cpu_tests` to target a specific test file

## Style Notes

- Rust 2024 edition — uses `let` chains in `if let` expressions freely
- The codebase uses `take_mut::take` for in-place mutation of `&mut self`
- `Rc<RefCell<...>>` is used extensively for shared mutable state in the AST
- `ExpTypeInfo` wraps `TypeState` and implements `Deref<Target = TypeState>`
- `unwrap_known()` on `ExpTypeInfo` returns a cloned `Type` (panics if not `Known`)
