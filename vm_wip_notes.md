# Bytecode VM — WIP Notes

A handoff document for the in-progress bytecode VM. This is a **second interpreter**, separate from the tree-walking one in `src/interpreter.rs`. The existing interpreter is too slow for things like DSP; this one compiles easl to a flat, register-style bytecode and runs it in a tight dispatch loop.

Status as of this writing: the core execution model and calling convention are working end-to-end (literals, scalar `f32` arithmetic, function calls with multi-arg passing). Most expression kinds and almost all builtins are still `todo!()`. The pieces that exist are believed correct and are test-covered; the work remaining is mostly "fill in more arms," plus a few larger features (structs, vectors, control flow, optimization passes).

## Where the code lives

- `src/vm/bytecode.rs` — the VM: `Op`, `Instruction`, `Function`, `Code`, `BytecodeProgram`, and the `execute()` dispatch loop.
- `src/compiler/program.rs` — `Program::compile_to_bytecode_program` (entry point), `BytecodeCompilationState`, `IntermediateBytecodeFunction`, `composite_functions_in_usage_order`.
- `src/compiler/expression.rs` — `TypedExp::compile_to_bytecode` (the tree-walk that emits instructions).
- `tests/vm_tests.rs` + `data/vm/*.{easl,txt}` — the test suite.

## Execution model

`BytecodeProgram`:
```rust
struct BytecodeProgram {
  code: Code,                    // function_instructions: Vec<Instruction>, functions: Vec<Function>
  stack: Vec<u32>,               // single flat value stack, all slots are raw u32 bits
  call_stack: Vec<Range<u32>>,   // the continuation: saved instruction ranges of suspended callers
}
```
- `Instruction { op: Op, arg_positions: [u16; 3], return_position: u16 }`. Positions are **absolute** indices into `stack`.
- `Function { instructions: Range<u32>, return_position: u16 }`. `instructions` is a half-open range into `code.function_instructions`; `return_position` is where this function's return value lands (== its `stack_frame_start`).
- Values are untyped u32 bits. Each op reinterprets as needed (`f32::from_bits` / `.to_bits()`, `i32 as u32`, etc.).

**Static absolute addressing (key design decision).** There is no per-call frame base. Every function is assigned a fixed, disjoint region of the single shared `stack` at compile time, and instruction operands are absolute slot indices. This was chosen over a dynamic frame-base model because:
- It makes globals and locals one uniform address space (a global is just a low slot), so no separate global address space / load-store ops are needed.
- The hot path is `stack[const_index]` with no offset arithmetic.
- It's valid because easl has **no recursion** (the call graph is a DAG), so a function is never simultaneously active with itself, and each activation can use one fixed region.
- Trade-off accepted: this rules out multi-shot continuations, but single-shot suspend/resume (the efficiently-implementable subset of algebraic effects we care about) still works. See "Continuations" below.

**The dispatch loop** (`execute`):
- Pops the current frame's instruction range (`Range<u32>`) off `call_stack` into a local `ip`. (`ip.next()` yields the next instruction index and advances; cloning a `Range<u32>` snapshots the resume point.)
- Loops: fetch `code.function_instructions[ip.next()]` (via `get_unchecked`), `match` on `op`.
- `InvokeFunction` pushes the caller's remaining `ip` onto `call_stack` and replaces `ip` with the callee's instruction range.
- When `ip` is exhausted, pop `call_stack` to resume the caller; if `call_stack` is empty, halt.
- So `call_stack` is the entire continuation. Suspension (not yet implemented) would just push the current `ip` back and return; `execute` could then be called again to resume.

**Allocation discipline:** `from_code` sizes `stack` to `max(max_touched_index over all instructions) + 1` and pre-reserves `call_stack` capacity to the function count (a safe upper bound on call depth since no recursion). So `execute` never allocates.

**`unsafe`:** instruction fetch and slot access use `get_unchecked`/`get_unchecked_mut`; `Move` uses `ptr::copy` (memmove). This is sound **only if the compiler emits in-bounds slot indices** — there's no runtime bounds checking. A `debug_assert` validating function ranges / slot bounds at `from_code` time was suggested but not yet added; worth doing.

## Op semantics (the non-obvious ones)

- **`Constant`**: writes one u32 to `return_position`. The u32 value is split across two `arg_positions`: `arg_positions[0]` is the high 16 bits, `arg_positions[1]` the low. VM recombines as `(arg_positions[0] << 16) | arg_positions[1]`. (Split done with arithmetic, not byte reinterpret, so it's endianness-independent.)
- **`Move`**: block copy. `arg_positions[0]` = source start, `arg_positions[1]` = **count of u32s to move**, `return_position` = dest start. Implemented with `ptr::copy` (memmove — handles overlapping ranges in both directions). `count == 1` is the scalar case; larger counts move whole multi-slot structs in one instruction.
- **`InvokeFunction`**: `arg_positions[0]` = index into `code.functions` of the callee. Other fields unused.
- **`Cos` / `PlusF32`**: scalar f32 ops via the `f32_unary` / `f32_binary` helpers (read arg slot(s), apply, write `return_position`).
- **`max_touched_index`** (used to size the stack) is op-aware because operands don't always mean "slot": `InvokeFunction` → 0 (its operand is a function index, the callee's own instructions account for its slots); `Constant` → just `return_position` (its args are immediate halves, not slots); `Move` → `max(return_position, src) + count - 1` (the move spans `count` slots; guarded for `count == 0`); everything else → max of `return_position` and `arg_positions`.

## Calling convention

Each function occupies `[stack_frame_start, stack_frame_start + frame_size)`, a fixed absolute region.
- **Arguments** live at the **start** of the callee's region, in signature order: arg0 at `stack_frame_start`, arg1 at `stack_frame_start + size(arg0)`, etc. These slots are reserved up front (so body temporaries don't collide with them).
- **Return value** also lands at `stack_frame_start` (overwriting arg0's slot — fine, since args are dead by the time the function returns).
- A **call** (the `Composite` arm of `Application`) emits: one `Move` per argument copying it from the caller's temp slot into the callee's arg slot, then `InvokeFunction(callee_index)`, then one `Move` copying the callee's return value (at the callee's `stack_frame_start`) back into a fresh result slot in the caller.

The argument layout is stored once (per function) and shared by all three places that need it — the up-front reservation, the callee-side binding (mapping arg names → slots in `locals`), and the caller-side arg `Move`s — so they can't drift. This lives in `IntermediateBytecodeFunction { name, instructions, stack_frame_start, arg_positions, arg_sizes }`.

## Compilation pipeline

`Program::compile_to_bytecode_program(self) -> (BytecodeProgram, Vec<Arc<str>>)`:
1. Assign each top-level var (global) a slot at the bottom of the stack.
2. Iterate `composite_functions_in_usage_order()` — a **topological sort** of user (Composite, non-generic, non-HoF) functions, callees before callers. This is also now used by the C/WGSL backend (it replaced a C forward-declaration prepass — emitting in dependency order means declaration-before-use, which both C and WGSL require). It relies on no-recursion (DAG); a `panic!` guards against cycles. Topological order matters for bytecode because `InvokeFunction` references the callee by index, and callees-first guarantees the callee is already in `finished_functions` (with a known index) when a caller is compiled.
3. For each function: `open_function`, reserve arg slots (recording `arg_positions`/`arg_sizes`), `compile_to_bytecode` the body, `close_function`.
4. `finalize` → `(BytecodeProgram, names)`. The returned `Vec<Arc<str>>` is function names; **index in that vec == index in `code.functions`** (== the `InvokeFunction` operand). Look up the function you want to run by name to get its index.

`BytecodeCompilationState`: `consumed_stack_space` (bump pointer), `globals`/`locals` (`HashMap<Arc<str>, u16>` name → absolute slot), `instructions`, `finished_functions: Vec<IntermediateBytecodeFunction>`, `current_function`. `take_stack_slot(size)` returns the current bump pointer and advances it by `size`.

`TypedExp::compile_to_bytecode(&self, state) -> Option<u16>`: tree-walk; returns the slot holding the expression's result (`None` for unit / no-value). Slot allocation is **naive** — every value gets fresh slots, never reused. Register allocation / stack coloring are intentionally deferred to future passes over the bytecode (see "Planned").

## Running a compiled program

```rust
let (mut program, names) = compiled_program.compile_to_bytecode_program();
let f_index = names.iter().position(|n| &**n == "f").unwrap();
program.prepare_to_run_function(f_index);
program.execute();
let slot = program.get_function_return_position(f_index);
let result = f32::from_bits(program.stack[slot as usize]); // reinterpret per the return type
```

## Current state — what's implemented

**Expression kinds** (`compile_to_bytecode`): `NumberLiteral` (i32/u32/f32, incl. int-literal-standing-in-for-f32), `BooleanLiteral`, `Block` (returns last), `Name` (globals then locals lookup), `Function` (binds args to `locals`, moves body result to the return slot), `Application` for **builtins** (only `cos` and `+`-on-`f32` so far) and **Composite** (user function calls — full multi-arg support). `Uninitialized | Wildcard | Unit` → `None`.

**Ops** (`execute`): `Move`, `InvokeFunction`, `Constant`, `Cos`, `PlusF32`. The `Op` enum also declares ~28 more math ops (`Sin`, `Sqrt`, `Pow`, `Fma`, `Atan2`, hyperbolics, etc.) that are **stubbed in the enum but not yet handled** (`_ => todo!()`). Adding a scalar one is usually a one-liner via `f32_unary`/`f32_binary`.

**Tests** (`data/vm/`): `cos`, `plus`, `nested_plus`, `fn_call`, `fn_call_with_arg`, `fn_call_with_two_args`. All passing.

## What's left — TODO

- **Builtins**: everything except `cos` and `+`. The ~28 stubbed math ops; `i32`/`u32` arithmetic; comparisons; type conversions; bit ops. Add `Plus{I32,U32}`, etc.
- **Operand-type dispatch**: builtins currently dispatch on name then, for `+`, on **return type**. That works for homogeneous arithmetic but is wrong in general — for ops where the return type differs from the operand type (comparisons returning `bool`, `length`/`dot` returning scalar from a vector), dispatch on **argument types** instead. The `Type::Function` (`f`) with both `args` and `return_type` is in hand at the dispatch site.
- **Vectors & structs (multi-slot values)**: `take_stack_slot` already takes a size, and `Move` already does multi-slot copies, so the plumbing is there. Still needed: `StructConstructor`/`EnumConstructor` arms, and component-wise vector builtins (a `vec4f + vec4f` fans out into 4 `PlusF32`s over consecutive slots — the VM is scalar-slot-based). `data_size_in_u32s` gives the slot count for a type.
- **Control flow & bindings**: `Let`, `Match`, `ForLoop`, `WhileLoop`, `Break`, `Continue`, `Return`, `Discard`, `Access`, `ArrayLiteral` are all `todo!()`. `Let`/`For` introduce new local bindings (same `locals` map; note: bindings are never *unbound* — the front-end's deshadowing + scope validation guarantee a name is always rebound before use and locals never shadow globals, so a flat never-cleared map is correct). Branching ops (conditional jumps) don't exist yet — the `Op` enum has no jump/branch op; control flow will need them.
- **Optimization passes over the bytecode** (the planned home for cleverness, kept out of the naive tree-walk):
  - *Register allocation / slot reuse* — currently every value gets a fresh slot and nothing is reused, so the stack is much larger than necessary. This is liveness-based register allocation on the linear bytecode.
  - *Stack coloring across functions* — functions get disjoint regions today even when they're never simultaneously active; coloring the call DAG would let non-overlapping functions share space. (Caller/callee must stay disjoint; only provably-non-simultaneous functions can overlap.)
  - *Redundant `Move` elimination* — e.g. the `Function` arm always emits a body-result→return-slot `Move` even when the body could have written the return slot directly.
- **Suspend / resume (algebraic effects)**: the `call_stack`-as-continuation design supports it, but there's no suspend opcode or `RunStatus` yet. When added: a suspend pushes the current `ip` back onto `call_stack` and returns from `execute`; resuming calls `execute` again. (Single-shot only — see addressing trade-off above.) Note effects will also need to think about preserving the `stack` data a suspended continuation refers to, since regions get reused.
- **Wiring**: `compile_to_bytecode_program` isn't hooked into any real entry point / CLI yet; it's exercised only by `vm_tests`. There's also no `debug_assert` validating that emitted slot indices are in-bounds (recommended given the pervasive `get_unchecked`).

## Design principles (why things are the way they are)

- **Indices, not pointers.** Instruction pointers are `Range<u32>`, operands are `u16` slot indices. Keeps structures relocatable / arena-friendly and avoids self-referential structs. (An earlier `&'static [Instruction]` experiment was rejected for this reason.)
- **Single dispatch loop + explicit `call_stack`, not host recursion.** Decouples easl call depth from the Rust stack and is what makes pause/resume possible. The hot per-instruction path is identical to the recursive version (frame info in registers; the explicit stack only does work at call/return).
- **Naive lowering, smart passes later.** `compile_to_bytecode` stays a dumb, correct tree-walk; slot reuse / coloring / peepholes go in separate bytecode passes where global liveness info is available (which a single tree-walk fundamentally lacks). The `vm_tests` suite is the correctness oracle for those future passes — each should keep the tests green.
- **`unsafe` is accepted for the hot path.** `get_unchecked` and `ptr::copy` are used deliberately; the safety contract is "the compiler emits valid, in-bounds indices," to be backed by a construction-time `debug_assert`.

## Extending the VM (recipes)

**Dev loop (test-driven).** Add `data/vm/<name>.easl` defining a zero-arg `f` that returns `f32`, plus `data/vm/<name>.txt` containing the expected float; register `vm_test!(<name>);` in `tests/vm_tests.rs`; run `cargo test --features window --test vm_tests <name>`. (Until non-scalar return is supported, the test reads the return slot as a single `f32`, so `f` must return `f32` — wrap whatever you're testing to produce one.) Always pass `--features window`.

**Add a scalar builtin** (e.g. `sin`, `sqrt`, an `i32` `+`):
1. Add the variant to `Op` in `bytecode.rs` (if not already stubbed there).
2. Add its `execute` arm in `bytecode.rs` — unary f32 via `instruction.f32_unary(stack, f32::sin)`, binary f32 via `f32_binary`, or write a custom `unsafe` arm for other types (mirror the `Cos`/`PlusF32`/`Move` arms; read args with `get_unchecked`, write `return_position` with `get_unchecked_mut`).
3. Add a dispatch arm under `FunctionImplementationKind::Builtin` in `compile_to_bytecode` (`expression.rs`), keyed on the builtin name. For overloaded builtins, match on the **operand type(s)** (from `f.args`), not the return type. Emit the op with the arg result slots in `arg_positions` and a fresh `take_stack_slot(1)` as `return_position`; return `Some(result_position)`.
4. No `max_touched_index` change is needed for single-slot ops (the `_` arm covers them). Only ops whose operands aren't plain slots, or that span multiple slots, need their own `max_touched_index` arm (see `Move`/`Constant`/`InvokeFunction`).

**Add an expression kind** (e.g. `Let`, `Match`): implement its arm in `compile_to_bytecode`, returning `Option<u16>` — `Some(slot)` where the result lives, or `None` for a unit/no-value expression. Allocate result/temporary slots with `state.take_stack_slot(size)` (size from `data_size_in_u32s`). Bindings go in `state.locals` (name → absolute slot); they're never unbound (see the bindings note in TODO). Branching kinds (`Match`, `if`, loops) will first need new branch/jump `Op`s — none exist yet.
