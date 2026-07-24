# Overnight report: CPU runtime on the bytecode VM

Status: IN PROGRESS — this header will be replaced with a final summary.

## Architecture (decided up front, before any code)

Goal: run `@cpu` entry points on the bytecode VM with byte-identical observable
behavior to the tree-walking interpreter, selectable via a runtime flag
(default VM), with every interpreter-touching test suite exercising both.

### 1. Host interface: one `HostCall` opcode + a cold metadata table

The VM stays a pure math engine. All CPU-orchestration (print, dispatch,
sync, window queries, dynamic arrays) goes through **one new opcode**,
`Op::HostCall`, whose `arg_positions[0]` indexes a `Vec<HostOp>` table on
`Code`. `execute` becomes generic: `execute_with_host<H: VmHost>` — the audio
path calls it with a no-op host, so after monomorphization the audio hot loop
is unchanged (the `HostCall` arm is dead code there; audio-mode compilation
never emits it). No per-instruction overhead is added anywhere.

Rationale for one opcode + table over many opcodes: keeps the `Op` enum and
dispatch loop small; host ops are cold by design so an extra indirection into
a side table costs nothing that matters; adding new host functionality never
touches the dispatch loop again.

### 2. Suspension for `spawn-window` / `close-window`

`execute_with_host` returns `RunResult::{Finished, Suspended(HostSuspend)}`.
`spawn-window` cannot be an ordinary host call because the host must *re-enter
the VM* (running the frame function once per frame) while `main` is paused —
a host call holds `&mut stack`, so reentry would double-borrow. Instead the
instruction suspends: the continuation stays in `call_stack` (the
architecture's documented design intent), the driver stashes it aside, runs
the window loop (each frame = `prepare_to_run_function(frame_fn)` + execute),
then restores the continuation and resumes `main`. `close-window` suspends the
frame execution the same way (mirroring the tree-walker's `CloseWindow`
exception).

Frame-closure captured scope: written once into the frame function's trailing
scope-argument slots at spawn time. Because VM slots are static and persist,
mutations to captured state persist across frames exactly like the
tree-walker's `Function::Scoped` semantics — no per-frame copying.

### 3. Sync tracking: compile-time indices, explicit instructions

Mirrors the tree-walker's semantics at the same granularity, with all name
resolution moved to compile time:

- `Code` gains a **binding table**: one entry per GPU-bound global (name kept
  only for logging/sync-trace parity; group/binding; `Type`; address space;
  storage = fixed slots or host-side dynamic).
- At every application site whose effect read-set (`read_and_written_globals`
  — the readback set, which already excludes `array-length`-only reads)
  touches binding globals, the compiler emits `HostOp::CheckGpuToCpu{binding}`
  **before** the application's code — the exact placement where the
  tree-walker calls `check_cpu_readable` (before argument evaluation).
- After each application whose write-set touches binding globals, it emits
  `HostOp::MarkCpuWritten{binding}` — mirroring `mark_cpu_written` after
  application evaluation (interpreter.rs ~4692).
- Nested applications produce redundant checks (outer + inner) just as the
  tree-walker re-checks at every application eval; a check on a `Synced`
  binding is an early-return, so this matches both semantics and rough cost.
- Dispatch sites get a **dispatch table** entry: entry-point name plus the
  entry's read/write sets (`gpu_read_and_written_globals` — includes
  length-only reads, since `arrayLength()` needs the buffer uploaded) resolved
  to binding indices at compile time. The runtime handler replicates
  `collect_dirty_uploads` + `mark_gpu_written` from those index sets.

### 4. `Value` as the universal interchange for host-side data

Two new conversions: `Value::from_vm_words(ty, &[u32])` and
`Value::to_vm_words()` (flat VM layout: no vec3 padding, flat matrices).
Everything else **reuses the tree-walker's existing machinery**:

- Upload serialization: slots → `Value` → `to_uniform_bytes` (same bytes).
- Readback: `from_gpu_bytes` → `Value` → `to_vm_words` → slots (same decode).
- Print: slot(s) → `Value` → the same `Display` formatting (identical stdout).
- Dynamic (unsized) arrays and textures don't fit static slots at all, so
  those binding globals live host-side **as `Value`s** — including
  `Value::ZeroedArray`, preserving the `BufferUpload::Clear` no-alloc
  optimization verbatim. Element access from VM code goes through host ops
  (`DynLoad`/`DynStore`/`DynLen`); rare on the CPU side by nature.

This trades a little conversion cost at cold host boundaries for guaranteed
behavioral parity and zero duplicated layout logic.

### 5. Frame-loop unification: `FrameDriver`

`run_window_loop`, `StringIO`'s simulated frames, and `CaptureIO`'s headless
frames all currently hardcode `eval(body, env)`. They're refactored around a
small trait (`FrameDriver`: `io_mut`, `wgsl`, `binding_infos`, `run_frame`),
with one implementation backed by the tree-walker (body + env) and one by the
VM runtime. `IOManager::run_spawn_window` is generalized to take a driver, so
each IO manager's frame-loop strategy is written once and works for both
backends.

### 6. Compilation modes

`compile_to_bytecode_program` keeps its exact current behavior for audio.
A new CPU mode compiles the `@cpu` entry and its transitive callees, does NOT
skip CPU-exclusive functions (that's the point), still skips GPU-only entry
points, lowers CPU-exclusive builtins to host ops, and excludes
unsized-array/texture globals from slot allocation (they're host-side).
`Code` also gains `min_stack_size` (from the compiler's bump allocator) so
host-op result slots are always inside the allocated stack.

### 7. Public API + tests

`CpuRuntime::{TreeWalking, BytecodeVm}` on the runner entry points; existing
no-flag entry points default to **BytecodeVm** per the plan. Every
interpreter-driven suite (cpu, buffer, sync, window) runs each test on both
backends against the same expected output/golden.

## Judgment calls to review (running list)

- (filled in as the night progresses)

## Final status

- (filled in at the end)
