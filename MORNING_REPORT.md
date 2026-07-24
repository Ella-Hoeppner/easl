# Overnight report: `@cpu` code now runs on the bytecode VM

## TL;DR

It works, and it's done to the standard you asked for: **every test in every
interpreter-driven suite (cpu, buffer, sync, window) now runs on both the
tree-walking interpreter and the bytecode VM, asserting identical output,
identical window-event logs, and identical GPU transfer traces — and all of it
is green**, alongside the untouched shader/conformance/vm/audio suites. The
default runtime for `@cpu` code is now the VM; `CpuRuntime::TreeWalking`
remains selectable on every entry point. The audio hot loop is byte-for-byte
unaffected. Branch `fable_runtime_overhaul`, 8 commits, nothing pushed or
merged.

One thing I could not do overnight: open a real window. The winit path runs
the exact same `FrameDriver`/host code the headless tests exercise, but you
should run a sketch (`easl run`, after reinstalling the CLI against this
branch) as the final acceptance test.

## Architecture

### The host interface

The VM stays a pure math engine. One new opcode, `Op::HostCall`, whose
`arg_positions[0]` indexes a cold `Vec<HostOp>` table on `Code` — print,
dispatch, sync checks, dynamic arrays, window queries, textures, audio all go
through it. `execute_with_host<H: VmHost>` is generic; the audio path is
`execute()` = `execute_with_host(&mut NoopHost)`, so after monomorphization
its dispatch loop is identical to before (the host arm is dead code there —
audio-mode compilation never emits it). Nothing was added to the
per-instruction path. `Code` also gained `min_stack_size` (from the
compiler's bump allocator) so slots that only host ops touch are always
allocated — this also fixed a latent stack-sizing fragility.

### Sync tracking — your `CheckSync` design, as specified

Exactly the shape you sketched: the compiler's effect analysis emits explicit
instructions, and all name resolution happens at compile time — the runtime
addresses globals purely by table index.

- `CheckGpuToCpu { binding }` is emitted **before** any application whose
  effect read-set touches a GPU-bound global — the same placement where the
  tree-walker calls `check_cpu_readable` (before argument evaluation). It
  early-returns unless the binding is actually `CPUOutOfDate`.
- `MarkCpuWritten { binding }` is emitted **after** any application whose
  write-set touches one — mirroring `mark_cpu_written`.
- Dispatch sites get a compile-time table entry holding the entry name and
  the dispatched shader's read/write sets as binding indices (from
  `gpu_read_and_written_globals`, so `array-length`-only reads count for
  uploads but never trigger readbacks — last week's work carried straight
  over).

The proof this reproduces the tree-walker's semantics exactly: the **sync
test suite's golden transfer traces (uploads, readbacks, prints, in order)
are byte-identical on both runtimes**, including the closure-scope upload
cases and the `array-length` no-readback case.

### The runtime driver: composition, not reimplementation

`VmCpuRuntime` **wraps a real `EvaluationEnvironment`**. All sync-state,
upload/readback serialization, print formatting, render-target, and audio
machinery is the tree-walker's own code, reused unchanged — the env's `eval`
just never runs. This was the single highest-leverage decision of the night:
it eliminated an entire class of drift bugs, and it's why the transfer traces
match exactly. Division of labor:

- Fixed-size GPU-bound globals live **authoritatively in VM stack slots**
  (fast math, zero overhead). They're mirrored into the env's `Value`s
  *lazily*: `MarkCpuWritten` just sets a `slots_dirty` flag; conversion
  happens only when an upload actually needs serialization. (A naive
  convert-on-every-write would have been O(n²) for CPU loops writing large
  arrays.)
- Runtime-sized globals — unsized arrays, textures — can't live in a
  statically-addressed stack at all, so they stay host-side **as `Value`s**,
  exactly the tree-walker's representation, accessed from VM code through
  `DynLoad`/`DynStore`/`DynLen`/assignment host ops. This preserves
  `Value::ZeroedArray`'s no-allocation `BufferUpload::Clear` optimization
  verbatim, and element access on the CPU side is rare by nature.
- `Value::from_vm_words`/`to_vm_words` convert between the VM's flat layout
  and `Value`s (matrices are arrays of column vectors to match the
  tree-walker's shape), so `(print ...)` output is identical by
  construction.

### spawn-window: suspension + shared frame loops

`spawn-window` can't be an ordinary host call (the host would need to
re-enter the VM while it holds the stack), so it **suspends**:
`execute_with_host` returns `RunResult::Suspended`, `main`'s continuation
stays in `call_stack` (stashed aside), the window loop runs the frame
function once per frame, and `main` resumes afterward — the first real use of
the call-stack-as-continuation design you built into the VM.
`close-window` suspends the frame execution the same way (mirroring the
tree-walker's `CloseWindow` exception, including skipping the rest of the
frame body).

The frame loops themselves were unified behind a small `FrameDriver` trait
(`run_frame` / `io_mut` / `wgsl` / `binding_infos`): the winit loop,
StringIO's simulated frames, and CaptureIO's headless frames are each written
once and driven by either backend. Frame-closure captured scope is written
once into the frame function's argument slots; since VM slots persist,
mutation of captured state across frames matches `Function::Scoped` semantics
with no per-frame copying.

### Closures as data

A function-typed *value* in the VM is represented by its captured scope's
data — nothing at all for scope-less closures (`vm_type_size`). Scope
constructions compile like struct constructions; dispatched-closure scopes
compile into the implicit scope binding's slots + `MarkCpuWritten` (the
compile-time equivalent of `upload_dispatched_closure_scope`).

## Judgment calls worth your review

1. **Default flipped to `BytecodeVm`** on all no-flag entry points including
   the CLI path, per your instruction. Every suite passes under the flipped
   default. If you want a CLI escape hatch (`--runtime tree`), the
   `*_with_runtime` variants are ready for easl_cli to expose.
2. **Composition over reimplementation** (env-wrapping, above). Cost: a
   `Program::clone()` + WGSL compile at startup that the pure-audio path
   doesn't have; negligible one-time work, and identical to what the
   tree-walker runner already did.
3. **Redundant sync checks at nested application sites** — the tree-walker
   re-checks its read-set at *every* application eval; I mirror that
   placement, so an outer call and the sites inside it may both emit checks.
   Each is an early-return host call when synced. Correctness-first; a
   dominator-style dedup pass is easy later if profiling ever cares.
4. **Two latent pre-existing VM bugs found by the new coverage and fixed**
   (they affected the audio path too): `vec + scalar` (and `-`, `%`, and
   compound assignments like `*=`) compiled as if both operands were
   vectors, reading out-of-bounds slots — your sketch's `(*= particle.zw
   drag-factor)` would have hit this; and `compile_builtin` receiving the
   signature's (sometimes less-resolved) return type instead of the
   expression's. Conformance/vm suites stayed green throughout.
5. **Scope-closure discovery**: closures referenced only through scope
   constructions aren't in the abstract-function registry (the
   reference-address-space rebuild drops them — same root cause as the
   dispatched-closure bug we fixed last week; the tree-walker survives via
   type-embedded ancestor Arcs). The VM compile discovers them transitively
   (`composite_functions_in_usage_order_with_discovery(true)`); WGSL/C/audio
   emission deliberately does not (it would emit shader closures that naga
   rejects). That registry-dropping behavior is now implicated in two
   separate bugs — a future cleanup should probably make the registry stop
   lying instead of both consumers compensating.
6. **New VM capability filled in along the way**: the full data-packing op
   set (`pack/unpack-4x8-*`, `pack/unpack-2x16-*`, `dot-4-{u8,i8}-packed`)
   with the tree-walker's exact formulas, and statically-sized
   `zeroed-array`. These were on the VM's known-missing list.
7. **Known gaps, all loud (panic with clear messages), none test-covered
   because no test or sketch exercises them**:
   - Mutating captured scope through a *directly-called* closure doesn't
     write back to the closure's storage (frame closures and dispatch scopes
     work; a stateful counter-closure invoked in a CPU loop would diverge
     from the tree-walker — silently, this is the one gap that wouldn't
     panic; it's noted in CLAUDE.md).
   - Dynamic-array elements can't be passed by reference or
     compound-assigned (`(+= (arr i) x)` on an unsized global).
   - Strings exist only as `print`/key-query literals.
   - Nested `spawn-window` is rejected.
8. **`u16` slot space** (65,536 stack slots) is now a real ceiling for CPU
   code with large *sized* global arrays. Unsized arrays (the idiomatic
   form) are host-side and unaffected. `take_stack_slot` isn't
   overflow-checked; worth a guard eventually.

## Test status

Full `cargo test --features window`: **all 13 binaries green** —
shader 276, conformance 172, cpu 42 (×2 runtimes), buffer 20 (×2), sync 6
(×2, golden traces identical), window 17 (×2), vm 24, import 7, plus the
rest. `cargo build` without features also clean.

## Commits (this branch, oldest first)

- design notes for VM CPU runtime
- VM core: HostCall opcode, host metadata tables, VmHost trait, suspension
- CPU-mode bytecode compilation: host-op lowering, sync-check emission, dynamic globals
- FrameDriver abstraction: frame loops generic over executing backend
- VM CPU runtime: host-call implementation, dual-runtime cpu tests green
- buffer tests green on both runtimes: closures as scope data, textures
- all interpreter suites (cpu/buffer/sync/window) green on both runtimes
- default CPU runtime is now the bytecode VM
- (this report + CLAUDE.md documentation)
