# GPU builtins

Shader-stage-specific builtins: builtin-attribute lookups, fragment derivatives, and atomics.

## Builtin attribute lookups

Every wgsl [builtin input](https://www.w3.org/TR/WGSL/#builtin-inputs-outputs) can be read by calling it as a zero-argument function, in the appropriate shader stage (or helpers called from it) — no annotated parameter required. These correspond one-to-one to `@{builtin ...}` input annotations.

### `vertex-index`

```easl
(vertex-index): u32
```

**Vertex.** The index of the current vertex within the draw call.

### `instance-index`

```easl
(instance-index): u32
```

**Vertex.** The index of the current instance.

### `position`

```easl
(position): vec4f
```

**Fragment.** The framebuffer-space position of the current fragment (`.xy` are pixel coordinates). As an *output* annotation on a vertex shader, `@{builtin position}` marks the clip-space position — and a vertex shader returning a bare `vec4f` gets that annotation automatically.

### `front-facing`

```easl
(front-facing): bool
```

**Fragment.** True when the current fragment belongs to a front-facing primitive.

### `sample-index`

```easl
(sample-index): u32
```

**Fragment.** The current sample index under multisampling.

### `sample-mask`

```easl
(sample-mask): u32
```

**Fragment.** The input sample coverage mask. (Also usable as an output annotation, `@{builtin sample-mask}`.)

### `global-invocation-id`

```easl
(global-invocation-id): vec3u
```

**Compute.** The invocation's global position across the whole dispatch — the usual way to index a compute thread's work item.

### `local-invocation-id`

```easl
(local-invocation-id): vec3u
```

**Compute.** The invocation's position within its workgroup.

### `local-invocation-index`

```easl
(local-invocation-index): u32
```

**Compute.** The invocation's linearized index within its workgroup.

### `workgroup-id`

```easl
(workgroup-id): vec3u
```

**Compute.** The position of the current workgroup within the dispatch.

### `num-workgroups`

```easl
(num-workgroups): vec3u
```

**Compute.** The dispatch size, in workgroups.

There is one output-only builtin attribute with no lookup function: `@{builtin frag-depth}` (an `f32` fragment-shader output overriding the fragment's depth).

## Derivatives

All derivative functions are **fragment-only** and operate on `F` (`f32` or a float vector), element-wise.

### `dpdx`, `dpdy`

```easl
(dpdx x: F): F
(dpdy x: F): F
```

The partial derivative of `x` with respect to window-space x (or y) — the difference between the value in neighboring fragments of the 2×2 quad.

### `dpdx-coarse`, `dpdy-coarse`

```easl
(dpdx-coarse x: F): F
```

Derivatives computed with quad-level (coarse) precision.

### `dpdx-fine`, `dpdy-fine`

```easl
(dpdx-fine x: F): F
```

Derivatives computed with per-fragment (fine) precision.

## Atomics

`(Atomic T)` — with `T` an integer type — wraps a value for atomic access. Atomics live in storage (or workgroup) memory:

```easl
(var counter: (Atomic u32))
```

All atomic functions take the atomic by reference (automatically — pass the variable itself).

### `atomic-load`

```easl
(atomic-load a: (Atomic T)): T
```

Atomically reads the value.

### `atomic-store`

```easl
(atomic-store a: (Atomic T) value: T): ()
```

Atomically writes `value`.

### `atomic-add`, `atomic-sub`

```easl
(atomic-add a: (Atomic T) value: T): T
```

Atomically adds (or subtracts) `value`, returning the **previous** value — as do all the read-modify-write atomics below.

### `atomic-min`, `atomic-max`

```easl
(atomic-min a: (Atomic T) value: T): T
```

Atomic minimum / maximum.

### `atomic-and`, `atomic-or`, `atomic-xor`

```easl
(atomic-and a: (Atomic T) value: T): T
```

Atomic bitwise and / or / xor.

### `atomic-exchange`

```easl
(atomic-exchange a: (Atomic T) value: T): T
```

Atomically replaces the value with `value`, returning what was there before.

## See also

- `(discard)` — fragment-only control flow, covered in the [language guide](../language.md#return-and-discard).
- Fragment-only texture sampling — [`texture-sample`](textures.md#texture-sample) and [`texture-sample-bias`](textures.md#texture-sample-bias).
