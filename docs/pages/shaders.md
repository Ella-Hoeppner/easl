# Writing shaders

An easl file can hold several *entry points*: functions the GPU, or easl's CPU runtime, calls directly. Helpers, structs, and globals are shared between them.

## Entry points

```easl
@vertex
(defn vertex []: vec4f ...)

@fragment
(defn fragment []: vec4f ...)

@compute
@{workgroup-size 64}
(defn simulate [] ...)

@cpu
(defn main [] ...)
```

`@vertex`, `@fragment`, and `@compute` are [wgsl's entry points](https://www.w3.org/TR/WGSL/#entry-point). A compute shader's `@{workgroup-size N}` (or `X Y Z`) sets its threads per workgroup, defaulting to 1. `@cpu` marks the function [the CPU runtime](cpu.md) runs.

Functions passed to `dispatch-render-shaders` or `dispatch-compute-shader` become entry points automatically, so these annotations are optional in programs that dispatch their own shaders, and shaders can be written inline as `fn`s.

## Shader inputs and outputs

Builtin inputs can be read by calling them like functions, anywhere in the right stage: `(vertex-index)`, `(position)`, `(global-invocation-id)`. See the [full list](reference/gpu-builtins.md#builtin-attribute-lookups). Struct fields and values passed between stages get `@{location N}` numbers automatically, in order, and a vertex shader returning a bare `vec4f` returns its position:

```easl
(struct Varyings
  @{builtin position} pos: vec4f
  uv: vec2f)

@vertex
(defn vertex []: Varyings
  (let [corner (match (vertex-index)
                 0 (vec2f -1.)
                 1 (vec2f -1. 3.)
                 _ (vec2f 3. -1.))]
    (Varyings (vec4f corner 0. 1.) corner)))

@fragment
(defn fragment [in: Varyings]: vec4f
  (vec4f in.uv 0. 1.))
```

The explicit forms remain available when you want them: `@{builtin vertex-index}` on an argument, `@{location 0}` on a field or return type. Builtins use wgsl's names in kebab-case, with wgsl's rules about which stages they're valid in.

A value passed from the vertex shader to the fragment shader is interpolated across each triangle. `@{interpolate …}` on the field or argument chooses how, like wgsl's `@interpolate`:

- `perspective` (the default for floats): perspective-correct interpolation.
- `linear`: interpolation in screen space.
- `flat`: no interpolation; every fragment sees one vertex's value. Integer values (and vectors of them) are always flat: they get it without asking, and any other setting is an error.

`perspective` and `linear` take a sampling, as `perspective-centroid` for example: `center` (the default), `centroid`, or `sample`. `flat` takes `first` (the default) or `either`. A builtin can't have an interpolation, since it isn't a value passed between stages:

```easl
(struct Varyings
  @{builtin position} position: vec4f
  @{interpolate flat} id: u32
  @{interpolate linear-centroid} uv: vec2f)
```

## GPU-bound variables

A top-level `var` is a is shared by the CPU and GPU default, with the exact rules about when/where it can be written determined by it's *address space*.

```easl
(var particles: [vec4f])            ; unnanotated vars default to @storage-write
@uniform (var frame-index: u32)
@storage (var palette: [16: vec3f])
@local (var seed: u32 0u)           ; not a binding: one copy per invocation
```

| Address space | |
| --- | --- |
| `storage-write` | read-write storage buffer; the default. Not usable in vertex shaders |
| `storage` (or `storage-read`) | read-only storage buffer |
| `uniform` | read-only in shaders |
| `workgroup` | shared within a compute workgroup |
| `local` | one copy per GPU invocation or CPU thread; the only kind that can have an initial value (wgsl's `private`) |

Binding numbers can be annotated on any global variable like `@{address uniform group 0 binding 0}`, or more tersely as `@[uniform 0 0]`. However, if you don't explicitly annotate these binding numbers, easl will simply infer them for you, based on the order that the variables are declared in your code, and when using the easl CPU runtime all the bindings will be managed automatically for you, so you should never need to think about binding numbers. But the syntax for assigning them explicitly is included for use-cases that involve compiling easl to wgsl to be executed in some other host language.

Textures and samplers need no annotation:

```easl
(var tex: (Texture2D f32))
(var tex-sampler: Sampler)
```

## Fragment-only builtins

`texture-sample`, `texture-sample-bias`, the derivatives (`dpdx`, `dpdy`, and their `-coarse`/`-fine` variants), and `(discard)` only work in fragment shaders. Helper functions can use them, as long as only fragment shaders call those helpers.

## The compiled WGSL

The compiler specializes generics, inlines function arguments, turns enums and `match` into structs and `switch`es, and writes readable WGSL. Names keep their spelling with `-` replaced by `_`, and specialized definitions get type suffixes (`map` with `T = f32` becomes `map_f32`). The output has the entry points, everything they call, and any other function you wrote that can run on the GPU, so an easl file also works as a WGSL library. Reading the output is often the quickest way to see how a compiler feature works.
