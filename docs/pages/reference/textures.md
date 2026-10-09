# Textures

Textures and samplers are declared as global `var`s:

```easl
(var tex: (Texture2D f32))
(var tex-sampler: Sampler)
```

`(Texture2D T)` is a 2d texture with components of type `T` (wgsl `texture_2d<T>`), the only texture kind so far; `Sampler` is wgsl's `sampler`. CPU code can create textures with [`load-image`](cpu-builtins.md#load-image) and [`blank-texture`](cpu-builtins.md#blank-texture), and render into them with [`set-render-target`](cpu-builtins.md#set-render-target).

Textures live on the GPU. Assigning one to another variable doesn't copy its pixels: both variables share it until one is rendered into, which first gives that variable its own copy (made on the GPU), so the other keeps its contents.

A `Sampler` (wgsl's `sampler`) decides how `texture-sample` and its variants read a texture: how they filter between texel centers, and what they read outside `[0, 1]`. CPU code constructs one with [`Sampler`](cpu-builtins.md#sampler); a sampler that's never set uses nearest filtering and clamps to the edge.

```easl
(= tex-sampler (Sampler FilterMode/Linear AddressMode/Repeat))
```

### `texture-sample`

```easl
(texture-sample t: (Texture2D f32) s: Sampler coords: vec2f): vec4f
```

Samples the texture at `coords` (normalized `[0,1]` UV coordinates) with the given sampler's filtering. **Fragment shaders only** (implicit derivatives are needed for mip selection) — use `texture-sample-level` elsewhere.

### `texture-sample-bias`

```easl
(texture-sample-bias t: (Texture2D f32) s: Sampler coords: vec2f bias: f32): vec4f
```

Like `texture-sample`, with `bias` added to the computed mip level. **Fragment shaders only.**

### `texture-sample-level`

```easl
(texture-sample-level t: (Texture2D f32) s: Sampler coords: vec2f level: f32): vec4f
```

Samples at an explicitly-specified mip level; callable from any shader stage.

### `texture-sample-grad`

```easl
(texture-sample-grad t: (Texture2D f32) s: Sampler coords: vec2f ddx: vec2f ddy: vec2f): vec4f
```

Samples using explicitly-provided derivatives for mip selection; callable from any shader stage.

### `texture-sample-base-clamp-to-edge`

```easl
(texture-sample-base-clamp-to-edge t: (Texture2D f32) s: Sampler coords: vec2f): vec4f
```

Samples the base mip level with coordinates clamped to the edge of the texture.

### `texture-load`

```easl
(texture-load t: (Texture2D T) coords: (vec2 C) level: L): (vec4 T)   ; C, L: integer
```

Reads a single texel directly by integer texel coordinates and mip level — no sampler, no filtering.

### `texture-dimensions`

```easl
(texture-dimensions t: (Texture2D T)): vec2u
(texture-dimensions t: (Texture2D T) level: L): vec2u    ; L: integer
```

The texture's size in texels, at the base mip level or an explicitly-given one.

### `texture-gather`

```easl
(texture-gather component: C t: (Texture2D T) s: Sampler coords: vec2f): (vec4 T)   ; C: integer
```

Gathers one component (`0`–`3` selecting x/y/z/w) from the four texels that would participate in bilinear filtering at `coords`, returning them as a single vector.
