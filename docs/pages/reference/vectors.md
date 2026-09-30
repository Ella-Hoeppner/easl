# Vectors

Vector types come in sizes 2, 3, and 4, with element-type suffixes: `vec3f` is `(vec3 f32)`, `vec2u` is `(vec2 u32)`, `vec4i` is `(vec4 i32)`, and `vec2b` is `(vec2 bool)` (the `b` suffix is an easl nicety; wgsl spells it `vec2<bool>`). Components are the fields `x`, `y`, `z`, `w`; there are no `rgba` aliases.

## Constructors

### `vec2`, `vec3`, `vec4`, `vec2f`…`vec4b`

<!-- index: vec2 vec3 vec4 vec2f vec3f vec4f vec2i vec3i vec4i vec2u vec3u vec4u vec2b vec3b vec4b -->

```easl
(vec3f x: f32 y: f32 z: f32): vec3f       ; one scalar per component
(vec3f v: vec2f z: f32): vec3f            ; any mix of vectors and scalars
(vec3f x: f32): vec3f                     ; single scalar: splat to all components
(vec3 ...): (vec3 T)                      ; unsuffixed: element type inferred
```

Every vector type is constructed by calling its name. Arguments may be any mix of scalars and smaller vectors, as long as the components sum to the target size — `(vec4f a b)` works for two `vec2f`s, as does `(vec4f a.xy z 1.)`. A single scalar argument splats: `(vec3f 0.)` is `(vec3f 0. 0. 0.)`.

For the suffixed constructors (`f`/`i`/`u`/`b`), each argument is independently converted to the target element type, so `(vec2f 1u 2i)` is valid. The unsuffixed constructors (`vec2`, `vec3`, `vec4`) leave the element type to inference.

## Component access and swizzles

Covered in the [language guide](../language.md#vectors): `v.x`, `v.zyx`, `(.xy v)`, and swizzle assignment `(= v.xy (vec2f 0.))` are all supported.

## Geometric functions

These operate on float vectors; `V` below means any of `vec2f`, `vec3f`, `vec4f` (the same type in every `V` position).

### `length`

```easl
(length v: V): f32
```

The Euclidean length of `v`.

### `distance`

```easl
(distance a: V b: V): f32
```

`(length (- a b))`.

### `normalize`

```easl
(normalize v: V): V
```

`v` scaled to unit length.

### `dot`

```easl
(dot a: V b: V): f32
```

The dot product.

### `cross`

```easl
(cross a: vec3f b: vec3f): vec3f
```

The cross product (3-component vectors only).

### `reflect`

```easl
(reflect incident: V normal: V): V
```

Reflects `incident` about `normal`: `incident - 2 * (dot normal incident) * normal`. `normal` should be unit-length.

### `refract`

```easl
(refract incident: V normal: V eta: f32): V
```

The refraction of `incident` through the surface with the given `normal`, where `eta` is the ratio of indices of refraction. Returns the zero vector on total internal reflection. Both vectors should be unit-length.

### `face-forward`

```easl
(face-forward v: V incident: V reference: V): V
```

`v` if `(dot reference incident)` is negative, otherwise `(- v)` — orients a normal to face against the incident direction.

## See also

- Element-wise math on vectors — every function in [math](math.md) taking `F` or `(vecN T)`.
- Vector comparisons producing `(vecN bool)` — [operators](operators.md#comparison), collapsed with [`any`/`all`](math.md#any-all).
- Matrix–vector products — [matrices](matrices.md#matrix-arithmetic).
