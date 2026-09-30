# Conversions & bits

Type conversions, bit reinterpretation, data packing, and bit-manipulation functions. See the [notation guide](overview.md#notation).

## Scalar conversions

### `f32`, `i32`, `u32`, `bool`

```easl
(f32 x: T): f32       ; T: scalar
(i32 x: T): i32
(u32 x: T): u32
(bool x: T): bool
```

Each scalar type name doubles as a conversion function, converting the *value* (not the bits): `(i32 2.7)` is `2i`, `(f32 5u)` is `5.`. To convert a whole vector's element type, use the [vector constructors](vectors.md#constructors) — `(vec3i some-vec3f)`.

### `into`

```easl
(into x: T): S
```

Converts `x` to whatever type the context expects: scalar casts, same-size vector conversions, scalar-to-vector splats, and fixed-size to runtime-sized arrays. `~x` is shorthand. Pin the target with an ascription when context doesn't: `~5u: f32`. You can define `into` for your own types; it's an ordinary overloadable function.

## Bit reinterpretation

### `bitcast`

```easl
(bitcast x: T): S                    ; T, S: scalar
(bitcast v: (vecN T)): (vecN S)      ; element-wise
```

Reinterprets the raw bits of a 32-bit value as another 32-bit type, with no numeric conversion. The destination type is determined by inference, so it's usually pinned with a type ascription:

```easl
(bitcast 1.0): u32        ; 0x3F800000
(bitcast some-vec2u): vec2f
```

## Data packing

Pack functions squeeze a small float or integer vector into a single `u32`; unpack functions reverse the process.

### `pack-4x8-snorm`, `unpack-4x8-snorm`

```easl
(pack-4x8-snorm v: vec4f): u32
(unpack-4x8-snorm p: u32): vec4f
```

Four signed-normalized floats (clamped to `[-1, 1]`) in 8 bits each.

### `pack-4x8-unorm`, `unpack-4x8-unorm`

```easl
(pack-4x8-unorm v: vec4f): u32
(unpack-4x8-unorm p: u32): vec4f
```

Four unsigned-normalized floats (clamped to `[0, 1]`) in 8 bits each.

### `pack-2x16-snorm`, `unpack-2x16-snorm`

```easl
(pack-2x16-snorm v: vec2f): u32
(unpack-2x16-snorm p: u32): vec2f
```

Two signed-normalized floats in 16 bits each.

### `pack-2x16-unorm`, `unpack-2x16-unorm`

```easl
(pack-2x16-unorm v: vec2f): u32
(unpack-2x16-unorm p: u32): vec2f
```

Two unsigned-normalized floats in 16 bits each.

### `pack-2x16-float`, `unpack-2x16-float`

```easl
(pack-2x16-float v: vec2f): u32
(unpack-2x16-float p: u32): vec2f
```

Two half-precision (f16) floats.

### `pack-4x8-i8`, `unpack-4x8-i8`

```easl
(pack-4x8-i8 v: vec4i): u32
(unpack-4x8-i8 p: u32): vec4i
```

The low 8 bits of four signed integers.

### `pack-4x8-u8`, `unpack-4x8-u8`

```easl
(pack-4x8-u8 v: vec4u): u32
(unpack-4x8-u8 p: u32): vec4u
```

The low 8 bits of four unsigned integers.

### `pack-4x8-i8-clamp`, `pack-4x8-u8-clamp`

```easl
(pack-4x8-i8-clamp v: vec4i): u32
(pack-4x8-u8-clamp v: vec4u): u32
```

Like `pack-4x8-i8` / `pack-4x8-u8`, but clamping each component into the 8-bit range instead of truncating. (Unpacking is the same as the non-clamp variants.)

### `dot-4-u8-packed`, `dot-4-i8-packed`

```easl
(dot-4-u8-packed a: u32 b: u32): u32
(dot-4-i8-packed a: u32 b: u32): i32
```

The dot product of two vectors of four 8-bit integers, each packed into a `u32` — unsigned and signed variants.

## Bit manipulation

These operate on `T: integer` scalars and integer vectors, element-wise.

### `count-one-bits`

```easl
(count-one-bits x: T): T
(count-one-bits v: (vecN T)): (vecN T)
```

The number of set bits (population count).

### `count-leading-zeros`, `count-trailing-zeros`

```easl
(count-leading-zeros x: T): T
(count-trailing-zeros x: T): T
```

The number of consecutive zero bits from the most-significant end (`leading`) or least-significant end (`trailing`); `32` for zero input. Vector forms are element-wise.

### `first-leading-bit`

```easl
(first-leading-bit x: T): T
(first-leading-bit v: (vecN T)): (vecN T)
```

For `u32`: the position of the highest set bit, or `0xFFFFFFFF` if none. For `i32`: the position of the highest bit that differs from the sign bit, or `-1` for `0` and `-1`.

### `first-trailing-bit`

```easl
(first-trailing-bit x: T): T
(first-trailing-bit v: (vecN T)): (vecN T)
```

The position of the lowest set bit, or all-ones if the input is zero.

### `reverse-bits`

```easl
(reverse-bits x: T): T
(reverse-bits v: (vecN T)): (vecN T)
```

Reverses the order of the 32 bits.

### `extract-bits`

```easl
(extract-bits x: T offset: u32 count: u32): T
(extract-bits v: (vecN T) offset: u32 count: u32): (vecN T)
```

Extracts `count` bits starting at `offset`. For `i32`, the extracted field is sign-extended; for `u32`, zero-extended.
