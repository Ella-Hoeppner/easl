# Builtin reference

These pages list every builtin function in easl, with all the signatures each accepts. For an alphabetical list of everything, see the [function index](../functions.md).

- **[Operators](operators.md)** — assignment, arithmetic, comparison, boolean, and bitwise operators.
- **[Math functions](math.md)** — trigonometry, exponentials, rounding, interpolation, clamping.
- **[Vectors](vectors.md)** — vector constructors and geometric functions.
- **[Matrices](matrices.md)** — matrix constructors and arithmetic.
- **[Conversions & bits](conversions.md)** — type conversions, `bitcast`, data packing, bit manipulation.
- **[Arrays](arrays.md)** — array utilities.
- **[Textures](textures.md)** — texture sampling and loading.
- **[GPU builtins](gpu-builtins.md)** — builtin-attribute lookups, derivatives, atomics.
- **[CPU builtins](cpu-builtins.md)** — printing, windowing, GPU dispatch, input queries, audio.

## Notation

Signatures are written in easl style, `(name arg: Type ...): ReturnType`, with these conventions for describing overload families:

| Notation | Meaning |
| --- | --- |
| `T: scalar` | `T` may be `f32`, `i32`, or `u32` |
| `T: integer` | `T` may be `i32` or `u32` |
| `T: scalar or bool` | `T` may be `f32`, `i32`, `u32`, or `bool` |
| `(vecN T)` | any of `(vec2 T)`, `(vec3 T)`, `(vec4 T)` — i.e. `vec2f`…`vec4u` |
| `F` | `f32` or any of `vec2f`, `vec3f`, `vec4f`, applied element-wise |
| `()` | the unit type (no return value) |

When a signature says a function takes `F` in several positions, all of those positions take the *same* type — `(pow (vec3f ...) (vec3f ...))` is valid, `(pow (vec3f ...) 2.)` is not (splat the scalar first: `(pow v (vec3f 2.))`).

Functions marked **associative** accept two *or more* arguments: `(+ a b c d)` works and expands to nested two-argument calls.

Many functions correspond directly to wgsl builtins; where the name differs it's a mechanical case conversion (`inverse-sqrt` → `inverseSqrt`, `texture-sample` → `textureSample`).
