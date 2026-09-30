# Math functions

Throughout this page, `F` means `f32` or any float vector (`vec2f`, `vec3f`, `vec4f`), applied element-wise; all `F` positions in one signature take the same type. See the [notation guide](overview.md#notation).

## Trigonometry

### `sin`, `cos`, `tan`

```easl
(sin x: F): F
```

The sine, cosine, or tangent of `x` (radians).

### `asin`, `acos`, `atan`

```easl
(asin x: F): F
```

Inverse sine, cosine, and tangent; results in radians.

### `atan2`

```easl
(atan2 y: F x: F): F
```

The angle of the point `(x, y)` — `atan` of `y / x` using the signs of both arguments to select the quadrant.

### `sinh`, `cosh`, `tanh`

```easl
(sinh x: F): F
```

Hyperbolic sine, cosine, and tangent.

### `asinh`, `acosh`, `atanh`

```easl
(asinh x: F): F
```

Inverse hyperbolic sine, cosine, and tangent.

## Exponentials and logarithms

### `exp`, `exp2`

```easl
(exp x: F): F      ; e^x
(exp2 x: F): F     ; 2^x
```

### `log`, `log2`

```easl
(log x: F): F      ; natural logarithm
(log2 x: F): F     ; base-2 logarithm
```

### `pow`

```easl
(pow base: F exponent: F): F
```

`base` raised to `exponent`, element-wise.

### `sqrt`, `inverse-sqrt`

```easl
(sqrt x: F): F
(inverse-sqrt x: F): F    ; 1 / sqrt(x)
```

### `ldexp`

```easl
(ldexp x: f32 exp: i32): f32
(ldexp x: (vecN f32) exp: (vecN i32)): (vecN f32)
```

`x * 2^exp`, with the exponent given as an integer (element-wise integer vector for the vector form).

## Rounding and parts

### `floor`, `ceil`, `round`, `trunc`

```easl
(floor x: F): F
```

Round downward, upward, to nearest (ties to even), or toward zero.

### `fract`

```easl
(fract x: F): F
```

The fractional part: `x - (floor x)`.

## Sign, magnitude, and range

### `abs`

```easl
(abs x: T): T                  ; T: scalar
(abs x: (vecN T)): (vecN T)
```

Absolute value. Works on floats and integers (a no-op for `u32`).

### `sign`

```easl
(sign x: T): T                 ; T: scalar
(sign x: (vecN T)): (vecN T)
```

`1`, `0`, or `-1` matching the sign of `x`.

### `min`, `max`

```easl
(min a: T b: T): T             ; T: scalar
(min a: (vecN T) b: (vecN T)): (vecN T)
```

Element-wise minimum / maximum. Both are **associative** — `(max a b c)` works.

### `clamp`

```easl
(clamp x: T low: T high: T): T             ; T: scalar
(clamp x: (vecN T) low: (vecN T) high: (vecN T)): (vecN T)
```

`x` restricted to the range `[low, high]`.

### `saturate`

```easl
(saturate x: F): F
```

`x` clamped to `[0, 1]`.

## Interpolation and steps

### `mix`

```easl
(mix a: F b: F t: F): F
(mix a: (vecN f32) b: (vecN f32) t: f32): (vecN f32)
```

Linear interpolation, `a * (1 - t) + b * t`. The second form broadcasts a scalar `t` across a vector blend.

### `step`

```easl
(step edge: F x: F): F
```

`0.` where `x < edge`, `1.` elsewhere.

### `smoothstep`

```easl
(smoothstep edge0: F edge1: F x: F): F
```

Smooth Hermite interpolation from `0.` to `1.` as `x` moves from `edge0` to `edge1`.

### `fma`

```easl
(fma a: F b: F c: F): F
```

Fused multiply-add: `a * b + c`.

## Angle conversion

### `degrees`, `radians`

```easl
(degrees x: F): F     ; radians → degrees
(radians x: F): F     ; degrees → radians
```

## Boolean reductions

<a id="any-all"></a>

### `any`

```easl
(any v: bool): bool
(any v: (vecN bool)): bool
```

True if any component is true.

### `all`

```easl
(all v: bool): bool
(all v: (vecN bool)): bool
```

True if every component is true. `any` and `all` are the usual way to collapse an element-wise vector comparison into a single condition: `(all (< a b))`.
