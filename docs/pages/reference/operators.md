# Operators

All operators in easl are ordinary prefix-notation functions: `(+ 1 2)`, `(= a 5.)`, `(< x y)`. See the [notation guide](overview.md#notation) for how signature families are written.

## Assignment

### `=`

```easl
(= place: T value: T): ()
```

Assigns `value` to `place`, for any type `T`. The first argument must be a mutable *place* expression: a `@var` binding, a mutable global, a struct field, an array element, or a swizzle.

```easl
(= x 5.)
(= v.xy (vec2f 0.))
(= (arr 3u) 1.)
```

## Arithmetic

### `+`, `-`, `*`, `/`, `%`

```easl
(op a: T b: T): T                        ; T: scalar
(op a: (vecN T) b: (vecN T)): (vecN T)   ; element-wise
(op a: (vecN T) b: T): (vecN T)          ; scalar broadcast on the right
(op a: T b: (vecN T)): (vecN T)          ; scalar broadcast on the left
```

The five arithmetic operators, on any scalar type and element-wise on vectors, with scalar broadcasting on either side. `%` follows wgsl remainder semantics and also works on floats. `+` and `*` are **associative**: they accept two or more arguments.

`-` and `/` additionally have unary forms:

```easl
(- x: G): G     ; negation. G = f32, i32, or a vector of either
(/ x: F): F     ; reciprocal: (/ x) is 1/x
```

Matrices have their own arithmetic overloads — see [matrices](matrices.md#matrix-arithmetic).

### `+=`, `-=`, `*=`, `/=`, `%=`

```easl
(op= place: T value: T): ()                       ; T: scalar
(op= place: (vecN T) value: (vecN T)): ()
(op= place: (vecN T) value: T): ()                ; scalar broadcast
```

Compound assignment: `(+= a b)` is equivalent to `(= a (+ a b))`. The first argument must be a mutable place. Matrix compound assignments are covered in [matrices](matrices.md#compound-assignment).

## Comparison

### `==`, `!=`

```easl
(op a: T b: T): bool                          ; T: scalar or bool
(op a: (vecN T) b: (vecN T)): (vecN bool)     ; element-wise
```

Equality and inequality. Vector comparisons are element-wise and produce a boolean vector — combine with [`any` or `all`](math.md#any-all) to get a single `bool`.

### `<`, `>`, `<=`, `>=`

```easl
(op a: T b: T): bool                          ; T: scalar
(op a: (vecN T) b: (vecN T)): (vecN bool)     ; element-wise
```

Ordered comparisons, on any scalar type and element-wise on vectors.

## Boolean

### `&&` / `and`

```easl
(&& a: bool b: bool): bool
```

Logical and. **Associative.** `and` is an alias for `&&`.

### `||` / `or`

```easl
(|| a: bool b: bool): bool
```

Logical or. **Associative.** `or` is an alias for `||`.

### `!` / `not`

```easl
(! b: bool): bool
```

Logical negation. `not` is an alias for `!`.

## Bitwise

### `&`, `|`, `^`

```easl
(op a: T b: T): T                        ; T: integer
(op a: (vecN T) b: (vecN T)): (vecN T)
(op a: (vecN T) b: T): (vecN T)
(op a: T b: (vecN T)): (vecN T)
```

Bitwise and, or, and xor on integer types, element-wise on integer vectors with scalar broadcasting. All three are **associative**.

### `<<`, `>>`

```easl
(op a: T b: T): T                        ; T: integer
(op a: (vecN T) b: (vecN T)): (vecN T)
(op a: (vecN T) b: T): (vecN T)
(op a: T b: (vecN T)): (vecN T)
```

Left and right bit shifts. `>>` is a logical shift for `u32` and an arithmetic (sign-preserving) shift for `i32`, as in wgsl.

### `&=`, `|=`, `^=`, `<<=`, `>>=`

```easl
(op= place: T value: T): ()              ; T: integer
(op= place: (vecN T) value: (vecN T)): ()
(op= place: (vecN T) value: T): ()
```

Compound-assignment forms of the bitwise operators.
