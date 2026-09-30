# Matrices

Matrix types are named `matNxM` where `N` is the number of **columns** and `M` the number of **rows**, each ranging over 2–4 — the same convention as wgsl. `mat3x2f` has 3 columns of `vec2f`. Storage is column-major. As in wgsl, matrix math is defined for `f32` elements (`mat2x2f` … `mat4x4f`, where `matNxMf` is shorthand for `(matNxM f32)`).

## Constructors

### `mat2x2`…`mat4x4`, `mat2x2f`…`mat4x4f`

<!-- index: mat2x2 mat2x3 mat2x4 mat3x2 mat3x3 mat3x4 mat4x2 mat4x3 mat4x4 mat2x2f mat2x3f mat2x4f mat3x2f mat3x3f mat3x4f mat4x2f mat4x3f mat4x4f -->

```easl
(matNxMf e0: f32 e1: f32 ...): matNxMf     ; N*M scalars, in column-major order
(matNxMf c0: vecMf c1: vecMf ...): matNxMf ; N column vectors
(matNxM ...): (matNxM T)                   ; unsuffixed: element type inferred
```

Matrices are constructed from `N*M` scalars in column-major order, or from `N` column vectors of size `M`:

```easl
(mat2x2f 1. 0.
         0. 1.)                     ; columns (1,0) and (0,1)

(mat3x3f col-a col-b col-c)         ; three vec3f columns
```

## Matrix arithmetic

These are additional overloads of the [arithmetic operators](operators.md#arithmetic). `mat` below means any `matNxM` of `f32`; dimensions must agree as noted.

### `+`, `-` (matrices)

```easl
(+ a: matNxM b: matNxM): matNxM
(- a: matNxM b: matNxM): matNxM
```

Element-wise matrix addition and subtraction.

### `*` (matrices)

```easl
(* m: matNxM s: T): matNxM         ; scalar scale (either argument order)
(* s: T m: matNxM): matNxM
(* m: matNxM v: vecN): vecM        ; matrix × column vector
(* v: vecM m: matNxM): vecN        ; row vector × matrix
(* a: matKxM b: matNxK): matNxM    ; matrix × matrix
```

Matrix products in every form wgsl supports: scaling by a scalar, transforming a column vector (`m * v`), the row-vector form (`v * m`), and matrix–matrix multiplication (the inner dimensions must agree; the product of a `matKxM` and a `matNxK` is a `matNxM`).

Note there is **no matrix division** — wgsl defines none, and neither does easl.

## Compound assignment

### `+=`, `-=`, `*=` (matrices)

```easl
(+= place: matNxM value: matNxM): ()
(-= place: matNxM value: matNxM): ()
(*= place: matNxM s: T): ()            ; scale in place
(*= place: matNxN value: matNxN): ()   ; square matrices only
(*= place: vecN m: matNxN): ()         ; v = v * m, square matrices only
```

Matrix compound assignment exists exactly where wgsl's rule allows — `a op= b` is valid whenever `a op b` produces `a`'s type. That gives `+=` and `-=` for any matrix, `*=` by a scalar for any matrix, and the matrix-multiplying forms only for square matrices (and for a vector multiplied through a square matrix).

## Other matrix functions

### `determinant`

```easl
(determinant m: matNxNf): f32
```

The determinant of a square matrix.

### `transpose`

```easl
(transpose m: matNxMf): matMxNf
```

The transpose — columns become rows.
