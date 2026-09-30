# Arrays

Array syntax is covered in the [language guide](../language.md#arrays). `[N: T]` is a fixed-size array type, `[T]` a runtime-sized one.

### `array-length`

```easl
(array-length arr: [N: T]): u32
(array-length arr: [T]): u32
```

The number of elements in an array: a compile-time constant for fixed-size arrays, the current length for runtime-sized ones. `length` is an alias. It never forces a GPU→CPU readback, since the GPU can't resize a buffer.

### `zeroed-array`

```easl
(zeroed-array): [N: T]              ; size and element type from inference
(zeroed-array len: u32): [T]        ; CPU only: runtime-sized zeroed array
```

An array of zeros. With no arguments, the size and element type come from context (ascribe them if needed: `(zeroed-array): [256: f32]`). With a length, it's a runtime-sized array, in CPU code only; this is how to size a runtime-sized storage buffer:

```easl
(var buf: [vec4f])

@cpu
(defn main []
  (= buf (zeroed-array 4096u))
  ...)
```

Assigning a zeroed array to a storage buffer clears it on the GPU, without allocating or uploading anything on the CPU.

### `into-dynamic-array`

```easl
(into-dynamic-array arr: [N: T]): [T]
```

Converts a fixed-size array to a runtime-sized one, e.g. to fill a runtime-sized storage buffer. CPU code only; `~arr` also works where a `[T]` is expected.

```easl
@storage (var points: [vec2f])

@cpu
(defn main []
  (= points (into-dynamic-array [(vec2f 0.) (vec2f 1.) (vec2f 0.5)])))
```

### `push`

```easl
(push arr: [T] value: T): [T]
(insert arr: [T] index: u32 value: T): [T]
(remove arr: [T] index: u32): [T]
(reverse arr: [T]): [T]
```

Return a new array with `value` appended, inserted at `index`, the element at `index` removed, or the order reversed; `arr` itself is unchanged, so growing an array is `(= arr (push arr x))`. [`concat`](cpu-builtins.md#concat) joins arrays. CPU and audio code only.
<!-- index: insert remove reverse -->
