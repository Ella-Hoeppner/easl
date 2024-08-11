# tynt

WIP shader language with a lispy syntax (using [SSE](https://github.com/Ella-Hoeppner/SSE)) that compiles to WGSL.

Feature goals:
  * full feature parity with WGSL - anything that can be expressed with WGSL will also be expressible with tynt
  * fully expression-based, including scoped `let` blocks that return the values they produce, and inline `if` statements
  * full type inference, including for function arg types and return types
  * support for macros, defined in rust
  * closures and higher-order functions
  * extend vector arithmetic to arbitrary-sized arrays, including multidimensional arrays
  * tuples and anonymous structs