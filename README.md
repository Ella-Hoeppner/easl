# tynt

WIP shader language with a lispy syntax (using [SSE](https://github.com/Ella-Hoeppner/SSE)) that compiles to WGSL.

Feature goals:
  * full feature parity with WGSL - anything that can be expressed with WGSL will also be expressible with tynt
  * fully expression-based, including scoped `let` blocks that return the values they produce, and inline `if` statements
  * full type inference, including for function arg types and return types
  * support for macros/preprocessing, defined in rust
  * closures and higher-order functions
  * tuples and anonymous structs
  * typeclasses
  * clojure-like loop construct for tail-recursion-style iteration
  * type holes

## todo
* get basic type inference and compilation working
  * for now all arithmetic only needs to work on floats
* support threading macros
* support shadowing
* convert + and * calls with >2 args to chains of 2-arg calls
* convert - and / calls with 1 arg to calls to negate and invert, respectively
* support internal lets (lets inside function applications)
* make functions first class
  * add a `Fn` type, i.e. `(Fn [In1 In2 ... InN] Output)`
  * add a `Function` case to `Exp`
* support higher-order functions
  * any invocation of one of these functions will need to have that argument inlined
    * for now there can be a restriction that the values for these arguments must always be constant, support non-constant fn args via dynamic dispatch later
* add type classes
  * support generic variables bound by typeclass
    * monomorphization
    * account for ambiguity of function type variables in type inference
  * core built-in number typeclasses: Add, Subtract, Negate, Multiply, Divide
    * these will be automatically defined on the built in number + vector types
    * have "Num" as a shorthand for the combination of all of these
  * support impl'ing typeclasses on types
    * can test this by impl'ing Add on bool
  * support impl'ing on typeclass combination aliases
    * this should require providing a signature for all functions in each typeclass within the alias
  * support user-defined typeclasses
  * support user-defined typeclass combination aliases
* Support anonymous structs
  * each anonymous struct compiles to a concrete struct with a name derived from its field and types
* Support tuples
  * compile to structs
* add closures, lambda lifting
  * closures will need to be compiled to a struct holding captured values, along with a corresponding function for "invoking" instances of the struct
* loop construct
* add typeclasses for trig ops (including a Trig combo alias), exp, exp2, log, log2, pow, ceil/floor, abs, clamp, length, distance, dot, fract, mod, sign, smoothstep, sqrt, step