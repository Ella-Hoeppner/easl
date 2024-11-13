# tynt

WIP shader language with a lispy syntax (using [SSE](https://github.com/Ella-Hoeppner/SSE)) that compiles to WGSL.

Feature goals:
  * full feature parity with WGSL - anything that can be expressed with WGSL will also be expressible with tynt
  * fully expression-based, including scoped `let` blocks that return values, and inline `if` and `match` statements
  * full type inference, including for function arg types and return types
  * support for macros/preprocessing, defined in rust
  * closures and higher-order functions
  * tuples and anonymous structs
  * typeclasses
  * clojure-like loop construct for tail-recursion-style iteration
  * type holes
  * sum types (probably, eventually)

## todo
### steps to get to expressive parity with wgsl/glsl
* add comments
  * both expression-level comments with `#_` and line-level comments with `;`

* support declaring custom type constraints
  * each constraint is just defined by a function
  * `(constraint T: Add (fn + [a: T b: T]: T))`
  * `(constraint Add (fn + [a: _ b: _]: _))`
  * `(constraint Add (defn + [a: _ b: _]: _))`

* restrict vec constructor with an `(Into T)` constraint that ensures the arg can be converted into the type of the type contained within the vector
  * right now, because we need to be able to support `(vec2 1i 2u)`, cases like `(vec2 1i (vec4 1i))` don't give good errors, since the constructor just looks like `(vec2 T1 T2)` and there's nothing preventing `T2` from being filled in with `vec4`

* make `def` work
  * should just compile to a const, I guess?

* loops
  * `for`
  * `while`
  * `break` and `continue`

* better validation of match blocks
  * return errors rather than panicking in `TypedExp::compile` when a match block is invald
  * check that there's only one wildcard arm 

* add macros and syntactic conveniences
  * `if` statements as sugar over `match`
  * parse integer literals with ambiguous types, to allow them to be treated as floats
  * handle inline accessor syntax like `a.x`
  * shadowing
  * internal lets (i.e. lets inside applications) and blocks
  * threading macros
  * have a special case for associative functions like +, *, min, and max, allowing them to be called with n arguments
  * convert - and / calls with 1 arg to calls to negate and invert, respectively
  * add typeclass combination aliases, e.g. Arithmetic as a combination of Add, Subtract, Negate, Multiply, Divide
    * Arithmetic should be built-in, but should also support user-defined aliases


* write a bunch of tests
  * the current shaders can be converted into tests, but there should also be test cases for invalid programs that ensure the right kinds of errors are returned

* add source tracing and helpful error messages for all compilation errors

### extra features, once core language is solid
* support arrays

* support higher-order functions
  * any invocation of one of these functions will need to have that argument inlined
    * for now there can be a restriction that the values for these arguments must always be constant, support non-constant fn args via dynamic dispatch later

* support anonymous structs/structural
  * each anonymous struct compiles to a concrete struct with a name derived from its field and types

* support tuples
  * compile to structs

* support impl'ing typeclasses on types
    * can test this by impl'ing Add on bool
  * support impl'ing on typeclass combination aliases
    * this should require providing a signature for all functions in each typeclass within the alias

* support user-defined typeclasses

* clj-style `loop` construct

* add closures, lambda lifting
  * closures will need to be compiled to a struct holding captured values, along with a corresponding function for "invoking" instances of the struct

* add typeclasses for trig ops (including a Trig combo alias), exp, exp2, log, log2, pow, ceil/floor, abs, clamp, length, distance, dot, fract, mod, sign, smoothstep, sqrt, step