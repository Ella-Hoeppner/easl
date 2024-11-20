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
* `constrain` and `mutually_constrain` should probably be combined into one thing, I don't think there are really any circumstances where we need to just do one-sided constraining, and having `mutually_constrain` do it both ways when that will usually be wasted effort just seems silly

* throw an error if control flow expressions are used outside their proper context
  * `break` and `continue` can only be used inside a loop
  * `discard` can only be used inside a `@fragment` function

* better validation of match blocks
  * return errors rather than panicking in `TypedExp::compile` when a match block is invald
  * check that there's only one wildcard arm

* add core macros and syntactic conveniences
  * `if` statements as sugar over `match`
  * parse integer literals with ambiguous types, to allow them to be treated as floats
  * handle inline accessor syntax like `a.x`
  * threading macros
  * have a special case for associative functions like +, *, min, and max, allowing them to be called with n arguments
  * convert / calls with 1 arg to calls to negate and invert, respectively
  * shadowing (might already work actually? not sure, needs testing)



* support declaring custom type constraints
  * each constraint is just defined by a function
  * `(constraint T: Add (fn + [a: T b: T]: T))`
  * `(constraint Add (fn + [a: _ b: _]: _))`
  * `(constraint Add (defn + [a: _ b: _]: _))`

* support declaring type constraints that are the combinations of others, e.g.:
  * `(constraint Arithmetic [Add Sub Mul Div])`

* restrict vec constructor with an `(Into T)` constraint that ensures the arg can be converted into the type of the type contained within the vector
  * right now all the generics are restricted with `Scalar`, which works well enough for all the built-in vec types, but doing `(Into T)` instead should make it possible to have the same convenience with vectors of custom types (e.g. complex numbers)



* support lifting internal lets

* as a special case, when there are nested `vec` constructors like `(vec4f (vec3 0) 1)`, allow the typechecker to assume that the inner `vec3` is a `vec3f`. Similarly



* write a bunch of tests
  * the current shaders can be converted into tests, but there should also be test cases for invalid programs that ensure the right kinds of errors are returned
  * maybe the tests should like actually feed the output into a wgsl compiler? Could even use hollow to like open a window that shows all the different things so that it can be visually checked whether everything is working

### extra features, once core language is solid
* allow arguments to be declared as `@var`
  * oh huh wgsl doesn't actually support this... I guess this could be supported by just sorta macroexpanding `@var` arguments into functions with a let block that shadows the arg

* as a special case of some kind, have there be a way to declare aliases for `vec` for any type, so that you could e.g. if you had a complex number type, you could do `(vec-alias-suffix c Complex)` and automatically get `vec2c`, `vec3c`, and `vec4c` types, and their variadic constructors

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
