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
* generic functions

* add typeclasses
  * support generic arguments bound by typeclass
    * monomorphization
    * account for ambiguity of function type variables in type inference
  * core built-in number typeclasses: Add, Subtract, Negate, Multiply, Divide
    * these will be automatically defined on the built in number + vector types

* support `match` expressions
  * treat if statements a special case of match blocks
  * want logic for exhaustivity checking. For now the only exhaustible type is `Bool`, so basically for everything other than `Bool` there should be a requirement to have an "other" arm in the match block
    * eventually I'll probably try to support sum types, and for those I'll want exhaustivity checking to, so maybe have like an `is_exhaustable` fn on Type or smth that for now just only returns true for `Bool`

* loops
  * `for`
  * `while`
  * `break` and `continue`

* make vectors generic
  * `vec<n>f` should be an alias to `(vec<n> f32)`
    * so I'll need to implement aliases as part of this

* add macros and syntactic conveniences
  * parse integer literals with ambiguous types, to allow them to be treated as floats
  * handle inline accessor syntax like `a.x`
  * shadowing
  * internal lets (i.e. lets inside applications) and blocks
  * threading macros
  * have a special case for associative functions like +, *, min, and max, allowing them to be called with n arguments
  * convert - and / calls with 1 arg to calls to negate and invert, respectively
  * add typeclass combination aliases, e.g. Arithmetic as a combination of Add, Subtract, Negate, Multiply, Divide
    * Arithmetic should be built-in, but should also support user-defined aliases

* add source tracing and helpful error messages for all compilation errors

### extra features, once core language is solid
* support higher-order functions
  * any invocation of one of these functions will need to have that argument inlined
    * for now there can be a restriction that the values for these arguments must always be constant, support non-constant fn args via dynamic dispatch later

* Support anonymous structs
  * each anonymous struct compiles to a concrete struct with a name derived from its field and types

* Support tuples
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