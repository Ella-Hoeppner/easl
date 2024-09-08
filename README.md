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
  * sum types (probably, eventually)

## todo
* support multiple distinct signatures for the same type, e.g. all the different signatures for `vec4f`

* treat accessors as functions, handle types accordingly

* support `let` expressions

* add typeclasses
  * support generic variables bound by typeclass
    * monomorphization
    * account for ambiguity of function type variables in type inference
  * core built-in number typeclasses: Add, Subtract, Negate, Multiply, Divide
    * these will be automatically defined on the built in number + vector types

* support `match` expressions
  * treat if statements a special case of match blocks
  * want logic for exhaustivity checking. For now the only exhaustible type is `Bool`, so basically for everything other than `Bool` there should be a requirement to have an "other" arm in the match block
    * eventually I'll probably try to support sum types, and for those I'll want exhaustivity checking to, so maybe have like an `is_exhaustable` fn on TyntType or smth that for now just only returns true for `Bool`
* add mutability and assignment functions
  * `=` will be the most important assignment operator at first
  * the type system should track whether each variable is mutable. This will be indicated with a "mut" metadata tag in a let block, e.g. `(let [@mut a: T] ...)`
    * assignment operators like `=` will check that their first 
* add `block` expressions
  * like clojures `do`. I think `block` is a better name (?)
  * support multiple expressions in the body of a `let`, read as a `block`
* add syntax for reading struct fields
  * same syntax as kudzu, ideally
  * will want to make sure assignment (and mutability checking) work with these, too

* add macros and syntactic conveniences
  * shadowing
  * internal lets (lets inside function applications)
  * threading macros
  * convert + and * calls with >2 args to chains of 2-arg calls
  * convert - and / calls with 1 arg to calls to negate and invert, respectively

* loops
  * `for`
  * `while`
  * `break` and `continue`

* add typeclass combination aliases, e.g. Arithmetic as a combination of Add, Subtract, Negate, Multiply, Divide
  * Arithmetic should be built-in, but should also support user-defined aliases

* support impl'ing typeclasses on types
    * can test this by impl'ing Add on bool
  * support impl'ing on typeclass combination aliases
    * this should require providing a signature for all functions in each typeclass within the alias

* support user-defined typeclasses

* support higher-order functions
  * any invocation of one of these functions will need to have that argument inlined
    * for now there can be a restriction that the values for these arguments must always be constant, support non-constant fn args via dynamic dispatch later

* Support anonymous structs
  * each anonymous struct compiles to a concrete struct with a name derived from its field and types
* Support tuples
  * compile to structs

* clj-style `loop` construct

* add closures, lambda lifting
  * closures will need to be compiled to a struct holding captured values, along with a corresponding function for "invoking" instances of the struct

* add typeclasses for trig ops (including a Trig combo alias), exp, exp2, log, log2, pow, ceil/floor, abs, clamp, length, distance, dot, fract, mod, sign, smoothstep, sqrt, step