# easl

WIP shader language with a lispy syntax (using [SSE](https://github.com/Ella-Hoeppner/SSE)) that compiles to WGSL.

Feature goals:
  * full feature parity with WGSL - anything that can be expressed with WGSL will also be expressible with easl
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
### high priority
* clean up/simplify type parsing, I've got `Type::from_easl_tree`, `AbstractType::from_easl_tree`, and `AbstractType::from_ast` that all seem like they have pretty overlapping functionality, probably don't need all three

* allow type annotation on binding names in let blocks, e.g. `(let [x: f32 0] ...)` currently crashes because it can't handle the type annotation on `x`

* implement `TypedExp::validate_match_blocks`
  * ensure that
    * there's only one wildcard
    * the wildcard appears at the end
    * all patterns are just literals
    * no patterns are repeated
    * the wildcard doesn't appear if the other patterns would already be exhaustive, i.e. you can't have `true` and `false` and a wildcard case when matching a bool

* mark some functions like `+`, `*`, `min`, and `max` as "associative", allowing them to be called with n arguments
  * the type system should potentially handle this for any binary function it would be cool to have this as a thing that can be exposed to the user from within the language, e.g. `(defn @associative f [] ...)`

* add a 1-argument arity to `/` that acts as `#(/ 1 %)`
  * this might be a little bit tricky in that this is the first case where will be a bit tricky in that this is the first instance where we need *some* of the implementations of a function to be compiled and some don't

* shadowing
  * seems to typecheck fine but wgsl actually doesn't allow shadowing within the same scope, so I guess during compilation we could track bindings and change shadowed names

* return an error if control flow expressions are used outside their proper context
  * `break` and `continue` can only be used inside a loop
  * `discard` can only be used inside a `@fragment` function

* better validation of match blocks
  * return errors rather than panicking in `TypedExp::compile` when a match block is invald
  * check that there's only one wildcard arm

* support declaring custom type constraints
  * each constraint is just defined by a function
  * `(constraint T: Add (fn + [a: T b: T]: T))`
  * `(constraint Add (fn + [a: _ b: _]: _))`
  * `(constraint Add (defn + [a: _ b: _]: _))`

* support declaring type constraints that are the combinations of others, e.g.:
  * `(constraint Arithmetic [Add Sub Mul Div])`

* restrict vec constructor with an `(Into T)` constraint that ensures the arg can be converted into the type of the type contained within the vector
  * right now all the generics are restricted with `Scalar`, which works well enough for all the built-in vec types, but doing `(Into T)` instead should make it possible to have the same convenience with vectors of custom types (e.g. complex numbers)

* add a special case for inferring the type of vectors/scalars when it would normally get stuck due to being inside another vector constructor
  * e.g. right now `(vec4f 1)` fails because it can't tell if the `1` is a float, int, or uint - it could be any since vec4f can accept any of those. But since it will be converted to a float regardless, its type doesn't actually affect the semantics of the program, so it's silly to throw a type inference error. It should just infer it to be a float, or more generally, ambiguous number literals can be inferred to be the same type as the surrounding vector
  * this isn't just an issue for scalars though, since `(vec4f (vec3 0f) 1f)` would also fail to compile due to not being able to infer the type of `(vec3 0f)` - just as with a scalar it could be a float, int, or uint vector, and would be converted to a float vector either way, so the type ambiguity doesn't affect the semantics and it should just be assumed to be the same type as the outer vector
  * I think the inference rule that really needs to be implemented is:
    * If an expression is a vector with generic type T1 constructor, and one of it's types is either a number literal of unknown type T2 or a vector constructor with unknown generic type T2, constrain T1 with T2
  * hmm as I think about this more, there might be a more general inference rule that could solve this, one that works on `(Into T)` rather than on vectors
    * once `(Into T)` exists, the signature for a vector constructor will change from looking like `(fn (vec4f T A: Scalar) [a: A]: (vec4f T))` to `(fn (vec4f T A: (Into T)) [a: A]: (vec4f T))`. All scalars will implement `into` for one another, so this will feel exactly like the current approach. But it will also get stuck in the same place, because all the scalars satisfy `(Into ...)` for whatever the type of the vector is. But this could be resolved by a special inference rule: If type inference stalls with the type of a function argument being narrowed down to one `OneOf` several possibilities including some particular type `T`, and that function argument is constrained by `(Into T)`, collapse the `OneOf` into `Known(T)`
      * a practical benefit of this is that it'll work on custom vector types, like if someone defined `vecc` as a vector of complex numbers, and implemented `(Into Complex)` for floats, then `(vecc 1.)` work automatically. It'll also work on any custom types that expect `(Into ...)` that could run into ambiguity

* support lifting internal `let`s and `if`s

* matrices

### low-priority extra features, once core language is solid
* optimize
  * split `propagate_types` into two functions, one which happens only once, and one which gets called repeatedly. Much of the logic in `propagate_types` needs to happen once but is wasteful if done repeatedly

* special casing around `Texture2D`
  * right now I've made it so it has a field `_: T`, because monomorphization needs there to be at least one field, but this is kinda weird
  * you definitely shouldn't be able to access the `_` field, and you shouldn't be allowed to construct it
  * might need this for other types too? Maybe have a like `external_only` or `opaque` type that prevents it from being constructed or from having it's fields accessed 

* Optimize
  * probably using `Rc<str>` in place of `String` in most places would be a significant improvement
    * probably should do this in SSE too
  * keep abstract ancestors (for functions and structs) in `Rc`s

* write a bunch of tests
  * the current shaders can be converted into tests, but there should also be test cases for invalid programs that ensure the right kinds of errors are returned
  * maybe the tests should like actually feed the output into a wgsl compiler? Could even use hollow to like open a window that shows all the different things so that it can be visually checked whether everything is working

* implement a more specialized/optimized version of `mutually_constrain` that doesn't just rely on calling `constrain` twice to handle both directions

* allow arguments to be declared as `@var`
  * oh huh wgsl doesn't actually support this... I guess this could be supported by just sorta macroexpanding `@var` arguments into functions with a let block that shadows the arg

* as a special case of some kind, have there be a way to declare aliases for `vec` for any type, so that you could e.g. if you had a complex number type, you could do `(vec-alias-suffix c Complex)` and automatically get `vec2c`, `vec3c`, and `vec4c` types, and their variadic constructors

* support higher-order functions
  * any invocation of one of these functions will need to have that argument inlined
    * for now there can be a restriction that the values for these arguments must always be constant, support non-constant fn args via dynamic dispatch later

* support anonymous/structural structs
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
