# easl

Enhanced Abstraction Shader Language

WIP shader language with a lispy syntax that compiles to WGSL.

Feature goals:
  * full feature parity with WGSL - anything that can be expressed with WGSL will also be directly expressible with easl (easl may eventually support glsl as a compilation target as well, though this is not a priority for now)
  * fully expression-based, including scoped `let` blocks that return values, and inline `if` and `match` statements
  * a powerful type system supporting pervasive type inference, function overloading, and Constraints (similar to typeclasses/traits, but able to coexist with ad-hoc overloading)
  * rust-style enums/sum types
  * closures and higher-order functions
  * tuples and anonymous structs
  * clojure-like loop construct for tail-recursion-style iteration
  * type holes

## todo
### high priority
* formatter: 
  * have a separate threshold for the max size allowed for top-level `(def ...)`
    * a lot of things that would be perfectly readable on one line are getting split to multiple lines, feels like `def`s should almost always be one line unless they're very long
  * let top-level struct-like metadata appear on one line if it's under some threshold (probably should be another separate threshold)

* improve error messages

### medium priority, necessary to call the language 0.1

* allow indexing vectors and matrices with integers

* track `discard`, `break`, and `continue` with the effects system rather than the current ad-hoc system

* there are several places where gensyms are generated, but not guaranteed to be completely safe. Need to have a system that tracks all names in the program and allows for safe gensym-ing
  * deshadowing
  * `->` bindings
  * deexpressionify

* support declaring custom type constraints
  * each constraint is just defined by a function
  * what should the syntax be?
    * `(constraint T: Add (fn + [a: T b: T]: T))`
    * `(constraint Add (fn + [a: _ b: _]: _))`
    * `(constraint Add (defn + [a: _ b: _]: _))`

* support declaring type constraints that are the combinations of others, e.g.:
  * `(constraint Arithmetic [Add Sub Mul Div])`

* support reference types in userspace functions
  * Main thing here is that functions should be able to take references as inputs, e.g. `(defn f [x: &i32] (+= (deref x) 1))`. The parser already has `&` as an operator, and this should be usable both at the type level and at the term level for constructing a reference from a variable, though only when that variable is mutable. I think just having `deref` for derefencing is fine? It's kinda verbose but I don't think it's worth using a symbol just for a dereferencing prefix, dereferences will be pretty uncommon
  * Passing a reference to a mutable variable to a function should trigger the mutability special case of the `let`-lifter, just like if an assignment operator crossed the 

* special casing around `Texture2D`
  * right now I've made it so it has a field `_: T`, because monomorphization needs there to be at least one field, but this is kinda weird
  * you definitely shouldn't be able to access the `_` field, and you shouldn't be allowed to construct it
  * might need this for other types too? Maybe have a like `external_only` or `opaque` type that prevents it from being constructed or from having it's fields accessed 
  * or maybe have something like rust's `PhantomData`?

### low priority, extra features once core language is solid
* allow arguments to be declared as `@var` by just wrapping the whole body in a `let` that shadows the arguments

* restrict vec constructor with an `(Into T)` constraint that ensures the arg can be converted into the type of the type contained within the vector
  * right now all the generics are restricted with `Scalar`, which works well enough for all the built-in vec types, but doing `(Into T)` instead should make it possible to have the same convenience with vectors of custom types (e.g. complex numbers)

* Allow associative functions to have signatures like `(Fn [A: (Into T) B: (Into T)]: T)`, and in the case of n arguments, generalize this to n different generic arguments.
  * without this, things like `(+ (vec3f 0.) (vec3f 1.) 5.)` aren't allowed, but they have a very unambiguous meaning and disallowing them seems wrong given that you could just as well expand them to two applications of `+`.

* add a special case for inferring the type of vectors/scalars when it would normally get stuck due to being inside another vector constructor
  * e.g. right now `(vec4f 1)` fails because it can't tell if the `1` is a float, int, or uint - it could be any since vec4f can accept any of those. But since it will be converted to a float regardless, its type doesn't actually affect the semantics of the program, so it's silly to throw a type inference error. It should just infer it to be a float, or more generally, ambiguous number literals can be inferred to be the same type as the surrounding vector
  * this isn't just an issue for scalars though, since `(vec4f (vec3 0f) 1f)` would also fail to compile due to not being able to infer the type of `(vec3 0f)` - just as with a scalar it could be a float, int, or uint vector, and would be converted to a float vector either way, so the type ambiguity doesn't affect the semantics and it should just be assumed to be the same type as the outer vector
  * I think the inference rule that really needs to be implemented is:
    * If an expression is a vector with generic type T1 constructor, and one of it's types is either a number literal of unknown type T2 or a vector constructor with unknown generic type T2, constrain T1 with T2
  * hmm as I think about this more, there might be a more general inference rule that could solve this, one that works on `(Into T)` rather than on vectors
    * once `(Into T)` exists, the signature for a vector constructor will change from looking like `(fn (vec4f T A: Scalar) [a: A]: (vec4f T))` to `(fn (vec4f T A: (Into T)) [a: A]: (vec4f T))`. All scalars will implement `into` for one another, so this will feel exactly like the current approach. But it will also get stuck in the same place, because all the scalars satisfy `(Into ...)` for whatever the type of the vector is. But this could be resolved by a special inference rule: If type inference stalls with the type of a function argument being narrowed down to one `OneOf` several possibilities including some particular type `T`, and that function argument is constrained by `(Into T)`, collapse the `OneOf` into `Known(T)`
      * a practical benefit of this is that it'll work on custom vector types, like if someone defined `vecc` as a vector of complex numbers, and implemented `(Into Complex)` for floats, then `(vecc 1.)` work automatically. It'll also work on any custom types that expect `(Into ...)` that could run into ambiguity

* make
  ```
    structs: &Vec<Rc<AbstractStruct>>,
    skolems: &Vec<Rc<str>>,
    source_trace: SourceTrace,
  ```
  into something like "ConcretizationContext" or something, this exists in a bunch of places

* write a bunch of tests
  * the current shaders can be converted into tests, but there should also be test cases for invalid programs that ensure the right kinds of errors are returned
  * maybe the tests should like actually feed the output into a wgsl compiler? Could even use hollow to like open a window that shows all the different things so that it can be visually checked whether everything is working

* Optimize
  * split `propagate_types` into two functions, one which happens only once, and one which gets called repeatedly. Much of the logic in `propagate_types` needs to happen once but is wasteful if done repeatedly

* implement a more specialized/optimized version of `mutually_constrain` that doesn't just rely on calling `constrain` twice to handle both directions

* as a special case of some kind, have there be a way to declare aliases for `vec` for any type, so that you could e.g. if you had a complex number type, you could do `(vec-alias-suffix c Complex)` and automatically get `vec2c`, `vec3c`, and `vec4c` types, and their variadic constructors

* support higher-order functions
  * any invocation of one of these functions will need to have that argument inlined
    * for now there can be a restriction that the values for these arguments must always be constant, support non-constant fn args via dynamic dispatch later

* support anonymous/structural structs
  * each anonymous struct compiles to a concrete struct with a name derived from its field and types

* support tuples
  * compile to structs

* support enums

* clj-style `loop` construct

* add closures, lambda lifting
  * closures will need to be compiled to a struct holding captured values, along with a corresponding function for "invoking" instances of the struct

* add typeclasses for trig ops (including a Trig combo alias), exp, exp2, log, log2, pow, ceil/floor, abs, clamp, length, distance, dot, fract, mod, sign, smoothstep, sqrt, step
