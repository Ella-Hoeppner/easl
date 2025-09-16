# easl

Enhanced Abstraction Shader Language

WIP shader language with a lispy syntax that compiles to WGSL.

Feature goals:
  * full feature parity with WGSL - anything that can be expressed with WGSL will also be directly expressible with easl (easl may eventually support glsl as a compilation target as well, though this is not a priority for now)
  * fully expression-based, including scoped `let` blocks that return values, and inline `if` and `match` statements
  * a powerful type system supporting pervasive type inference, function overloading, and Constraints (similar to typeclasses/traits, but able to coexist with ad-hoc overloading)
  * rust-style enums/sum types and pattern matching
  * closures and higher-order functions
  * tuples and anonymous structs
  * clojure-like `loop` construct for tail-recursion-style iteration
  * type holes

## todo
### high priority, necessary to call the language 0.1
* enums
  * finish implementing `bitcastable_chunk_accessors` for nested enums
  * finish implementing `bitcasted_from_enum_data_inner` for structs and arrays

* change `var` address space and access declaration system to use the metadata system rather than the special-cased `[]` form
  * so instead of `@{group 0 binding 0} (var [uniform] ...)`, you would do `@{group 0 binding 0 address uniform} (var ...)`
    * access, like `read` or `read-write` for `storage`-addressed vars, will be declared with the `access` metadata property
      * or, as a shorthand, you should be able to do like `@{address storage-read}`/`@{address storage-read-write}`
  * default address space for top-leve vars if none is provided will be `private`

* rename `=` to `set!` or `set` or something (maybe `:=`), then make `=` be equality checking, i.e. make it do what `==` currently does
  * mutable variables don't come up *that* often, so reserving something as simple as `=` for it feels weird.

* there are several places where gensyms are generated, but not guaranteed to be completely safe. Need to have a system that tracks all names in the program and allows for safe gensym-ing
  * cases where we need this:
    * deshadowing
    * deexpressionify
    * desugar_swizzle_assignments
    * `->` bindings
      * guess whatever system this is should be made available for macros in general

* make compiler usable as a command-line tool, in addition to a library
  * example usages
    * `easl compile shader.easl --output shader.wgsl`
      * if `--output` isn't specified then it'll just use the same file name but with `.wgsl` extension
      * maybe also just support like `easl shader.easl`, skipping the `compile`. So like the default thing the CLI does is compile to a file, if you don't specify `run` or `fmt`
    * `easl run shader.easl --fragment frag`
      * runs shader as a purefrag
    * `easl run shader.easl --vertex vert --fragment frag --vertices 300`
      * runs vertex and fragment shader. If `--vertex` is specified, `--fragment` and `--vertices` must also be specified
    * `easl fmt shader.easl`
      * formats file, support `--output` option but default to just overwriting the file
  * when running, there'll need to be at least some basic uniforms available. I guess for now just like, resolution and time (seconds since startup)
    * can make this more sophisticated eventually once the CPU-side of the language is usable, but for now it's more just for demo
  * need to make errors more human-readable




* anonymous structs

* tuples
  * treat this as basically a special case of anonymous structs
  * what should syntax be?
    * would be nice to just use `[]` but I'm using that for arrays already...
      * would it be possible to overload `[]` to use it for both things? type inference might be tricky
    * guess I could do like `{[]}` or `{()}`? That kinda sucks though
    * `'()` would work I guess but i also kinda hate that

* closures

* support a `@render` function tag that acts as a fragment and vertex shader in one
  * Basically it'll act like a vertex shader that returns a fragment shader
  * so it'll just have to be a function with a return type of like `[]`

* finish support for constraints
  * support declaring custom type constraints
    * each constraint is just defined by a function
    * what should the syntax be?
      * `(constraint T: Add (fn + [a: T b: T]: T))`
      * `(constraint Add (fn + [a: _ b: _]: _))`
      * `(constraint Add (defn + [a: _ b: _]: _))`
    * also support declaring type constraints that are the combinations of others, e.g.:
      * `(constraint Arithmetic [Add Sub Mul Div])`
  * Right now each generic argument in a function signature is associated with a `Vec<TypeConstraint>`. But instead each should be associated with a single `TypeConstraint`, which should itself be an enum with a `Union(HashSet<Self>)` variant for when an argument has more than one constraint.
    * It will be important to get the `Eq` implementation for these right, such that `catch_duplicate_signatures` can properly detect when two signatures will be recognized as the same even if the corresponding generic constraints are written in different orders
    (e.g. `(defn |T: [A B]| [a: T]: T ...)` and `(defn |T: [B A]| [a: T]: T ...)` should be treated as the same)

* `catch_duplicate_signatures` needs to be extended to catch partial overlaps in the type-domains of generic functions
  * like, right now you can implement `(defn + [a: f32 b:f32]: f32 ...)` without error, even though this conflicts with the builtin definition `(defn + |T| [a: T b:T]: T)`. The two signatures aren't equal, so no error is detected, but it should be, since the `f32` implementation is just a special-case of the generic definition.
    * this needs to not only catch the "non-generic signature is a special case of generic signature" case, but also the "two generic signatures overlap" case, like `(defn f |T: Scalar| [a: T]: T)` and `(defn f |T: Integer| [a: T]: T)`

* Overhaul references
  * References shouldn't be first-class values. You shouldn't be able to create a binding of type `&f32` or anything like that. Functions can mark some of their arguments as references, but you'll just pass normal values for those arguments, and the fact that it's a reference will always be inferred - there will be no syntax for constructing a reference.
  * Probably get rid of the special syntax for the reference types too, just use a `@ref ...` metadata syntax or maybe a `(Ref ...)` generic struct syntax
  * On the GPU-side it seems like references are basically only ever used for `arrayLength` and atomic stuff. So I don't really know why a user would ever want to define a function that uses references outside of the context of atomics.
    * I guess maybe if you have really big data structures and you wanna pass them to helper functions sometimes it might be more efficient to pass around pointers rather than having them be copied when they're passed to functions (though the compilers probably avoid that kind of copying typically I assume?)
    * On the CPU-side version having good support for references in user-defined functions will definitely be important, though. So it's important to get this right.
  * Mutable references should be a different thing. Also second-class, and only exist as function inputs.
  * The semantics for this will basically just be that
    * Arguments of type `T` can only accept a `T`
    * Arguments of type `&T` can accept a `T`, `&T`, or `&mut T`.
    * Arguments of type `&mut T` can accept a `&mut T`, or a `T` so long as that binding is marked as a `var`.
  * I guess there should be a `clone` function auto-derived on most types so that you can turn `&T` or `&mut T` into a `T`. But this shouldn't be defined for certain special built-in types like atomics(?) or textures.
    * certain types should also basically have something like rust's `copy` where it basically inserts an implicit `clone` when you try to use a `&T` or `&mut` as a `T`.

* formatter: 
  * have a separate threshold for the max size allowed for top-level `(def ...)`
    * a lot of things that would be perfectly readable on one line are getting split to multiple lines, feels like `def`s should almost always be one line unless they're very long
  * let top-level struct-like metadata appear on one line if it's under some threshold (probably should be another separate threshold)

* improve error messages
  * rn if a function is overloaded and it's arguments don't resolve to a valid signature, then the application expression using that function also won't resolve, even if the function always returns the same type
    * for instance, in the expression `(vec4f (vec3f 1. 1.) 1.)`, then inner application of `vec3f` is invalid since recieves 2 scalar args. However, if you try to compile this epxression, you'll also get an error on the `vec4f` saying "Couldn't infer types". But that shouldn't happen - regardless of what the input types to `vec3f` are, the return type will be `vec3f`, so it doesn't make sense for `vec4f` to not be able to converge
  * When there are multiple "couldn't infer types" errors, such that one is a subtree of another, only display the innermost one, I think? The outermost one usually won't be helpful.

* allow indexing vectors and matrices with integers

* add builtin `(Into T)`/`(defn into [a: A]: B)` constraint
  * have a unary prefix operator `~` that is shorthand for this
  * Currently vec constructors' arguments are restricted with `Scalar`, but should use this instead to make them more general
  * Currently there's some ugly special-case logic in `TypedExp::compile` that uses `builtin_vec_constructor_type` to wrap all the arguments of vector constructors because of cases like `(vec2f 1u)` where the argument type doesn't line up with the type of the vector. I don't want the user to have to do explicit casting, so it just inserts a cast around each argument (even those that are of the right type - it doesn't check). Once we restrict vec constructors with `(Into T)` rather than `Scalar` it should be possible to make that logic a lot cleaner. Like, for each argument of type `(Into T)`, if the argument is `T` then just let it pass through without a wrapper, but if it's anything else then wrap it in the corresponding scalar/vector cast function. Those casting functions which will basically be their implementation of `(Into T)`, so maybe it can even be treated more generally as like these two principles:
    * for any builtin function that we treat as accepting `(Into T)`-constrained arguments, during compilation, wrap each argument that isn't directly `T` in the corresponding `into` function.
    * the specific signatures of `into` that targets those scalar types should special-case-monomorphize to just being the name of the scalar.
  * Allow associative functions to have signatures like `(Fn [A: (Into T) B: (Into T)]: T)`, and in the case of n arguments, generalize this to n different generic arguments.
    * without this, things like `(+ (vec3f 0.) (vec3f 1.) 5.)` aren't allowed, but they have a very unambiguous meaning and disallowing them seems wrong given that you could just as well expand them to two applications of `+`.
  * Add a general rule to the typechecker, such that if ever typechecking would fail because it can't resolve a `OneOf` down to a `Known` on a function argument of type `(Into T)`, such that one of the `OneOf` possibilities is just `T`, then assume that the argument is of type `T`, and proceed with typechecking.
    * Right now the expression `(vec4f 1)` fails to typecheck, because it can't decide whether to type the `1` subexpression as a `F32`, `I32`, or `U32`. This happens because `vec4f` can technically accept any of those since the argument is just constrained by `Scalar`, and `1` can syntactically be any of the scalar types. But if instead the argument was constrained by `(Into F32)`, then the above rule would allow this to typecheck properly.

* special casing around `Texture2D`
  * right now I've made it so it has a field `_: T`, because monomorphization needs there to be at least one field, but this is kinda weird
  * you definitely shouldn't be able to access the `_` field, and you shouldn't be allowed to construct it
  * might need this for other types too? Maybe have a like `external_only` or `opaque` type that prevents it from being constructed or from having it's fields accessed 
  * or maybe have something like rust's `PhantomData`?

### low priority, extra features once core language is solid
* so much of the code in `AbstractEnum` and `AbstractStruct` is incredibly similar, should abstract those out to share most of that functionality for maintainability. Same for the `UntypedEnum`/`UntypedStruct` and also just `Enum`/`Struct
  * main difference is that enum has `variants` while struct has `fields`, but ultimately both of these are just associations between names and types, and in most helper functions they get treated the same way, so those can just turn into some multi-purpose `attributes` or something

* allow arguments to be declared as `@var` by just wrapping the whole body in a `let` that shadows the arguments

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
