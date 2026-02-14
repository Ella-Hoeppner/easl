# todo
## Highest priority
### necessary for basic wgsl feature parity + stuff I wanna get done before calling the language "production ready
* fix type output failure in `complex_monomorphize`

* have options on the builtin functions such that `print` isn't available when targetting wgsl
  * I guess maybe `validate_raw_program` should take like an enum describing the target or smth, and have a `TargetExclusive(PlatformEnum)` effect or smth for functions like `print`, and have a final stage in `validate_raw_program` that catches 
    * alternatively could just make it so `print` is allowed when compiling to wgsl but just doesn't do anything, I guess that's the behavior the user will want 99% of the time
  * a few test cases are failing the wgsl checks cause of this, should make a different success tester macro for these few cases that just runs typechecking and doesn't actually compile to wgsl
    * or maybe just roll these into the CPU tests idk



* when a match pattern is just a name, make it act basically as a wildcard and just bind that name to whatever the value is in the body

* `*=` doesn't work when multiplying a vec by a matrix

* allow importing from other files
  * for now just a simple string-concatenation approach will be fine I think, eventually probably want a proper module system but for now this will be enough
    * I guess file origin will need to be tracked as part of a `SourceTrace`? might be tricky to get that right

* atomics
* barrier functions
* `workgroupUniformLoad`
* subgroup functions
* quad functions

* extended texture types:
  * 1d, 3d textures
  * texture arrays
  * multisampled textures
  * sampler_comparison
  * texture_storage
  * all the functions that use these

* `catch_duplicate_signatures` needs to be extended to catch partial overlaps in the type-domains of generic functions
  * like, right now you can implement `(defn + [a: f32 b:f32]: f32 ...)` without error, even though this conflicts with the builtin definition `(defn (+ T) [a: T b:T]: T)`. The two signatures aren't equal, so no error is detected, but it should be, since the `f32` implementation is just a special-case of the generic definition.
    * this needs to not only catch the "non-generic signature is a special case of generic signature" case, but also the "two generic signatures overlap" case, like `(defn (f T: Scalar) [a: T]: T)` and `(defn f |T: Integer| [a: T]: T)`

* formatter: 
  * have a separate threshold for the max size allowed for top-level `(def ...)`
    * a lot of things that would be perfectly readable on one line are getting split to multiple lines, feels like `def`s should almost always be one line unless they're very long
  * `defn`s where the fn has generic variables, and therefore the first inner form looks like `(fn-name generic-name)`, get formatted weird
  * let top-level struct-like annotation appear on one line if it's under some threshold (probably should be another separate threshold)
  * special-case the formatting of other special forms when inside a `->` expression
    * right now matches get formatted like:
      ```
      (match x
        0 a
        1 b
        _ c)
      ```
      with each case and arm on the same line. But if you try to restructure this with a `->`, it ends up getting formatted like this:
      ```
      (-> x
          (match 0
            a 1
            b _
            c))
      ```
      with the cases and arms kinda mismatched. It's trying to just apply the same logic that it does normally, but it mistreats the first case as if it were the scrutinee. Ideally it should be formatted like:
      ```
      (-> x
          (match 
            0 a
            1 b
            _ c))
      ```
      Just kinda leaving the spot of the scrutinee blank and then formatting the rest as normal.
      * A similar issue happens with `if`, `when`, `while`.
      * However, I guess whether or not this special formatting should be adopted depends on whether there's an explicit `<>` used in the threading expression, cause if so then it shouldn't do this special case but instead just do what it currently does. Like if you instead wanted to restructure the above example to thread through one of the arms instead of the scrutinee and therefore couldn't rely on the default implicit `<>` positioning:
        ```
        (-> a
            (match x
              0 <>
              1 b
              _ c))
        ```

* improve error messages
  * rn if a function is overloaded and it's arguments don't resolve to a valid signature, then the application expression using that function also won't resolve, even if the function always returns the same type
    * for instance, in the expression `(vec4f (vec3f 1. 1.) 1.)`, then inner application of `vec3f` is invalid since recieves 2 scalar args. However, if you try to compile this epxression, you'll also get an error on the `vec4f` saying "Couldn't infer types". But that shouldn't happen - regardless of what the input types to `vec3f` are, the return type will be `vec3f`, so it doesn't make sense for `vec4f` to not be able to converge
  * When there are multiple "couldn't infer types" errors, such that one is a subtree of another, only display the innermost one, I think? The outermost one usually won't be helpful.

* allow indexing vectors and matrices with integers
  * It's already possible to use the `(f a)` syntax where `f` is something other than a function, but for now it only works when `f` is an array. So I guess I can just extend that logic to check if `f` is a vector or matrix as well?
    * Or maybe have some constraint like `Indexable` for this and make arrays, vecs, and mats implement it, so there's less special casing. And in the long run even maybe let the user implement `Indexable` on their own types.

* special casing around `Texture2D` and `Sampler`
  * right now I've made it so it has a field `_: T`, because monomorphization needs there to be at least one field, but this is kinda weird
  * you definitely shouldn't be able to access the `_` field, and you shouldn't be allowed to construct it
  * might need this for other types too? Maybe have a like `external_only` or `opaque` type that prevents it from being constructed or from having it's fields accessed 
  * or maybe have something like rust's `PhantomData`?

* flesh out support for `break`
  * Right now if you try to `break` inside of a `match` block, you get invalid wgsl code, since the compiled wgsl `break` will just get caught by the surrounding `switch`
    * demonstrated in the `break_in_match.easl` test
    * so there needs to be a more sophisticated system for matching individual `break` statements up with the specific scopes that they should break too, and adding extra variables to track `break`s that cross boundaries
  * support a `breakable` construct, such that you can surround a block and then call `break` arbitrarily inside it
    * should make it so that these `breakable` expressions can optionally return a value, and the `break` used inside them will accept a value of the proper type as an "argument". So basically make `break` just take an argument in general, and make the zero-argument `(break)` syntax just be shorthand for a unit argument
    * maybe go ahead and support labels on this too, so you can do like:
      ```
      (breakable 'a
                  ...
                  (if ...
                    ...
                    (break 'a ...)))
      ```
      * not sure how easy this will be, if it turns out to be tricky it can be deprioritized
      * if we do this, make it possible to add labels to for/while loops too, e.g. `(for 'a [...] ...)` or `(while 'b ...)`
        * might as well support this for `continue` statements as well if we get to this point

* support a special `TODO` operator that acts like rusts `todo!`, i.e. will satisfy the typechecker by taking on whatever type it needs to while you work on other parts of the program
  * I guess for now this should just prevent compilation? But it shouldn't be a normal compilation error I don't think, or at least it shouldn't show up in the LSP or the CLI `check` command as such.
    * On the cpu side of things it would be fine to run code containing this and just crash if it's encountered, but can't really do that on the GPU
    * maybe allow compilation so long as it's a zeroable type? But still would need to prevent compilation if it's ever used as e.g. a texture

* support builtin functions that don't just directly correspond to any wgsl functions
  * current ones I can think of:
    * `bi->uni`, `uni->bi`
      * eventually these should be generic over some `T: |(Add f32) (Mul f32)|`, but for now can just do them over the vecf types
    * `rot: (Fn [f32]: mat2x2f)` 
  * these will need to be represented as `FunctionImplementationKind::Composite`s, but will just have their definitions builtin ratehr than being user-defined
  * need to make sure these don't always get compiled to the output if they aren't used, though, unlike normal composite functions

## Secondary priority
### nice features to have once the language is at wgsl-parity
* support destructuring structs in `match` and `let` blocks

* more threading macros
  * `->=`
    * shorthand for `(= x (-> x ...))`
    * requires that the final type be the same as the initial type, but may support different types in between
  * `cond->`
    * should be similar to clojure's, but with the same ability as `->` to designate the positioning of the threaded expression with `<>`
    * I guess every step will need to be of the same type, since the steps are optional and therefore can't reliably change the type of the expression passing through
    * also allow the `<>` to be used in the conditions 
      * e.g.
        ```
        (cond-> x
          (< y 0.5) (* 5.)
          (> <> 3.) (- 1.))
        ```
      * this'll make it significantly more powerful than clojure's version
  * `cond->=`
    * just a combination of the above two
  

* consider making an effect type for each builtin, so that rather than specifying a builtin as part of the input to a shader stage, you can just call a function with the name of the builtin value, e.g. `(vertex-index)`. That acts as an effect, and at compile time this effect gets handled on each entry point by just inlining the argument into the entry that uses it (and to the helper functions that invoke it)
  * Maybe also have effects for setting these as output, e.g. `(set-frag-depth! 0.)` or `(set-position! 0.)`
  * just seems like it might be a more ergonomic alternative to having to add a bunch of extra fields to the struct
  * hmm, this would kinda cause a problem if you tried to explicitly use a builtin name as an input, since now those input names would have a name-collision with a top-level fn

* support vecs over arbitrary types
  * wgsl doesn't do this, it's just restricted to scalars and bool
  * These'll need to be compiled just like normal structs, and there'll need to be special logic for swizzling on these

* would be nice to expose `bitcast` to the user for arbitrary structs/enums
  * would need to throw an error when the types aren't of the same size I guess

* anonymous structs

* tuples
  * treat this as basically a special case of anonymous structs
  * what should syntax be?
    * would be nice to just use `[]` but I'm using that for arrays already...
      * would it be possible to overload `[]` to use it for both things? type inference might be tricky
    * guess I could do like `{[]}` or `{()}`? That kinda sucks though
    * `'()` would work I guess but i also kinda hate that

* implement `#` as a shorthand anonymous function syntax
  * like clojure's `#(...)`, but works on any kind of expression, e.g. `#[% 1.]` for a function that returns an array, or `#5` for a constant function that always just returns `5`.
  * I guess this at least needs to be able to represent a function of 0 args (if no `%` is present), or a function of 1 arg. Could also eventually extend it to support `%1` `%2` like clojure does for n-arg fns. 
    * But sometimes a `#(...)` expression that doesn't use `%` inside shouldn't actually be treated as a 0-arg fn, but instead a n>0-arg fn that just ignores it's argument(s). At least, it would be ideal to support that. This ambiguity doesn't occur with the `(fn [...] ...)` syntax since you at least have to name each argument even if you don't use them in the body, so the number of args is never ambiguous. But if we wanna support this in the `#` syntax then the *number of arguments* will itself need to be part of what gets inferred, which seems somewhat tricky.
      * as a workaround the user could always do `#(let [_ %] ...)` to indicate that there should be an argument in `%`, but then just not use it in the expression that actually gets returned. But it would be sorta lame to have to do that

* support a `@render` function tag that acts as a fragment and vertex shader in one
  * Basically it'll act like a vertex shader that returns a fragment shader
  * so it'll just have to be a function with a return type of like `[vec4f (Fn [] vec4f)]`

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

* make `Option` a built-in type, and have a few helper functions for it
  * maybe `Result` too?

* const generics, for making functions/types generic over array sizes
  * I think its probably fine to just support `u32` exclusively for this, that's the main usecase anyways
  * can just use the generic type syntax for this, but with a `: u32` after the generic name. So like a function generic over array size would look like
    ```
    (defn (sum N: u32) [arr [N: f32]]: f32
      (let [@var sum 0.]
        (for [i 0 (< i N) (= i (+ i 1))]
          (+= sum (arr i)))
        sum))
    ```

* flesh out the effect system
  * have a way to annotate the effects that a function has
    * I think this should be optional? Undecided
  * allow effects like `break`, `continue` to cross function boundaries
    * this will basically require any function that would normally return a `T` but can throw the `break` effect to essentially return a union of `T` and some unit `Break` type, and then match on that wherever the function is used to see if it threw the `break` effect and propagate it outwards if so.
      * this might be a good place to start introducing anonymous unions, but just used internally for now, not exposed to the user
  * allow user to define their own effect types, and have a built-in `(handle <handler> <code>)` operator
    * probably want to have some kind of support for like, mutable variables that are scoped to each individual instance of a handler. Not sure quite how to do that though
      * might need some kind of module system for this? But even if the variables were scope to the inside of some module, that would kinda imply that different handlers would all share the same state for those variables, which seems wrong
      * Maybe handlers should be structs that implement some interface, rather than functions? And then use the fields of the struct to represent whatever mutable state you'd wanna store?
        * so the interface might be like:
          ```
          (constraint (Handler T I R)
                      (defn handle [handler: &mut _ input: I]: Result<R, T>))
          ```
          it returns an `Result<R, T>` rather than just an `R` so the `Err(T)` case can be used to represent the case where control flow is violated

* introduce some built-in type constraints for row polymorphic functionality
  * I guess the core thing will be a constraint like `(HasField 'x T)`
    * any function that accepts an argument of a generic type with constraint `(HasField 'x T)` should be able to access the field `.x` on elements of that type inside its body
  * would also be really nice to include some builtin row-polymorphic functions like `assoc`, `dissoc`, `update`, `select-fields` with behavior analogous to clojure
    * can also have `update=` for modifying a mutable variable in place given a fn, following along the `+=` naming convention
    * these will need to take a special kind of value as an "argument", which is a symbol that corresponds to the field name being acted upon
      * so like `(assoc s 'field-name 5.)`, the `'field-name` here is a special thing that needs to be known at compile-time
        * I guess this could be represented as having a special `Label` built-in type, which can only be constructed by with this symbol-literal syntax. Make it so the user isn't allowed to ever define a function that returns a `Label`, or use `Label` as a field type in a struct. That way it's guaranteed by construction that any value of type `Label` that appears in the code will be knowable at compile time, while still making it possible for the user to define functions that accept `Label`s as inputs
    * so `update`'s type signature might be something like:
      ```
      (defn (update F S: (HasField field-name F))
            [s: S
             field-name: Label
             f: (Fn [F] F)]: S
            ...)
      ```
      this does require something a little weird, which is that the thing passed to `HasField` is technically a term-level value introduced as an argument to the function. But I think that can be supported as a special case, i.e. any `Label`-type argument to a function can be used in type constraints that need labels. I *think* that typechecking should still be decidable here as long as all `Label`s are guaranteed to be compile-time constants
    * alternatively if I don't wanna mix the term and type level like that, I guess all labels could be handled as type-level values. In this case I guess `Label` would be a constraint and each individual label would actually be a unit type. Kinda in the direction of how rust handles functions. An alternative type signature using this approach might look like:
      ```
      (defn (update F
                    N: Label
                    S: (HasField N F))
            [s: S
             field-name: N
             f: (Fn [F] F)]: S
            ...)
      ```
      * don't love this though for the same reason I don't like rusts way of handling functions. These labels *feel* like values, and the "they're actually unit types that-satisfy a constraint" thing feels hacky, and probably unintuitive to people not super comfortable with type-level stuff. Though I guess these singatures will be confusing for those kinds of people either way. I think I'll probably go with the term-level `Label` approach and fall back to this type-level approach if it turns out to be hard to make decidable
    * The signature for `assoc` will be a bit more complicated since changes the shape of the struct:
      ```
      (defn (assoc F 
                   S: Struct
                   T)
            [s: S
             field-name: Label
             value: T]: (Assoc S field-name T)
            ...)
      ```
      I guess this technically should work for the previous one too, don't actually need multiple signatures. `Assoc` here will be a special type-level operator that takes in a struct, a label, and another type, and returns the struct with the same structure as the old one but with the new field added. If the original struct already had a field with that name it will be changed to the new type, otherwise it will just be added as a new field of the provided type. `Struct` will also be a type constraint that all structs satisfy, but that enums, primitives, and function types don't. `T: (HasField ...)` entails `T: Struct`
    * `dissoc` will look like:
      ```
      (defn (dissoc S: |Struct (HasField field-name)|)
            [s: S
             field-name: Label]: (Dissoc S field-name)
            ...)
      ```
      `Dissoc` will be another special type-level operator. I also use `HasField` here with no second argument. When used this way it should accept any struct with a field of that name, regardless of the type of the field.
      * might be nice to support the variadic case of `dissoc` as well, but not essential if that turns out to be tricky for any reason
    * `select-fields` is a bit harder to describe. Presumably it should accept n arguments for n different `Label`s, or accept an array of `Label`s. Either way that makes it hard to express the constraint on the struct type. I guess with a few more new type-level operators, it could look like:
      ```
      (defn (select-fields N: u32
                           S: (HasFields field-names)
                           T)
            [s: S
             field-names: [N: Label]]: (SelectFields S field-names)
            ...)
      ```
      `HasFields` accepts an array of `Label`s and accepts a type only if it contains all the field names. `SelectFields` accepts a struct and an array of field names and returns the struct with the specified subset of fields, of the same type that they appear in the input struct.
      * this will mean that arrays of `Label`, in addition to `Label`s themselves, will need to be able to cross the term/type boundary when used as a function arguments. But that seems fine - since `Label`s are guaranteed to be compile-time constants, so are arrays of `Label`s
    * might be nice for `update` to be able to change the type of the field...
      so the signature might need to look like
      ```
      (defn (update O N S: (HasField field-name F))
            [s: S
             field-name: Label
             f: (Fn [O] N)]: (Assoc S field-name N)
          ...)
      ```



## Low priority
### extra features once core language is solid
* interpreter and partial evaluator
  * can use rust_apfloat, https://github.com/rust-lang/rustc_apfloat, for deterministic float operations
   * at least when it's being run as a partial evaluator. Maybe have a mode to just use normal operations for extra speed when used in the interpreter, but keep that mode on when doing partial evaluation

* uniformity analysis of some kind? Want to make sure the user never gets errors from the compiled wgsl, so I guess some form of uniformity analysis mimicking wgsl's will be necessary

* support non-inlinable higher-order functions
  * e.g. right now if you had some fn `f` that accepts a function as an argument, you can call it like:
    ```
    (f inner-f)
    ```
    but you can't call it like
    ```
    (f (if b inner-f-1 inner-f-2))
    ```
    because the argument has to be inlinable at compile-time
  * to support this there will need to be some construct that contains a discriminant to identify which function is being passed around at runtime, along with whatever closure state either of them need. Another good use-case for internally using an anonymous union

* enhance match blocks
  * support nested destructuring of structs and enums
  * support multiple patterns grouped into one
    * can use the `|...|` enclosers for this, e.g.
      ```
      (enum Blah A B C)
      (match x: Blah
        |A B| ...
        C ...)
      ```
  * guard clauses on patterns
    * what would the syntax for this be?
    * I guess there could be a `(guard ...)` special form that can appear after an arm and can contain one or more boolean expressions. So there'd just be a check after each pattern for whether the next form is a `(guard ...)`, and if so the interpret it as a guard and use the following thing as the body
      * don't love that this means that a match statement might have an odd number of forms sometimes... makes structural editing a bit harder and feels too much like hidden structure
    * Could use a `@guard` annotation like:
      ```
      (match x
        None ...
        @guard [(Some x) (< x 5.)] ...
        (Some x) ...
        )
      ```
      so I guess the `@guard` annotation would always expect a `[]` following it, where the first element is the pattern and the remainder are boolean expressions that may use any values bound in the pattern
      * kinda ugly but I guess it would work? Don't really wanna introduce a new syntactic construct just for this...
    * instead of annotation syntax I guess it could also just be a special `(guard ...)` form, so like the above could be rewritten as:
      ```
      (match x
        None ...
        (guard (Some x) (< x 5.)) ...
        (Some x) ...
        )
      ```
      * a bit less ugly I think, but still not great

* some kind of module system, so that code can be broken up across multiple files, and imports can be scoped to ony particular parts of the codebase

* support extensions:
  * f16
  * clip_distances
  * dual_source_blending
  * subgroups
  * primitive_index

* so much of the code in `AbstractEnum` and `AbstractStruct` is very similar, should abstract those out to share most of that functionality for maintainability. Same for the `UntypedEnum`/`UntypedStruct` and also just `Enum`/`Struct
  * main difference is that enum has `variants` while struct has `fields`, but ultimately both of these are just associations between names and types, and in most helper functions they get treated the same way, so those can just turn into some multi-purpose `attributes` or something

* add a special case for inferring the type of vectors/scalars when it would normally get stuck due to being inside another vector constructor
  * e.g. right now `(vec4f 1)` fails because it can't tell if the `1` is a float, int, or uint - it could be any since vec4f can accept any of those. But since it will be converted to a float regardless, its type doesn't actually affect the semantics of the program, so it's silly to throw a type inference error. It should just infer it to be a float, or more generally, ambiguous number literals can be inferred to be the same type as the surrounding vector
  * this isn't just an issue for scalars though, since `(vec4f (vec3 0f) 1f)` would also fail to compile due to not being able to infer the type of `(vec3 0f)` - just as with a scalar it could be a float, int, or uint vector, and would be converted to a float vector either way, so the type ambiguity doesn't affect the semantics and it should just be assumed to be the same type as the outer vector
  * I think the inference rule that really needs to be implemented is:
    * If an expression is a vector with generic type T1 constructor, and one of it's types is either a number literal of unknown type T2 or a vector constructor with unknown generic type T2, constrain T1 with T2
  * hmm as I think about this more, there might be a more general inference rule that could solve this, one that works on `(Into T)` rather than on vectors
    * once `(Into T)` exists, the signature for a vector constructor will change from looking like `(fn (vec4f T A: Scalar) [a: A]: (vec4f T))` to `(fn (vec4f T A: (Into T)) [a: A]: (vec4f T))`. All scalars will implement `into` for one another, so this will feel exactly like the current approach. But it will also get stuck in the same place, because all the scalars satisfy `(Into ...)` for whatever the type of the vector is. But this could be resolved by a special inference rule: If type inference stalls with the type of a function argument being narrowed down to one `OneOf` several possibilities including some particular type `T`, and that function argument is constrained by `(Into T)`, collapse the `OneOf` into `Known(T)`
      * a practical benefit of this is that it'll work on custom vector types, like if someone defined `vecc` as a vector of complex numbers, and implemented `(Into Complex)` for floats, then `(vecc 1.)` work automatically. It'll also work on any custom types that expect `(Into ...)` that could run into ambiguity

* write a bunch of tests
  * the current shaders can be converted into tests, but there should also be test cases for invalid programs that ensure the right kinds of errors are returned
  * maybe the tests should like actually feed the output into a wgsl compiler? Could even use hollow to like open a window that shows all the different things so that it can be visually checked whether everything is working

* Optimize
  * split `propagate_types` into two functions, one which happens only once, and one which gets called repeatedly. Much of the logic in `propagate_types` needs to happen once but is wasteful if done repeatedly
  * implement a more specialized/optimized version of `mutually_constrain` that doesn't just rely on calling `constrain` twice to handle both directions

* as a special case of some kind, have there be a way to declare aliases for `vec` for any type, so that you could e.g. if you had a complex number type, you could do `(vec-alias-suffix c Complex)` and automatically get `vec2c`, `vec3c`, and `vec4c` types, and their variadic constructors

* clj-style `loop` construct

* arbitrarily sized unsigned integers

* untagged unions/anonymous sum types

* maybe try to have something like a "virtual fields" system that allows you to use field accessor syntax for things that aren't actually fields, but act somewhat like fields
  * e.g. I could imagine it would be nice to be able to do like `x.magnitude` if `x` is a vector. And not just getting it, but assigning into it, like `(= x.magnitude 5)` as an alternative to writing `(= x (* 5. (normalize x)))`. So this would involve "setters" as well as "getters" (setters could be optional, such that virtual fields which don't implement a setter can't be used this way, but the accessor can still be used to get the value)
  * This would be a nice way to make swizzling into more of a special case of a broader feature, rather than a purely special-cased concept on vectors.
    * unsure if making this elegance is actually worth the cost in terms of language complexity?

* replace the `@associative` thing with a more general system for handling variadic functions with a single signature. Something more like clojure's approach where a function can accept n arguments but recieve them as a data structure.
  * So like instead of
    ```
    @associative
    (defn my-operator [x: f32 y: f32]: f32
      ...)
    ```
    you would do something like
    ```
    (defn (my-operator N: u32) [& values: [N: f32]]: f32
      ...)
    ```
    here the `&` indicates "rest of the arguments", like in clojure. Not sure if I love using that symbol for this though, could go with something else
  * i dunno, maybe having `@associative` as a shorthand is still a bit nicer. If you just want to write a binary operator that can be used associatively with n arguments, it's nice to be able to write something that reflects that intent clearly, rather than having to write an explicit for loop over an array of inputs. But still should definitely have this variadic syntax since it can cover many other use-cases.
    * Maybe eventually treat `@associative` as something that invokes a macro macro or something and expands the provided definition into the explicit const-generic definition

* spread operator
  * Just seems like it would be a nice feature. Like if you had a vec3 and you wanted to create an array of 5 elements containing it's three fields and then two other constant values, instead of writing that as `[v.x v.y v.z 0 1]`, you could just write `[..v 0 1]`, where `..` is the unary prefix spread operator. Or if you wanted to convert an array of 3 elements to a vector it would just be `(vec3 ..a)` rather than `(vec3 (a 0) (a 1) (a 2))`. I guess in this case there'll probably be an `into` implementation anyways so it could just be `~a`, but I think there would be plenty of cases where this would be useful.
  * Could also use this as an intuitive bit of syntax for the "rest of the arguments" operator in variadic function signatures, e.g. 
    ```
    (defn (my-operator N: u32) [x: u32 ..values: [N: f32]]: f32
        ...)
    ```
