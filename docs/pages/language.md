# Language guide

## Expressions and calls

Function calls are written by writing the name of the function within a pair of parentheses, followed by the arguments, each separated by whitespace (no commas needed). This same syntax is used even for arithmetic and assignment; Easl has no infix operators.

```easl
(f a b)        ; f(a, b);
(+ 1 2)        ; 1 + 2
(= a 5.)       ; a = 5.;
```

Since there are no infix operators, many special characters are free to be used in names, such as `-_!?*+><$%^&`. Functions and variables names in easl typically use **kebab-case** (`texture-sample`), while types use **PascalCase** (`Texture2D`).

## Comments

Easl has three types of comments, `;` for single line comments, `;* ... *;` for  block comments, and `#_` for expression comments.

```easl
; to the end of the line

;* a block comment,
   possibly spanning lines *;

#_(skips the next expression)
```

`#_` skips exactly one expression. This makes it different from other types of comments in that the content inside the comment must still parse to valid easl syntax, though it of course doesn't need to type-check. `#_` is very useful for temporarily disabling part of an expression.

## Literals and types

Eash has primitive integer types `u32`, `i32`, and `f32`. You can write an explicit literal of each type like `5u`, `5i`, `5.`, respectively, but you can can also do an ambiguous `5` and let the compiler figure it out (in most cases).

You can ascribe a type to an expression with the `:` operator, like `5: u32`. This ascription operator can occur anywhere in an expression including nested inside other forms, e.g.

```
(+ (* 4 5): f32 3)
```

## Let

`let` binds names in a scope. , and takes the value of the last one. Like everything in easl, it's an expression, usable anywhere a value is:

```easl
(let [x 5.
      y 6.]
  (+ x y))

(- 2.
   (let [b 5.]
     (* b b)))
```

Bindings are immutable unless marked `@var`:

```easl
(let [@var x 1.]
  (+= x 2.))
```

A local may shadow another local, but top-level names cannot be shadowed.

`let` expressions can yield values to their surrounding scope, meaning that they can be used even when you're deeply nested inside other code. For instance:

```
(+ 3.
   (let [x (/ 5. 2.)]
     (* x x)))
```

## Vectors

Vector types have 2–4 components of `f32`, `i32`, `u32`, or `bool`: `vec3f`, `vec2u`, `vec4b`. Constructors take any mix of scalars and vectors that adds up to the right size. Vectors also support swizzling syntax for accessing components of vectors in a specified order.

```easl
(let [a (vec2f 0. 1.)
      b (vec4f a 2. 3.)]
  b.zyx)
```

See the [vectors reference](reference/vectors.md) for more info.

## Arrays

Array literals are written with the `[]` brackets, e.g.:

```easl
(let [arr [1. 2. 3.]]
    (print (array-length arr))) ; prints 3
```

Array types are written like `[5: f32]` to represent an array of `f32`s of size `5u`. Dynamically-sized array types are written simply like `[f32]`.

You index into an array using the application syntax, e.g.:

```easl
(let [arr [1. 2. 3.]]
    (print (arr 1u))) ; prints 2.
```

## Assignment

The special `=` function is used to assign to variables. It can also be used to assign to struct fields, array elements, or swizzles: 

```easl
(let [@var a 0.]
  (= a 5.))
(let [@var b (vec2f 0.)]
  (= b.x 1.))
(let [@var arr [0. 1.]]
  (= (arr 0u) 2.))
(let [@var v (vec4f 0.)] ; swizzle assignment, which wgsl lacks
  (= v.xy (vec2f 1.)))
```

Easl also supports `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, and `>>=`.

## Structs

Structs are declared like so:

```easl
(struct Particle
  pos: vec3f
  mass: f32)
```

Structs are constructed with the fields in the order they're declared in the type, like so:

```
(Particle (vec3f 0.) 1.)
```

Fields can be accessed like `s.pos` (when `s` is a name) or `(.pos s)`.

## Enums and `match` blocks

Enums represent a union over several types, with a tag to differentiate between them. Each possible variant of the union has a name, and also may carry a value with it. For instance:

```easl
(enum Shape
  Empty
  (Circle f32)
  (Box vec2f))
```

You construct an enum value using it's name, applied as a function if the variant holds a name, or just referred to as a non-applied name if it's a unit-variant (i.e. no internal type). `match` expressions can be used to destructure an enum and access the values held inside:

```easl
(defn area [s: Shape]: f32
  (match s
    (Circle r) (* 3.14159 r r)
    (Box size) (* size.x size.y)
    Empty 0.))

(defn sum-of-areas [shapes: [Shape]]: f32
  (let [@var sum 0.]
    (for [i (array-length shapes)]
      (+= sum (area (shapes i))))
    sum))

@cpu
(defn main []
  (print (sum-of-areas [Empty
                        (Circle 2.)
                        (Box 3. 4.)
                        Empty
                        (Circle 0.5)])))
```

Easl has a built-in enum called `Option`, with `(Some ...)` and `None` variants. This type is [generic](reference/language.md#Generics), so it can hold any kind of value in it's `Some` variant.

`match` blocks can also be used on primitive scalar types, not just enums.

```easl
(match x
  0u 5.
  1u 6.
  _  (* (f32 x) 3.))
```

The `_` character, when used in a match pattern, represents a wildcard, and matches any value that hasn't yet been matched by an earlier arm. All match blocks must be exhuastive, so a match block must list out every possible variant (in the case of an enum) or have a `_` as it's last pattern.

`match` blocks can also match vector values against vector literals, but for the moment it isn't possbile to match other struct types.

## `if` and `when`

Conditional branching is done primarily with the `if` and `when` expressions.

`if` takes a boolean value, followed by two expressions representing the true and false branches:

```
(if (< x 0.)
  (print "x is negative")
  (print "x isn't negative"))
```

`if` expressions can yield a value where they're called, so they can easily be used inside other types of expressions. For instance, the above code could be rewritten as:

```
(print (if (< x 0.)
          "x is negative"
          "x isn't negative"))
```

This also means that each branch of an `if` expression must have the same type.

`when` represents a one-sided conditional, with no "else" clause. Unlike `if`, this expression does not yield a value (more specifically, `when` expressions are treated as having the unit type, `()`). `when` expressions take a boolean value, followed by any number of further expressions, all of which will be evaluated in order if and only if the boolean is `true`. For instance:

```
(when (< x 0.)
  (print "x was negative")
  (= x 5.)
  (print "but not anymore!"))
```

## `do`

The `do` expression is used to execute several expressions in order. This can be useful, for example, if you want to  have multiple expressions execute on a single side of an `if` conditional, e.g.:

```
(if (< x 0.)
  (do (print "x was negative")
      (= x 5.)
      (print "but not anymore!"))
  (print "x wasn't negative"))
```

## Loops

`for` loops in easl have several different signatures, the most general of which is similar to a c-style for loop. For instance:

```easl
(for [i 0i (< i 10) (+= i 1)]
  (+= x (f32 i)))
```

The `[...]` form following the `for` loop is expected to have four expressions inside. The first should be a name for a variable, in this case `i`. The next expression will be the initial value for the expression, here `0u`. The following two expressions represent the loop condition and increment statement, respectively.

There are also two shorthand syntaxes that simplify common `for`-loop use-cases. You can place two expressions, a name and an upper limit, and this will be interpreted as a zero-initialized value that increments by 1 each iteration until it reaches the limit. The following code is therefore equivalent to the above:

```easl
(for [i 10u]
  (+= x (f32 i)))
```

In cases where you don't even need the iteration index to be named but simply need to iterate some number of times, you can also just place a single integer inside the `[...]` form:

```easl
(for [4u] (*= x 2.))
```

`while` loops simply expect a boolean expression to act as a condition, followed by any number of other body expressions.

```
(while (< x 0.5)
  (+= x 0.1))
```

Easl supports `break` and `continue` statements in both `for` and `while` loops. These statements are treated as zero-argument functions in easl.

```easl
(for [i 10u]
  (*= x 2.)
  (when (< i 5u)
    (continue))
  (+= x 1)
  (when (> x 100.)
    (break)))
```

## Functions

`defn` declares a top-level function. The name of the function must be followed by a pair of `[]` brackets with the names of the arguments inside, and each of the arguments must be annotated with a type.

```easl
(defn print-twice-x [x: f32]
  (print (* x 2.)))
```

For arguments that return values, the `[]` must be followed by a type annotation describing the return type, e.g.:

```easl
(defn print-and-return-product [x: f32 y: f32]: f32
  (let [product (* x y)]
    (print product)
    product))
```

Functions that elide this `:` return-type annotation, like the first example in this section, are presumed to return the unit type `()`.

Inside a function, the special `return` statement can be used to exit from the function early, returning a value. `return` is called with normal function syntax: 

```easl
(defn f [x: f32]: f32
  (when (< x 0.) (return 0.))
  (sqrt x))
```

In a function that returns the unit type, `return` can be called either by explicitly passing the unit value like `(return ())`, or abbreviated simply to `(return)`.

## Reference and Variable arguments

Function arguments can be annotated with several different markers to change their meaning. Most simply, an argument can be marked with `@var` to declare that it's mutable within the scope of the function body, i.e. it can be assigned to. This has no effect on the original value passed in from the scope where the function was called, it's purely a convenience feature for being able to mutate the argument inside the body.

An argument can also be marked with `@ref` to say that it should be passed by reference. This is mainly useful for being able to pass large objects like arrays to a function without having to do a large stack-copy, and can also be useful for defining abstractions over certain built-in types that have special ownership rules, like textures.

An argument can also be marked with both `@var` and `@ref` at once, which makes it a *mutable reference*, meaning that assignments inside the function's body *do* affect the value passed in from the scope where the function is invoked. For instance:

```easl
(defn increment [@var @ref x: f32]
  (+= x 1.))

(let [@var total 0.]
  (increment total)
  (increment total)
  total)
```

This `@var @ref` annotation is the equivalent of the "inout" concept in traditional shader languages, and is very similar to the concept of a `&mut` value in Rust.

When calling a function that marks one of it's arguments as `@var @ref`, the value that you pass to that argument must be mutable, otherwise the compiler will give you an error. For instance:

```easl
(let [a 0.]
  (increment a)) ; this would cause an error, because a is not mutable

(let [@var b 0.]
  (increment b)) ; this would work fine, because b is marked @var
```

Importantly, references are second-class values in easl. Easl has no first-class pointers, and this `@var @ref` annotation exists only for function arguments - a field of a struct or enum cannot be marked as `@var @ref`, because there is no way to store pointers in easl. Easl's reference system offers a big ergonomic improvement over wgsl's pointer system, because you don't need to worry about the address space of the references at all, or do any kind of dereferrencing operation - the compiler will infer all of that for you.

## Top-level definitions

`def` is used to define a top-level constant.

```easl
(def gravity: f32 9.8)
```

`var`, on the other hand, is used to define a top-level variable.
```
(var particles: [4096: vec4f])
```

By default, un-annotated `var`s like this are shared between the CPU and GPU, and will be synced automatically for you. In some cases, however, you might not want a value to be synced, for performance reasons or simply for convenience. For instance, a system for generating random numbers might need a seed, and you might want the state of that seed to be independent when used on the GPU and the CPU. In those cases, you can use the `@local` annotation to tell the compiler to let this value be un-synced, keeping it "local" to each execution context:

```
@local
(var rng-state: u32)
```

See [GPU-bound variables](shaders.md#gpu-bound-variables) for more detail about the various address spaces that easl offers.

## Generics

Functions, structs, and enums can be generic. You specify generic variables by putting the name of the function/struct/enum in parentheses, and listing the type variables as though they were arguments to a function. You can use this same pseudo-function syntax to specify the value of each type argument.

```easl
(struct (Pair T)
  first: T
  second: T)

(defn (swap T) [p: (Pair T)]: (Pair T)
  (Pair p.second p.first))
```

Easl also has const-generic `u32` values, which are mainly useful for declaring functions that are generic over the size of an array. These const-generics are declared much like any other kind of generic, but with a trailing `: u32` to denote that they take on an integer value, rather than a type. For instance:

```easl
(defn (print-array-floats C: u32) [arr: [C: f32]]
  (for [i (array-length arr)]
    (print (arr i))))
```

All generics are inlined at compile time, so there is no performance overhead associated with using this feature; the code will behave exactly as though you had written the inlined version by hand.

## Higher-order functions and closures

Easl supports higher-order functions, i.e. functions which can accept other functions as arguments and/or return functions. The type of a function is written like `(Fn [f32 i32] u32)`, where the types between the `[]` brackets represent the argument types, and the last expression represents the return type. For instance:

```easl
(defn gradient [pos: vec2f f: (Fn [vec2f] f32)]: vec2f
  (let [eps 0.001]
    (- (vec2f (f (+ pos (vec2f eps 0.)))
              (f (+ pos (vec2f 0. eps))))
       (f pos))))
```

The above function takes another function, `f`, as an argument. It invokes the passed function twice inside it's body, here with the goal of calculating a finite-differences approximation of the gradient of the passed function. This function can be used as follows:

```easl
(defn my-sdf [pos: vec2f]: f32
  (- (length (+ pos (vec2f -1. 0.)))
     0.5))

@cpu
(defn main []
  (print (gradient (vec2f 3. 5.)
                   my-sdf)))
```

It's also possible to create a local function with `fn`:

```easl
@cpu
(defn main []
  (print (gradient (vec2f 3. 5.)
                   (fn [pos]
                     (- (length (+ pos (vec2f -1. 0.)))
                        0.5)))))
```

This code is equivalent to the previous example, just that rather than defining `my-fn` as a top-level function, the function is simply constructed in-place. Often the compiler will be able to infer the argument and return types of a `fn`, as it can in the above example, but of course you can always disambiguate with `:` type ascriptions when necessary.

Functions created with `fn` are *closures*, meaning they can capture and use variables from the surrounding scope. For instance, we could abstract some important values out in the above code:

```easl
@cpu
(defn main []
  (print (gradient (vec2f 3. 5.)
                   (let [sphere-center (vec2f -1. 0.)
                         sphere-radius 0.5]
                     (fn [pos]
                       (- (length (+ pos sphere-center))
                          sphere-radius))))))
```

`fn` can also capture mutable variables, and can mutate the value internally, allowing closures to effectively carry state around with it that changes as it gets called. For instance, here's a higher-order fn that generates "counter" functions:

```easl
(defn generate-counter []: (Fn [] u32)
  (let [@var counter 0u]
    (fn []
      (+= counter 1u)
      counter)))

@cpu
(defn main []
  (let [counter-a (generate-counter)
        counter-b (generate-counter)]
    (print (counter-a))
    (print (counter-a))
    (print (counter-a))
    (print (counter-b))
    (print (counter-b))))
```

This program prints out `1u 2u 3u 1u 2u`. As you can see, each of the values returned from `generate-counter` has it's own autonomous inner state - the counter variable inside the returned `counter-a` closure is independent from the counter variable inside `counter-b`.

## The threading operator, `->`

`->`, the "threading operator", is a very powerful tool for ergonomically structuring code in easl. `->` passes a value through a series of steps. `<>` marks where the value from the previous step goes in the next step goes, and a step that doesn't use `<>` anywhere gets that value automatically inserted in the position of it's first argument:

```easl
(-> x
    (+ <> 6.)
    (* 3.))
```

The above is the equivalent to `(* (+ x 6.) 3)`.

Calls to single-argument functions inside a `->` can also be abbreviated by simply eliding the surrounding parentheses, e.g.:

```easl
(-> x
    (+ <> 6.)
    sqrt
    (* 3.))
```

The value of each step in a threading expression is only ever computed once, even if `<>` appears several times in a step. For instance, this computes the square of `(+ x 5)`:

```
(-> x
    (+ 5.)
    (* <> <>))
```

This way of structuring code can be very convenient for making long chains of function calls much more readable than they would be in inlined form. Also, when laid out in this threaded form, it's very easy to temporarily disable one of the steps of a chain of transformations by adding a `#_` comment in front of one of the steps. This can be extremely useful for debugging and exploring effects in shaders.

## Annotations

As seen in several places before, easl uses syntax like `@annotation expression` to attach extra data to an expression. An annotation is a word (`@var`, `@fragment`), a `{}` map (`@{workgroup-size 16}`), or a `[]` list (`@[uniform 0 0]`), and several can stack. [Writing shaders](shaders.md) covers the ones for entry points, bindings, and shader inputs and outputs.

One very important annotation is the `@cpu` annotation, which is used to mark a function as an entry point in a file for easl's CPU runtime. For instance:

```
@cpu
(defn main []
  (print "Hello, world!"))
```

If you save the above text as a `.easl` file, and run it via the CLI with `easl run my_file.easl`, you'll see the "Hello, world!" string printed to stdout. Without the `@cpu` above this function, however, the CLI would fail to run the file, with a message explaining that no entry points were found anywhere in the file; the compiler would assume that `main` was simply a helper function intended to be used by other parts of your code.

## The `into` operator: `~`

The `~` prefix operator is just a shorthand for calling a function called `into` on an expression. That is to say, `~x` is just a shorter way of writing `(into x)`. Many builtin implementations of this `into` function exist for common conversions, e.g. in situations where a `vec3f` is expected, you can simply write `~1.` to convert the `1.` into a `vec3f`, i.e. a shorter way of writing `(vec3f 1.)`.

Since this is a purely syntactic transformation, it works seamlessly with any new `into` overloads you introduce in your own code. For instance:

```
(struct Complex
  real: f32
  imaginary: f32)

(defn + [a: Complex
         b: Complex]: Complex
  (Complex (+ a.real b.real)
           (+ a.imaginary b.imaginary)))

(defn into [real: f32]: Complex
  (Complex real 0.))

@cpu
(defn main []
  (print (+ (Complex 2 -1) ~1)))
```

See [`into`](reference/conversions.md#into).

## Imports

Easl supports a simple system for importing code from one file to another, via the top-level `import` statement:

```easl
(import "color.easl")
```

This effectively just brings in all of the code from the other file, as if it had been copy-pasted in place of the `import` statement. The path is expressed as a string, and is relative to the importing file.

Easl's import system for now is very simplistic, and it'll probably be replaced with a more sophisticated module/namespace system in the near future.
