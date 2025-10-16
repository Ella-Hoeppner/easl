# Easl Syntax

### Basic syntax
Easl's syntax differs greatly from traditional shader languages, being much more similar to the syntax of Lisp-like languages, especially Clojure.

All function calls are denoted by a pair of parentheses containing the function to be called, followed by the arguments to be passed to the function, separated by whitespace. For instance:

```
(f a b)
```

This bit of code represents an application of the function `f` to the arguments `a` and `b`. This might look a bit unusual if you're used to C-style syntax which would write the name of the function before the parentheses, like `f(a, b)`, but the simplicity easl's syntax has many advantages. For one, in easl, no commas are necessary to separate arguments - the whitespace alone is enough.

Another big difference from C-style syntax is the lack of infix operators. If you want to add two numbers together, C-style syntax would have you do that as `1 + 2`. However, in easl, this would be written as `(+ 1 2)`. You simply treat `+` as a function of two arguments, and call it like you would any other function. The same is true for all other arithmetic and logical operators, and the assignment operators like `=` and `+=`; to assign to a variable, you do `(= a 5)` rather than `a = 5`.

The fact that infix operators aren't used in easl frees up the `-` symbol to be used as a separator in multi-word names, since it won't be confused with the infix subtraction opereation. This can be more aesthetically pleasing than the `_` separator or using camel case like other languages do. For instance, you can name a variable `my-variable` rather than `my_variable` or `myVariable`. The built-in functions in easl all use this style, which is referred to as "kebab case". Type names in easl, on the other hand, all use camel case with a capitalized first letter, e.g. `MyType`.

Function and variable names in easl may include a wide variety of symbols that aren't allowed in most languages. For instance, you can freely use `-`, `_`, `!`, `?`, `*`, `+`, `>`, `<`, `@`, `$`, `%`, `^`, and `&`. Reserved characters that may not be used in names are `(`, `)`, `[`, `]`, `{`, `}`, `|`, `~`, `@`, `:`, `/`, `'`, and `#`, because they are used for built-in syntactic constructs (or are planned to be).

### Relationship to wgsl
Easl compiles to wgsl, the WebGPU Shader Language. For now, easl's primary intended purpose is to be used as an alternative to wgsl when using the WebGPU API. For this reason, there are certain features of easl that closely mirror features specific of wgsl, especially the rules around builtin i/o attributes and shader entry points (mentioned below in the **Annotations** section). Easl doesn't attempt to reinvent the wheel here, and instead just closely follows wgsl's approach. Because these features are so similar to their wgsl counterparts, the details of exactly what they mean and what they're used for won't be covered in much detail in this document, but you can always find more detail about them in the [wgsl spec](https://www.w3.org/TR/WGSL/#address-space).

If you're already familiar with wgsl, you'll find that everything you can express in wgsl can be straightforwardly translated into easl. Easl simply offers an alternative syntax and several powerful high-level abstraction features that wgsl and other shader languages don't offer.

You can expect that any function or type from wgsl (with a few caveats, see [the README](https://github.com/Ella-Hoeppner/easl/blob/main/README.md)) will also be available in easl. However, function names have all been changed from camel case to kebab case, e.g. `textureSample` becomes `texture-sample`, and all type names have been changed to use camel case with a capital first letter, e.g. `texture_2d` becomes `Texture2D`.

### Let bindings
To bind names to values in easl, you use the special `let` form inside a pair of parentheses, followed by the values you want to bind. For instance, here's some easl code that declares some let bindings and uses them, and then the equivalent wgsl code:

```
(let [x 5.
      y 6.]
  (= z x)
  (+= z y))
```

```
let x = 5.;
let y = 6.;
z = x;
z += y;
```

The `let` form always expects there to be a pair of square brackets `[...]` immediately following the `let` symbol, and expects those square brackets to contains pairs of names and values, separated by whitespace. After those square brackets, the `let` form expects there to be one or more other expressions, which are referred to as the *body* of the `let` block. These expressions may make use of the values defined inside the square brackets, but expressions outside of the `let` block may not. In other words, the bindings introduced in a `let` block are scoped to the inside of body. For instance, the following is not allowed, because the `x` and `y` variables are no longer in scope where they're used:

```
(let [x 5.
      y 6.])
(= z x)
(+= z y)
```

### Expression-based syntax
`let` blocks can also produce values that get returned into the scope where they're used. For instance, say we had some function `f` that accepts two `f32`s as arguments, and consider the following code:

```
(f 2.
   (let [b 5.]
     (* b b)))
```

Here we have a `let` block in the place where the second argument would go, which wouldn't be valid at all in wgsl. However, in easl, this code compiles just fine. When a `let` expression is used in a position that expects a value, the `let` expression takes on the type and value of the last expression inside it, in this case `(* b b)`. The local variable `b` is inferred to have type `f32`, and therefore `(* b b)` will also be an `f32`, so this whole `let` block is itself simply treated as an expression of type `f32`, no different from any other expression. So in this case, the code is equivalent to if we had passed in `25.` as the second argument.

The above example doesn't have a direct equivalent as wgsl code. The closest representation in wgsl would look like:

```
let b = 5.;
f(2., b * b)
```

Here the `b` binding has to be declared *before* the call to `f`, rather than *inside* it. This means that it isn't clear just by looking at this bit of code whether `b` will only used here, or instead used further down in the code. In the easl example, you know at a glance that `b` won't be used anywhere else, since it goes out of scope before the call to `f` is even completed.

This ability to declare bindings right where they need to be used, including deeply nested inside other expressions, can be a poweful tool for organizing and reasoning about code in easl. This capacity to use things like `let` blocks as values and place them directly inside other expressions is called *expression-based syntax*. Many modern programming languages are embracing the convenience and expressivity of that this style of syntax has to offer, but before easl, no shader langauge has supported this.

Easl uses this expression-based style for it's other control structures as well, such as `if` and `match` blocks.

### Variables
By default, bindings introduced in a `let` block are immutable, meaning that you can't assign into them with operators like `=` or `+=`. To declare a binding as mutable, you prefix it's name with a `@var` annotation. For instance, this easl is equivalent to the following wgsl:

```
(let [@var x 1.
      y 2.]
  (+= x y))
```

```
var x = 1.;
let y = 2.;
x += y;
```

### Type Ascription
Most of the time, easl can infer the types of your expressions for you, without you needing to explicitly write out the types of bindings. However, in some cases, easl might not be to determine all the types in your code. For instance:

```
(let [x 5]
  ...)
```

Here the compiler has no way of determining the exact type of `x` - it could be an `f32`, a `u32`, or an `i32`, but the compiler can't tell which one you intended (unless some code further down in the let block restricts the type of `x`). To resolve this, you can use the *type ascription* operator, `:`. For instance:

```
(let [x: u32 5]
  ...)
```

This indicates that `x` is of type `u32`, resolving the ambiguity. You can also use the type ascription operator on the value itself rather than the name, if you prefer:

```
(let [x 5: u32]
  ...)
```

The type ascription operator can be used on any expression in your code, not just on bindings. For instance, if you wanted to call a function `f` that could accept either of an `f32` or an `i32`, and you want to indicate that the number literal is a `i32` to avoid ambiguity, you could write:

```
(f 5: i32)
```

When it comes to number literals, there's also a shorthand for specifying which number type you want to use. You can add a suffix of `f`, `i`, or `u` to a number literal to specify that the number is an `f32`, `i32`, or `u32`, respectively. For instance, `5u`, `-3i`, and `6f` are all valid, unambiguously-typed number literals. You can also disambiguate a number as a float by including a decimal, e.g. `6.`.

### Accessors
To access the fields of a struct, such as a vector, you simply can simply use `.` to refer to the field, as in most languages:

```
(let [my-vec (vec3f 1. 2. 3.)
      b: f32 a.x]
  ...)
```

Alternatively, you can also access a field in a way that is more akin to function application:

```
(let [my-vec (vec3f 1. 2. 3.)
      b: f32 (.x a)]
  ...)
```

These are equivalent, and which you use is up to you. However, it is important to note that the first syntax, where you use the `.` directly between the name of a value and the name of a field with no whitespace, is *only* valid if the expression on the left-hand side of the `.` is a *name* - it can't be a compound expression. For instance, the following isn't valid:

```
(vec3f 1. 2. 3.).x
```

Instead, you'd have to write:

```
(.x (vec3f 1. 2. 3.))
```

You can assign into fields using either syntax:

```
(let [@var a (vec3f 1. 2. 3.)]
  (= a.x 2.)
  (= (.y a) 3.))
```

### Vectors
Easl's vector types are essentially the same to wgsl's, including the way constructors work. There are vectors of size 2, 3, and 4, referred to as a `vec2`, `vec3`, and `vec4`, respectively. You can add a suffix `i`, `f`, or `u` to the end of a vector type to denote which kind of scalar value it contains, or can use the un-suffixed version and let easl try to infer the types for you.

```
(let [a (vec3f 0. 1. 2.)
      b (vec3 0. 1. 2.)]
  ...)
```

When constructing a vector, you may provide a mix of other vectors or scalars, so long as the number of components adds up to the right amount:

```
(let [a (vec2f 0. 1.)
      b (vec3f a 2.)
      c (vec2f b.x 0.)
      d (vec4f a c)]
  ...)
```

Easl also supports `b` as a suffix denoting a vector of booleans, unlike wgsl.

Easl supports swizzling, as in wgsl and other shader languages. This means you can access multiple fields of a vector at the same time, treating them as another vector. For instance:

```
(let [a (vec4f 1. 2. 3. 4.)
      b: vec3f a.zyx]
  ...)
```

Swizzles can be used with both the infix `<name>.<value>` syntax or the prefix `(.<value> <name>)` syntax.

Additionally, easl supports the ability to assign into swizzles, which wgsl lacks.

Unlike some other shader languages, easl does not support any aliases for vector fields names, e.g. `rgba` as alternatives to `xyzw`.

### Functions
Functions in easl are declared with the special `defn` keyword, which can only be used at the top level of your file.

```
(defn f [x: f32 y: f32]: i32
  (i32 (+ x y)))
```

Immediately after the `defn`, there must be a name for the function, in this case `f`. The name must then be followed by a pair of square brackets, containing the names of the arguments, and each argument must have a type ascribed to it with the `:` operator. The square brackets may themselves be followed by another type ascription operator `:` along with the return type of the function, here `: i32`. After that, the function body may consist of any number of other expressions enclosed inside the outermost pair of parentheses. So overall, this `defn` is declaring a function called `f` that takes in two arguments, both of type `f32`, and returns an `i32`. Internally this function simply adds the two arguments together, casts the result to an `i32`, and returns that value.

Easl requires explicit type ascriptions on all function arguments, so the `:` operator for each name in the argument list is mandatory. The output type ascription, however, is optional. If not provided, the function will be presumed not to return any value. More specifically, functions with no return value are treated as a having the "unit type" as their return type. The return type is rarely explicitly written down, but when needed, it can be written as `()`. In other words, the following two signatures are equivalent:

```
(defn f [x: f32]
  ...)

(defn f [x: f32]: ()
  ...)
```

As demonstrated in the first example in this section, you don't have to explicitly `return` a value in easl. The last expression in the body of the function will always be used as a return value. However, `return` is still made available as an optional way of returning values from a function. You use the `return` operator by simply calling it as if it were a function, for instance:

```
(defn f [x: f32 y: f32]: i32
  (return (i32 (+ x y))))
```

You can also use the `return` operator to express a conditional early return by only using it inside of certain branches of an `if` or `match` statement.

### If expressions
The special keyword `if` is used to branch your code conditional on some boolean value, just like in most other languages. For instance:

```
(if b
  (= x 5.)
  (= x 10.))
```

An `if` expression requires exactly three inner expressions. The first expression must be a boolean, and the next two expressions represent the code to execute if the boolean is true or false, respectively.

Just as with `let` blocks, `if` expressions can always be used inside of other expressions, and can be treated as values in the positions that they're used. For instance, the above example could be rewritten as:

```
(= x (if b 5. 10.))
```

Here the whole `(if b 5. 10.)` expression has the type `f32`, because it's true and false clauses are `5.` and `10.`, which are both inferred to be `f32`s. Because of this ability to return values, the two branches of an `if` statement *must* have the same type, and you'll get a type errror if you try to write one that returns different types from the two branches. The previous example where we use the `=` operator in each branch satisfies this constraint because the `=` operator is treated as a function that returns the unit type.

The fact that `if` expressions can be used both at the top level and inside other expressions represents a significant ergonomic improvement over traditional, non-expression-based shader languages. In glsl, for instance, if you want to write a conditional at the statement-level, then you write it as `if (...) {...} else {...}`. But if you want to write a conditional inside another expression, you're forced to use an entirely different syntax, the "ternary operator" `... ? ... : ...`. Wgsl is even worse in this regard, with it's bizarre `select()` function instead of the ternary operator. Easl simplifies the situation sigificantly by allowing you to use the single `if` expression for conditionals, regardless of where they occur in your program.

### Do blocks

The `do` keyword can be used to combine multiple expressions into a single expression. This is mainly useful when you want to execute more than one expression inside a single branch of an `if` or `match` expression. For instance, consider this wgsl code:

```
if (x < 0.5) {
  x += 1.;
  x *= 2.;
  return x;
} else {
  y = 6. + x;
  y *= x;
  return y;
}
```

Easl's `if` expressions can only contain one expression in the `true` clause and one expression in the `false` clause, but to translate this wgsl code we need to have three expressions in each clause. We can accomplish that by using `do` to squeeze three expressions into a single expression:

```
(if (< x 0.5)
  (do (+= x 1.)
      (*= x 2.)
      (return x))
  (do (= y (+ 6. x))
      (*= y x)
      (return y)))
```

### When expressions

All `if` expressions are required to have clauses for handling both the `true` and `false` case. However, wgsl and other languages allow you to write `if` statements that only have code in the `true` clause:

```
if (x < 0.5) {
  x += 1.;
  x *= 2.;
  return x;
}
```

Since easl's `if` expressions return values, they can't handle cases like these, as there would be no value to return in the `false` case. To address this, easl also has a `when` expression that acts like a one-sided `if` expression. For instance, the above wgsl can be expressed in easl as:

```
(when (x < 0.5)
  (+= x 1)
  (*= x 2)
  (return x))
```

Unlike `if` blocks which must always contain exactly 2 expressions after the conditional, `when` blocks may contain any number of expressions after the conditional, and either all of them will be executed, or none of them will, depending on the value of the conditional. And unlike `if` blocks, `when` blocks do not return a value, and instead always have the unit type.

Strictly speaking, `when` blocks aren't essential, because they can always be expressed in terms of an `if` expression and a `do` expression. For instance, the above example could be rewritten as:

```
(if (x < 0.5)
  (do (+= x 1)
      (*= x 2)
      (return x))
  ())
```

However, the `when` approach is more concise and more clearly conveys intention, so it's preferrable when you're trying to do a one-sided conditional.

### Loops
Easl supports both `for` and `while` loops, like most traditional imperative languages.

`for` loops are written like so:

```
(for [i 0 (< i 10) (= i (+ i 1))]
  (+= x (* i i)))
```

The `for` must be followed by a pair of square brackets enclosing four elements: a name (`i`) for an incrementation variable, a value of type `i32`, (`0`), a conditional that determines whether the loop should continue (`(< i 10)`), and finally an expression that modifies the incrementation variable to progress the loop `(= i (+ i 1))`. This translates very directly into the equivalent wgsl code:

```
for (var i = 0; i < 10; i = i + 1) {
  x += i * i;
}
```

For now, the incrementation variable must be of type `i32`, but this limitation will be addressed soon.

`while` loops are written as:

```
(while (< x 0.5)
  (+= x 0.1))
```

The first expression after the `while` is treated as the conditional used to check whether the loop will continue after each iteration, and must be a boolean. After that, there may be any number of other expressions that will all be executed in each iteration of the loop.

`while` and `for` expressions both always have the unit type, meaning they do not return a value. Therefore, their only effect on the program will happen via the side effects that they cause to occur inside their bodies.

`break` and `continue` can be used to modify the control flow of loops, just like in wgsl and other C-like languages. For instance:

```
(while true
  (+= x 0.1)
  (when (> x 1.)
    break))

(for [i 0 (< i 10) (= i (+ i 1))]
  (when (== (% i 2) 0)
    continue)
  (+= x (* i i)))
```

You can use `break` and `continue` either by just writing them as word with no enclosing syntax like in the above example, or by enclosing them in parentheses like `(break)` or `(continue)` to mirror the syntax of a function call with zero arguments. Either syntax works just fine, and they mean exactly the same thing.

### Discard
The `discard` keyword can be used in functions to discard the current pixel, when executing a fragment shader. `discard` keyword may be used directly in the fragment entry point itself, or in a helper function that the fragment entry point invokes. However, it must not be used in a vertex or compute entry point, or in any helper function invoked by such an entry point.

Like `break` and `continue`, `discard` can be used alone as a single word or enclosed in a pair of parentheses like `(discard)`. Both syntaxes are valid, and equivalent. For example:

```
(defn discard-if-outside-zero-to-one [x: f32]: f32
  (if (< x 0.)
    discard
    (if (> x 1.)
      (discard)
      x)))
```

### Arrays
Arrays in easl are constructed with the square brackets. For instance, you can construct an array of three `f32`s like:

```
(let [x [1. 2. 3.]]
  ...)
```

The type of an array is written as `[<N>: <type-name>]`, where `N` is some positive integer literal. For instance, the above example can be given the type ascription:

```
(let [x: [3: f32] [1. 2. 3.]]
  ...)
```

The type of an unsized array can be written simply as a `[<type-name>]`, e.g. `[f32]`. Unsized arrays are only valid in certain restricted use-cases, e.g. when provided as a storage binding from the CPU.

To access the values of an array, you simply use a pair of parentheses with two inner expressions, where the first is the name of the array, and the second is the index (of type `i32` or `u32`) you wnat to access. For instance, the expression `(arr 0u)` will access the first element of the array `arr`. This is identical to the syntax for applying functions, and an array can in many ways be thought of as simply a function that simply map indeces to values. However, unlike function applications, this array access syntax can also be used in assignment expressions, e.g. `(= (arr 0u) 1.)` will set the first element of `arr` to `1.`.

You can create a zeroed array with the builtin `(zeroed-array)` function. This function takes no arguments, and the size and type of the array will simply be inferred from context. Or, if you want to be explicit, the type can of course be ascribed, e.g. `(zeroed-array): [3: f32]`.

### Top-level variables
While `let` bindings are used to introduce bindings locally within function bodies, there are two other keywords that are used to introduce global bindings: `def` and `var`.

`def` is used to declare immutable top-level values, aka constants, or `const` bindings in wgsl. For instance:

```
(def my-constant: f32 5.)
```

The `def` must be followed by a name with a type ascription, and then a literal value.

`var` is very similar to `def`, but is used to declare top-level values that can me mutated, or that are introduced dynamically from the CPU, rather than being compile-time constants. Otherwise, the syntax is identical:

```
(var my-constant: f32 5.)
```

However, it's also valid to create `var`s without giving them initial values: 

```
(var my-constant: f32)
```

Whether or not it's valid to provide an initial value depends on the *address space* of the variable. Variables in the `private` address space may be given initial values, while variables in other address spaces (`workgroup`, `uniform`, `storage`, or `handle`) must not. See the "Annotations" section for for info on how to specify an address space, or the [wgsl spec](https://www.w3.org/TR/WGSL/#address-space) for more information on what they are. If you don't annotate any address space on a `var`, it will default to the `private` address space, so you'll be allowed to provide an initial value.

There's also an `override` keyword that can be used to define [override constants](https://www.w3.org/TR/WGSL/#override-expressions). This keyword behaves indentically to `def`, but the compiled wgsl code will mark those bindings with `override` rather than `const`, allowing them to be overriden at pipeline-creation time in WebGPU.

### Structs
Custom struct types can be declared at the top-level with the `struct` keyword. Structs consist of some number of distinct fields, each with their own name and type. For instance:

```
(struct MyStruct
  a: f32
  b: i32
  c: vec3u)
```

The `struct` keyword must be followed by a name that denotes the name of the new struct type, followed by 1 or more names with types ascribed via the `:` operator.

Whenever you declare a struct, a *constructor* for that struct is automatically created. A constructor is a function that can be used to create instances of the struct. The constructor takes one argument for each of the fields declared in the struct, in the order that the fields are declared, and return an instance of the struct. For instance, instances of the above type can be created via it's constructor like:

```
(let [my-instance (MyStruct 2.5 1i (vec3u 0u))]
  ...)
```

Structs may not be recursive, i.e. you may not define a struct that refers to itself as one of it's fields, or to any other type that refers, directly or indirectly, to it.

### Enums
Easl supports Sum types, referred to as "enums", via the `enum` keyword. Easl's enums are more similar to rust's concept of an "enum" than the concept that appears in languages like C, in the sense that easl's enums may hold values, rather than just being syntactic sugar for a set of integer constants. For instance:

```
(enum Option
  None
  (Some f32))
```

The `enum` keyword must be followed by a name for the new type, followed by a series of one or *variants*. Each variant will either be a name, or a pair of parentheses containing a name and a type. In the latter case, the type that appears after the name of the variant is the type that is "contained" within instances of that variant. This allows different instance of the same `enum` type to contain different types of values. The `Option` type that we define above, for instance, represents a value that either contains nothing (the `None` variant), or a value that contains a single `f32` (the `Some` variant).

When an enum is declared, a *constructor* is created for each variant. For variants that contain no fields, such as the `None` field in our example, the constructor is simply a constant with the same name as the variant itself. When a variant does contain a field, the constructor is a function that accepts the type of that inner field as an input, and returns an instance of that variant containing the provided value as an output. For instance, the following code shows valid usages for both constructors of the above `enum` type:

```
(let [a: Option None
      b: Option (Some 5.)]
  ...)
```

Currently, enums may only contain at most a single field. However, this limitation will eventually be restricted, allowing enums to contain multiple internal fields.

As with structs, enums may not be recursive.

### Match blocks
In place of the `case` statements that exist in traditional C-like shader languages, easl provides `match` expressions. `match` expressions allow your program to branch into multiple different paths, similar to an `if` statement but with (potentially) more than two branches. For instance:

```
(let [a (match x
          0u 5.
          1u 6.
          2u 7.
          _ (* (f32 x) 3.))]
  ...)
```

The `match` keyword must be followed by a value called the *scrutinee*, followed by a series of *arms*, where each arm is a pair of two expressions: a *pattern*, which represents a possible value that the scrutinee could match, and a *body*, which is an expression to execute if the pattern is matched. Each pattern must of be of the same type as the scrutinee, and all the body expressions must be of the same type.

When a match block executes, it checks the value of the scrutinee against each pattern, in the order that they appear in the code, executing and returning the body corresponding to the first matched pattern. The `_` character is a special "wildcard" pattern that will match any value. It can be used as the last pattern in a match block to catch any remaining value not caught by the earlier arms.

`match` blocks in easl are most closely analgous to `switch` statements in wgsl, but there are several important differences. For one, `match` blocks don't require `break` statements to end the bodies of their arms - each body will be invoked in its entirity if and only if it's pattern is the one matched. Additionally, `match` expressions can, like any other expression, return a value to the location in which they are invoked, whereas a `switch` statement doesn't return a value and therefore can only influence the program via it's side effects.

Because the above example returns a value from the match block, there is no direct translation of it to wgsl. The closest representation to the above logic in wgsl would look like:

```
var a = 0.;
switch(x) {
  case 0u: {
    a = 5.;
    break;
  }
  case 1u: {
    a = 6.;
    break;
  }
  case 2u: {
    a = 6.;
    break;
  }
  default: {
    a = f32(x) * 3.;
    break;
  }
}
...
```

This code is much longer, messier, and doesn't convey intention as well. We're forced to treat `a` as a `var` rather than a `let` so that it can be assigned to from inside the cases, and we have to look at each case individually to understand what it's doing, rather than automatically knowing that it's going to be producing a value that gets bound to `a`, as we could see in the easl example.

`match` expressions can also be used with `enum`s, and are the primary way to interact with the values contained within enum variants. For instance, here's a function with uses a `match` block on the `Option` type, as described in the previous section:

```
(defn unwrap-or [o: Option default: f32]: f32
  (match o
    (Some value) value
    None x))
```

This function accepts an `Option`, which will either be a `Some` variant containing some `f32` value, or a `None` variant containing no value. In the case that the input option is a `Some`, the first arm of the match block will be invoked, and the value that was inside the option will be returned. In the case that the input option is `None`, however, the latter arm will be invoked, and the function will instead return the `default` value provided as the second input to the function.

When matching on an enum, the patterns are written with the same syntax as the constructors of the variants you want to match. The `Some` variant in our example has a constructor that accepts one argument, so the pattern is written as `(Some value)`. And the `None` variant's constructor is simply a constant rather than a function, because it is a variant with no, and therefore the pattern is simply written as `None`. However, it's important to note that, in the case like `(Some value)` where the constructor is a function with an argument, pattern *does not* represent an *application* of the constructor. Instead, it represents a *destructuring* of the variant, i.e. it's taking `value` *out* of the existing instance, not constructing a new instance. That is why the first arm in the above example is able to return `value` - in the body of a pattern that matches against a variant with an internal value, the name put in the place of that internal value in the pattern is *bound* to that value in the body that occurs just after that pattern.

All `match` blocks must be *exhaustive*, which is to say they must have at least one pattern that will match any possible value that the scrutinee could have. In the first example, we accomplished this by having a wildcard pattern `_` at the end, which will catch any value not caught by previous arms. If we hadn't included the wildcard character, and had instead just written the match block like the following, we would get a compile error:

```
(let [a (match x
          0u 5.
          1u 6.
          2u 7.)]
  ...)
```

This compile error occurs because there would be no body to execute, and therefore no value to bind `a` to, if `x` had a value other than the three listed in the match block.

A match block where the scrutinee is of type `f32`, `u32`, or `i32` *must* have a wildcard pattern `_` for it's last arm. This is the only way to guarantee exhaustivity for these types, since it isn't possible to provide individual arms for each possible value. However, when matching on an enum, a wildcard pattern isn't necessary as long as every possible variant that the enum could have is covered by a pattern.

For now, the scrutinee of a match block can only be a `bool`, `f32`, `u32`, `i32`, or an enum. It isn't currently possible to match on a vector or any custom struct type. This restriction will eventually be lifted.

### Annotations
Earlier we saw that a binding in a `let` expression can be marked as mutable by prefixing it's name with `@var`, e.g.:

```
(let [@var x 5.]
  ...)
```

This is an example of a more general concept of an *annotation*. The `@` operator is used to annotate an expression with some other bit of data, which in this case is simply the word `var`.

In general, the syntax for an annotation will always be `@<annotation> <expression>`. There are two types of annotations: singular words, like `var`, or a pair of `{}` brackets containing one or more pairs of values, e.g. `{value-a 1 value-b 2}`.

There are only a limited number of valid ways of using annotations, which are detailed in the remainder of this section and the next section.

Functions can be marked as [*entry points*](https://www.w3.org/TR/WGSL/#entry-point) by prefixing them with an annotation of the name of the kind of entry point, either `fragment`, `vertex`, or `compute`. Compute entry points may also be annotated with `@{workgroup-size <N>}`, where `N` is a positive integer, to denote the size of a workgroup for the compute shader. For instance:

```
@fragment
(defn f [...]: vec4f
  ...)

@compute
@{workgroup-size 16}
(defn c [...]
  ...)
```

When multiple annotations are used consecutively, as with the `@compute @{workgroup-size 16} ...` in the above example, they are both treated as annotating the expression that occurs directly after them.

In addition to being usable inside of `let` bindings, `@var` can also be used to mark the arguments of functions as mutable, for instance:

```
(defn add-five [@var x: f32]: f32
  (+= x 5.)
  x)
```

Note that this only makes a function mutable *inside* the scope of the function where it is used. Passing a binding `add-five` from another function will not affect it's value (in other words, this isn't like an `inout` variable in glsl).

The binding group, binding number, and [address space](https://www.w3.org/TR/WGSL/#address-space) of a top-level variable may be annotated like so:

```
@{group 0
  binding 0
  address uniform}
(var dimensions: vec2f)
```

If no address space is specified, `private` is assumed. To mark a variable in the `storage` namespace as `read_write`, you can annotate it with `@{address storage-write}`.

Annotations are also used to denote builtin input/output values in structs and entry point type signatures. For instance, here's an example for a vertex and fragment shader demonstrating how `builtin` input/output attributes and `location` attributes can be annotated on struct fields and on function inputs and outputs:

```
(struct VertexOutput
  @{builtin position} position: vec4f
  @{location 0} corner-pos: vec2f)

@vertex
(defn vertex [@{builtin vertex-index}
              vertex-index: u32
              @{builtin instance-index}
              instance-index: u32]: VertexOutput
  (let [corner (match vertex-index
                 0 (vec2f -1.)
                 1 (vec2f -1. 3.)
                 _ (vec2f 3. -1.))]
    (VertexOutput (vec4f corner 0. 1.) corner)))

@fragment
(defn fragment [in: VertexOutput]: @{location 0} vec4f
  (vec4f in.corner-pos 0. 1.))
```

The available `builtin` annotation kinds are the same [as in wgsl](https://www.w3.org/TR/WGSL/#builtin-inputs-outputs), but with all names converted to kebab case. The restrictions on which `builtin` values can be used on which stages are also the same as wgsl.

Easl also includes several helpful shortcuts that cut down on the need for many of these explicit annotations. For instance, whenever a vertex shader returns a `vec4f`, which would normally need to have the `@{builtin position }` annotation to be a valid vertex shader, the annotation can be emitted. In other words, the following are equivalent:

```
(defn v [...]: @{builtin position} vec4f
  ...)
```

```
(defn v [...]: vec4f
  ...)
```

Additionally, when an input/output value or a struct in a type used in the input or output of an entry point doesn't have any annotations, it will be inferred to have a `@{location <N>}` annotation, with `N` determined by its positioning relative to the other fields. Finally, instead of doing the full `@{builtin ...} ...` annotation to specify a builtin input/output attribute, you can simply use the name of a builtin value for the field, and prefix it with the more concise `@builtin` annotation. With these two shortcuts, the annotations previous example can be significantly simplified:

```
(struct VertexOutput
  @builtin position: vec4f
  corner-pos: vec2f)

@vertex
(defn vertex [@builtin vertex-index: u32
              @builtin instance-index: u32]: VertexOutput
  (let [corner (match vertex-index
                 0 (vec2f -1.)
                 1 (vec2f -1. 3.)
                 _ (vec2f 3. -1.))]
    (VertexOutput (vec4f corner 0. 1.) corner)))

@fragment
(defn fragment [in: VertexOutput]: vec4f
  (vec4f in.corner-pos 0. 1.))
```

### Generics
Functions, structs, and enums may all be *generic*, meaning that they may refer to "generic types" that may be filled in with various concrete types in the various places that they are used. For instance, it's possible to define a generic struct that contains a field of some generic type `T`:

```
(struct (MyStruct T)
  a: T
  b: [T: 3]
  c: f32)
```

While a non-generic struct, as described in the previous section, simply has a name following the `struct` keyword, when using generics, the name is replaced with a pair of parentheses containing the name as the first element, followed by the names of whatever generic variables the type uses.

Functions and enums also use this approach of replacing what would normally be a simple name with a parenthesized expression containing the name followed by generic variables. For instance:

```
(enum (Option T) (Some T) None)
```

This version of the `Option` enum is similar to the one described earlier, but instead of always containing an `f32` like the original version, it can be used to contain any type `T`, making it useful in a much wider variety of circumstances. We can also rewrite the `unwrap-or` function to work on generic `Option`s:

```
(defn (unwrap-or T) [o: (Option T) default: T]: T
  (match o
    (Some value) value
    None x))
```

As you can see, when referring to the generic `Option` type here, we refer to it as `(Option T)`, which means that we're filling in it's generic variable with the `T` introduced in the scope of this function. The same syntax is used to specify the generic varaibles of enum types. The fact this syntax is identical to the syntax normally used for function applications is not a coincidence - generic types can, after all, be thought of as type-level functions.

The built in `vec` types are also represented as generic types. As such, the type `vec4f` can also be written as `(vec4 f32)` - `vec4` is merely a shorthand alias for this type. However, for now, vector types are restricted to only containing `f32`s, `i32`s, `u32`s, or `bool`s as their internal values.

### Higher-order functions
Functions in easl are capable of accepting other functions as inputs. For instance:

```
(defn apply-twice [f: (Fn [f32] f32) x: f32]: f32
  (f (f x)))
```

The first argument to the above `apply-twice` function is of type `(Fn [f32] f32)`, which is the type representing functions that themselves accept one argument of type `f32`, and return an `f32`. This means that we can pass other functions in the place of that argument, for instance:

```
(defn double [x: f32]: f32
  (* x 2.))

(defn quadruple [x: f32]: f32
  (apply-twice double x))
```

Currently, there is a significant restriction on the use of higher-order functions in easl; all functions passed as inputs to higher-order functions must be compile-time constants, so that they can be inlined to their usage site at compile time. In practice this basically means that you just can't use an `if` or `match` statement to pass different functions to a higher-order function based on some criteria. For instance, the following isn't allowed, because it uses an `if` statement to decide the value of the higher-order argument:

```
(defn quadruple-or-octuple [x: f32 octuple: bool]
  (apply-twice (if octuple
                quadruple
                double)
               x))
```

This limitation will eventually be lifted as the compiler matures. But for now, the upside is that this restriction guarantees that there will never be any performance penalty for making use of higher-order functions, since all function calls are inlined at compile time. In other words, easl's higher-order functions are a *zero-overhead abstraction*. And even with this limitation, they allow for ergonomic abstractions that reduce code duplication in ways that have previously been impossible in shader languages. For instance, here's a higher-order function that uses the finite differences method to approximate the gradient of any function passed into it:

```
(defn gradient [f: (Fn [vec3f] f32)
                x: vec3f]: vec3f
  (let [center (f x)]
    (vec3f (- (f (+ x (vec3f 0.001 0. 0.)))
              center)
           (- (f (+ x (vec3f 0. 0.001 0.)))
              center)
           (- (f (+ x (vec3f 0. 0. 0.001)))
              center))))
```

### Associative functions

### Shadowing
Easl allows you to shadow local bindings with other local bindings, by reusing a name inside a `let` expression that was previously used in the enclosing scope. However, easl forbids shadowing of global bindings.

### Comments
Easl supports several different types of comments. Like most langauges, easl supports both single-line and multi-line comments.

Single-line comments start with `;` and continue to the end of the line:

```
; I'm a comment! I can write whatever I want here!
; I can even have unbalanced parentheses :)
(defn f ...)
```

Multi-line comments start with `;*` and end with `*;`:

```
;* I'm a block comment.
I can take up as many lines as I want!
But when you want me to end you can just type *;
(defn f ...)
```

Easl also supports *expression comments*, which begin with a `#_` and continue to the end of following expression. This may be multiple lines, or a single line, or even just a small part of a single line, depending on what expression you place it on:

```
; The `defn` below this line will be completely ignored, since it starts with #_
#_(defn f [x: f32]: f32
    (+ x x))

(defn f [x: f32]: f32
  ; The `return` expression below will also be ignored
  #_(return 5.)
  (+ x
     #_(this expression also be ignored! which is good because this definitely isnt
        a valid function call)
     x))
```

Expression comments are different from single-line or multi-line comments in that the code inside them must still parse as structurally valid easl code (though of course it doesn't need to typecheck). This means that if you write an expression comment but then try to change the code inside the, e.g. by adding unbalanced parentheses, then you might change the boundaries of where the expression ends, and create parsing errors in your file. So if you just want to write some plaintext for documentation purposes and don't actually want it to be parsed, you're better off using single-line or multi-line comments. But expression comments can be very useful for quickly making temporary changes to the logic of your code.

### Threading expression
The *threading expression* can be used to write code in a way that avoids deep nesting. A threading expression starts with the special `->` keyword, and looks like this:

```
(-> x
    (+ <> 6)
    (* <> 3)
    (/ <> 2)
    (- <>)
    [<> 5])
```

The above expression is precisely equivalent to:

```
[(- (/ (* (+ x 6) 3) 2)) 5]
```

The way to read the threading macro is to think of each `<>` that occurs in an expression as a "hole", which is filled with the value of the previous expression. So starting with the first expression, each expression effectively gets "threaded into" the position of the `<>` hole in the following expression. So in the above example, starting with `x`, the expression get threaded into the hole where the `<>` in next expression, which is `(+ <> 6)`, meaning that so far the expression is equivalent to `(+ x 6)`. Now, that expression gets fed into the `<>` hole in the following expression, which is `(* <> 3)`, so the new expression is now `(* (+ x 6) 3)`. This process continues until the last expression, in this case `[<> 5]`, which is reflected by the fact that the square brackets end up at the outermost layer of the expression.

In other words, the threading macro allows you to turn the normal ordering of function calls inside out, writing the calls that will happen earliest before the outermost calls, rather than inside them.

Whether it's better to write an expression using a thread expression, or the normal nested syntax, is a matter of styling and depends a lot on context and personal taste. However, it can often make deeply nested code more concise and easier to read, so it's a tool worth having.

Any of the various kinds of expressions that easl supports can be used in a threading expression. For example, the following two pieces of code are equivalent:

```
(-> x
    (match <>
      1 20.
      2 30.
      _ 0.)
    (* <> 2.)
    (if (< y 0.5)
      <>
      -20.)
    <>: f32
    (do (= a <>)
        false))
```

```
(do (= a (if (< y 0.5)
           (* (match x
                1 20.
                2 30.
                _ 0.)
              2.)
           -20.): f32)
    false)
```

As a convenience, if a step in the chain of expressions is a function application where the `<>` is the first argument, e.g. `(f <> b)`, it may instead be shortened to simply `(f b)`. In other words, when the `<>` doesn't occur anywhere in an expression, it's simply inserted in the position of the first argument inside the parentheses. Further, if one of the steps is an application of a function to a single argument, `(f <>)`, then it may be shortened to simply `f`, the name of the function.

With these shortcuts, the original example of the thread expression given in this section can be simplified into:

```
(-> x
    (+ 6)
    (* 3)
    (/ 2)
    -
    [<> 5])
```

You may also use the `<>` expression twice inside a single step. For instance, `(-> x (* <> <>))` is the equivalent of `(* x x)`. When this syntax is used, each intermediate value will only be computed once, even if it is used multiple times in the next stage, so the threading expression will never have a negative impact on performance compared to another way of writing the expression.

This feature is heavily inspired by Clojure's [threading macros](https://clojure.org/guides/threading_macros), but aims to be simpler and more flexible. Rather than the several different threading macros like `->`, `->>`, and `as->` that clojure provides, easl opts for a single, more flexible threading expression `->` that acts as a combination of clojure's `->` and `as->`. Easl thread expression makes use of the special "hole" symbol `<>` to declare position the threaded expression rather than requiring an explicit name like clojure's `as->` does, and falling back to the behavior of the clojure's `->` macro if the hole symbol isn't used.
