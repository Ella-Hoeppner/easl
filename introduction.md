# Easl Syntax

### Basic syntax
Easl's syntax differs greatly from traditional shader languages, being much more similar to the syntax of Lisp-like languages, especially Clojure.

All function calls are denoted by a pair of parentheses containing the function to be called, followed by the arguments to be passed to the function, separated by whitespace. For instance:

```
(f a b)
```

This bit of code represents an application of the function `f` to the arguments `a` and `b`. This might look a bit unusual if you're used to C-style syntax which would write the name of the function before the parentheses, like `f(a, b)`, but the simplicity of easl's syntax has many advantages. For one, in easl, no commas are necessary to separate arguments - the whitespace alone is enough.

Another big difference from C-style syntax is the absence of infix operators. If you want to add two numbers together, C-style syntax would have you do that as `1 + 2`. However, in easl, this would be written as `(+ 1 2)`. You simply treat `+` as a function of two arguments, and call it like you would any other function. The same is true for all other arithmetic and logical operators, and the assignment operators like `=` and `+=`; to assign to a variable, you do `(= a 5)` rather than `a = 5`.

The fact that infix operators aren't used in easl frees up the `-` symbol to be used as a separator in multi-word names, since it won't be confused with the infix subtraction operation. This can be more aesthetically pleasing than the `_` separator or using camel case like other languages do. For instance, you can name a variable `my-variable` rather than `my_variable` or `myVariable`. The built-in functions in easl all use this style, which is referred to as "kebab case". Type names in easl, on the other hand, all use camel case with a capitalized first letter, e.g. `MyType`.

Function and variable names in easl may include a wide variety of symbols that aren't allowed in most languages. For instance, you can freely use `-`, `_`, `!`, `?`, `*`, `+`, `>`, `<`, `@`, `$`, `%`, `^`, and `&`. Reserved characters that may not be used in names are `(`, `)`, `[`, `]`, `{`, `}`, `|`, `~`, `@`, `:`, `/`, `'`, and `#`, because they are used for built-in syntactic constructs (or are planned to be).

### Relationship to wgsl
Easl compiles to wgsl, the WebGPU Shader Language. For now, easl's primary intended purpose is to be used as an alternative to wgsl when using the WebGPU API. For this reason, there are certain features of easl that closely mirror features specific of wgsl, especially the rules around builtin i/o attributes and shader entry points (mentioned below in the **Annotations** section). Easl doesn't attempt to reinvent the wheel here, and instead just closely follows wgsl's approach. Because these features are so similar to their wgsl counterparts, the details of exactly what they mean and what they're used for won't be covered in much detail in this document, but you can always find more detail about them in the [wgsl spec](https://www.w3.org/TR/WGSL/#address-space).

If you're already familiar with wgsl, you'll find that everything you can express in wgsl can be straightforwardly translated into easl. Easl simply offers an alternative syntax and several powerful high-level abstraction features that wgsl and other shader languages don't offer.

You can expect that any function or type from wgsl (with a few caveats, see [the README](https://github.com/Ella-Hoeppner/easl/blob/main/README.md)) will also be available in easl. However, function names have all been changed from camel case to kebab case, e.g. `textureSample` becomes `texture-sample`, and all type names have been changed to use camel case with a capital first letter, e.g. `texture_2d` becomes `Texture2D`.

Easl attempts to make the compiled wgsl human-readable whenever possible. Most variable names will be preserved through compilation, though with any `-` symbols replace with `_`. If you're curious about how easl handles certain abstractions, looking at the compiled wgsl code may provide some insight.

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

### Comments
Easl supports several different types of comments. Easl supports single-line comments, multi-line comments, and expression comments.

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

Expression comments begin with a `#_` and continue to the end of following expression. This may be multiple lines, or a single line, or even just a small part of a single line, depending on what expression you place it on:

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

Expression comments are different from single-line or multi-line comments in that the code inside them must still parse as structurally valid easl code (though of course it doesn't need to typecheck). So if you just want to write some plaintext for documentation purposes and don't actually want it to be parsed, you're better off using single-line or multi-line comments. But expression comments can be very useful for quickly making temporary changes to the logic of your code.

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

This ability to declare bindings right where they need to be used, including deeply nested inside other expressions, can be a poweful tool for organizing and reasoning about code in easl. This capacity to use things like `let` blocks as values and place them directly inside other expressions is called *expression-based syntax*. Many modern programming languages are embracing the convenience and expressivity that this style of syntax has to offer, but before easl, no shader langauge has supported this.

Easl uses this expression-based style for its other control structures as well, such as `if` and `match` blocks.

### Variables
By default, bindings introduced in a `let` block are immutable, meaning that you can't assign into them with operators like `=` or `+=`. To declare a binding as mutable, you prefix its name with a `@var` annotation. For instance, this easl is equivalent to the following wgsl:

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

Easl requires explicit type ascriptions on all function arguments, so the `:` operator for each name in the argument list is mandatory. The output type ascription, however, is optional. If not provided, the function will be presumed not to return any value. More specifically, functions with no return value are treated as a having the "unit type" as their return type. The return type is rarely explicitly ascribed in easl, but when needed, it can be written as `()`. In other words, the following two signatures are equivalent:

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

The fact that `if` expressions can be used both at the top level and inside other expressions represents a significant ergonomic improvement over traditional, non-expression-based shader languages. In glsl, for instance, if you want to write a conditional at the statement-level, then you write it as `if (...) {...} else {...}`. But if you want to write a conditional inside another expression, you're forced to use an entirely different syntax, the "ternary operator" `... ? ... : ...`, or the `select()` function in wgsl. Easl simplifies the situation sigificantly by allowing you to use the single `if` expression for conditionals, regardless of where they occur in your program.

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

### Loops
Easl supports `for` and `while` loops.

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

`break` and `continue` can be used to modify the control flow of loops, just like in wgsl and other C-like languages. These operator are treated as functions of zero arguments, so call them by placing them alone inside a pair of parentheses, i.e. `(break)` or `(continue)`. For instance:

```
(while true
  (+= x 0.1)
  (when (> x 1.)
    (break)))

(for [i 0 (< i 10) (= i (+ i 1))]
  (when (== (% i 2) 0)
    (continue))
  (+= x (* i i)))
```

### Discard
The `discard` keyword can be used in functions to discard the current pixel, when executing a fragment shader. `discard` may be used directly in the fragment entry point itself, or in a helper function that the fragment entry point invokes. However, it must not be used in a vertex or compute entry point, or in any helper function invoked by such an entry point. Just like `break` and `discard`, you invoke `discard` by putting it alone inside a pair of parentheses, calling it like a function of zero arguments:

```
(defn discard-when-negative [x: f32]: f32
  (if (< x 0.)
    (discard)
    x))
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
Earlier we saw that a binding in a `let` expression can be marked as mutable by prefixing its name with `@var`, e.g.:

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

This limitation will eventually be lifted as the compiler matures. But for now, the upside is that this restriction guarantees that there will never be any performance penalty for making use of higher-order functions, since all function calls are inlined at compile time. In other words, easl's higher-order functions are a *zero-cost abstraction*. And even with this limitation, they allow for ergonomic abstractions that reduce code duplication in ways that have previously been impossible in shader languages. For instance, here's a higher-order function that uses the finite differences method to approximate the gradient of any function passed into it:

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

The way to read the threading macro is to think of each `<>` that occurs in an expression as a "hole", which is filled with the value of the previous expression. So starting with the first expression, each expression effectively gets "threaded into" the position of the `<>` hole in the following expression. So in the above example, starting with `x`, the expression get threaded into the hole where the `<>` in next expression, which is `(+ <> 6)`, meaning that so far the expression is equivalent to `(+ x 6)`. Now, that expression gets fed into the `<>` hole in the following expression, which is `(* <> 3)`, so the new expression is now `(* (+ x 6) 3)`. This process continues until the last expression, in this case `[<> 5]`, which is reflected by the fact that the square brackets end up at the outermost layer of the inlined expression.

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

This feature is heavily inspired by Clojure's [threading macros](https://clojure.org/guides/threading_macros), but aims to be simpler and more flexible. Rather than the several different threading macros like `->`, `->>`, and `as->` that clojure provides, easl opts for a single, flexible threading expression `->` that acts as a combination of clojure's `->` and `as->`. Easl thread expression makes use of the special "hole" symbol `<>` to declare position the threaded expression rather than requiring an explicit name like clojure's `as->` does, whole falling back to the behavior of the clojure's `->` macro if the hole symbol isn't used.

Easl may eventually include other kinds of threading expressions, such as a `cond->` expression similar to clojure's.

# Example Shader Program
The following is an example of a full shader program, one of the examples packaged with the [Easl CLI](https://github.com/Ella-Hoeppner/easl_cli). It implements a simple [raymarcher](https://iquilezles.org/articles/raymarchingdf/), and demonstrates several of easl's important features. You can run this code for yourself by [installing the CLI](https://github.com/Ella-Hoeppner/easl_cli?tab=readme-ov-file#installation).

```
@{group 0
  binding 0
  address uniform}
(var dimensions: vec2f)

@{group 0
  binding 1
  address uniform}
(var time: f32)

(def triangles: u32
     1)

(defn uni->bi [x: vec2f]: vec2f
  (-> x (* 2.) (- 1.)))

@vertex
(defn vertex [@builtin vertex-index: u32]: vec4f
  (vec4f (match vertex-index
           0 (vec2f -1.)
           1 (vec2f -1. 3.)
           _ (vec2f 3. -1.))
         0.
         1.))

(enum (Option T) None (Some T))

(struct Ray
  pos: vec3f
  dir: vec3f)

(defn advance [r: Ray
               d: f32]: Ray
  (Ray (+ r.pos (* d r.dir)) r.dir))

(defn gradient [f: (Fn [vec3f] f32)
                x: vec3f]: vec3f
  (let [center (f x)]
    (vec3f (- (f (+ x (vec3f 0.001 0. 0.)))
              center)
           (- (f (+ x (vec3f 0. 0.001 0.)))
              center)
           (- (f (+ x (vec3f 0. 0. 0.001)))
              center))))

(defn raymarch [sdf: (Fn [vec3f] f32)
                ray: Ray]: (Option vec3f)
  (let [@var current-dist 0.]
    (for [i 0 (< i 256) (= i (+ i 1))]
      (let [current-pos (.pos (advance ray current-dist))
            dist (sdf current-pos)]
        (when (< (abs dist) 0.001)
          (return (Some (normalize (gradient sdf current-pos)))))
        (when (> dist 5.)
          (break))
        (+= current-dist (* 0.9 dist))))
    None))

(defn sd-box [pos: vec3f
              size: vec3f]: f32
  (let [q (- (abs pos) size)]
    (+ (length (max q (vec3f 0.)))
       (min (max q.x (max q.y q.z)) 0.))))

(defn rot [angle: f32]: mat2x2f
  (let [c (cos angle)
        s (sin angle)]
    (mat2x2f c (- s) s c)))

(defn sd-scene [@var pos: vec3f]: f32
  (-= pos (vec3f 0. 0. 2.5))
  (= pos.zy
     (* pos.zy (rot (* 0.4 time))))
  (= pos.xz
     (* pos.xz (rot (* -0.7 time))))
  (+ (sd-box pos (vec3f 0.75))
     (* 0.025
        (cos (+ (* 11. pos.x) (* 9. time))))
     (* 0.01
        (cos (+ 2. (* 4. pos.y) (* 4. time))))
     (* 0.03
        (cos (+ 5. (* 7. pos.z) (* 12. time))))))

@fragment
(defn fragment [@builtin position: vec4f]: vec4f
  (let [screen-pos (-> position
                       .xy
                       (/ dimensions)
                       uni->bi
                       (* (/ dimensions
                             (min dimensions.x dimensions.y))))
        surface-normal (raymarch sd-scene
                                 (Ray (vec3f 0.)
                                      (normalize (vec3f screen-pos 1.))))]
    (match surface-normal
      (Some normal) (vec4f (pow (* (+ normal 1.) 0.5)
                                (vec3f 2.2))
                           1.)
      None (vec4f (vec3f 0.) 1.))))
```

The `raymarch` function in the above example is structured quite differently than you would see in a traditional shader language. This raymarch function takes in a `Ray` and a function called `sdf`, which should be a [signed distance function](https://en.wikipedia.org/wiki/Signed_distance_function). The `sdf` accepts a `vec3f` representing a position, and returns an `f32` representing the distance to a surface. The raymarcher itself returns an `(Option vec3f)`, which represents the surface normal of the location where the ray intersected the sdf, if any was found. In the case that the ray never intersects the surface, the `None` variant will be returned, otherwise the `Some` variant will be returned, containing the surface normal.

The fact that our `raymarch` function accepts the `sdf` function *as an argument* makes it much more general and powerful than any equivalent function in a traditional shader language. If you were to rewrite the above shader in a language like glsl or wgsl, you'd have to omit the `sdf` argument to `raymarch`, and instead just directly inline a call to `sd-scene` inside the body of `raymarch`.

That approach comes with a big downside, which is that if you ever wanted to raymarch another sdf in the same shader, you'd have to just duplicate the entire `raymarch` function, but with the inlined call to `sd-scene` swapped with a call to whatever other sdf you wanted to use. In easl, you could simply call the same `raymarch` function in several different places with several other functions in place of the `sdf` argument, and your code would work just fine, but with much less duplicated code. And there is no performance penalty associated with this usage of higher-order functions, because all calls to higher-order functions are transformed at compile-time into versions of the function with all the higher-order arguments inlined, just as if you had written it that way by hand.

To put it another way, our `raymarch` function is a direct representation of the concept of a raymarcher itself, abstracted away from the details of any specific sdf. Traditional shader languages are completely incapable of representing this kind of abstraction.

Additionally, the `Option` enum provides a nice way to represent of the fact that raymarchers do not always hit their target. In a traditional shader language, without sum types, you'd have to use some other convention to denote that the raymarcher exited without converging on the surface. For instance, you might decide to return a special value like `(vec3f 0.)` when the ray diverges, and then just check for that value everywhere you use the raymarching function. Or, you might return a struct containing both a `vec3f` and a `bool`, such that the `bool` is set to false when the ray diverges. However, both of these are more error-prone and less maintainable than the `Option`-based approach that easl provides. Both other approaches make it very easy to accidentally forget to check the validity of the output before using the `vec3f` value, which can lead bugs. The `Option` approach makes that kind of mistake impossible, since there's not even any `vec3f` to access unless you use a `match` block to handle both possible cases of the returned value.

### Details on the compilation of Enum types

Thankfully, the more type-safe and convenient representation that the `Option` enum provides in the above example doesn't come with any runtime cost; the `Option` approach is just as efficient as the alternative, and compiles to essentially the same code. Easl's enums are a *zero-cost abstraction*.

The compiled wgsl code for the `Option` type used in the above example looks like this:

```
struct Option_vec3f {
    discriminant: u32,
    data: array<u32, 3>
}

const None_vec3f: Option_vec3f = Option_vec3f(0, array(0, 0, 0));

fn Some_vec3f(value: vec3f) -> Option_vec3f {
  return Option_vec3f(
    1u,
    array(
      bitcast<u32>(value.x),
      bitcast<u32>(value.y),
      bitcast<u32>(value.z)
    )
  );
}
```

Here you can see the definition for the `Option` type itself as a wgsl struct, and the two constructors, `None` and `Some` The name of the type and each variant is suffixed with `_vec3f` because this specific code represents the [*monomorphized*](https://en.wikipedia.org/wiki/Monomorphization) version of our generic `Option` type for the particular inner type `vec3f`. Since the `None` constructor has no arguments, it is simply a represented as constant, while the `Some` constructor is represented as a function, since it must accept some inner `vec3f` value.

The `Option_vec3f` struct represents our `(Option vec3f)` enum type. All compiled enums look like this one, with `discriminant` and `data` fields, with the only difference being the size of the `data` array.

The `discriminant` field is used to designate which variant each instance belongs to. In this case, a discriminant of 0 represents the `None` variant, while 1 represents `Some`. No other discriminant value will ever be used for this type. In general, an enum with n different variants will use the values 0 through n-1 as discriminants, one for each variant.

The `data` array will in general contain as many `u32`s as would be needed to represent the largest internal type of any variant. Here only variant with an internal type is `Some`, which contains a `vec3f`, which just consists of 3 `f32`s, which have the same size as `u32`s, so the `data` array is of size `3`. The `Some_vec3f` constructor function takes in a `vec3f`, and uses `bitcast` to store each individaul field of the `vec3f` type into a different slot in the `data` array.

In the above example, the `raymarch` function which returns the `(Option vec3f)` is invoked in the `fragment` function, which then uses a `match` expression on the result to handle both possible variants. The compiled code for that `match` expression looks like this:

```
let surface_normal: Option_vec3f = ...;
switch(surface_normal.discriminant) {
  case 1u: {
    let normal = vec3(
      bitcast<f32>(surface_normal.data[0u]),
      bitcast<f32>(surface_normal.data[1u]),
      bitcast<f32>(surface_normal.data[2u])
    );
    return vec4f(
      vec3f(
        pow(
          ((normal + 1f) * 0.5f),
          vec3f(f32(2.2f))
        )
      ),
      f32(1f)
    );
  }
  default: {
    return vec4f(vec3f(vec3f(f32(0f))), f32(1f));
  }
}
```

The `match` expression gets converted into a `switch` statement with two `case`s, one for each of the arms in the `match`. The first `case`, which checks that the discriminant is `1`, corresponds to the `(Some normal)` branch of the `match` expression. The first statement inside that `case` defines the `normal` binding as a `vec3f` constructed from the elements stored within the `data` field of the `Option_vec3f`, using `bitcast` again to convert the values from `u32`s to `f32`s. The second case, which `default` since it is the last possible case, represents the `None` branch of the `match` expression.

While the compiled wgsl code may look complex, it's actually a very direct and performant translation of the easl code. The `bitcast` operator in wgsl doesn't actually impose any runtime cost - it's simply a compile-time abstraction that lets the type system know you're intentionally reinterpreting the bits of a value, and won't actually insert any instructions in the final compiled machine code. And the `switch` block with the two cases should perform equivalently to an `if` statement that used the `discriminant` of the enum as its condition. So in terms of the actual runtime implications, this compiled code from the earlier raymarching example should perform the same as an alternative hand-written wgsl raymarcher that returned a struct containing both a `bool` for whether the ray converged on the surface and a `vec3f` for the surface normal direction.
