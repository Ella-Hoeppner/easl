# Easl, the Enhanced Abstraction Shader Language
A shader language with a lispy syntax that compiles to wgsl.

Easl provides a powerful type system and supports high-level abstractions that aren't available in traditional shader languages. Easl is currently a work-in-progress, and breaking changes should be expected, but it's already mature enough for certain use-cases.

This repository contains the core compiler code. This repository can be used as a crate, so you use the easl compiler directly as part of a rust project to convert easl source code to wgsl code. If you want to use easl in a standalone way, rather than as a crate from another rust project, check out the [Easl CLI](https://github.com/Ella-Hoeppner/easl_cli).

## Feature goals:
| Feature | Explanation | Implementation Status |
| :------ | :---------- | :-------------------: |
| Full feature parity with wgsl | Anything that can be expressed with wgsl will also be directly expressible with easl, including all builtin functions and control operators. | 🚧* |
| Fully expression-based | Inline `if` and `match` blocks, and scoped `let` blocks that return values | ✅ |
| Powerful type inference | Top-level functions are required to have explicit type signatures for clarity, but inside the body of a function, Easl has a type inference system that obviates the need for most explicit type ascriptions. | ✅ |
| Generic types and functions | | ✅ |
| Function overloading | You can overload functions with multiple different type different signatures, including built-in functions like `+`. | ✅ |
| Sum Types | aka rust-style Enums | ✅* |
| Tuples | | ❌ |
| Higher-order functions | Functions that can accept other functions as inputs, and return other functions as outputs | ✅* |
| Closures | Anonymous functions that capture variables from the scope in which they're created | ❌ |
| Type Constraints | Similar to typeclasses/traits/interfaces, but able to coexist seamlessly with arbitrary function overloading | 🚧 |
| Anonymous Structs | Structs without names, characterized only by the names and types of their fields. Useful for grouping values together in a way that offers more clarity than a tuple, without having to explicitly declare a new type. | ❌ |
| Row Polymorphism | The ability to define functions that operate on any struct matching a certain shape, e.g. a function that can operate over any struct type with a field named `x` | ❌ |
| Algebraic Effects | Sophisticated, type-safe manipulation of control flow. There will be some limitations compared to other algebraic effect systems: continuations will be single-shot, cannot escape the scope of the handler, and can only be called in the tail position | 🚧 |
| CPU-side interpreter | While the primary goal of easl is to be a shader language that is executed on the GPU, there will also be an interpreter for the language that can run on the CPU. This will be useful for testing and debugging code in ways that are impossible on the GPU, and for making simple applications with both CPU and GPU logic without having to use a separate host language. | ❌ |

* All core types, math functions, and control flow operations from wgsl are already implemented. Missing features include atomics, barrier functions, texture types other than basic 2d textures, and extension features like subgroup/quad functions. Finishing support for these missing features is a top priority.
* Sum Types are supported, but for now may only contain at most a single internal field. This limitation will be resolved as soon as tuple are implemented.
* Higher-order functions currently have a significant restriction: all function-type arguments must be inlinable at compile time. A function `f` that takes another function as an argument may be called like `(f g)`, but not like `(f (if condition f g))`, because the compiler needs to be able to inline the higher-order argument at compile time. If you violate this restriction, you'll get a compilation error. This limitation will eventually be resolved, but is blocked behind several other unimplemented internal features, so it may take some time.

---

Easl doesn't yet have fully-fledged documentation. For now, an explanation of easl's syntax and language features is available in [syntax.md](https://github.com/Ella-Hoeppner/easl/blob/main/syntax.md), and some [simple example shaders are included with the Easl CLI repo](https://github.com/Ella-Hoeppner/easl_cli/tree/main/examples).
