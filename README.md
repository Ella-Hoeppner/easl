# easl

[The **E**nhanced **A**bstraction **S**hader **L**anguage](https://easl-lang.org/)

A lispy shader language that compiles to [WGSL](https://www.w3.org/TR/WGSL/), for use in the WebGPU ecosystem.

Easl keeps all the expressiveness of WGSL, and adds a bunch of other high-level features: 

* Sum types and exhaustive `match` blocks
* Closures
* Higher-order functions
* Expression-based syntax with `let`, `if`, and `match` expressions usable at any level of depth

Easl also has a CPU runtime that can open windows, dispatch shaders, automatically sync data between the CPU and GPU, and even generate audio. This makes easl an ideal language for making generative art, creating synthesizers, and building shader-powered GUIs without the need for complex graphics libraries.

For documentation, check out **[easl-lang.org](https://easl-lang.org/)**. To quickly jump in and see some examples of what the language can do, check out the [web editor](https://editor.easl-lang.org/). If you're already familiar with shader programming and want to see a showcase of easl's unique features, try checking out the [Raymarching walkthrough](https://easl-lang.org/raymarch.html). 

## Local development

To run easl programs locally, install the [easl CLI](https://github.com/Ella-Hoeppner/easl_cli). (it needs the wasm32 target, for compiling programs to web pages):

```
git clone https://github.com/Ella-Hoeppner/easl_cli
cd easl_cli && cargo install --path .
easl run examples/simple.easl
```

To compile easl programs to the web, you'll need to add the `wasm32` target for rust installed. If you use rustup, that should be as simple as:

```
rustup target add wasm32-unknown-unknown 
```

`easl run my_file.easl` runs a program, `easl compile my_file.easl` compiles it to WGSL (or, with `--web`, to a web page), and `easl format` formats it.

There's also a [language server with a client for VS Code](https://github.com/Ella-Hoeppner/easl_lsp).