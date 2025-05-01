### ♡ **How is this better than C?**

- It's not. It never will be.

### ♡ **Feature list**

| Feature                  | Support | Notes                                                                                                        |
| ------------------------ | ------- | ------------------------------------------------------------------------------------------------------------ |
| Custom Allocators        | Yes     | The allocator can be changed at runtime, but must be GC or Arena by default.                                 |
| Lambdas (Closures)       | Partial | Lambdas exist, but don't capture their surrounding scopes.                                                   |
| Pattern matching         | No      | Support planned in the future.                                                                               |
| Operator overloading     | Partial | Can overload `x[y]`, `&x[y]`, `x[y] = z` `x == y`, aswell as length and hash.                                |
| First class functions    | Partial | Functions can be passed around, but decay to a fallback with no interface.                                   |
| Generics                 | Partial | Generic functions and structs exist, generic enums do not currently exist but are planned.                   |
| Type inference           | Yes     | The return type of functions is inferred. You can use `let x = 1` or `x := 1` to infer variable types        |
| Concurrency              | No      | Support planned in the future.                                                                               |
| `Maybe`/`Option`         | Yes     | Pointers can be `nil`, but an additional `Option` feature exists in the stdlib.                              |
| Manual memory management | Yes     | Use `mem::malloc` and `mem::free` to allocate without the GC, or use `#set_allocator(HeapAllocator::new())`. |
| Coroutines               | No      | Support planned in the future.                                                                               |
| Rich errors              | Partial | `nil` values + enums + tuples + `$panic` allow for decent error handling, but no standard has been set.      |
| Macros/metaprogramming   | Partial | Constants act as macros in a way, they will be recreated each time they're referenced.                       |
| Modules/namespaces       | Partial | Functions and constants can be namespaces but only 1 level deep. Modules exist but do not qualify imports.   |
| Tail call optimization   | No      | Support planned in the future.                                                                               |
| C FFI                    | Yes     | All C code is callable directly within Elle and vice versa.                                                  |
| Fast compilation times   | Partial | All projects should compile < 1s, but it is being constantly improved.                                       |
| LSP                      | Partial | Yes, but only for Zed. A language client extension doesn't exist for editors like NVIM or VSCode yet.        |
| Standard formatter       | No      | Support planned in the future.                                                                               |
| GC Tuning                | No      | Support planned in the future.                                                                               |
| Standard library         | Partial | A standard library exists but is lacking features and documentation                                          |
| Introspection            | Partial | Only introspection at runtime of function call metadata at the call site, structs cannot be reflected        |
| Deferring                | Yes     | Currently slightly broken, but should not affect most use-cases                                              |

| What kind of...   | Answer          | Notes                                                                       |
| ----------------- | --------------- | --------------------------------------------------------------------------- |
| Syntax            | C/Rust style    | Can look like C or rust depending on the features used.                     |
| Paradigm          | Procedural      | Functions can be values, but primarily the language is procedural           |
| Memory management | GC              | GC by default, can become Arena by default, can be swapped to anything      |
| Typing            | Static          | The language is statically typed                                            |
| Mutability        | Mutable         | All variables are mutable, always                                           |
| Lifetimes         | N/A             | GC means the lifetime of objects is not explicitly managed by the developer |
| Compiler backend  | QBE             | LLVM is too bloated for my needs, therefore this uses QBE instead           |
| Compilation model | AOT             | Compiled to native code (via QBE)                                           |
| Target platform   | Native only     | Only supports whatever platforms QBE supports, with WASM support planned    |
| Strings           | Null terminated | To be fully C ABI compliant, strings are null terminated and not sized      |
| Extension         | `.le`           | History: `.elle` -> [`.elle`, `.l`] -> [`.elle`, `.le`] -> `.le`            |
| License           | GPL-3           | Since April 11th 2025 3:18 PM GMT+1                                         |
| Wrapping          | Curly only      | Prefers `if x { y }` rather than `if (x) y` (curly braces are mandatory)    |
| Loops             | Iterative only  | `for x := 1; x < 10; x += 1` or `for x in [1, 2, 3]` or `for x in 0..10`    |
| Enums             | Basic           | Non-generic enums with an optional literal value, **not** tagged unions     |
| Namespaces        | Basic           | Functions and constants can be namespaced, but only 1 level deep            |
| Modules           | Basic           | Modules do not automatically namespace anything                             |
