# ₊˚ Elle ♡︎

## An experimental, purely functional language built in Rust

#### This language is not designed to be used by any developer, it is intended simply as an experiment to better understand the internals of modern languages and how they work :3

### ✩ *If you like this project, consider giving it a star!* ✩

### ♡ **Hello, World!**

Writing a hello world program in Elle is super easy.
<br>
In fact it's very similar to C!

```ts
pub fn main() {
    puts("Hello world!\n");
}
```

Let's dissect the code:

* The `pub` keyword declares the main function as public/exported
* The `fn` keyword declares the identifier as a function
* The word `main` defines the function as the entry point of our program.
* The function call `puts` is interoperable with C. It takes the 0th argument and writes it to the standard output.

* That's it! ✩

Elle uses the QBE compiler backend. This means that files compile into QBE's intermediate language before being executed.

Let's also take a look at the QBE IL source:

```ts
export function w $main() {
@start
    %tmp_2 =w call $puts(l $main_1)
    ret
}
data $main_1 = { b "Hello world!\n", b 0 }
```

* The `main_1` data segment is used to store the literal string later used in `puts`
* The function is exported, as denoted with the `export` keyword
* The function returns a `w` (`word`), is called `main`, and uses the `$` sigil to denote it is global.
* The `@start` directive describes the beginning of the function
* We then use the `call` operation and the global `puts` function with the `l` (`long`) data section we stored earlier.
* The compiler falls back to returning the literal `0` if no specific return value is specified. Therefore, we `ret` at the end.

* Simple enough! ♡

### ♡ **Example Syntax**

Please keep in mind that syntax such as `if statements`, `loops`, `optional types`, `arrays`, `pointers`, `references`, etc have *not* been implemented at all yet. This means that the code below will *not* compile. It is indended as a pure example of how the syntax is designed to look later on.

```rs
// Import statements follow a lib:file@{method1, method2...} format;
use elle:int@{random};

const languageName = "Elle";

// Use `pub` to make functions public so they can be imported by other files
// You *must* expose the main function for it to be runnable
pub fn main() {
    Int resWithThree = randomWithMultiplier(3); // Returns a random number between 0 and 10 multiplied by 3
    printMessage("Result is %d", resWithThree);

    Int maybeRes = randomWithPossibleError();

    if (maybeRes == -1) {
        // This is a character because it's a single quote.
        printMessage('a');
        printMessage("Oh no! We failed.");
    } else {
        printMessage("Result is %d", maybeRes);
    }
}

fn randomWithMultiplier(Int multiplier) -> Int {
    // Use the ret keyword to return from the operation
    ret random(0, 10, true) * multiplier;
}

// Operations can either return a value or void.
// `nil` is the undefined/null value in Elle.
// Use the '?' operator at the end of the return type to denote that the function can return nil.
fn randomWithPossibleError() -> Int {
    Int result = random(0, 5, true);

    ret if (result == 3) {
        -1;
    } else {
        val;
    }
}

// No return argument needed if function returns void
// Note that this means the return type is inferred based on the return value
op printMessage(String message) {
    printf!("[%s] %s", languageName, message);
}
```

* Semicolons are enforced and are required for parsing
* Comments start with // and are ignored when parsing
* Single quotes are strictly for characters
* Double quotes are strictly for strings
* Constants must be at the top level of files & start with `const`
* Returning from operations is done with the `ret` keyword

> If you have any questions, please [raise an issue](https://github.com/acquitelol/elle/issues/new) :3

### ♡ **How to run**

* Elle has no third-party dependencies, so you can simply do the following:
  * Ensure you have [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html), [Rust](https://www.rust-lang.org/), and the [QBE](https://c9x.me/compile/) compiler backend.

    ```terminal
        $ git clone https://github.com/acquitelol/elle
    ```

    ```terminal
        $ cd elle/dist
    ```

    ```console
        $ cargo run
    ```

    ```terminal
        $ qbe -o out.s main.ssa && cc out.s && ./a.out
    ```

  * **You're done!**

> You can now edit the `main.elle` file in the `dist` directory, and then rebuild to see the output.

### ♡ **Licensing**

* Copyright © 2024 Rosie ([acquitelol](https://github.com/acquitelol))

<hr />

<a href="#top">⇡ Back to top️!</a>
