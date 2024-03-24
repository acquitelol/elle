# ₊˚ Elle ♡︎

## An experimental, purely functional language built in Rust

#### This language is not designed to be used by any developer, it is intended simply as an experiment to better understand the internals of modern languages and how they work :3

### ✩ *If you like this project, consider giving it a star!* ✩

### ♡ **Example Syntax**

```ts
// Import statements follow a lib:file@{method1, method2...} format;
use elle:io@{print};
use elle:int@{random};

// Use `pub` to make functions public so they can be imported by other files
// You *must* expose the main function for it to be runnable
pub op main() {
    let resWithThree: Int = randomWithMultiplier(3); // Returns a random number between 0 and 10 multiplied by 3 using positional arguments
    let resWithSixteen: Int = randomWithMultiplier(multiplier: 16); // Returns a random number between 0 and 10 multiplied by 16 using keyword arguments
    printMessage(`First result is %{resWithThree} and second is %{resWithSixteen}`);

    let maybeRes: Int? = randomWithPossibleError();

    if (maybeRes) {
        // In this scope, maybeRes is just Int32 not Int32?
        printMessage(`Result is %{maybeRes}`);
    } else {
        // This is a character because it's a single quote.
        printMessage('a');
        printMessage("Oh no! We failed.");
    }
}

op randomWithMultiplier(Int multiplier) -> Int {
    // If a function uses keyword arguments they must *all* be keyword arguments
    // Use the ret keyword to return from the operation
    ret random(between: 0, and: 10, included: true) * multiplier;
}

// Operations can either return a value or void.
// `nil` is the undefined/null value in Elle.
// Use the '?' operator at the end of the return type to denote that the function can return nil.
op randomWithPossibleError() -> Int? {
    let result: Int = random(0, 5, true);

    // Match keyword works very similar to other languages
    ret match result {
        3 -> nil,
        val -> val,
    }
}

// No return argument needed if function returns void
// Note that this is *only* if the function returns void
op printMessage(String message) {
    print(`[Elle] %{message}`);
}
```

* Semicolons are enforced
* Comments start with //
* Backticks (`) are required for string interpolation
* Single quotes are strictly for characters
* Double quotes are strictly for strings

> If you have any questions, please [raise an issue](https://github.com/acquitelol/elle/issues/new) :3

### ♡ **How to build**

* Ensure you have [cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html) and [rust](https://www.rust-lang.org/).
* Run `cargo run`
* **You're done!**

### ♡ **Licensing**

* Copyright © 2024 Rosie ([acquitelol](https://github.com/acquitelol))

<hr />

<a href="#top">⇡ Back to top️!</a>
