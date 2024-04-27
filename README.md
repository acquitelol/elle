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
    puts("Hello world!");
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
data $main_1 = { b "Hello world!", b 0 }
```

* The `main_1` data segment is used to store the literal string later used in `puts`
* The function is exported, as denoted with the `export` keyword
* The function returns a `w` (`word`), is called `main`, and uses the `$` sigil to denote it is global.
* The `@start` directive describes the beginning of the function
* We then use the `call` operation and the global `puts` function with the `l` (`long`) data section we stored earlier.
* The compiler falls back to returning the literal `0` if no specific return value is specified. Therefore, we `ret` at the end.

* Simple enough! ♡

### ♡ **Example Syntax**

Please keep in mind that syntax such as `arrays`, etc have *not* been implemented at all yet. The code below should compile, but it is indended as a pure example of how the syntax is designed to look.

```dart
// Import statements follow a lib:file@{method1, method2...} format;
// use elle:int@{random};

const languageName = "Elle";

// Use `pub` to make functions public so they can be imported by other files
// You *must* expose the main function for it to be runnable
pub fn main() {
    srand(time(0));

    Int resWithThree = randomWithMultiplier(10, 3); // Returns a random number between 0 and 10 multiplied by 3
    printMessage(resultWithNumber(resWithThree));

    Int maybeRes = randomWithPossibleError();

    if (maybeRes == -1) {
        printMessage("Oh no! We failed.\n");
    } else {
        printMessage(resultWithNumber(maybeRes));
    }
}

fn resultWithNumber(Int num) {
    Char result[64];
    sprintf!2(result, "Result is %d\n", num);

    return result;
}

fn randomWithMultiplier(Int upper, Int multiplier) -> Int {
    // Use the ret keyword to return from the operation
    Int res = rand() % upper;
    return res * multiplier;
}

// Operations can either return a value or void.
fn randomWithPossibleError() -> Int {
    Int result = rand() % 6;

    if (result == 3) {
        return -1;
    } else {
        return result;
    }
}

// No return argument needed if function returns void
// Note that this means the return type is inferred based on the return value
fn printMessage(String message) {
    printf!("[%s] %s", languageName, message);
}
```

* Semicolons are enforced and are required for parsing
* Comments start with // and are ignored when parsing
* Single quotes are strictly for characters
* Double quotes are strictly for strings
* Constants must be at the top level of files & start with `const`
* Returning from functions is done with the `return` keyword

<hr />

### ♡ **Exact literals**

* These expressions will expand into the exact characters you type into the intermediate language code.
* Typing `$storeb 0, %tmp_12$` will write exactly `storeb 0, %tmp_12` into the intermediate language, completely ignoring types, sigils, etc.
* Only use this for basic operations, it is not intended as a replacement for writing Elle code as block-scoped variables are written with a temporary counter and cannot be referenced directly from exact literals.

* The function syntax `func!(a, b, c)` works as follows:
  * `func(a, b, c)` expands to `func(a, b, c)`
  * `func!(a, b, c)` expands to `func(a, $...$, b, c)`
  * `func!2(a, b, c)` expands to `func(a, b, $...$, c)`

> The number after the `!` can be anything. It is intended to handle variadic functions, as in QBE IR, variadic functions must declare the point at which the variadic arguments begin. So while typing the exact literal `$...$` is valid, writing it out every time for each `printf`, `sprintf`, `scanf` call, etc can get exhausting, which is why this macro exists in the first place.

### ♡ **Static buffers**

* You can allocate a buffer with the `Type buf[size]` syntax.
* This expands into `data $buf_5 = { z (size * type_size) }`

#

* Assuming you wrote the above code, you would be able to reference the `buf` variable which is a pointer to the data section that holds the buffer. The buffer may be called anything though, of course.
* Example:

```dart
Char out[128];
gets(out); // This is unsafe but this is only a demonstration
puts(out);
```

### ♡ **Dereferencing pointers**

* Strings are always pointers. This means that you can find an offset from a pointer and then store data at it.
* Example

```dart
String test = "bbbbbbbb"; // Returns a pointer to the start of raw bytes
Long offset = test + 2; // 0th index + 2 = 3rd index

// In this case we're storing a Byte here
offset <- Byte 97; // Stores 'a' at the 3rd index
```

### ♡ **If statements**

* You can define an `if` statement and then an optional `else` statement
* If statement expressions are wrapped in `()`
* There is currently no `else if` or similar. A workaround is to just define another `if statement` with your new condition.
* Example:

```dart
Int a = 0; // Variables must be initialized.

if (expression) {
    a++;
} else {
    a--;
}
```

### ♡ **While loops**

* Even though you can loop via recursion, there is also a while loop primitive available.
* While loop expressions are wrapped in `()`
* There is no `do while`, `finally`, or `for loop` functionality at the time of writing this.
* Example

```dart
while (expression) {
    // do code
}
```

* You also have access to block scoped variables inside of this loop. This means you can create a pseudo `for loop` with the following code:

```dart
Int i = 0;

while (i < 10) {
    printf!("%d\n", i);
    i++;
}
```

<hr />

### ♡ **Base methods**

As a general rule of thumb, if a symbol is inside of the C standard library and is a function (regardless of whether it's dynamically linked or not), it will be callable directly from Elle, granted that you pass the correct arguments.

Due to Elle compiling to QBE IR, this means that we cannot access dynamically-linked non-functional symbols, so this means that globals from C stdlib such as the `stdin` and `stdout` file pointers do not exist. However, you can use the methods below to define them.

* Pointers are always a Long type.
* This isn't *all* the methods available, other methods such as `malloc` and `free` are also defined.
* For more information about the functions available please read <https://bit.ly/stdlib-defs>

#### ♡ **Getting a file pointer to standard input/output**

```dart
Long stdin = fdopen(0, "r");
Long stdout = fdopen(1, "w");
```

#### ♡ **Printing text to the standard output**

```dart
printf!("formatter %d", 1);
puts("strings only, any other types will segfault");
```

#### ♡ **Using `scanf` to get a user input**

```dart
Char buf[64];
scanf!("%63s", buf); // 1 less than the buffer size
```

#### ♡ **Using `fgets` to get a user input**

```dart
Long stdin = fdopen(0, "r");
Char buf[64];
fgets(buf, 64, stdin);
```

#### ♡ **Getting the length of a string**

```dart
String test = "aaaaa";
Long len = strlen(test); // 5
```

#### ♡ **Parsing a string to a number**
>
> Note that this is unlike a type conversion, rather it's assuming the string itself contains a serializable number of the type of the function. `"55" -> 55`, `"abc" -> 0`

```dart
String x = "5";
Int xAsInt = atoi(x);
Long xAsLong = atol(x);
```

> If you have any questions, please [raise an issue](https://github.com/acquitelol/elle/issues/new) :3

### ♡ **How to run**

* Elle has no third-party dependencies, so you can simply do the following:
  * Ensure you have [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html), [Rust](https://www.rust-lang.org/), and the [QBE](https://c9x.me/compile/) compiler backend.

    ```terminal
        $ git clone https://github.com/acquitelol/elle
    ```

    ```terminal
        $ cd elle
    ```

    ```console
        $ gmake compile
    ```

  * **You're done!**

> You can now edit the `main.elle` file in the `dist` directory, and then rebuild (via `gmake compile`) to see the output.

### ♡ **Licensing**

* Copyright © 2024 Rosie ([acquitelol](https://github.com/acquitelol))

<hr />

<a href="#top">⇡ Back to top️!</a>
