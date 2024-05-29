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

    return 0;
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

* The function syntax `func.(a, b, c)` works as follows:
  * `func(a, b, c)` expands to `func(a, b, c)`
  * `func.(a, b, c, d)` expands to `func(4, $...$, a, b, c, d)`
  * `func.(a, b, c)` expands to `func(3, $...$, a, b, c)`
  * `func.(a, b)` expands to `func(2, $...$, a, b)`

> The number placed before the `$...$` is the number of arguments. This is a simple way to allow to get the number of arguments that were passed into the function without needing to manually specify them.

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

### ♡ **Standalone blocks**

* A standalone block is somewhat equivalent to an `if (true)` statement, although they are not implemented exactly the same internally. Therefore, `defer` behaves the same.

Here's a simple example:

```dart
pub fn main() {
    Int a = 0;

    {
        a++;
        // If we do *something* here like calling defer then
        // the defer would run when this block leaves its scope
    }

    return 0;
}
```

And it is relatively clear how this code is essentially equal to:

```dart
pub fn main() {
    Int a = 0;

    if (true) {
        a++;
        // If we do *something* here like calling defer then
        // the defer would run when this block leaves its scope
    }

    return 0;
}
```

### ♡ **While loops**

* Even though you can loop via recursion, there is also a while loop primitive available.
* While loop expressions are wrapped in `()`
* There is no `do while` or `finally` functionality at the time of writing this.
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

### ♡ **For loops**

* Iterates from one to the other.
* Basic example:

```dart
for i = 0 to 10 {
    printf!("%d\n", i);
}
```

This works, but there are some other features that are available when using for loops:

* Step size:

```dart
for i = 0 to 10 step 2 {
    printf!("%d\n", i);
}
```

* Iterator types:

```dart
for Double i = 0 to 1.5 step 0.5 {
    printf!("%f\n", i);
}
```

Keep in mind that these for loops are just syntactic sugar for while loops. This means that internally, they just create a while loop.

<hr />

### ♡ **Variadic Functions**

A variadic function is a function that can take in a variable amount of arguments. This works similar to C except that there are macros which allow you to get the argument size.

Here's a basic example of a variadic function which takes in any amount of arguments and returns their sum:

```dart
fn add(Int size, ...) {
    Int res = 0;
    Variadic args[size];
    Defer free(args);

    for _ = 0 to size - 1 {
        res += args yield Int;
    }

    return res;
}
```

Let's go through an explanation for how this works:

* L1: Declare the function signature. We declare an `Int size` as a static positional argument, then the `...` denotes that the function is variaidic.
* L2: Initialize the result at `0`.
* L3: Declare the `args` variable as a pointer to the start of the variadic arguments. This is denoted by `Variadic name[size]`. You can also not declare a size by writing `Variadic name[]`, and this allocates memory for a fixed 8 bytes (64 bits). This call internally calls the `malloc` function with the size specified and then calls `vastart` on the returned pointer.
* L4: Defer the call to `free`. This means that `free` will only be called on the pointer when the function is about to go out of scope, ie at any return statements or an implicit function exit for subroutines that don't return a value.
* L6: Declare a for loop with an unused iterator from 0 to the size - 1. This will allow you to loop through all of the arguments that will be provided by the user. This is necessary because you can yield arguments forever, however if you don't know how many there are then you will enter uninitialized memory. A method of passing the argument length will be shown later at the call site.
* L7: Yield the next argument from the `args` pointer as an `Int` type, and add it to the result value
* L9: Return the summed value. Right before this point, the `free` call that we deferred earlier would be called.

At the call-site, using this function is very easy. It can be done like this:

```dart
pub fn main() {
    Int res = add.(1, 2, 3, 4);
    printf!("%d\n", res);
    return 0;
}
```

Notice the `add.(a, b)` syntax. This is a compile time macro which automatically adds the argument length as the 0th argument of the function, substituting it for the size of the variadic function. This means that calling `add.(a, b, c)` is actually identical to calling `add!(3, a, b, c)`, you simply no longer need to pass the argument length manually, like in C.

<hr />

### ♡ **Defer statements**

A defer statement is commonly used to group together memory allocation and deallocation. A simple explanation is that it consumes whatever operation is defined inside and only runs this code when the function is about to go out of scope, ie during a return in a block/if statement/while loop, or an implicit return due to the function scope being left.

A very simple example of this is declaring a variable and deferring printing its value, like this:

```dart
fn print_int(Int num) {
    printf!("%d\n", num);
}

pub fn main() {
    Int i = 0;

    // If this were not in a defer statement, then this would print 0
    // However, it will print 25 instead.
    Defer print_int(i);

    i += 5;
    i *= i;

    return 0;
}
```
You can see how this only calls `print_int` right before it returns 0, which is indeed *after* the `i` variable has had changes made to it. This also works if you return in other scopes, such as if statements, while loops, standalone blocks, etc, as stated above. Any defer statements in inner blocks will not be called on any return, rather will only be called when the inner block is about to leave scope.

This also means that if you, hypothetically, design a program like this
```dart
fn print_int(Int num) {
    printf!("%d\n", num);
}

pub fn main() {
    Int i = 0;
    Defer print_int(i);

    {
        Defer print_int(i);
        i += 2;
    }

    i *= i;
    return 0;
}
```

The expected output is 2, then 4.
This is because it will call `print_int` once when the standalone block will leave scope, at which point `i` is 2, then it will call `print_int` again when the function itself will leave scope, at which it will be 4 because `i` was squared (`i *= i`).

You can also write something like this:
```dart
pub fn main() {
    Int i = 0;
    Defer print_int(i);

    {
        Defer print_int(i);
        i += 2;

        {
            return 0;
        }
    }

    i *= i;
    return 0;
}
```
Here we expect `i` (`2`) to be printed to the console twice. Why? When the function returns, the scope created by the standalone block is also inherently about to be left. Hence, we also need to call all non-root deferrers here.

The most useful application of deferring is for memory management, however.

Consider this code:
```dart
pub fn main() {
    Long size = 10;
    Pointer numbers = malloc(size * 8); // 8 = size of a Long
    Defer free(numbers);

    for Long i = 0 to size - 1 {
        numbers[i * 8] = i * 2;
        Long res = numbers[i];
        printf!("numbers[%ld] = %ld\n", i, res);
    }

    // (Ignore that this never runs)
    if (numbers[2] + 1 * 5 == 10) {
        // Calls `free` here
        return 1;
    }

    // Calls `free` here
    return 0;
}
```

Without deferring, you would have to call `free` at every single place where you return. Not only is this inefficient, but also very easy to forget.

Of course for a function like the above, you are able to determine what path the code will take at compile time, however if you use something like `rand()` you no longer have the ability to do this, so you need to call `free` manually at all points where the function leaves its scope. This is an elegant method to prevent that.

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
