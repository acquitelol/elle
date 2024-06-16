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

<hr />

### ♡ **Exact literals**

* These expressions will expand into the exact characters you type into the intermediate language code.
* Typing `#storeb 0, %tmp_12#` will write exactly `storeb 0, %tmp_12` into the intermediate language, completely ignoring types, sigils, etc.
* Only use this for basic operations, it is not intended as a replacement for writing Elle code as block-scoped variables are written with a temporary counter and cannot be referenced directly from exact literals.

* The function syntax `func!(a, b, c)` works as follows:
  * `func(a, b, c)` expands to `func(a, b, c)`
  * `func!(a, b, c)` expands to `func(a, $...$, b, c)`
  * `func!2(a, b, c)` expands to `func(a, b, $...$, c)`

> The number after the `!` can be anything. It is intended to handle variadic functions, as in QBE IR, variadic functions must declare the point at which the variadic arguments begin. So while typing the exact literal `#...#` is valid, writing it out every time for each `printf`, `sprintf`, `scanf` call, etc can get exhausting, which is why this macro exists in the first place.

* The function syntax `func.(a, b, c)` works as follows:
  * `func(a, b, c)` expands to `func(a, b, c)`
  * `func.(a, b, c, d)` expands to `func(4, $...$, a, b, c, d)`
  * `func.(a, b, c)` expands to `func(3, $...$, a, b, c)`
  * `func.(a, b)` expands to `func(2, $...$, a, b)`

> The number placed before the `#...#` is the number of arguments. This is a simple way to allow to get the number of arguments that were passed into the function without needing to manually specify them.

### ♡ **Static buffers**

* You can allocate a buffer with the `type buf[size]` syntax.
* This expands into `data $buf_5 = { z (size * type_size) }`

#

* Assuming you wrote the above code, you would be able to reference the `buf` variable which is a pointer to the data section that holds the buffer. The buffer may be called anything though, of course.
* Example:

```c++
char out[128];
gets(out); // This is unsafe but this is only a demonstration
puts(out);
```

### ♡ **Dereferencing pointers**

* Strings are always pointers. This means that you can find an offset from a pointer and then store data at it.
* Example

```cpp
char *test = "bbbbbbbb"; // Returns a pointer to the start of raw bytes
test[2] = 97; // Stores 'a' at the 3rd index
```

### ♡ **If statements**

* You can define an `if` statement and then an optional `else` statement
* If statement expressions can be wrapped in `()` but this is not mandatory
* There is currently no `else if` or similar. A workaround is to just define another `if statement` with your new condition.
* Example:

```cpp
int a = 0; // Variables must be initialized.

if expression {
    a++;
} else {
    a--;
}
```

### ♡ **Standalone blocks**

* A standalone block is somewhat equivalent to an `if true` statement, although they are not implemented exactly the same internally. Therefore, `defer` behaves the same.

Here's a simple example:

```cpp
pub fn main() {
    int a = 0;

    {
        a++;
        // If we do *something* here like calling defer then
        // the defer would run when this block leaves its scope
    }

    return 0;
}
```

And it is relatively clear how this code is essentially equal to:

```cpp
pub fn main() {
    int a = 0;

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
* While loop expressions can be wrapped in `()` but this is not mandatory
* There is no `do while` or `finally` functionality at the time of writing this.
* Example

```cpp
while expression {
    // do code
}
```

* You also have access to block scoped variables inside of this loop. This means you can create a pseudo `for loop` with the following code:

```cpp
int i = 0;

while (i < 10) {
    printf!("%d\n", i);
    i++;
}
```

<hr />

### ♡ **For loops**

* Iterates from one to the other.
* For loop expressions can be wrapped in `()` but this is not mandatory
* Basic example:

```cpp
for (int i = 0; i < 10; i++) {
    printf!("%d\n", i);
}
```

* More advanced example:
```cpp
fn fact(long n) -> long {
    if n <= 1 {
        return 1;
    }

    return n * fact(n - 1);
}

fn get_e() {
    double res = 0.0;

    for long i = 0; i < 50; i++ {
        res += 1.0 / fact(i);
    }

    return res;
}

pub fn main() {
    double e = get_e();
    printf!("e = %.50f\n", e);
    return 0;
}
```

Keep in mind that these for loops are just syntactic sugar for while loops. This means that internally, they just create a while loop.

<hr />

### ♡ **Variadic Functions**

A variadic function is a function that can take in a variable amount of arguments. This works similar to C except that there are macros which allow you to get the argument size.

Here's a basic example of a variadic function which takes in any amount of arguments and returns their sum:

```cpp
fn add(int size, ...) {
    int res = 0;
    variadic args[size];
    defer free(args);

    for (int _ = 0; _ < size - 1; _++) {
        res += args yield int;
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

```cpp
pub fn main() {
    int res = add.(1, 2, 3, 4);
    printf!("%d\n", res);
    return 0;
}
```

Notice the `add.(a, b)` syntax. This is a compile time macro which automatically adds the argument length as the 0th argument of the function, substituting it for the size of the variadic function. This means that calling `add.(a, b, c)` is actually identical to calling `add!(3, a, b, c)`, you simply no longer need to pass the argument length manually, like in C.

<hr />

### ♡ **Defer statements**

A defer statement is commonly used to group together memory allocation and deallocation. A simple explanation is that it consumes whatever operation is defined inside and only runs this code when the function is about to go out of scope, ie during a return in a block/if statement/while loop, or an implicit return due to the function scope being left.

A very simple example of this is declaring a variable and deferring printing its value, like this:

```cpp
fn print_int(int num) {
    printf!("%d\n", num);
}

pub fn main() {
    int i = 0;

    // If this were not in a defer statement, then this would print 0
    // However, it will print 25 instead.
    defer print_int(i);

    i += 5;
    i *= i;

    return 0;
}
```
You can see how this only calls `print_int` right before it returns 0, which is indeed *after* the `i` variable has had changes made to it. This also works if you return in other scopes, such as if statements, while loops, standalone blocks, etc, as stated above. Any defer statements in inner blocks will not be called on any return, rather will only be called when the inner block is about to leave scope.

This also means that if you, hypothetically, design a program like this
```cpp
fn print_int(int num) {
    printf!("%d\n", num);
}

pub fn main() {
    int i = 0;
    defer print_int(i);

    {
        defer print_int(i);
        i += 2;
    }

    i *= i;
    return 0;
}
```

The expected output is 2, then 4.
This is because it will call `print_int` once when the standalone block will leave scope, at which point `i` is 2, then it will call `print_int` again when the function itself will leave scope, at which it will be 4 because `i` was squared (`i *= i`).

You can also write something like this:
```cpp
pub fn main() {
    int i = 0;
    defer print_int(i);

    {
        defer print_int(i);
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
```cpp
pub fn main() {
    long size = 10;
    long *numbers = malloc(size * 8); // 8 = size of a Long
    defer free(numbers);

    for (long i = 0; i < size - 1; i++) {
        numbers[i] = i * 2;
        long res = numbers[i];
        printf!("numbers[%ld] = %ld\n", i, res);
    }

    // (Ignore that this never runs)
    if numbers[2] + 1 * 5 == 10 {
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

### ♡ **Type definitions**

Elle's types are quite similar to C in terms of their definition. They can be a recursive pointer type too such as `char *`. Although C has a limit on the number of pointers that a type can have, Elle does not because types are options and as such, the concept of a `void *` does not exist.

These are the mappings of types in Elle:

`nil` - No type. Can be used to give a function that doesn't return anything an explicit return type. Keep in mind that this is purely semantic and cannot be used as a standalone type for variables.
`bool` - A mapping to `int`, and works purely as a semantic for boolean literals like `true` or `false` that expand to `1` or `0` respectively.
`char` - A mapping to `byte` representing a character in ASCII.
`int` - A "word", also knows as a 32 bit signed integer.
`long` - A signed integer of the size specified by your computer's architecture. On x64 computers this is a 64-bit signed integer, while on x86 computers this is a 32-bit signed integer.
`single` - A 32-bit signed floating point number.
`float` - A mapping to a `single`.
`double` - A 64-bit signed floating point number, providing double the precision of a `single`.
`function` - A type that maps to a `byte`. This is intended to be used as a pointer to the first byte of a function definition.
`pointer` - Denoted by `<type> *` -> As pointers are just a number, an address in memory, a pointer in ELle is just a `long` that holds extra context by holding another type. This is most useful when creating an array to be able to determine the offset multiplier when indexing. You could replace all pointer types with `long` however you lose the context of an inner type that the pointer is.. pointing to, so you may get an incorrect offset based on the inner type of the pointer.
`string` - A mapping to a `char *`, which is a pointer to the start of the array of bytes (characters).



### ♡ **Type Conversions / Type Casting**

You can cast a type in the exact same manner as C:

```cpp
pub fn main() {
    float a = 1.5;
    int b = (int)a + 2;
}
```

You can also cast to pointer types, however note that, unlike C, casting to a pointer type when using `malloc` is *not* necessary because malloc is not interfaced in the Elle internals.

This means you can write:
```cpp
pub fn main() {
    double *a = malloc(1024 * 8); // Where 8 = size of a double
}
```
and Elle will not complain. You can, if you wish, cast it, however it will have no effect at the moment.

### ♡ **Unary operators**

There are 3 unary operators in Elle:
`!`, `-`, and `+`.

Any identifier or literal can be prefixed by one of these operators.

Example:

```cpp
pub fn main() {
    bool myBool = false;

    if !myBool {
        puts("Hello world!");
    }
}
```

### ♡ **Arithmetic operations**

All arithmetic operations are declared with 2 expressions on the left and right of an operator. This means you can call functions, do other arithmetic operations inside of operations, etc.

This is the mapping defined by Elle:

`^` - Xor
`*` - Multiply
`/` - Divide
`+` - Add
`-` - Subtract
`%` - Modulus

Keep in mind that you can also assign these to other values.
This means the following code is valid:

```cpp
pub fn main() {
    int a = 1;
    a ^= 1; // a is now 0;
    printf!("%d", a);

    return 0;
}
```

And of course, this works for every arithmetic operator.

By default, Elle follows the [order of operations](https://github.com/acquitelol/elle/blob/rewrite/src/lexer/enums.rs#L88-L99) described by mathematics (typically named as BIDMAS or PEMDAS), which means you can also wrap expressions in `()` to evaluate them before other expressions.

Example:
```cpp
pub fn main() {
    int a = 1 + (5 ^ 2); // Xor has a lower precedence than addition

    // We're expencting this to be 8
    // because 5 ^ 2 = 7 and 7 + 1 = 8
    // without the brackets it would be 4
    // because it would be 6 ^ 2 = 4
    printf!("%d", a);

    return 0;
}
```

### ♡ **Array literals**

You can create array literals in a very similar manner to a language like C:

```cpp
const long MAX_SIGNED_LONG = 9_223_372_036_854_775_807;
const long MIN_SIGNED_LONG = -MAX_SIGNED_LONG - 1;

pub fn main() {
    long test[] = {MAX_SIGNED_LONG, MIN_SIGNED_LONG, -39};

    for (int i = 0; i < #arrlen(test); i++) {
        printf!("test[%d] = %ld\n", i, test[i]);
    }

    return 0;
}
```

You can also specify a size in between the square brackets.
`long test[3] = {MAX_SIGNED_LONG, MIN_SIGNED_LONG, -39};`

These arrays create a buffer of the size specified or the length of the array multiplied by the size of the type of each value.

Keep in mind that this is the length of the array not the size in bytes.

### ♡ **Size directives**

There are currently 2 size directives in Elle:
`#size()` and `#arrlen()`

You can put both **types** and **buffers** inside of the `#size()` directive and it returns the size of the identifier verbatim.

You can only place **buffers** inside of the `#arrlen()` directive as it returns the size of the buffer divided by the size of each type. This is exactly equivalent to `#size(arr) / #size(arr_type`

### ♡ **Constants**

Anything at the top-level must be prefixed by `const` or `fn`. Constants can be public. A constant internally creates a function that is automatically called when the constant is referenced. Constants that create pointers are called at the top level of each function to bring them in scope.

Example:
```cpp
const int WIDTH = 100;
const int HEIGHT = 24;
const int SIZE = WIDTH * HEIGHT;

pub fn main() {
    printf!("%d\n", SIZE);
    return 0;
}
```

In the above code, all of the constants are technically function definitions that return the value after the `=` sign. However, when they're referenced, the function is automatically called. Therefore, you dont need to type `SIZE()` or similar, you can just directly reference `SIZE` as if it was a constant.

It indeed is a constant, because although it can return a different value (it can call any function), it cannot be redeclared.

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

> You can now `gmake compile` to compile the compiler into an executable, then run `gmake run <name>` to run a file from the `/examples` directory. For example, you can run `gmake run donut` and it will run `/examples/donut.elle`.

### ♡ **Licensing**

* Copyright © 2024 Rosie ([acquitelol](https://github.com/acquitelol))

<hr />

<a href="#top">⇡ Back to top️!</a>
