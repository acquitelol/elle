# ₊˚ Elle ♡︎

### A procedural programming language built in Rust and QBE

‎ ‎ ╱|、
<br />
(˚ˎ 。7
<br />
|、˜〵
<br />
じしˍ,)ノ
<br />

### ♡ **How is this better than C?**

- It's not. It never will be. As it stands, this is a project developed by a single person, me. I am neither smart enough nor efficient enough to mimic an enterprise programming language compiler such as clang.

### ✩ *If you like this project, consider giving it a star!* ✩

### ♡ **Hello, World!**

Writing a hello world program in Elle is super easy:

```c
use std/io;

fn main() {
    io::println("Hello world!");
}
```

Let's dissect the code:

* The `fn` keyword declares the identifier as a function
* The word `main` defines the function as the entry point of our program.
* The function call `io::println` is a function which prints all of its arguments.

* Simple enough! ♡

<hr />

### ♡ **If statements**

* An if statement is an expression that evaluates a block if the condition is non-zero, with an optional `else` block which is evaluated if the condition **is** zero.

* You can define an `if` statement and then an optional `else` statement
* If statement expressions can be wrapped in `()` but this is not mandatory
* There is currently no `else if` or similar. A workaround is to just define another `if statement` with your new condition.
* Example:

```c
i32 a = 0;

if expression {
    a += 1;
} else {
    a -= 1;
}
```

<hr />

### ♡ **While loops**

* A while loop is an expression that evaluates the block specified **only** if the condition is non-zero, otherwise breaks and continues execution on the primary branch.

* Even though you can loop via recursion, the while loop primitive may be simpler to understand and use in many cases, therefore it is provided in Elle.
* While loop expressions can be wrapped in `()` but this is not mandatory
* There is no `do while` or `finally` functionality at the time of writing this.
* Example:

```c
while expression {
    // do code
}
```

* You also have access to block scoped variables inside of this loop. This means you can create a pseudo `for loop` with the following code:

```c
i32 i = 0;

while i < 10 {
    io::println(i);
    i += 1;
}
```

Please keep in mind that you also have access to the `break` and `continue` keywords while inside of a loop, which break execution early or continue to the next iteration respectively.

<hr />

### ♡ **For loops**

* A for loop is an expression that has 3 main parts:

1. Variable declaration - Declaring an iterator to be used in the loop
2. Condition - The condition to break out of the loop
3. Variable step - The amount that the variable should increase on each iteration.

Essentially, the loop creates the variable defined in (1), and evaluates the block (code) specified, aswell as (3), until the condition defined in (2) is false (zero), when it returns to the main branch and continues execution.

* For loop expressions can be wrapped in `()` but this is not mandatory
* Basic example of a for loop that prints the digits 0-9 to the stdout:

```c
for i32 i = 0; i < 10; i += 1 {
    io::println(i);
}
```

* More advanced example:
```c
use std/io;

fn fact(i64 n) -> i64 {
    if n <= 1 {
        return 1;
    }

    return n * fact(n - 1);
}

fn get_e() {
    f64 res = 0.0;

    for i64 i = 0; i < 50; i += 1 {
        res += 1.0 / fact(i);
    }

    return res;
}

fn main() {
    f64 e = get_e();
    io::dbg(e);
}
```

Please keep in mind that you also have access to the `break` and `continue` keywords while inside of a loop, which break exeuction early or continue to the next iteration respectively.

<hr />

### ♡ **Standalone blocks**

* A standalone block is somewhat equivalent to an `if true` statement, although they are not implemented exactly the same internally. It creates a block of code that is executed on a seperate "branch" to the main code in the function. This means that if you run something like `defer` inside of a standalone block it would call that when the *standalone block* leaves scope, not the function itself.

Here's a simple example:

```c
fn main() {
    i32 a = 0;

    {
        a += 1;
        // If we do *something* here like calling defer then
        // the defer would run when this block leaves its scope
    }
}
```

And it is relatively clear how this code is essentially equal to:

```c
fn main() {
    i32 a = 0;

    if true {
        a += 1;
        // If we do *something* here like calling defer then
        // the defer would run when this block leaves its scope
    }
}
```

<hr />

### ♡ **Function Metadata**

* Elle can provide you with extra metadata using the `ElleMeta` struct.

This is done by ensuring the 0th argument of your function is typed to use the ElleMeta struct.
<br />
The compiler will automatically supply the struct to you when the function is called, you do not need to manually call it.

This struct is not defined in Elle code, however its equivalent structure may look like:
```c
struct ElleMeta {
    string *exprs; // An array of every argument's expression passed to the function as a string
    string *types; // An array of the type of every argument supplied to the function
    i32 arity; // The number of arguments supplied to the function. This does NOT include the metadata parameter.
    string caller; // The caller of the function as a string
};
```

> [!IMPORTANT]
> You do not need to supply the structure yourself. This is automatically managed by the compiler.

This means that here:

```c
fn square(i32 a) {
    return a * 2;
}

fn main() {
    i32 res = square(5);
}
```

`square` will not be passed `ElleMeta`.

However, here:

```c
fn square(ElleMeta meta, i32 a) {
    return a * 2 + meta.arity;
}

fn main() {
    i32 res = square(5);
}
```

`square` will be passed `ElleMeta`. Please notice how it is NOT passed by the caller. It is automatically passed by the compiler if it is required.

<hr />

### ♡ **Variadic Functions**

* A variadic function is a function that can take in a variable amount of arguments. This works similar to C except that there are macros which allow you to get the argument size.

Here's a basic example of a variadic function which takes in any amount of arguments and returns their sum:

```c
fn add(ElleMeta meta, ...) {
    // Note: `i32` should be the same as the type
    // you are yielding from later.
    variadic args[meta.arity];
    i32 res = 0;

    for i32 i = 0; i < meta.arity; i += 1 {
        res += args.yield(i32);
    }

    return res;
}
```

Let's go through an explanation for how this works:

* L1: Declare the function signature.
* L2: Declare the `args` variable as a pointer to the start of the variadic arguments. This is denoted by `variadic name[length]`. This call internally stack allocates memory of the size specified and then calls `vastart` on the returned pointer.
* L3: Initialize the result at `0`.
* L5: Declare a for loop with an unused iterator from 0 to the length. This will allow you to loop through all of the arguments that will be provided by the user. This is necessary because you can yield arguments forever, however if you don't know how many there are then you will enter uninitialized memory.
* L6: Yield the next argument from the `args` pointer as an `i32` type, and add it to the result value
* L9: Return the summed value. Right before this point, the `free` call that we deferred earlier would be called.

At the call-site, using this function is easy. It can be done like this:

```c
fn main() {
    i32 res = add(1, 2, 3, 4);
    io::println(res);
}
```

Examples that contain variadic functions include [`concat.l`](https://github.com/acquitelol/elle/blob/rewrite/examples/concat.l) and [`variadic.l`](https://github.com/acquitelol/elle/blob/rewrite/examples/variadic.l).

<hr />

### ♡ **Exact literals**

> [!NOTE]
> You will probably never use exact literals when writing pure Elle code. Their only realistic use is to implement language features that don't exist yet.

* An exact literal is Elle's way of implementing inline IR into the language. This basically means that you can write intermediate language code directly in Elle which compiles without any type, size, scope, or name context.

You can create an "exact literal" by wrapping the inline IR with `$$` on both sides of the expression, and ensuring you include a semicolon at the end.

You can also use the manual return directive, which states that Elle should **NOT** include an automatic return if the function does not return anything by default. You can do this by writing `$$__MANUAL_RETURN__$$` at any point in the top level of your function declaration (not in an inner block like an if statement or loop).

Here is a basic example that dereferences an `i32 *` to the underlying `i32`:

```c
use std/io;

fn deref(i32 *ptr) -> i32 {
    $$__MANUAL_RETURN__$$;
    $$%deref.val =w loadsw %ptr.1$$;
    $$ret %deref.val$$;
}

fn main() {
    i32 some_buffer[1];
    some_buffer[0] = 123;

    // Print the value at the 0th index (pointer start + 0)
    // This is identical to `some_buffer[0]`
    io::println(deref(some_buffer + 0));
}
```

* These expressions will expand into the exact characters you type into the intermediate language code.
* Typing `$$storeb 0, %tmp_12$$` will write exactly `storeb 0, %tmp_12` into the intermediate language, completely ignoring types, sigils, etc.
* Only use this for basic operations, it is not intended as a replacement for writing Elle code as block-scoped variables are written with a temporary counter and cannot be referenced directly from exact literals.

<hr />

### ♡ **Static buffers**

* A static buffer is a basic allocation of stack memory with a specified, static size.
* You can allocate a buffer with the `type buf[size];` syntax.

* Assuming you wrote the above code, you would now have a variable in scope, defined with the name `buf`. This variable is a pointer to the type specified.
* Example:

```c
char out[128];
out[0] = 'a'; // Keep in mind that `out` is a `char *`
io::println(out[0]);
```

<hr />

### ♡ **Defer statements**

* A defer statement is commonly used to group together memory allocation and deallocation. A simple explanation is that it stores whatever operation is defined inside and does not run it until the function is about to go out of scope, ie during a return, a block being left, or an implicit return due to the function scope being left.

A very simple example of this is declaring a variable and deferring printing its value, like this:

```c
use std/io;

fn main() {
    i32 i = 0;

    // If this were not in a defer statement, then this would print 0
    // However, it will print 25 instead.
    // Realistically this code only runs right before `return 0`.
    defer io::print(i);

    i += 5;
    i *= i;
}
```
You can see how this only calls `io::print` right before it returns 0, which is indeed *after* the `i` variable has had changes made to it. This also works if you return in other scopes, such as if statements, while loops, standalone blocks, etc, as stated above. Any defer statements in inner blocks will not be called on any return, rather will only be called when the inner block is about to leave scope.

This also means that if you, hypothetically, design a program like this
```c
use std/io;

fn main() {
    i32 i = 0;
    defer io::print(i);

    {
        defer io::print(i);
        i += 2;
    }

    i *= i;
}
```

The expected output is 2, then 4.
This is because it will call `io::print` once when the standalone block will leave scope, at which point `i` is 2, then it will call `print_int` again when the function itself (`main`) will leave scope, at which point it will be 4 because `i` was squared (`i *= i`).

You can also write something like this:
```c
fn main() {
    i32 i = 0;
    defer io::print(i);

    {
        defer io::print(i);
        i += 2;

        {
            return 0;
        }
    }

    i *= i;
}
```
Here we expect `i` (`2`) to be printed to the console twice. Why? When the function returns, the scope created by the standalone block is also inherently about to be left. Hence, we also need to call all non-root deferrers here.

The most useful application of deferring is for memory management, however.

Consider this code:

```c
use std/io;

fn main() {
    i64 size = 10;
    i64 *numbers = malloc(size * #size(i64));
    defer free(numbers);

    for i64 i = 0; i < size - 1; i += 1 {
        numbers[i] = i * 2;
        i64 res = numbers[i];
        io::printf("numbers[{}] = {}", i, res);
    }

    if numbers[2] + 1 * 5 == 10 {
        // Calls `free` here
        return 1;
    }

    // Calls `free` here
}
```

Without deferring, you would have to call `free` at every single place where you return. Not only is this inefficient, but also very easy to forget.

Of course for a function like the above, you are able to determine what path the code will take at compile time, however if you use something like `rand()` you no longer have the ability to do this, so you need to call `free` manually at all points where the function leaves its scope. This is an elegant way to prevent that.

<hr />

### ♡ **Type definitions**

* A type definition is used to differentiate between the scope and size of different variables. You must define a type when declaring variables, taking variables as arguments in a function, and yielding the next value from a variadic argument pointer.

Elle's types are quite similar to C in terms of their definition. They can be a recursive pointer type too such as `char **` (An array of strings). Although C has a limit on the number of pointers that a type can have (it is 2 in the C spec), Elle does **not**.

These are the mappings of types in Elle:

- `void` - A mapping to `word`, usually used for `void *` or function return signatures
- `bool` - A mapping to `i8`, and works purely as a semantic for boolean literals like `true` or `false` that expand to `1` or `0` respectively.
- `char` - A mapping to a `byte` representing a character in ASCII.
- `i8` - A "byte", also known as an 8-bit integer.
- `i16` - A "short", also known as a 16-bit signed integer, or half the size of an i32.
- `i32` - A "word", also known as a 32-bit signed integer.
- `i64` - A signed integer of the size specified by your computer's architecture, up to 64-bit.
- `f32` - A 32-bit signed floating point number.
- `f64` - A 64-bit signed floating point number, providing double the precision of `f32`.
- `fn` - A type that maps to a `byte`. This is intended to be used as a pointer to the first byte of a function definition.
- `pointer` - Denoted by `<type> *` -> As pointers are just a number, an address in memory, a pointer in Elle is just an `i64` that holds extra context by holding another type so that it can use its size to calculate an offset when indexing its memory.
- `string` - A mapping to a `char *`, which is essentially an array of characters, or a "c-string".

<hr />

### ♡ **Type Conversion / Casting**

* A type conversion consists of converting a variable from one type to another, usually compromising precision if converting to a type with a lower size (f64 -> f32) or having more precision if promoting a type (i32 -> i64).

You can cast a type in a similar manner to C.
<br />

Here is an example that casts a float to an integer to add it to another integer:

```c
fn main() {
    f32 a = 1.5;
    i32 b = (i32)a + 2;
}
```

Casting is not necessary here, because the Elle compiler is smart enough to automatically cast the `f32` to an `i32` when compiling the arithmetic operation, based on a [weight](https://github.com/acquitelol/elle/blob/rewrite/src/compiler/enums.rs#L405-L414) that each type is assigned.

<br />

You can also cast to pointer types, however note that, unlike C, casting to a pointer type when using `malloc` is *not* necessary because the Elle compiler automatically casts the `void *` into the type of the variable.

This means you can write:
```c
fn main() {
    f64 *a = malloc(1024 * #size(f64));
}
```
and Elle will not complain. You can, if you wish, cast it, however it will have no effect.

<hr />

### ♡ **Unary operators**

* A unary operator is a token used as a prefix to a literal or identifer to apply some operation to it, like negating it.

There are 5 unary operators in Elle:
<br />
- `!` - Bitwise NOT
- `&` - Stack address
- `-` - Negative number
- `+` - Positive number
- `*` - Pointer dereference

Any identifier or literal can be prefixed by one of these operators.

Example of using bitwise `NOT`:

```c
use std/io;

pub fn main() {
    bool myBool = false;

    if !myBool {
        io::println("Hello world!");
    }
}
```

This can also be used for negative or positive values:

```c
const i64 MAX_SIGNED_LONG = 9_223_372_036_854_775_807;
const i64 MIN_SIGNED_LONG = -MAX_SIGNED_LONG - 1;
```

Using unary `-` will multiply the expression by -1 while unary `+` will multiply the expression by 1.

The unary `&` operator is used to get the memory address of a local variable in a function. Here is an example:

```c
use std/io;

fn other(i32 *something) {
    io::println(*something);
}

pub fn main() {
    i32 a = 39;
    other(&a);
    return 0;
}
```

Here we declare `a` as 39, then we pass the "address" of `a` to `other` as a pointer to an `i32`, then this pointer is dereferenced.

<hr />

The unary `*` operator is used to dereference a pointer to a value:

```c
use std/io;

fn other(i32 *a, string *str) {
    io::printf("(fn other)\n\ta = {}\n\tstr = {}", *a, *str);
    *a = 542;
}

fn main() {
    i32 a = 39;
    string str = "Hello world!";

    other(&a, &str);
    io::printf("(fn main)\n\ta = {}", a);
}
```

The example also implies that you can store values at those dereferenced addresses. You can put as many tokens as you want after the operator. It will yield until:
- it matches a semicolon (`;`)
- it matches an arithmetic operator
- it reaches the end of the token vector

This means that if you want to manipulate the address before it is dereferenced, you can wrap it in `()`.

This code:
```c
io::println(*a + 1);
```
will dereference `a` and then add 1 to the result.

This code, however:
```c
io::println(*(a + 1));
```
will first add 1 to the address of `a`, and then will dereference that address.

<hr />

### ♡ **Arithmetic operations**

* All arithmetic operations are declared with an expression on the left and right of an operator. This means you can call functions, do other arithmetic operations inside of operations, etc.

This is the mapping defined by Elle:

- `^` - Xor
- `*` - Multiply
- `/` - Divide
- `+` - Add
- `-` - Subtract
- `%` - Modulus
- `&` - Bitwise And
- `|` - Bitwise Or
- `<<` - Shift Left
- `>>` - Shift Right
- `&&` - Logical And (Not usable when declaring a variable)
- `||` - Logical Or (Not usable when declaring a variable)

Keep in mind that you can also use these operators when doing a variable declaration.
This means the following code is valid:

```c
use std/io;

fn main() {
    i32 a = 1;
    a ^= 1; // a is now 0
    io::println(a);
}
```

And of course, this works for every arithmetic operator, not just `^`.

Elle follows the standard [order of operations](https://github.com/acquitelol/elle/blob/rewrite/src/lexer/enums.rs#L104-L117) described by mathematics (typically defined as BIDMAS or PEMDAS), which means you can also wrap expressions in `()` to evaluate them before other expressions that may have a higher precedence.

Example of a program that calculates the xor (`^`) and sum (`+`) of some values:

```c
use std/io;

fn main() {
    i32 a = 1 + (5 ^ 2); // Xor has a lower precedence than addition

    // We're expecting this to be 8 because
    //  5 ^ 2 = 7 and 7 + 1 = 8, however
    // without the brackets it would be 4
    // because it would evaluate to 6 ^ 2 = 4
    io::println(a);
}
```

<hr />

### ♡ **Array literals**

* An array literal is a simple and intuitive syntax to automatically allocate stack memory for a buffer and assign values at each offset based on the literal definition. Essentially, an expression like this:

```c
i32 *some_arr = [512, 1, -3];
```

would first allocate memory to a buffer and store that in a variable called `some_arr` with the size of `3 * 4 = 12` (because there are 3 items and the size of an `i32` is 4 bytes) and then it would offset the pointer returned and store each value specified at that address.

So it would first store `512` at `some_arr + 0`, then it would store `1` at `some_arr + 4` (offset accounting the size of the array type), then finally would store `-3` at `some_arr + 8`.

<br />

You can view a more detailed example of array usage at [array.l](https://github.com/acquitelol/elle/blob/rewrite/examples/array.l).
Array literals are not required to be assigned to a variable. Please look at this example:

```c
use std/io;

fn other(i64 *arr, i32 val) {
    io::printf("\narr[0] = {}\nval = {}", arr[0], val);
}

fn main() {
    other([MAX_SIGNED_LONG], [123][0]);
}
```

where we pass an array literal directly to another function or operation. An array literal, internally, will simply return the memory address of the start of the array. As these arrays has no variable declaration linked to them, there is no way to get their type, however we can infer this type based on the type of the values inside, so it can still be indexed correctly.

You can also get the size and length of these arrays. Simply wrap them in `#size` or `#arrlen` just like if you wanted to get the size of an array that was declared to a variable. For more information, please read the size directives chapter.

<hr />

### ♡ **Size directives**

* A "size directive" is similar to a `sizeof` builtin in C. It returns the size of various definitions verbatim.

There are currently 2 size directives in Elle:
`#size()` and `#arrlen()`

You can put both **types** and **expressions** inside of the `#size()` directive and it returns the size of the statement provided.

You can only place **expressions** inside of the `#arrlen()` directive as it returns the size of the buffer divided by the size of each type. This is exactly equivalent to `#size(arr) / #size(arr_type)`. It will crash if you try  to use it on a buffer that wasn't defined in the function that the directive is called from.

For example, take this snippet:

```c
use std/io;

fn other(i32 *buf) {
    io::printf(
        "(fn other)\n\t#size(buf) = {}\n\t#arrlen(buf) = {}",
        #size(buf),
        #arrlen(buf)
    );
}

fn main() {
    i32 buf[100];
    buf[0] = 123;

    io::printf(
        "(fn main)\n\t#size(buf) = {}\n\t#arrlen(buf) = {}",
        #size(buf),
        #arrlen(buf)
    );

    other(buf);
}

```

At this part:

```c
io::printf(
    "(fn other)\n\t#size(buf) = {}\n\t#arrlen(buf) = {}",
    #size(buf),
    #arrlen(buf)
);
```

Elle will throw a compilation error. The `buf` buffer was not defined in the function called `other`, so therefore the compiler does not have enough context to get its length, as arguments in Elle are not fat (they don't contain extra metadata).

Essentially, contextually this means that the `buf` variable is just an `i32 *` to the compiler. As it no longer has context to the size of the `buf` allocation, it cannot evaluate the `#arrlen` directive, and throws an error.

In this example:

```c
use std/io;

fn other(i32 *buf) {
    io::printf("(fn other)\n\t#size(buf) = {}", #size(buf));
}

fn main() {
    i32 buf[100];
    buf[0] = 123;

    io::printf(
        "(fn main)\n\t#size(buf) = {}\n\t#arrlen(buf) = {}",
        #size(buf),
        #arrlen(buf)
    );

    other(buf);
}
```

The code will compile successfully, because `#arrlen` is no longer used on a buffer that isn't defined in the function where the directive was called.

<hr/>

Finally, here is a basic example of using `#arrlen` to loop through an array of strings and print their values:

```c
use std/io;

fn main() {
    string *some_array = ["abc", "meow", "test"]";

    for i32 i = 0; i < #arrlen(some_array); i += 1 {
        io::printf("some_array[{}] = {}", i, some_array[i]);
    }
}
```

<hr />

### ♡ **Constants**

* A constant is a value that cannot be redeclared. In Elle, constants can only be defined at the top level of files, and vice versa too, where the top level of files can *only* be constants and functions. You cannot define normal variables at the top level.
* Constants can be public, declared using the `pub` keyword.
* Constants that create pointers (such as string literals) are referenced as the first statement of each function to bring them in scope.

Consider this example that uses constants:

```c
use std/io;

const i32 WIDTH = 100;
const i32 HEIGHT = 24;
const i32 SIZE = WIDTH * HEIGHT;

pub fn main() {
    io::println(SIZE);
    return 0;
}
```

In the above code, all of the constants are technically function definitions that return the value after the `=` sign. However, when they're referenced, the function is automatically called. Therefore, you dont need to type `SIZE()` or similar, you can just directly reference `SIZE` as if it was a constant.

It is labelled as a "`constant`", because although it can return a different value (it can call any function), it cannot be redeclared.

<hr />

### ♡ **Non-base-10 literals**

* These are literal numbers which are not declared in base 10.

These may include:

- Hex - 0xFFFFFF
- Octal - 0o777777
- Binary - 0b111111
- Scientific - 2.1e3

Basic example:

```c
use std/io;

fn main() {
    i64 a = 0xDEADBEEF;
    i32 b = 0o273451456;
    i32 c = 0b111010011011111010010100101;
    i64 d = 1.2e9;
    f64 e = 2.7182818e2;

    io::dbg(a, b, c, d, e);
}
```

<hr />

### ♡ **Imports/modules**

Elle's module system works in the following way:

- Elle will look in the /usr/local/elle folder for modules (not implemented yet)
- Elle will look in the current working directory for modules

The syntax for importing is as follows:

```c
use path/to/module;
```

where, in your current directory, there is a `./path/to/module.elle` or a `./path/to/module.l` file.

The syntax to export a symbol from your current file is as follows:

```c
// ./module.l
pub const i32 myFavouriteNumber = 7;

pub fn foo() {
    return 1;
}
```

which you can then import
```c
use std/io;
use module;

fn main() {
    io::println(foo() + myFavouriteNumber);
}
```

You can also add `global pub;` to your module to automatically make every symbol public in the module.
If you want to make a symbol private after declaring them all public, use the `local` keyword.

Example:

```c
global pub;

const i32 a = 100; // Public
const i32 b = 10; // Public
local const i32 c = 5; // Private

// Private
local fn increment(i32 a) {
    return a + 1;
}
```

<hr />

### ♡ **Structs**

Structs are allocations in memory with a defined layout. In Elle, these are defined using the `struct` keyword.

Example:

```c
struct Bar {
    f32 myFloat;
};

struct Foo {
    i32 a;
    Bar bar;
    f64 baz;
};
```

You can then create these structures like this:

```c
fn main() {
    Foo foo = Foo {
        a = 12,
        bar = Bar {
            myFloat = 10.2
        },
        baz = 3.141592
    };

    io::println(foo.bar.myFloat);
}
```

If taking a pointer to them from another function, you can do so like this:

```c
use std/io;

fn other(Foo *foo) {
    foo.baz = 17.98;
    io::println(foo.a);
}

fn main() {
    Foo foo = ; // create Foo
    other(&foo);
}
```

> [!NOTE]
> There is no equivalent of the `a->b` operator in Elle. Any pointer to a struct will automatically be dereferenced before processing any fields in the struct.
> You can still manually dereference the struct pointer manually if you like, but it will have no difference compared to directly using dot notation.
> This means that the following code will accurately update the value inside the struct Foo:
```c
use std/io;

struct Foo {
    i32 a;
};

fn other(Foo *foo) {
    foo.a = 5;
}

fn main() {
    Foo foo = Foo { a = 100 };
    other(&foo);
    io::println(foo.a); // foo.a is now 5 not 100
}
```

You can also define methods on structs (and primitive types):

```c
use std/io;

struct Foo {
    i32 a;
};

fn Foo::add(Foo self, Foo other) {
    return Foo { a = self.a + other.a };
}

fn main() {
    Foo foo1 = Foo { a = 10 };
    Foo foo2 = Foo { a = 30 };

    Foo res1 = foo1.add(foo2);
    Foo res2 = Foo::add(foo1, foo2);

    io::dbg(res1.a, res2.a);
}
```

You can define `fn <Struct name>::<method name>(<Struct name> self, <args>)` to create instance methods.
<br />
You can then either call them through `instance.<method name>()` or `<Struct name>::<method name>(instance)`.
<br />
In this case, `foo1.add(foo2)` is an identical expression to `Foo::add(foo1, foo2)`
<br />
For more examples, please view [vectors.l](https://github.com/acquitelol/elle/blob/rewrite/std/vectors.l)

You may also specify that `self` is a `<ty> *` instead of a `<ty>` if you require editing it in-place:
```rs
use std/io;

struct Foo {
    i32 a;
};

fn Foo::divideBy(Foo *self, i32 num) {
    self.a /= num;
}

fn main() {
    Foo foo = Foo { a = 10 };
    foo.divideBy(2);

    io::dbg(foo.a); // foo.a = 5
}
```

The compiler will automatically pass the **address** of `foo` instead of `foo` itself to the function.
<br />
In the case of a method that takes in a `self` pointer, the identical expression to `foo1.divideBy(2)` is `Foo::divideBy(&foo1, 2)`.

<hr />

### ♡ **Argc and argv**

* These are variables that can be taken as arguments from the `main` function that allow you to pass extra data to the executable. Conventionally, the first argument, `argc`, is the number of arguments, and the second argument, `argv`, is an array of the arguments, or rather a pointer to them.

Due to Elle's compilation to QBE which implements the C ABI, getting input from `argc` and `argv` is actually *exactly* the same as C. There is practically no difference.

Consider this function which accepts argv and prints them all to the console:

```c
use std/io;

fn main(i32 argc, string *argv) {
    for i32 i = 0; i < argc; i += 1 {
        io::printf("argv[{}] = {}", i, argv[i]);
    }
}
```

It accepts `argc` as a signed 32-bit integer and `argv` as an array of `string` (denoted by `string *`, basically an array of strings). These arguments are optional, as you may have noticed from code examples above, where some `main` functions did not take an `argc` or `argv`.

You can also accept `string *envp` (and `string *apple` on MacOS/Darwin platforms, which provides arbitrary OS information, such as the path to the executing binary).

<hr />

### ♡ **External symbols**

* An external symbol is a definition for a function or constant that was defined elsewhere (such as in C) and is implicitly defined in Elle. This is used to give definition and context to functions that were not defined in Elle but you wish to use in when writing Elle code.
<br/>

You can do this with the following example:
```c
external fn printf(string formatter, ...);
```

It essentially tells Elle where it should put the variadic argument starter. You could exclude this, if you like, but you will have to explicitly declare where the variadic arguments begin, because Elle no longer has this context.

You can also make these statements public:
```c
pub external fn fprintf(FILE *fd, string formatter, ...);
```
In fact the order of prefixes before `fn` is not enforced, you can write `external pub fn` and achieve the same result.

You may also alias exported functions, and allow them to be accessible through a pseudo-namespace:

```c
pub external fn InitWindow(i32 width, i32 height, string title) @alias("raylib::init_window");
struct raylib {};

// You can now call raylib::init_window() and it will internally reference the InitWindow symbol
```

**Technical note:** This declaration does not emit any IR code. This means that all these definitions do is provide more information and context to the compiler. They do not change the output of the program directly.

<hr />

### ♡ If you have any questions, please **[raise an issue](https://github.com/acquitelol/elle/issues/new) :3**
All contributions to this project are welcome and I love talking about this stuff!

<hr />

### ♡ **How to run**

* Ensure you have [Rust](https://www.rust-lang.org/) and the [QBE](https://c9x.me/compile/) compiler backend.
* Elle has no third-party dependencies, so you can simply do the following:
    ```terminal
      $ git clone https://github.com/acquitelol/elle
    ```

    ```terminal
      $ cd elle
    ```

    ```console
      $ make compile
    ```

    **OR**

    ```console
      $ make compile-release
    ```

  * **You're done!**

> You can now run `make run <name>` to run a file from the `/examples` directory. For example, you can run `make run donut` and it will run `/examples/donut.l`.

<hr />

### ♡ **Licensing**

* Copyright © 2024 Rosie ([acquitelol](https://github.com/acquitelol))

<hr />

<a href="#top">⇡ Back to top️!</a>
