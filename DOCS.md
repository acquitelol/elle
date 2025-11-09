### ♡ **Variable declarations**

- Variables can be declared in 3 ways:

  - Using their type (useful for inference of information based on the left-hand side like generics)
  - Using let (useful for inferring based on the right-hand side)
  - Using the walrus operator `:=` (cannot be used with a type, equivalent to `let`)

- Example:

```rs
let a = 0; // a is inferred to be i32 because 0 is i32
i64 a = 5; // 5 is inferred to be i64 because a is i64
a := 0;    // acts the same as `let a = 0;`
```

This is especially useful for dynamic array declarations:

```rs
arr := []i64;
i64[] arr = [];
```

#

### ♡ **If statements**

- An if statement is an expression that evaluates a block if the condition is non-zero, with an optional `else` block which is evaluated if the condition **is** zero.

- You can define an `if` statement and then an optional `else` statement
- If statement conditions can be wrapped in `()` but this is not mandatory
- Example:

```rs
a := 0;

if expression {
    a += 1;
} else {
    a -= 1;
}
```

```rs
a := 1;

if a == 1 {
    $println("hello world");
} else if a == 2 {
    $println("foo bar baz");
} else {
    $println("test");
}
```

#

### ♡ **While loops**

- A while loop is an expression that evaluates the block specified **only** if the condition is non-zero, otherwise breaks and continues execution on the primary branch.

- Even though you can loop via recursion, the while loop primitive may be simpler to understand and use in many cases, therefore it is provided in Elle.
- While loop expressions can be wrapped in `()` but this is not mandatory
- There is no `do while` or `finally` functionality at the time of writing this.
- Example:

```rs
while expression {
    // do code
}
```

- You also have access to block scoped variables inside of this loop. This means you can create a pseudo `for loop` with the following code:

```rs
let i = 0;

while i < 10 {
    io::println(i);
    i += 1;
}
```

Please keep in mind that you also have access to the `break` and `continue` keywords while inside of a loop, which break execution early or continue to the next iteration respectively.

#

### ♡ **For loops**

- A for loop is an expression that has 3 main parts:

1. Variable declaration - Declaring an iterator to be used in the loop
2. Condition - The condition to break out of the loop
3. Variable step - The amount that the variable should increase on each iteration.

Essentially, the loop creates the variable defined in (1), and evaluates the block (code) specified, aswell as (3), until the condition defined in (2) is false (zero), when it returns to the main branch and continues execution.

- For loop expressions can be wrapped in `()` but this is not mandatory
- Basic example of a for loop that prints the digits 0-9 to the stdout:

```rs
for i32 i = 0; i < 10; i += 1 {
    io::println(i);
}
```

- More advanced example:

```rs
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
    $dbg(e);
}
```

Please keep in mind that you also have access to the `break` and `continue` keywords while inside of a loop, which break execution early or continue to the next iteration respectively.

#

### ♡ **Foreach loops**

- A foreach loop is an expression that has 2 main parts:

1. Variable declaration - Declaring a variable for each element (which may be a tuple destructure)
2. Iterator - The iterator value (which must have an `__iter__` function defined on its type)

- Example:

```rs
for x in ["a", "b", "c"] {
    io::println(x);
}
```

- Any iterable type can be used as an iterator:

```rs
for c in "hello world" {
    $dbg(c);
}

for i in 0..100 {
    $dbg(i);
}
```

You can also access the current index during a `foreach` loop:

```rs
for i, x in [1, 2, 3].iter().enumerate() {
    $dbg(#i(x), x);
}
```

Please keep in mind that you also have access to the `break` and `continue` keywords while inside of a loop, which break execution early or continue to the next iteration respectively.

#

### ♡ **Standalone blocks**

- A standalone block is somewhat equivalent to an `if true` statement, although they are not implemented exactly the same internally. It creates a block of code that is executed on a seperate "branch" to the main code in the function. This means that if you run something like `defer` inside of a standalone block it would call that when the _standalone block_ leaves scope, not the function itself.

Here's a simple example:

```rs
fn main() {
    let a = 0;

    {
        a += 1;
        // If we do *something* here like calling defer then
        // the defer would run when this block leaves its scope
    }
}
```

This block has a different scope, which means you can declare variables with the same name but a different type in it. You can learn more about this in the `Variable Shadowing` section.

And it is relatively clear how this code is essentially equal to:

```rs
fn main() {
    let a = 0;

    if true {
        a += 1;
        // If we do *something* here like calling defer then
        // the defer would run when this block leaves its scope
    }
}
```

#

### ♡ **Variable Shadowing**

Variable shadowing is when the variable defined in the previous scope is accessible in the current scope.

For example:

```rs
fn main() {
    x := 1;

    {
        // should x exist here? yes
        // whats its value? its 1
        $assert(x == 1, nil);

        // what if we redeclare it?
        x = 2;

        // now whats its value? its 2
        $assert(x == 2, nil);

        // what if we declare a new x?
        x := 3;

        // now whats its value? its 3
        $assert(x == 3, nil);
    }

    // now the scope ended, what should this x be?
    // well it should be 2 because the x in this scope was redeclared to 2
    // the newly-declared x in that scope doesnt exist in this scope
    $assert(x == 2, nil);
}
```

More complex example

```rs
fn main() {
    x := "foo"; // x is "foo"

    x := 1; // x is 1, it changes type!

    {
        x = 2; // x is 2
        x := "a"; // x is "a", now string again

        {
            x = "b"; // x is "b"
            // note: no := usage, so modifies previous scope's x
        }

        // x is "b" here
    }

    // x is 2 here
}
```

#

### ♡ **Function Metadata**

- Elle can provide you with extra metadata using the `ElleMeta` struct.

This is done by ensuring the 0th argument of your function has the type `ElleMeta`.
<br />
The compiler will automatically supply the struct to you when the function is called, you do not need to manually pass it to the function.

This struct is not defined in Elle code, however its equivalent structure may look like:

```rs
struct ElleMeta {
    string *exprs, // An array of every argument's expression passed to the function as a string
    string *types, // An array of the type of every argument supplied to the function
    i32 arity,     // The number of arguments. This does NOT include the ElleMeta argument.
    string caller, // The caller of the function as a string
    string file,   // The file where the function was called from
    i32 line,      // The line number of the function call + 1
    i32 column,    // The column number of the function call + 1
}
```

> [!IMPORTANT]
> You do not need to supply the structure yourself. This is automatically managed by the compiler.

This means that here:

```rs
fn square(i32 a) {
    return a * a;
}

fn main() {
    i32 res = square(5);
}
```

`square` will not be passed `ElleMeta`.

However, here:

```rs
fn square(ElleMeta meta, i32 a) {
    return a * a + meta.arity;
}

fn main() {
    i32 res = square(5);
}
```

`square` will be passed `ElleMeta`. Please notice how it is NOT passed by the caller. It is automatically passed by the compiler if it is required.

#

### ♡ **Allocators**

Elle has a moderately complicated allocator system. Here's how it works:

- By default:
  - garbage collection
- Using the `--nogc` flag at compilation:
  - arena-based allocation

#### **Changing the allocator:**

```rs
#set_allocator(MyAllocator::new());
```

#### **Resetting to the default allocator:**

```rs
#set_allocator(#env.default_allocator);
```

OR

```rs
#reset_allocator();
```

(These are equivalent expressions.)

> [!IMPORTANT]
> Make sure you don't forget to free any memory leftover when switching allocator! `#set_allocator` does **not** call the `free_self` method on the previous allocator when switching allocator, to allow for programs designed like this:

```rs
fn main() {
    arena := ArenaAllocator::new();
    #set_allocator(arena);
    x := [1, 2, 3]; // x is allocated through the ArenaAllocator
    #reset_allocator();
    $println(x); // allocates via default allocator
    #set_allocator(arena);
    #env.allocator.free_self(); // frees the arenas
    #reset_allocator(); // go back to default allocator
}
```

#### **What should an allocator have defined on it?**

- Allocators should have the following methods defined on them:
  - `MyAllocator::new()` (preferrably allocating the allocator structure itself via `mem::malloc`)
  - `MyAllocator::alloc(MyAllocator *self, i32 size) -> void *` (size in bytes to allocate, should return `void *`)
  - `MyAllocator::realloc(MyAllocator *self, void *ptr, i32 new_size) -> void *` (new_size in bytes. should return `void *`)
  - `MyAllocator::free(MyAllocator *self, void *ptr)` (frees a specific object passed by pointer, may be omitted if permitted by the allocation model, will become `noop`)
  - `MyAllocator::free_self(MyAllocator *self)` (destructor for the allocator itself including all of its allocations, **NOT** objects created by it)

#### **Disabling allocation altogether:**

- You can pass the `--noalloc` flag during compilation. Keep in mind that, while this will no longer define allocators, this means you won't be able to use almost any of the Elle standard library, as all of it depends on these allocators. This flag goes well with the `--nostd` flag.

#

### ♡ **Dynamic memory allocation**

- Elle has a notion of a `#env` directive which gives you an `ElleEnv *`.

This structure is also not defined in Elle code (like `ElleMeta`), but its equivalent structure may look like:

```rs
struct ElleEnv {
    ArbitraryAllocator *allocator,
    TAllocator *default_allocator,
}
```

(where `TAllocator` is either `GCAllocator` or `ArenaAllocator` depending on your compilation configuration.)

The allocator is completely abstracted away from you, which means that depending on the allocator, certain methods may not be set. They will be set to a `noop` function instead which returns `nil`.

Typically, you should be safe to assume that you have `#alloc` and `#realloc`. In specific environments you can also assume you have `#free`, but this usually set to a `noop`.

By default, memory deallocation is managed by the compiler via garbage collection. You can disable this by adding the `--nogc` flag, which will switch to using an `ArenaAllocator` model instead. If you prefer to manually manage memory altogether, you can either:

```rs
// Add this flag to your compilation command
// which completely stops custom allocators.

// You can now use mem::malloc, mem::free, etc
//
// Keep in mind that you will not be able to
// use most standard library features with
// allocators disabled.
--noalloc
```

OR

```rs
// Import the heap allocator
use std/allocators/heap;

// And use it in your main function
#set_allocator(HeapAllocator::new());

// Now #alloc will call malloc, keeps type QOL features
ptr := #alloc(i32, 5); // same as mem::malloc(#size(i32) * 5)
#free(ptr); // same as mem::free(ptr);
```

> [!IMPORTANT]
> Standard library functions do not free their memory because of the assumption of an auto-freeing allocator. If you use standard library functions with manual memory management, expect memory leaks.

#

Example of using dynamic memory allocation:

```rs
struct Foo {
    i32 a
}

fn Foo::new(i32 a) {
    foo := #alloc(Foo);
    foo.a = a;
    return foo;
}

fn main() {
    let foo = Foo::new(10);
    $dbg(foo);
}
```

Another example:

```rs
fn main() {
    // allocate space for 10 integers
    i32 *numbers = #alloc(i32, 10);
    numbers[1] = 39;

    $dbg(numbers[1]); // 39
    // dont need to free it
}
```

Keep in mind that you can also use the libc standard manual memory management functions, like `malloc`, `realloc`, and `free`. These methods are defined in `std/libc/mem`. These allocations will **not** be freed automatically because the garbage collector isn't tracking them.

The compiler also provides you handy builtins for easy and quick allocation: `#alloc` and `#realloc`. As these builtins take a _type_ and not the _size of a type_ they can actually evaluate to exactly `T *` instead of `void *` when called. This means you can write this:

```rs
let x = #alloc(i32, 5); // x -> i32 *
```

without needing to explicitly convert anywhere.

`#alloc` uses the form of `#alloc(T, size?)` where `T` is any type and `size?` is an optional count (similar to `calloc` behavior), which can be omitted to form just `#alloc(T)`.

`#realloc` uses the form of `#realloc(ptr, T, size?)` where `ptr` is any expression that evaluates to a pointer, `T` is any type and `size?` is an optional count (similar to `calloc` behavior), which can be omitted to form just `#realloc(ptr, T)`.

Example usage:

```rs
use std/prelude;

struct Foo {
    i32 a
}

fn Foo::new(i32 a) {
    let foo = #alloc(Foo);
    foo.a = a;
    return foo;
}

fn main() {
    let foo = Foo::new(6);
    $dbg(foo);
}
```

Using these directives, you can turn a verbose expression such as:

```rs
Machine *machine = #env.allocator.alloc(#size(Machine));
```

into the (much) cleaner:

```rs
machine := #alloc(Machine);
```

#

### ♡ **Variadic Functions**

- A variadic function is a function that can take in a variable amount of arguments. This works similar to C except that Elle provides you with mechanisms to make this much nicer to use, both as the producer and consumer of the function.

Here's a basic example of a variadic function which takes in any amount of arguments and returns their sum:

```rs
fn add(ElleMeta meta, ...args) {
    res := 0;

    for i := 0; i < meta.arity; i += 1 {
        res += args.yield(i32);
    }

    return res;
}
```

At the call-site, using this function is easy. It can be done like this:

```rs
fn main() {
    res := add(1, 2, 3, 4);
    io::println(res);
}
```

Examples that contain variadic functions include [`variadic.le`](https://github.com/acquitelol/elle/blob/rewrite/examples/tests/variadic.le).

#

### ♡ **Arrays**

There are 2 kinds of arrays in Elle: _dynamic_ and _static_.

Dynamic arrays are allocated on the heap, and are designed to grow or shrink, allowing you to push and pop values. They also have far more utility methods on them compared to static arrays. These kinds of arrays are created with the following syntax:

```bnf
array = "["[elements] "]" [type?];
elements = expression {"," expression} ;
```

Static arrays are allocated on the stack, and are designed to be static in size. These arrays define a static array type with a size known at compile time. They're declared with the following syntax:

```bnf
array = "#" "[" [elements] "]" ;
elements = expression {"," expression} ;
```

Both implement the `__iter__` method, which means this is valid:

```rs
for x in [1, 2, 3] {
    $dbg(x);
}

for x in #[1, 2, 3] {
    $dbg(x);
}
```

Dynamic arrays have special sugar when being typed:

```rs
i64[] x = [];

// OR

let x = Array::new<i64>();

// ... equivalent to ...

Array<i64> *x = Array::new();

// ... the most concise form ...

x := []i64;
```

Static arrays do not, but you can still use `let`/`:=`:

```rs
let x = #[1, 2, 3]; // x's type is `i32[3]`
x := #[1, 2, 3]; // x's type is `i32[3]`

// ... OR if you need the inference ...

f32[3] x = #[1, 2, 3];
```

You can also use `let`/`:=` when declaring dynamic arrays which have values:

```rs
x := [1, 2, 3]; // x's type is i32[]
y := ["a", "b", "c"]; // y's type is string[]
```

You can also define multi-dimensional arrays:

```rs
grid := [
    [1, 2],
    [3, 4]
]; // i32[][]

grid[0][1]; // 2
grid[1][0]; // 3

// ... or if you prefer explicit typing ...

char[][] x = [
    ['a', 'b'],
    ['c', 'd']
]; // char[][]

x[0][0]; // a
x[1][1]; // d
```

This also works for static arrays:

```rs
grid := #[
    #[1, 2],
    #[3, 4]
]; // i32[2][]

grid[0][1]; // 2
grid[1][0]; // 3

// ... or if you prefer explicit typing ...

char[2][2] x = #[
    #['a', 'b'],
    #['c', 'd']
]; // char[2][2]

x[0][0]; // a
x[1][1]; // d
```

Static arrays may also be generic over their type _and_ size:

```rs
fn foo<T, N>(T[N] arr) {
    for item in arr {
        $dbg(item);
    }
}
```

Keep in mind that you cannot perform mathematical operations on these types values, but you can add constraints like so to functions:

```rs
fn __arr__::mul<T, A, B, C>(T[A][B] a, T[B][C] b) {
    T[A][C] res;

    for i in 0..a.len() {
        for j in 0..b[0].len() {
            sum := 0;

            for k in 0..a[0].len() {
                sum += a[i][k] * b[k][j];
            }

            res[i][j] = sum;
        }
    }

    return res;
}
```

And the size can be transformed, allowing for compile time operations to the dimensions of the array:

```rs
fn __arr__::transpose<T, A, B>(T[A][B] xs) -> T[B][A] {
    T[B][A] res;

    for i in 0..xs.len() {
        for j in 0..xs[0].len() {
            res[j][i] = xs[i][j];
        }
    }

    return res;
}
```

Example usage:

```rs
fn main() {
    x := #[
        #[1, 2, 3],
        #[4, 5, 6]
    ]; // i32[2][3]

    transposed := x.transpose(); // i32[3][2]
}
```

Specifically for dynamic arrays, you can initialize them without giving them a value or explicitly using the array contructor of Array::new<T>() to create them:

```rs
fn main() {
    x := []i32; // x -> i32[]
    y := [1, 2, 3]f32; // 1, 2, 3 inferred as f32 and overall y -> f32[]
    z := ["a", "b", "c"]; // z -> string[], no explicit type means inferred
    w := []; // compilation error because T cannot be inferred
    $dbg(x, y, z, w);
}
```

This syntax essentially has 2 parts: the type and the values. You can specify the type and no values, values and no type, or both. But you must specify at least one most of the time for the compiler to be able to determine a type for the array;

It's worth noting that the `T[]` type syntax is actually sugar for `Array<T> *`. `T[][]` is equivalent to `Array<Array<T> *> *`.

#

### ♡ **Tuples and triples**

Tuples and triples are distinct data structures in Elle. Tuples have 2 items inside, Triples have 3 items.

Tuples have special sugar for their types, just like arrays. `(T, U)` is equivalent to `Tuple<T, U>`. Triples have no sugar, simply `Triple<T, U, V>`.

To define a tuple, use `$(x, y)` or `Tuple::new(x, y)`.

To define a triple, use `$$(x, y, z)` or `Triple::new(x, y, z)`.

You can put tuples inside of arrays:

```rs
let foo = [$(1, "a"), $(2, "b")];

// ... or if you prefer explicit typing ...

(i32, string)[] foo = [$(1, "a"), $(2, "b")];

// if you don't wanna put values inside but wanna use the `let` keyowrd you can do this

let foo = [](i32, string);

// ... nothing new here
```

#

### ♡ **Destructuring syntax**

Elle allows you to destructure various things (up to 3 elements):

- `Tuple<T, U>`
- `Triple<T, U, V>`
- `Array<T>`
- `Option<T>`
- `Result<T, E>`
- `Vector2`
- `Vector3`

... etc

The syntax is as follows:

```rs
x, y := <tuple_expr>;
let x, y = <tuple_expr>;
T x, y = <tuple_expr>;
```

```rs
x, y, z := <triple_expr>;
let x, y, z = <triple_expr>;
T x, y, z = <triple_expr>;
```

This is simply sugar for accessing the `x`, `y` (and optionally `z`) fields on the `<???_expr>`.

This example:

```rs
x, y := $(1, "a");
```

is desugared to:

```rs
__internal_tuple.2314 := $(1, "a").__tuple__();
x := __internal_tuple.2314.x;
y := __internal_tuple.2314.y;
```

A tuple destructure is an **expression**. It evaluates to the left hand side of the tuple/triple:

```rs
foo := (x, y := $(1, "a")); // foo == 1
```

This is useful because it allows us to place optionals into control flow structures, like conditionals and loops:

```rs
if is_some, val := Option::Some(39) {
    $dbg(val);
}

if is_some, val := Option::None<i32>() {
    $dbg(val);
}

if is_ok, val := Result::Ok<i32, string>(42) {
    $dbg(val);
}

if is_ok, val := Result::Err<i32, string>("oop") {
    $dbg(val);
}
```

`Option<T>` and `Result<T, E>`'s `x` field (the LHS of a tuple destructure) is `is_ok` or `is_some` depending on the structure, which allows us to do the above.

Here are a few more examples of why this is useful:

```rs
// Runs until `next_token` returns None
while _, current := lexer.next_token() {
    tokens.push(current);
}
```

```rs
// Will not run if the `find` returns None
if _, tmp_func := self.module.functions.find_with(
    fn(x, name) x.name == name,
    funcall.name.encoded
) {
    ty = tmp_func.return_type;
}
```

You can also destructure `Vector2`s and `Vector3`s as well as `Tuple`s and `Triple`s:

```rs
x, y := Vector2::new(1, 30);
x, y, z := Vector3::new(3, 10, 39);
x, y := $("a", 1);
x, y, z := $$(1, 2, 3.0);
```

This is especially useful for traversal algorithms like BFS:

```rs
fn Node::print_on_lines(Node *self) {
    i32[][] levels = [];
    queue := [$(self, 0)];

    while !queue.is_empty() {
        node, level := queue.remove(0).unwrap();
        ...
    }
    ...
}
```

To create a destructure for your own data structures, you can implement the `__tuple__` and `__triple__` methods:

```rs
struct Foo {
    i32 a,
    string b,
    f32 c
}

fn Foo::__tuple__(Foo self) {
    return $(self.a, self.b);
}

fn Foo::__triple__(Foo self) {
    return $$(self.a, self.b, self.c);
}
```

where now, destructuring your struct will call these functions:

```rs
a, b := Foo { a = 1, b = "a" };
a, b, c := Foo { a = 1, b = "a", c = 1.3 };
```

#

### ♡ **Ranges**

Ranges are ways you can define the start and end of a "`range`" of numbers. There are 2 kinds of ranges in elle: exclusive and inclusive. These ranges create lazy double ended iterators.

Exclusive ranges are defined with `x..y`, where `x` and `y` are expressions.
Inclusive ranges are defined with `x..=y`, where `x` and `y` are expressions.

An exclusive range means that `x` is inclusive but `y` is exclusive. An inclusive range means both are inclusive.

They can be used in `foreach` loops however, because they're arrays:

```rs
// 0..10 == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
for i in 0..10 {
    $dbg(i);
}

// 0..0 == []
for i in 0..0 {
    $dbg(i); // will never run
}

// 5..=10 == [5, 6, 7, 8, 9, 10]
for i in 5..=10 {
    $dbg(i);
}
```

#

### ♡ **Lambda functions**

Elle allows you to create single-line or multi-line lambda (anonymous) functions.

Here are basic examples of how you can use them:

```rs
use std/prelude;

fn main() {
    let arr = [1, 2, 3].iter().map(fn(x) x * 2);
    io::println(arr); // <[2, 4, 6] at 0xdeadbeef>
}
```

```rs
use std/prelude;

fn main() {
    let x = fn(i32 x) {
        let foo = x * 100;
        return (foo - 10) / 2;
    };

    $dbg(x(3));
}
```

Functions can have their signature expressed through the type system, which includes generics:

```rs
fn foo(fn(i32) -> i32 cb) {
    return cb(39);
}

fn main() {
    $dbg(foo(fn(x) x * 2));
}
```

```rs
// Will automatically infer U here based on what the lambda returns
fn foo<T, U>(fn(T) -> U cb) {
    return cb(39);
}

fn main() {
    // x = i32 must be specified explicitly here
    $dbg(foo(fn(i32 x) "{}".format(x)));
}
```

Please note the following:

- These lambdas do **not** capture surrounding variables
- They are not automatically passed ElleMeta by the compiler (because there is not enough context to do so)

This means that these examples won't work:

```rs
use std/prelude;

fn main() {
    let arr = [1, 2, 3];
    let a = 5;

    // The compiler will throw an error here
    let arr_doubled = arr.iter().map(fn(x) x * a);
    io::println(arr_doubled);

    // Settle for this instead
    let arr_doubled = arr.map_with(fn(x, a) x * a, a);
    io::println(arr_doubled);
}
```

#

### ♡ **Exact literals**

- An exact literal is an identifier which is not explicitly parsed. As in, you can make and call functions with arbitrary names which may be invalid in Elle but valid in the IR.

You can create an "exact literal" by wrapping the content you wish with "`" on both sides of the expression.

Here is a basic example:

```rs
use std/io;

fn `add.works`() {
    $assert(42 + 42 == 84, nil);
}

fn `mul.works`() {
    $assert(42 * 2 == 84, nil);
}

fn main() {
    `add.works`();
    `mul.works`();

    io::println("All `exact literal` tests have passed!".color("green").reset());
}
```

Here's another example;

```rs
fn `identity.foo.$.bar`(i32 x) {
    return x;
}

fn main() {
    io::println(`identity.foo.$.bar`(123)); // Valid in the IR but not in Elle functions
}
```

#

### ♡ **Static buffers**

- A static buffer is a basic allocation of stack memory with a specified size.
- You can allocate a buffer with the `type buf[size];` syntax.

This would allocate memory on the stack of that size and give you back a pointer to that type.

For example:

```rs
use std/prelude;

const i32 ARRAY_SIZE = 40;

fn main() {
    i32 foo[ARRAY_SIZE]; // foo's type is `i32 *`

    for i in 0..ARRAY_SIZE {
        foo[i] = (i + 10) * 100;
    }

    $dbg(foo[33]);
}
```

The size doesn't have to be known at compile time:

```rs
use std/prelude;

fn main() {
    let size = i32::parse(io::input("Enter a size -> "));
    i32 foo[size]; // foo's type is `i32 *`

    for i in 0..size {
        foo[i] = (i + 10) * 100;
    }

    $dbg(foo[size - 1]);
}
```

The type of a static buffer cannot be inferred. You must declare it explicitly.

#

### ♡ **Defer statements**

- A `defer` statement is commonly used to group together memory allocation and deallocation. A simple explanation is that it stores whatever statement is defined inside and inserts it when the current scope is about to be left, ie during a return, a block being exited, or an implicit return due to the function scope being left. `defer` statements are inserted backwards.

> [!IMPORTANT]
> If you create a `defer` statement which forces the current scope to be left, any other `defer` statements created before it will NOT be inserted. Observe:

```rs
fn main() {
    defer $println("hi"); // WILL NOT RUN
    defer return 1;
}
```

#

A very simple example of this is declaring a variable and deferring printing its value, like this:

```rs
use std/io;

fn main() {
    let i = 0;

    // If this were not in a defer statement, then this would print 0
    // However, it will print 25 instead.
    // Realistically this code only runs right before the main function leaves scope.
    defer io::print(i);

    i += 5;
    i *= i;
}
```

You can see how this only calls `io::print` right before it returns 0, which is indeed _after_ the `i` variable has had changes made to it. This also works if you return in other scopes, such as if statements, while loops, standalone blocks, etc, as stated above. Any defer statements in inner blocks will not be called on any return, rather will only be called when the inner block is about to leave scope.

This also means that if you, hypothetically, design a program like this

```rs
use std/io;

fn main() {
    let i = 0;
    defer io::print(i);

    {
        defer io::print(i);
        i += 2;
    }

    i *= i;
}
```

The expected output is 2, then 4.
This is because it will call `io::print` once when the standalone block will leave scope, at which point `i` is 2, then it will call `io::print` again when the function itself (`main`) will leave scope, at which point it will be 4 because `i` was squared (`i *= i`).

You can also write something like this:

```rs
fn main() {
    let i = 0;
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

```rs
use std/io

fn main() {
    let size = 10;
    i64 *numbers = mem::malloc(size * #size(i64));
    defer mem::free(numbers);

    for let i = 0; i < size - 1; i += 1 {
        numbers[i] = i * 2;
        let res = numbers[i];
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

#

### ♡ **Type definitions**

- A type definition is used to differentiate between the scope and size of different variables. You must define a type when declaring variables, taking variables as arguments in a function, and yielding the next value from a variadic argument pointer.

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
- `pointer` - Denoted by `<type> *` -> As pointers are just a number, an address in memory, a pointer in Elle is just an `i64` that holds extra context by holding another type so that it can use its size to calculate an offset when indexing its memory.
- `string` - A mapping to a `char *`, which is essentially an array of characters, or a "c-string".
- `fn((type)*) (-> type)?` - A type representing a function pointer. As it's a pointer, it can be `nil`.

#

### ♡ **Type Conversion / Casting**

- A type conversion consists of converting a variable from one type to another, usually compromising precision if converting to a type with a lower size (f64 -> f32) or having more precision if promoting a type (i32 -> i64).

Casting in Elle is a compiler builtin, hence it uses `#cast(T, expr)`.

<br />

Here is an example that casts a float to an integer to add it to another integer:

```rs
fn main() {
    f32 a = 1.5;
    i32 b = #cast(i32, a) + 2;
}
```

Casting is not necessary here, because the Elle compiler is smart enough to automatically cast the `f32` to an `i32` when compiling the arithmetic operation, based on a [weight](https://github.com/acquitelol/elle/blob/rewrite/src/compiler/qbe/type.rs#L649-L662) that each type is assigned.

<br />

You can also cast to pointer types, however note that, unlike C, casting to a pointer type when using `mem::malloc` is _not_ necessary because the Elle compiler automatically casts the `void *` into the type of the variable.

This means you can write:

```rs
fn main() {
    f64 *a = mem::malloc(1024 * #size(f64));
}
```

and Elle will not complain because implicitly converting `void *` -> `T *` and vice versa is allowed.

> [!IMPORTANT]
> Strings are different to regular pointers. Even though they are just `char *`, the compiler will not allow you to implicitly cast a `void*` to a `string`. You will need to explicitly cast it.

#

### ♡ **Unary operators**

- A unary operator is a token used as a prefix to a literal or identifer to apply some operation to it, like negating it.

There are 5 unary operators in Elle:
<br />

- `!` - Logical NOT
- `~` - Bitwise NOT
- `&` - Stack address
- `-` - Negative number
- `+` - Positive number
- `*` - Pointer dereference

Any identifier or literal can be prefixed by one of these operators.

Example of using logical `NOT`:

```rs
use std/io;

fn main() {
    let myBool = false;

    if !myBool {
        io::println("Hello world!");
    }
}
```

Example of using bitwise `NOT`:

```rs
use std/io;

fn main() {
    let a = 1;

    if ~a == -2 {
        io::println("Hello world!");
    }
}
```

This can also be used for negative or positive values:

```rs
const i64 MAX_SIGNED_LONG = 9_223_372_036_854_775_807;
const i64 MIN_SIGNED_LONG = -MAX_SIGNED_LONG - 1;
```

Using unary `-` will multiply the expression by -1 while unary `+` will multiply the expression by 1.

The unary `&` operator is used to get the memory address of a local variable in a function. Here is an example:

```rs
use std/io;

fn other(i32 *something) {
    io::println(*something);
}

pub fn main() {
    let a = 39;
    other(&a);
    return 0;
}
```

Here we declare `a` as 39, then we pass the "address" of `a` to `other` as a pointer to an `i32`, then this pointer is dereferenced.

#

The unary `*` operator is used to dereference a pointer to a value:

```rs
use std/io;

fn other(i32 *a, string *str) {
    io::printf("(fn other)\n\ta = {}\n\tstr = {}", *a, *str);
    *a = 542;
}

fn main() {
    let a = 39;
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

```rs
io::println(*a + 1);
```

will dereference `a` and then add 1 to the result.

This code, however:

```rs
io::println(*(a + 1));
```

will first add 1 to the address of `a`, and then will dereference that address.

#

### ♡ **Arithmetic operations**

- All arithmetic operations are declared with an expression on the left and right of an operator. This means you can call functions, do other arithmetic operations inside of operations, etc.

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
- `<>` - Concatenation (only works on strings)

Operators which exist but can't be used when declaring variables:

- `&&` - Logical And
- `||` - Logical Or
- `..` - Exclusive range
- `..=` - Inclusive range

Keep in mind that you can also use these operators when doing a variable declaration.
This means the following code is valid:

```rs
use std/io;

fn main() {
    let a = 1;
    a ^= 1; // a is now 0
    io::println(a);
}
```

And of course, this works for every arithmetic operator, not just `^`.

Elle follows the standard [order of operations](https://github.com/acquitelol/elle/blob/rewrite/src/lexer/enums.rs#L115-L136) described by mathematics (typically defined as BIDMAS or PEMDAS), which means you can also wrap expressions in `()` to evaluate them before other expressions that may have a higher precedence.

Example of a program that calculates the xor (`^`) and sum (`+`) of some values:

```rs
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

Here's another example, using the string concatenation operator:

```rs
use std/io; // std/io contains std/string so we don't need to import it

fn main() {
    string a = "a" <> "b";
    a <>= "c"; // Concatenation can be done declaratively
    $dbg(a); // Expected: (string) a = "abc"
}
```

#

### ♡ **Constants**

- A constant is a value that cannot be redeclared. In Elle, constants can only be defined at the top level of files, and vice versa too, where the top level of files can _only_ be constants and functions. You cannot define normal variables at the top level.
- Constants can be public, declared using the `pub` keyword.
- Constants that create pointers (such as string literals) are referenced as the first statement of each function to bring them in scope.

Consider this example that uses constants:

```rs
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

#

### ♡ **Non-base-10 literals**

- These are literal numbers which are not declared in base 10.

These may include:

- Hex - 0xFFFFFF
- Octal - 0o777777
- Binary - 0b111111
- Scientific - 2.1e3

Basic example:

```rs
use std/io;

fn main() {
    i64 a = 0xDEADBEEF;
    i32 b = 0o273451456;
    i32 c = 0b111010011011111010010100101;
    i64 d = 1.2e9;
    f64 e = 2.7182818e2;

    $dbg(a, b, c, d, e);
}
```

#

### ♡ **Imports/modules**

Elle's module system works in the following way:

- Elle will look in the `~/.local/include/elle/` folder for modules
- Elle will look in the current working directory for modules

The syntax for importing is as follows:

```rs
use path/to/module;
```

where, in the directory where the file is importing from, there is a `./path/to/module.le` file.

The syntax to export a symbol from your current file is as follows:

```rs
// ./module.le
pub const i32 myFavouriteNumber = 7;

pub fn foo() {
    return 1;
}
```

which you can then import

```rs
use std/io;
use module;

fn main() {
    io::println(foo() + myFavouriteNumber);
}
```

You can also enable a modifier globally for a module.

For example, by default everything in a module is private, but you can use the `pub` keyword to make it public.

```rs
// by default all private

fn foo() {} // foo is implicitly private
fn bar() {} // bar is implicitly private
pub fn baz() {} // baz is explicitly public
```

However, you can make everything in a module public by default, and then mark something as private with `!pub`:

```rs
global pub; // every function in the module is public

!pub fn foo() {} // foo is explicitly private
!pub fn bar() {} // bar is explicitly private
fn baz() {} // baz is implicitly public
```

Similarly, by default every function in a module has a definition, unless you use the `external` keyword:

```rs
// by default every method is defined unless specified with the `external` keyword
fn foo() {} // implicitly defined, requires a body
fn bar() {} // implicitly defined, requires a body
external fn bar(); // explicitly external, so requires just `;` and throws if you try to provide a body
```

You can make every function in a module be external by default. This is useful for headers of functions whose bodies are defined elsewhere, and prevents the repetition of `external fn` so much:

```rs
global external;

fn foo(); // implicitly external, requires just `;`
fn bar(); // implicitly external, requires just `;`
!external fn bar() {} // explicitly defined, so requires a body
```

Finally, you can group together global specifiers:

```rs
global pub, external;

fn foo(); // implicitly public and external
!pub fn bar(); // explicitly private and external
!pub !external fn baz() {} // explicitly private and defined
```

#

### ♡ **Structs**

Structs are allocations in memory with a defined layout. In Elle, these are defined using the `struct` keyword. Struct fields are seperated by commas (with an optional trailing comma). Struct definitions should not end in a semicolon.

Example:

```rs
struct Bar {
    f32 myFloat
}

struct Foo {
    i32 a,
    Bar bar,
    f64 baz,
}
```

You can then create these structures like this:

```rs
fn main() {
    foo := Foo {
        a = 12,
        bar = Bar {
            myFloat = 10.2
        },
        baz = 3.141592
    };

    io::println(foo.bar.myFloat);
}
```

Alternatively, you can use shorthand syntax similar to JavaScript to create structs with fields that are also variables in the local scope:

```rs
struct Foo {
    i32 a
}

fn main() {
    a := 4;
    foo := Foo { a }; // will work

    x := 4;
    foo := Foo { a = x }; // will also work
    foo := Foo { x }; // will not work; no field `x` exists on Foo
}
```

If taking a pointer to them from another function, you can do so like this:

```rs
use std/io;

fn other(Foo *foo) {
    foo.baz = 17.98;
    io::println(foo.a);
}

fn main() {
    foo := ..; // create Foo
    other(&foo);
}
```

> [!NOTE]
> There is no equivalent of the `a->b` operator in Elle. Any pointer to a struct will automatically be dereferenced before processing any fields in the struct.
> You can still manually dereference the struct pointer manually if you like, but it will have no difference compared to directly using dot notation.
> This means that the following code will accurately update the value inside the struct Foo:

```rs
use std/io;

struct Foo {
    i32 a
}

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

```rs
use std/io;

struct Foo {
    i32 a
}

fn Foo::add(Foo self, Foo other) {
    return Foo { a = self.a + other.a };
}

fn main() {
    Foo foo1 = Foo { a = 10 };
    Foo foo2 = Foo { a = 30 };

    Foo res1 = foo1.add(foo2);
    Foo res2 = Foo::add(foo1, foo2);

    $dbg(res1.a, res2.a);
}
```

You can define it like this to create instance methods:

```bnf
instance_method = "fn" namespace "::" name "(" [args] ")"
args = arg {"," arg} ;
arg = type name ;
```

You can then either call them like this:

```rs
let foo = Foo::new();
foo.bar();
```

or like this:

```rs
let foo = Foo::new();
Foo::bar(foo);
```

<br />
In this case, `foo1.add(foo2)` is an identical expression to `Foo::add(foo1, foo2)`
<br />
For more examples, please view [vectors.le](https://github.com/acquitelol/elle/blob/rewrite/std/vectors.le)

You may also specify that `self` is a `<ty> *` instead of a `<ty>` if you require editing it in-place:

```rs
use std/io;

struct Foo {
    i32 a
}

fn Foo::divideBy(Foo *self, i32 num) {
    self.a /= num;
}

fn main() {
    Foo foo = Foo { a = 10 };
    foo.divideBy(2);

    $dbg(foo.a); // foo.a = 5
}
```

The compiler will automatically pass the **address** of `foo` instead of `foo` itself to the function.
<br />
In the case of a method that takes in a `self` _pointer_, the equivalent expression to `foo1.divideBy(2)` is `Foo::divideBy(&foo1, 2)`.

You can also spread existing structs into other structs to provide default values.

> [!IMPORTANT]
> This does not override fields you set. The position of spreading is not relevant.

This means that the following pieces of code are equivalent:

```rs
struct Foo {
    i32 x,
    i32 y
}

fn main() {
    a := Foo { x = 1, y = 2 };
    b := Foo { ..a, x = 13 };
}
```

```rs
struct Foo {
    i32 x,
    i32 y
}

fn main() {
    a := Foo { x = 1, y = 2 };
    b := Foo { x = 13, ..a };
}
```

At the moment, duck typing for spreading is not supported. This means that code like this is not supported:

```rs
struct Foo {
    f32 x,
    f32 y
    bool z
}

fn main() {
    x := Foo { ..Vector2::new(1, 2), z = false };
}
```

#

### ♡ **Enumerations** (enums)

Enums are types which can hold variants. In Elle, these are defined using the `enum` keyword.

Example:

```rs
enum Animal {
    Cat,
    Dog,
    Bird
}

fn main() {
    my_animal := Animal::Cat;
}
```

Enumerations in Elle are dictinct aliases to the type they represent. By default, every enum uses an underlying type of `i32` to represent its variants.

Enum variants are seperated with commas (with an optional trailing comma). Enum definitions should not end in a semicolon.

You can change this with the `@repr(T)` attribute. Here's an example:

```rs
enum Foo @repr(u8) {
    A,
    B,
    C
}

fn main() {
    x := Foo::A; // x's memory representation is u8 internally
}
```

By default, when printed enums will print their variant name as a string. You can override this to print their raw value instead:

```rs
enum Foo @repr(u64) @nofmt {
    A,
    B,
    C
}

fn Foo::__fmt__(Foo *self, i32 nesting) {
    // replace `u64` with the repr type of your enum
    // NOTE: explicit repr types are optional. it will default to `i32`.
    return "{}".format(#cast(u64, self));
}
```

or just cast them to their repr type:

```rs
enum Foo @repr(u64) {
    A,
    B,
    C
}

fn main() {
    x := #cast(u64, Foo::B); // 1
}
```

You can also set the enum variants to specific values. This is quite restrictive, as it only allows:

- String literals
- Integer literals (including hex, binary, etc.)
- Character literals

> [!IMPORTANT]
> If you set one of the variants to a string, every other variant must also be set to a string.

> [!TIP]
> Integer and character literals will automatically continue from their offset for unset variants.

Here's an example using string literals:

```rs
// Note: the repr type is automatically set to String
// for enums which use string literals in their variants
enum Languages {
    Js = "JavaScript",
    Py = "Python",
    Elle = "Elle"
}

fn main() {
    $assert(Languages::Js != Languages::Elle, nil);
}
```

Here is an example using integer literals:

```rs
enum Foo {
    A = 13,
    B, // = 14
    C, // = 15
    D  // = 16
}

enum Bar {
    A = 4,
    B, // 5
    C, // 6
    D = 1,
    E, // 2
    F  // 3
}
```

Here is an example using character literals:

```rs
enum Char @repr(char) {
    A = 'a',
    B, // = 'b'
    C, // = 'c'
    D, // = 'd'
    E  // = 'e'
}

enum Misc @repr(char) {
    LeftParen = '(',
    RightParen, // = ')'
    Snowflake,  // = '*'
    Zero = '0',
    One,        // = '1'
    Two,        // = '2'
    Three,      // = '3'
    Four        // = '4'
}
```

> [!IMPORTANT]
> Structs with character literals do not automatically have their repr set, just like normal integer enums. This is because you may want to use a different repr type like `u8` or `i8` instead of `char`.

You can also define methods on enums. These methods may take in `self`, or may not. Here's an example:

```rs
use std/prelude;

enum Foo {
    Bar,
    Baz
}

fn Foo::from(i32 x) {
    return #cast(Foo, x);
}

fn Foo::is_baz(Foo self) {
    return self == Foo::Baz;
}

fn main() {
    foo := Foo::from(1);
    $dbg(foo.is_baz());

    $dbg(Foo::Bar.is_baz());
}
```

> [!IMPORTANT]
> Enums and **only** enums have a quirk: if there exists an enum with a variant and method that have the same name and you try to get the function pointer of the method, you will be unable to. The variant will always take priority. Observe:

```rs
enum Foo {
    x
}

fn Foo::x() {
    return 42;
}

fn main() {
    x := Foo::x;   // This will NEVER be the function `Foo::x()`
    x := Foo::x(); // Calling it, however, will call the function `Foo::x()`
}
```

Implementation details:

Enumerations are completely compile-time syntax sugar. Their variant values are directly inlined, though there is extra semantic information in their types, which disallows implicit conversions from their repr type to their defined type. Due to them being essentially a distinct existing type, their memory representation is just that of a primitive (pointer/char/int) in any case it is always an integer.

#

### ♡ **Generics**

- Elle allows you to create generic structs and functions which may hold any inner type.

For example, here's a generic function which allows you to pass both integers and floats:

```rs
fn add<T>(T x, T y) {
    return x + y;
}

fn main() {
    add(1, 2);
    add(1.2, 1.3);
}
```

Notice how seamless using the generic was? Elle was able to infer 2 things here: T is whatever type `x` and `y` are, and the return type is _also_ T. This means, even though you can, you usually don't _need_ to explicitly specify all the generics. This is a more verbose but still correct way to do it:

```rs
fn add<T>(T x, T y) -> T {
    return x + y;
}

fn main() {
    add<i32>(1, 2);
    add<f32>(1.2, 1.3);
}
```

Generic structs are created as follows:

```rs
struct Foo<T> {
    T a
}

fn main() {
    Foo<i32> x = Foo { a = 1 };
    Foo<string> y = Foo { a = "hello world!" };
}
```

In this struct, the `a` field can be of _any_ type. Note that for structs, you cannot explicitly declare their inner type. You must do so via inference. Elle will infer the inner type based on the struct's variable declaration most of the time. Take the example above, where we declare `Foo<i32> x = Foo { a = 1 };`. The Elle compiler sees that the type of the left hand side and right hand side are both of Foo, however it sees that the right hand side is a struct declaration of a generic struct, so it uses the left hand side to infer the inner types of the right hand side.

This allows for almost rust-like declarations of generic structs and their methods:

```rs
use std/io;

struct Foo<T, U> {
    T a,
    U b,
}

fn Foo::new<T, U>(T a, U b) -> Foo<T, U> {
    return Foo { a = a, b = b };
}

fn Foo::double_all<T, U>(Foo<T, U> *self) {
    self.a *= 2;
    self.b *= 2;
}

fn Foo::get_a<T, U>(Foo<T, U> self) -> T {
    return self.a;
}

fn Foo::get_b<T, U>(Foo<T, U> self) -> U {
    return self.a;
}

fn main() {
    Foo<i32, f32> foo = Foo::new(10, 1.2);
    foo.double_all();
    $dbg(foo.get_a());
    $dbg(foo.get_b(), foo.b);
}
```

From this you can get a quick grasp of how to use generics effectively. The struct uses 2 generics, and as all methods require to define the `self` argument's type, this means that you need to type <T, U> on every function that takes a Foo<T, U>. This is slightly verbose, and in the future I may allow for syntax to simplify it in the future.

#

### ♡ **Command line arguments (argc/argv)**

- You can optionally accept an array of strings (`string[]`) as the 0th argument in your main function, which will cause the compiler to create this array out of `argc` and `argv`.

If your main function does not accept this array, the array will not be created.

Here is an example of how you can use it:

```rs
fn main(string[] args) {
    let program = args.remove(0);

    for arg in args {
        if arg == "foo" {
            io::printf("i received a foo in my {}!", program);
        }
    }
}
```

Keep in mind that to use this, you must have the dynamic array module imported. You can either manually import `std/collections/array`, or import `std/prelude` which imports the array module inside.

#

### ♡ **Sigils (identifier prefixes)**

- As you might have noticed, Elle has a notion of "sigils" which are used as prefixes to the names of various things to give them a special meaning.

These are the current sigils:

The `$x` sigil (stdlib alias):

- Used to alias a common standard library function.

The `#x` sigil (directive):

- Used to denote a compiler built-in.

The `@x` sigil (attribute):

- Used to denote a tag that can be placed on a function or struct.

For more information on stdlib alises, directives and attributes, please read below the below chapters.

> `&` is not really a sigil, but it can be included here anyway. The `&x` expr gives you the address of `x`. You can read more about this in the Unary Operators chapter.

#

### ♡ **Standard library aliases**

- Used to alias a common standard library function which should be easily accessible but also shouldn't pollute the global namespace.
- Examples of this include:
  - `io::dbg(...)` -> `$dbg(...)`
  - `io::panic(...)` -> `$panic(...)`
  - `Tuple::new(...)` -> `$(...)`
  - `Triple::new(...)` -> `$$(...)`
  - `HashMap::with_entries(...)` -> `$map(...)`
- Note that this is created as an **alias** of the original function. This means you can call `io::dbg` instead of `$dbg`, for example.

#

### ♡ **Directives**

- These are compiler builtins you can call to get a result at compile-time.

The current existing directives are:

| Name                | Description                                                                                                     | Usage                      |
| ------------------- | --------------------------------------------------------------------------------------------------------------- | -------------------------- |
| Static array length | Gives you the length of a static array created using `#[1, 2, 3]` syntax                                        | `#len(expr)`               |
| Size expression     | Gives you the size of type `T` or expression `expr` in bytes as a u64                                           | `#size(T or expr)`         |
| Elle Environment    | Gives you a `ElleEnv *` which is a global environment structure                                                 | `#env`                     |
| Allocate memory     | Allows you to allocate a specific type using the current allocator                                              | `#alloc(T, size?)`         |
| Reallocate memory   | Allows you to reallocate a pointer with a specific type using the current allocator                             | `#realloc(expr, T, size?)` |
| Free memory         | Frees a pointer using the current allocator. If the allocator didn't define a `free` method, this does nothing. | `#free(ptr_expr)`          |
| Set allocator       | Sets the current allocator to the one specified by `expr`                                                       | `#set_allocator(expr)`     |
| Reset allocator     | Sets the current allocator back to `#env.default_allocator`. **Does not call `#env.allocator.free_self`.**      | `#reset_allocator()`       |
| Type conversion     | Uses a set of rules to convert `cast_expr` to type `T`. If it fails, it will throw a compile-time error.        | `#cast(T, cast_expr)`      |

#

### ♡ **Attributes**

- These are tags you can put on functions to specify extra functionality

The current existing attributes are:

| Name     | Description                                                                                         | Applied on                         | Usage                                       |
| -------- | --------------------------------------------------------------------------------------------------- | ---------------------------------- | ------------------------------------------- |
| Alias    | Allows you to specify a custom alias for a symbol, at compile-time                                  | External functions                 | `@alias(name)` or `@alias(namespace::name)` |
| Volatile | Specifies that Elle should not discard this symbol during dead code elimination                     | Functions                          | `@volatile`                                 |
| Format   | Puts every argument through its formatter before passing it to the function                         | Functions                          | `@fmt`                                      |
| NoFormat | If on an argument, doesn't run it through its formatter, else doesn't generate an `__fmt__` method. | Structs, Enums, Function Arguments | `@nofmt`                                    |
| NoEq     | Doesn't generate an `__eq__` method when applied to enums.                                          | Enums                              | `@noeq`                                     |
| Repr     | Uses a different type for the representation of this structure. Can be optional.                    | Enums                              | `@repr(T)`                                  |
| Unused   | Does not report a warning when the field is not set in a struct literal. Useful for padding.        | Struct fields                      | `@unused`                                   |

#

Example of attribute usage:

```rs
// Attributes go BEFORE the return type
// The alias attribute will be purposefully ignored
// because this function is not external
fn add(i32 x, i32 y) @alias(foo) @volatile -> i32 {
    return x + y;
}

// The volatile attribute will be purposefully ignored
// because external functions do not generate IR
external fn printf(string formatter, ...) @alias(formatted_print) @volatile;

enum Foo @repr(u8) {
    A,
    B,
    C
}
```

If you specify an alias attribute on a non-external function, you will only be warned, an error will **not** be thrown. Keep in mind that external functions do not generate IR, so the @volatile attribute will have no effect on them.

#

### ♡ **Formatters**

Elle allows you to specify how your structs should be formatted. By default, structs will automatically have a format function generated for them by the compiler. If you want to make your own, simply create it as a struct method:

```rs
struct Foo<T> {
    T a,
    T b,
}

fn Foo::__fmt__<T>(Foo<T> self, i32 nesting) {
    return string::format("{}", self.a + self.b);
}
```

Some things to keep in mind:

- The format function _must_ return a string.
- The format function takes a `nesting` argument. This is used to determine the depth of nested structs when printed.

If an automatically generated struct's format method is too much bloat and you need the size of your executable to be small, you can specify that a struct should not generate an automatic formatting method with the `@nofmt` attribute:

```rs
struct Foo<T> @nofmt {
    T a,
    T b,
}
```

If you try to print Foo<T> however, you will get a compiler error.

To create functions that use these formattings, you can specify the @fmt attribute:

```rs
use std/io;

fn foo(ElleMeta meta, ...args) @fmt {
    for i32 i = 0; i < meta.arity; i += 1 {
        string arg = args.yield(string); // The formatter will return a string
        // Do something with it like printing it
        io::println(arg);
    }
}

fn main() {
    foo(1, "hi", true);
}
```

To the compiler, this signals that every argument should be ran through its formatter. The equivalent code without `@fmt` is:

```rs
use std/io;

fn foo(ElleMeta meta, ...args) {
    for i32 i = 0; i < meta.arity; i += 1 {
        string arg = args.yield(string);
        io::println(arg);
    }
}

fn main() {
    // Keep in mind __fmt__ *must* return a string.
    // The compiler will throw an error if it doesn't.
    foo(i32::__fmt__(1, 0), string::__fmt__("hi", 0), bool::__fmt__(true, 0));
}
```

You can also define a function which formats everything _except_ the arguments you specify. This is especially useful for formatting instance methods defined on structs:

```rs
use std/prelude;

struct Foo {
    i32 x,
}

// ElleMeta is already not formatted
fn Foo::format(ElleMeta meta, @nofmt Foo *self, ...args) @fmt {
    res := "{}\n".format(self.x);

    // account for `self`
    for i in 0..meta.arity - 1 {
        res <>= args.yield(string);
        res <>= i < meta.arity - 2 ? "\n" : "";
    }

    return res;
}

fn main() {
    foo := Foo { x = 39 };
    $dbg(foo.format(1.2, "bar", "baz"));
}
```

#

### ♡ **Objects and linking**

You may specify that Elle should emit an Object file instead of an executable by passing the `-c` flag.

If you want to use an object file in your project, you can do so like this:

```rs
// foo.le
//
// `add` must be public so that it is exported
// and must be volatile to prevent DCE from removing it
pub fn add(i32 a, i32 b) @volatile {
    return a + b;
}
```

Then, compile it into an object file:

```console
$ ellec -c foo.le
```

which will emit foo.o

Finally, use it:

```rs
// main.le
use std/io;

external fn add(i32 a, i32 b) -> i32;

fn main() {
    io::println(add(23, 16));
}
```

and compile it:

```console
$ ellec main.le foo.o && ./main
```

#

### ♡ **External symbols**

- An external symbol is a definition for a function or constant that was defined elsewhere (such as in C) and is implicitly defined in Elle. This is used to give definition and context to functions that were not defined in Elle but you wish to use in when writing Elle code.
  <br/>

You can do this with the following example:

```rs
external fn printf(string formatter, ...);
```

It essentially tells Elle where it should put the variadic argument starter. You could exclude this, if you like, but you will have to explicitly declare where the variadic arguments begin, because Elle no longer has this context.

You can also make these statements public:

```rs
pub external fn fprintf(FILE *fd, string formatter, ...);
```

In fact the order of prefixes before `fn` is not enforced, you can write `external pub fn` and achieve the same result.

You may also alias exported functions, and allow them to be accessible through a pseudo-namespace:

```rs
namespace raylib;
pub external fn InitWindow(i32 width, i32 height, string title) @alias(raylib::init_window);

// You can now call raylib::init_window() and it will internally reference the InitWindow symbol
```

**Technical note:** This declaration does not emit any IR code. This means that all these definitions do is provide more information and context to the compiler. They do not change the output of the program directly.

#

### ♡ **C FFI**

Elle is entirely C ABI compliant. This means that C code is directly callable from within Elle:

- Structs are packed like C
- Strings are null terminated
- Nothing is mangled

♡ **You can import any C function like this:**

```rs
external fn ClearBackground(Color color);
```

- Structs are packed by default:

```rs
struct Color {
    u8 r,
    u8 g,
    u8 b,
    u8 a,
}
```

No `extern "C"` is necessary.

♡ **Strings are null terminated** which means you are able use the same string type for importing functions that take `char*` in C:

```rs
external fn printf(string fmt, ...) -> i32;
```

aswell as being able to call the function with no wrappers:

```rs
printf("Hello, world! %d\n", 42);
```

**Technical note:** `string` is sugar for `char *`. `any` is sugar for `void *`.

♡ **Linking with Elle code is as easy as C:**

`cc test.c -c`

```c
int square(int x) {
    return x * x;
}
```

`ellec test.le test.o --run`

```rs
use std/prelude;

external fn square(i32 x) -> i32;

fn main() {
    $dbg(square(5));
}
```

You can do the opposite:

`ellec test.le --noalloc --nofmt --nosm -c`

```rs
pub fn square(i32 x) {
    return x * x;
}
```

`cc -o test test.c test.o && ./test`

```c
#include <stdio.h>

extern int square(int x);

int main() {
    printf("%d\n", square(5));
}
```

♡ **Functions in Elle are callable from C, but not if they're namespaced:**

A namespaced function `foo::bar` internally creates `foo.bar`, which isn't importable from C. You can just make wrappers for these cases.

#

### ♡ **Lazy Iterators**

Elle allows you to define and use lazy iterators, which allow you to yield values lazily:

```rs
fn main() {
    x := 1..10;

    $dbg(x.next()); // Some(1)
    $dbg(x.next()); // Some(2)
}
```

```rs
fn main() {
    x := (8..=10).rev();

    $dbg(x.next()); // Some(10)
    $dbg(x.next()); // Some(9)
    $dbg(x.next()); // Some(8)
}
```

```rs
fn main() {
    x := Iterator::once(42);

    $dbg(x.next()); // Some(42)
    $dbg(x.next()); // None

    gen := fn() Iterator::once(42);
    x := gen().cycle(gen);

    $dbg(x.next()); // Some(42)
    $dbg(x.next()); // Some(42)
    $dbg(x.next()); // Some(42)
    $dbg(x.next()); // Some(42)
}
```

The iterator abstraction is also used in `foreach` loops:

```rs
for x in 1..10 {
    $dbg(x);
}
```

```rs
for i, x in (1..10).enumerate() {
    $dbg(x);
}
```

You can also iterate over types that implement the `__iter__` method. Some of these include:

- `Array<T>`
- `HashSet<T>`
- `HashMap<T, U>`
- `string`

Iterators can be both `SingleEnded` or `DoubleEnded` (can be yielded from the back of the iterator). This is enforced with a Type-based phantom generic on the `Iterator<T, AnyEnded>` type.

A method which returns `AnyEnded` may accept both a `SingleEnded` or a `DoubleEnded` iterator. The result will be the same type as the one that was passed in.

Iterators have some base methods which mutate or consume the iterator. These (as of 0.68.0) include:

| Method          | `???Ended`    | Description                                                                                         |
| --------------- | ------------- | --------------------------------------------------------------------------------------------------- |
| `next`          | `AnyEnded`    | Yields the next value from the front of the iterator.                                               |
| `next_back`     | `DoubleEnded` | Yields the next value from the back of the iterator, if it is `DoubleEnded`.                        |
| `count`         | `AnyEnded`    | Counts however many items are left in the iterator, consuming it.                                   |
| `fold`          | `AnyEnded`    | Reduces the iterator to a single value by calling it with an accumulator and current value.         |
| `sum`           | `AnyEnded`    | Adds together all of the values inside of the iterator into a single number.                        |
| `product`       | `AnyEnded`    | Multiplies together all of the values inside of the iterator into a single number.                  |
| `max`           | `AnyEnded`    | Finds the largest value in the iterator, consuming it. If no value is found, returns None.          |
| `min`           | `AnyEnded`    | Finds the smallest value in the iterator, consuming it. If no value is found, returns None.         |
| `nth`           | `AnyEnded`    | Skips `n` elements from the iterator provided, then returns the next value from it.                 |
| `last`          | `AnyEnded`    | Returns the last value from the iterator provided, consuming it.                                    |
| `collect`       | `AnyEnded`    | Collects the iterator's values into an array of the same type, consuming it.                        |
| `single`        | `DoubleEnded` | Turns a DoubleEnded iterator into a SingleEnded one.                                                |
| `find_with`     | `AnyEnded`    | Attempts to find the value in the iterator provided with the predicate and arg provided.            |
| `find`          | `AnyEnded`    | Attempts to find the value in the iterator provided with the predicate.                             |
| `any_with`      | `AnyEnded`    | Returns true if the predicate holds true for any value in the iterator, with an arg passed to it.   |
| `any`           | `AnyEnded`    | Returns true if the predicate holds true for any value in the iterator.                             |
| `all_with`      | `AnyEnded`    | Returns true if the predicate holds true for all values in the iterator, with an arg passed to it.  |
| `all`           | `AnyEnded`    | Returns true if the predicate holds true for all values in the iterator.                            |
| `position_with` | `AnyEnded`    | Returns the index of the first occurence when the predicate returns true, with an arg passed to it. |
| `position`      | `AnyEnded`    | Returns the index of the first occurence when the predicate returns true.                           |
| `for_each_with` | `AnyEnded`    | Consumes the iterator while calling a function on each value, with an arg passed to it.             |
| `for_each`      | `AnyEnded`    | Consumes the iterator while calling function on each value.                                         |
| `flat_map`      | `AnyEnded`    | Maps the values into some iterator then flattens all of the iterators into a single iterator.       |

Iterators are powerful in that they can have adapters applied to them, to perform many operations lazily.

These (as of 0.68.0) include:

| Adapter        | `???Ended`    | Description                                                                                                                                          |
| -------------- | ------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| `once`         | `SingleEnded` | Takes a value and yields it lazily a single time.                                                                                                    |
| `range`        | `DoubleEnded` | Creates an iterable range between 2 values, optionally including the end. There is also syntax sugar for this in the form of `x..y` and `x..=y`.     |
| `iota`         | `SingleEnded` | Yields infinitely from the `start` parameter provided.                                                                                               |
| `filter`       | `AnyEnded`    | Filters the values from the iterator passed in with the predicate provided.                                                                          |
| `map/map_with` | `AnyEnded`    | Projects the values from the iterator passed in with the callback provided, aswell as an arg if using `map_with`                                     |
| `rev`          | `DoubleEnded` | Switches `next` with `next_back`, essentially allowing for yielding from the back of the iterator.                                                   |
| `take`         | `AnyEnded`    | Takes `n` elements from the iterator provided. If the iterator has less than `n` elements in it, `Take` will yield however many elements are left.   |
| `chunks`       | `AnyEnded`    | Splits the elements from the iterator into `n` sized collected arrays of `T`. If there isn't an even split, the last array will have `< n` items.    |
| `windows`      | `AnyEnded`    | Returns a sliding window over the iterator of size `n`. If `n` is larger than the length of the iterator, this will yield nothing.                   |
| `cycle`        | `SingleEnded` | Accepts any `Iterator<T, AnyEnded>` and a generator for any `Iterator<T, AnyEnded>. Replaces old iterator once it runs out by calling the generator. |
| `intersperse`  | `SingleEnded` | Accepts any `Iterator<T, AnyEnded>` and yields the value provided each odd yield. `(0..3).intersperse(99).collect()` == `[0, 99, 1, 99, 2]`.         |
| `skip`         | `AnyEnded`    | Returns a new iterator which skips `n` elements from the iterator provided.                                                                          |
| `enumerate`    | `SingleEnded` | Accepts any iterator and yields values from it along with an index, in a tuple. `["a", "b"].iter().enumerate().collect()` == `[(0, a), (1, b)]`.     |
| `zip`          | `AnyEnded`    | Accepts any 2 iterators which may yield different values and yields a tuple of a value from each one. Stops whenever one has no values left.         |
| `chain`        | `AnyEnded`    | Accepts any 2 iterators which yield the same type and yields iterators from the second iterator once the first has no values left.                   |
| `peekable`     | `AnyEnded`    | Accepts any iterator and allows peeking values from the front of it.                                                                                 |
