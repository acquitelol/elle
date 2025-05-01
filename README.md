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

# ♡ **FAQ**

### ✩ I _heavily_ recommend reading the [FAQ](https://github.com/acquitelol/elle/blob/rewrite/FAQ.md) before trying the language. ✩

It will allow you to grasp the strengths and weaknesses of the language at a glance and quickly learn whether this language is one you would be comfortable using.

### ✩ _If you like this project, consider giving it a star!_ ✩

### ♡ **Hello, World!**

Writing a hello world program in Elle is super easy:

```rs
use std/io;

fn main() {
    io::println("Hello world!");
}
```

- Simple enough! ♡

<hr />

### ♡ If you have any questions, please **[raise an issue](https://github.com/acquitelol/elle/issues/new) :3**

All contributions to this project are welcome and I love talking about this stuff!

<hr />

### ♡ **How to run**

- Ensure you have [Rust](https://www.rust-lang.org/), [Cargo](https://crates.io/) and the [QBE](https://c9x.me/compile/) compiler backend.

  ```terminal
    $ git clone https://github.com/acquitelol/elle
  ```

  ```terminal
    $ cd elle
  ```

  ```console
    $ make
  ```

  to install the compiler, standard library, and runtime (installs into ~/.local/ by default)

  Add the compiler executable to your `$PATH`:

  ```console
    $ export PATH="$HOME/.local/bin:$PATH"
  ```

  **OR**

  ```console
    $ make compile-release
  ```

  to get only a compiler executable and not install anything (does not require root)

  - **You're done!**

#### ♡ You can now run `ellec` to get a help message of how to use the compiler!

Try compiling a simple example!

```console
  $ ellec ./examples/hello.le && ./hello
```

Try compiling an example with libraries!

```
  $ ellec ./examples/graphics/ball.le -z -lraylib && ./ball
```

<hr />

### ♡ **Licensing**

- Please read [LICENSE.md](https://github.com/acquitelol/elle/blob/rewrite/LICENSE.md)
- Copyright © 2024 Rosie ([acquitelol](https://github.com/acquitelol))

<hr />

<a href="#top">⇡ Back to top️!</a>
