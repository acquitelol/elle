<p align="center">
    <img src="https://github.com/acquitelol/elle/blob/rewrite/images/mia_512x.png?raw=true" alt="Mia" width="256">
</p>

<h2 align="center">
    <strong>
        ₊˚ Elle ♡︎
    </strong>
</h2>

Elle is a procedural programming language built in Rust which compiles to the [QBE](https://c9x.me/compile) backend. There is also a [mirror of this repository](https://git.sr.ht/~rosiepie/elle/) on SourceHut.

#

✩ It is _heavily_ recommended to read the [FAQ](https://github.com/acquitelol/elle/blob/rewrite/FAQ.md) before trying the language. It will allow you to grasp the strengths and weaknesses of the language at a glance and quickly learn whether this language is one you would be comfortable using.

#

✩ You can view the Elle documentation [here](https://github.com/acquitelol/elle/blob/rewrite/DOCS.md). Please note the language is still in active development and documentation is due to change at any time with no warning.

#### ♡ **Hello, World!**

```rs
use std/prelude;

fn main() {
    $println("Hello world!");
}
```

#

#### ♡ **How to run**

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
  $ ellec ./examples/hello.le --run
```

Try compiling an example with libraries!

```
  $ ellec ./examples/graphics/ball.le -z -lraylib --run
```

#

### ♡ **Licensing**

- Please read [LICENSE.md](https://github.com/acquitelol/elle/blob/rewrite/LICENSE.md)
- Copyright © 2024 Rosie ([acquitelol](https://github.com/acquitelol))

#

<a href="#top">⇡ Back to top️!</a>
