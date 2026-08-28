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

```rs
use std/prelude;

fn solve(string[] blocks) {
    sizes := blocks.slice(0, 6).iter().map(fn: it.iter().map(fn: it == '#').sum()).collect();
    total := 0;

    for part in blocks[6].nums<i32>().iter().chunks(8).map(fn: it.iter()) {
        area := part.take(2).product();
        total += area > sizes.iter().zip(part).map(fn: it.x * it.y).sum();
    }

    return total;
}

fn main(string[] args) {
    blocks := io::read_to_string(args[1]).split("\n\n");
    $dbg(solve(blocks));
}
```

```rs
use std/prelude;
use std/net/tcp;

let BUF_SIZE = 1024;

fn main() {
    server := TcpServer::bind(port := 8080);

    while server.is_err_and(fn: it == Errno::EADDRINUSE) {
        server = TcpServer::bind(port += 1);
    }

    server := server.unwrap();
    defer server.close().unwrap();
    $printf("Server is listening on port {}", port);

    connection := server.accept().unwrap();
    defer connection.close().unwrap();
    buf := Array::bytes(BUF_SIZE);

    while (_, received := connection.read(buf)) && received != 0 {
        $printf("Received {} bytes: {}", received, buf.join("").replace("\n", ""));
        connection.write(buf).expect("Failed to send response");
    }
}
```

#

#### ♡ **Projects made in Elle**

| Project        | Description                                                                                                                                       | Image                                                                                                    | More                                               |
| -------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------- | -------------------------------------------------- |
| **Raytracer**  | A pathtracer experiment using Raylib & OpenGL rendering scenes with up to 4.4 million triangles.                                                  | <img src="https://github.com/acquitelol/raytracer/raw/mistress/images/new_dragon.png?raw=1" width="240"> | [![view]](https://github.com/acquitelol/raytracer) |
| **Black Hole** | A simplified black hole simulation in real time using Raylib and OpenGL.                                                                          | <img src="https://github.com/acquitelol/blackhole/raw/mistress/assets/out2.gif?raw=1" width="240">       | [![view]](https://github.com/acquitelol/blackhole) |
| **Peephole**   | A speculative dystopia horror game designed around looking through a peephole.                                                                    | <img src="https://github.com/acquitelol/peephole/raw/mistress/assets/example.png?raw=1" width="240">     | [![view]](https://github.com/acquitelol/peephole)  |
| **AOC2025**    | All [Advent of Code](https://adventofcode.com/) problems in 2025 completed in Elle, running in 404.1ms ± 3.6ms cumulatively on an M4 MacBook Air. |                                                                                                          | [![view]](https://github.com/acquitelol/aoc2025)   |
| **Pomee**      | A 4-key 2D Virtual Scroller Rhythm Game with original music                                                                                       | <img src="https://github.com/acquitelol/pomee/raw/mistress/images/pomee.png?raw=1" width="240">          | [![view]](https://github.com/acquitelol/pomee)     |

#

#### ♡ **How to run**

> [!IMPORTANT]
> Ensure you have [Rust](https://www.rust-lang.org/), [Cargo](https://crates.io/) and the [QBE](https://c9x.me/compile/) compiler backend.

```terminal
git clone https://github.com/acquitelol/elle
```

```terminal
cd elle
```

(does not require root)

```console
make
```

to install the compiler, standard library, and runtime (installs into ~/.local/ by default)

Add the compiler executable to your `$PATH`:

```console
export PATH="$HOME/.local/bin:$PATH"
```

**OR**

```console
  $ make compile-release
```

to get only a compiler executable and not install anything.

#### ♡ You can now run `ellec` to get a help message of how to use the compiler!

Try compiling a simple example!

```console
ellec ./examples/hello.le --run
```

Try compiling an example with libraries!

```
ellec ./examples/graphics/newton.le -z -lraylib --run
```

#

### ♡ **Licensing**

- Please read [LICENSE.md](https://github.com/acquitelol/elle/blob/rewrite/LICENSE.md)
- Copyright © 2024 Rosie ([acquitelol](https://github.com/acquitelol))

#

<a href="#top">⇡ Back to top️!</a>

[view]: https://img.shields.io/badge/view-ffb5e4?style=for-the-badge&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFoAAABaCAYAAAA4qEECAAAACXBIWXMAAAsTAAALEwEAmpwYAAADyUlEQVR4nO2cTYiVVRjHfzednBxHCrHoUxTd6SgiYrlsodIXEQjS2lwYogtJpI2LxMGvXDXoJjIUDUskopqmdBzToChb6Cgx6iKX6TQZqOM8ceBAl2HGmXfmfDzve58f/Df3Dvc+//+d+97znvOcA4ZhGIZhGIZhGIZhGMZ4mAm8DuwDOoEbwC1AgAHgT6AL2A+8BjwWIdbp/rU/BL4Hbvr3Fl/LdeBbYK//u9YyfbQrgSPAv96QjFP9wEFgboAa5gGHgL8L1nAH+BR4EcUsAX4oaExG0D2gA3h2AjU85z+sewHq+A5oQxFNQDswGMCc1Ml9tdcVqONt4HbgGu4DO4GpZOYp4MfA5mSYPgGaH1KDe+5w5Bp6gNlkYg5wNbJB8ToDPDFCDe6x7kQ1XAFeSB3ykwlDFq9fgceHhXwxcQ1XUv5nNyW4XMhDvsLNXucy1dCd6prdnsmgeB3yylnDB7FDXuR/iaXBdR9YHDPoEONkqYjc3W4UVigwJ8r0UoygjygwJsrkxu/BJ4iKzl00gv4BZoQM+g0FpkSpXgkZ9D4FhkSp9oQMulOBIVGqr0MGfU2BIVGqvpBBF51AbyTdDhm03Q0yatAum2CEnlCXCumvkEH3KTAkSvVHyKB/UWBIlOqnkEF/ocCQKNWJkEG/r8CQKNW2kEGvUmBIlOrlkEHPAh4oMCXKNDjKovGkOKvAmCiTWwgJzmYFxkSZ3o0RtGu3GlJgTpTIXUqfIRJdCgyKErku1Gi8qsCgKNGamEHXgMsKTEpmuY6lR4jMBgVGJbPWkwDXDnVJgVnJpF7fFpeEtxQYlkxy20WSUcvY6CgZ5dqHk9MWaAuDlER3gYVkYqeCACSRdpCRaQ3yw9g7xvaOJCyv+CXkLrAMJWxVEIhE0hYU4UYhpxSEIoH1lfemitkV62jq8wseKlnsW1ml5Brw20hU82bJ562HgLWUhO0KApMJytVeKg4oCE0K6iNKiJuv/UxBeDJOnQSmUFKa/ZKPKNc3/i631ExXvj/RbXtuoSK0Kp1W7Qm9q0oDLcr2wpwu2xlKRZgGfK4g5C8jHZaliqnAxxlDPppyzU/D0O9ghpA7UrQJaKMG7E4YcrvGmbiUvBN515drrd2U26QWVvkDBmPMwrkTGo06FvljNUOF7I7kXFr/Bsb/PA38HCDk34Dn617XGOXG5vgkQj5WpVvq2NSA9woew+km7Hc1+shioqz2W4DHCrk/dU9cFVkwxomNvwPzcxdZFZqAjcB5v/jrznm+4B97NHdxhmEYhmEYhmEYhmEYNBj/AcBVJqVGsTFDAAAAAElFTkSuQmCC
