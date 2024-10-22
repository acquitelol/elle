use std/io;

struct Bar {
    i32 meow;
    i32 nya;
};

struct Foo {
    i64 a;
    f64 *b;
    i32 c;
    Bar bar;
    Bar *ptr;
};

fn other(Foo *a) {
    a.b[0] = 1.7113e2;
    a.bar.meow = 50001;
    a.ptr.nya = 10;

    io::dbg(a.ptr.nya, a.b[0]);
}

fn main() {
    Bar bar = Bar {
        meow = 9,
        nya = 8
    };

    Foo foo = Foo {
        a = 752,
        c = 12,
        b = [1.2, 512.6],
        bar = Bar {
            meow = 100,
            nya = 0
        },
        ptr = &bar
    };

    foo.c = 14;

    other(&foo);
    io::dbg(foo.c, foo.bar.meow, foo.ptr.nya);
}
