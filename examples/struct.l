use std/io;

def Bar {
    i32 meow;
    i32 nya;
}

def Foo {
    i64 a;
    f64 *b;
    i32 c;
    Bar bar;
    Bar *ptr;
}

fn other(Foo *a) {
    a.b[0] = 1.7113e2;
    a.bar.meow = 50001;
    a.ptr.nya = 10;

    printf("a.ptr.nya = %d\n", a.ptr.nya);
    printf("a.b[0] = %f\n", a.b[0]);
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
    printf("foo.c = %d\n", foo.c); // 14
    printf("foo.bar.meow = %d\n", foo.bar.meow); // 100
    printf("foo.barptr.nya = %d\n", foo.ptr.nya);
}
