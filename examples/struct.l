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
}

fn other(Foo *a) {
    (*a).b[0] = 1.7113e2;
    printf("a->b[0] = %f\n", (*a).b[0]); // 171.13
}

fn main() {
    Foo foo = Foo {
        a = 752,
        c = 12,
        b = [1.2, 512.6],
        bar = Bar {
            meow = 100,
            nya = 0
        }
    };

    foo.c = 14;

    other(&foo);
    printf("foo.c = %d\n", foo.c); // 14
    printf("foo.bar.meow = %d\n", foo.bar.meow); // 100
}
