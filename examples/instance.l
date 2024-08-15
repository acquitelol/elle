use std/io;

def Foo {
    i32 a;
}

fn Foo.add(Foo self, Foo other) {
    return Foo {
        a = self.a + other.a
    };
}

fn main() {
    Foo foo1 = Foo { a = 10 };
    Foo foo2 = Foo { a = 30 };

    Foo res1 = foo1.add(foo2);
    Foo res2 = Foo.add(foo1, foo2);

    dbg(res1.a, res2.a);
}
