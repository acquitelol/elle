use std/io;

struct Foo {
    i32 a;
};

// `Foo`, only gets a copy of Foo
fn Foo::add(Foo self, Foo other) {
    return Foo { a = self.a + other.a };
}

// `Foo *`, edits in-place
fn Foo::divideBy(Foo *self, i32 num) {
    self.a /= num;
}

fn main() {
    Foo foo1 = Foo { a = 10 };
    Foo foo2 = Foo { a = 30 };

    // The compiler automatically passes a reference to Foo if the function needs `Foo *` and not `Foo`
    foo2.divideBy(15);
    Foo resWithInstance = foo1.add(foo2);
    Foo resWithStruct = Foo::add(foo1, foo2);

    io::assert(
        resWithInstance.a == resWithStruct.a,
        "Instance and struct methods should produce equal results."
    );

    io::dbg(resWithInstance.a, resWithStruct.a);
}
