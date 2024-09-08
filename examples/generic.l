use std/io;

struct Foo<T> {
    T a;
};

fn Foo::double<T>(Foo<T> *self) {
    self.a *= 2;
}

fn Foo::get_a<T>(Foo<T> self) -> T {
    return self.a;
}

fn main() {
    Foo<i32> foo = Foo { a = 10 };
    foo.double();
    io::dbg(foo.get_a());
}
