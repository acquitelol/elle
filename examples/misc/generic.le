use std/io;

struct Foo<T, U> {
    T a;
    U b;
};

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
    return self.b;
}

fn main() {
    Foo<i32, f32> foo = Foo::new(10, 1.2);
    foo.double_all();

    io::dbg(foo.get_a());
    io::dbg(foo.get_b(), foo.b);
}
