use std/io;

fn foo<T, U>(U x, T y) {
    return x + y;
}

fn main() {
    // Expect this to be 2.2 NOT 2.0 and the type to be **f32** NOT i32.
    // The order of generics is purposefully switched in the function signature
    // It needs to infer than T = f32 and U = i32 based on the actual types provided
    // not in the order that they were provided
    //
    // In the old implementation it would check `U x` first, push that to a vec
    // and use that position to determine the "known" type for that generic.
    // That's wrong, because the position of it is 0 but the position of U is 1.
    //
    // With this new implementation we use a hashmap to determine the known type
    // for a generic which is not only cleaner but also far more correct than before.
    io::dbg(foo(1, 1.2));
    io::dbg(foo(1.2, 1));
}
