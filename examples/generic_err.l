// This file should **cause an error**
// Something along the lines of being
// unable to find the generics U and V.

fn foo<T, U, V>(T a, T b) -> U {
    return a + b;
}

fn main() {
    foo(1, 2);
}
