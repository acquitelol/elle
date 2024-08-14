use std/io;

const i32 ITERATIONS = 60;

fn fibonacci_cached(i32 n, i64 *cache) -> i64 {
    if n <= 1 {
        return (i64)n;
    }

    if cache[n] == -1 {
        cache[n] = fibonacci_cached(n - 1, cache)
            + fibonacci_cached(n - 2, cache);
    }

    return cache[n];
}

fn fibonacci_iterative(i32 n) -> i64 {
    if n <= 1 {
        return (i64)n;
    }

    i64 previous = 0;
    i64 current = 1;

    for i32 i = 1; i < n; i += 1 {
        i64 next = previous + current;

        previous = current;
        current = next;
    }

    return current;
}

fn main() {
    i64 cache[ITERATIONS + 1];
    memset(cache, -1, #size(cache));

    dbg(
        fibonacci_cached(ITERATIONS, cache),
        fibonacci_iterative(ITERATIONS)
    );
}
