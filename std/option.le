use std/libc/io;
use std/libc/mem;
global pub;

struct Option<T> {
    T __value;
    bool __is_some;
};

local fn alloc_some<T>(T value) -> Option<T> {
    Option<T> *option = malloc(#size(Option<T>));
    *option = malloc(#size(Option<T>));

    option.__value = value;
    option.__is_some = true;

    return *option;
}

local fn alloc_none<T>() -> Option<T> {
    Option<T> *option = malloc(#size(Option<T>));
    *option = malloc(#size(Option<T>));

    option.__value = 0;
    option.__is_some = false;

    return *option;
}

fn Option::Some<T>(T value) -> Option<T> {
    return alloc_some(value);
}

fn Option::None<T>() -> Option<T> {
    return alloc_none();
}

// Returns the inner T if the option is some, otherwise aborts with a custom message
fn Option::expect<T>(Option<T> self, string message) -> T {
    if !self.__is_some {
        io::fprintf(stderr, "%s\n", message);
        abort();
    }

    return self.__value;
}

// Returns the inner T if the option is some, otherwise aborts
fn Option::unwrap<T>(Option<T> self) -> T {
    return self.expect("Called `Option::unwrap()` on a `None` value");
}

// Returns the inner T if the option is some, otherwise returns the fallback
fn Option::unwrap_or<T>(Option<T> self, T fallback) -> T {
    if !self.__is_some {
        return fallback;
    }

    return self.__value;
}

// Returns true if the option has a value inside
fn Option::is_some<T>(Option<T> self) {
    return self.__is_some;
}

// Returns true if the option has no value inside
fn Option::is_none<T>(Option<T> self) {
    return !self.is_some();
}
