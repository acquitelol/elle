use std/io;
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

fn Option::expect<T>(Option<T> self, string message) -> T {
    if !self.__is_some {
        // io::eprintln(message);
        abort();
    }

    return self.__value;
}

fn Option::unwrap<T>(Option<T> self) -> T {
    return self.expect("Called `Option::unwrap()` on a `None` value");
}

fn Option::unwrap_or<T>(Option<T> self, T fallback) -> T {
    if !self.__is_some {
        return fallback;
    }

    return self.__value;
}

fn Option::is_some<T>(Option<T> self) {
    return self.__is_some;
}

fn Option::is_none<T>(Option<T> self) {
    return !self.is_some();
}
