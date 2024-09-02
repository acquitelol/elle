use std/libc/mem;
use std/io;

generic T;
global pub;

struct Option {
	T value;
	bool __private_has_value;
};

local fn alloc_option(T value, bool has_value) -> Option * {
    Option *option = malloc(#size(Option));
    *option = malloc(#size(Option));

    option.value = value;
    option.__private_has_value = has_value;

    return option;
}

fn Some(T val) {
    return *alloc_option(val, true);
}

const Option None = *alloc_option((T)nil, false);

fn Option::Some(T val) {
    return *alloc_option(val, true);
}

fn Option::None() {
    return None;
}

fn Option::expect(Option self, string message) {
    if !self.__private_has_value {
        io::eprintln(message);
        abort();
    }

    return self.value;
}

fn Option::unwrap(Option self) {
    return self.expect("Called `Option::unwrap()` on a `None` value");
}

fn Option::unwrap_or(Option self, T value) {
    if !self.__private_has_value {
        return value;
    }

    return self.value;
}

fn Option::is_some(Option self) {
    return self.__private_has_value;
}

fn Option::is_none(Option self) {
    return !self.is_some();
}
