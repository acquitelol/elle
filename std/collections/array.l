use std/libc/mem;
use std/libc/io;
use std/string;
global pub;

struct Array<T> {
    i32 size;
    i32 capacity;
    T *elements;
};

fn Array::len<T>(Array<T> *self) -> i32 {
    return self.size;
}

fn Array::is_empty<T>(Array<T> *self) -> bool {
    return self.size == 0;
}

fn Array::with_capacity<T>(i32 initialCapacity) -> Array<T> * {
    Array<T> *array = malloc(#size(Array<T>));
    *array = malloc(#size(Array<T>));
    array.elements = malloc(initialCapacity * #size(T));
    array.size = 0;
    array.capacity = initialCapacity;

    return array;
}

fn Array::new<T>(ElleMeta meta, ...) -> Array<T> * {
    Array<T> *array = Array::with_capacity(2);

    if meta.arity > 0 {
        variadic args[meta.arity];

        for i32 i = 0; i < meta.arity; i += 1 {
            array.push(args.yield(T));
        }
    }

    return array;
}

fn Array::resize<T>(Array<T> *self, i32 newCapacity) {
    self.elements = realloc(self.elements, newCapacity * #size(T));
    self.capacity = newCapacity;
}

fn Array::push<T>(Array<T> *self, T element) {
    while self.size >= self.capacity {
        self.resize((self.capacity || 1) * 2);
    }

    self.elements[self.size] = element;
    self.size += 1;
}

// from is inclusive, to is exclusive
//
// example:
// Array::range(0, 4) -> [0, 1, 2, 3]
// Array::range(0, 0) -> []
// Array::range(2, 5) -> [2, 3, 4]
fn Array::range(i32 from, i32 to) -> Array<i32> * {
    Array<i32> *array = Array::with_capacity(to);

    for i32 i = from; i < to; i += 1 {
        array.push(i);
    }

    return array;
}

fn Array::pop<T>(Array<T> *self) -> T {
    if self.is_empty() {
        return (T)nil;
    }

    T last = self.elements[(i32)self.size - 1];
    self.size -= 1;
    self.resize(self.size);

    return last;
}

fn Array::clear<T>(Array<T> *self) {
    self.resize(1);
    self.size = 0;
}

// Creates a new sized array from a null terminated string
fn Array::from_string(string str) -> Array<char> * {
    i32 length = str.len();
    Array<char> *array = Array::with_capacity(length);

    for i32 i = 0; i < length; i += 1 {
        array.push(str[i]);
    }

    return array;
}

// Turn an array into a string in format [1, 2, "xyz", 'y']
fn Array::to_string<T>(Array<T> *self) -> string {
    string res = "[";

    for i32 i = 0; i < self.len(); i += 1 {
        res.extend(string::as_string_wrapped(self.elements[i]));

        if i < self.len() - 1 {
            res.extend(", ");
        }
    }

    res.push(']');
    return res;
}

// Join an array's elements by a separator
fn Array::join<T>(Array<T> *self, string separator) {
    string res = "";

    for i32 i = 0; i < self.len(); i += 1 {
        res.extend(string::as_string(self.elements[i]));

        if i < self.len() - 1 {
            res.extend(separator);
        }
    }

    return res;
}

fn Array::contains<T>(Array<T> *self, T element) {
    for i32 i = 0; i < self.len(); i += 1 {
        if self.elements[i] == element {
            return true;
        }
    }

    return false;
}

// Alias for Array::join<T>()
fn Array::implode<T>(Array<T> *self, string separator) {
    return self.join(separator);
}

// Take all the elements of another Array<T> and push them into this Array<T>
fn Array::extend<T>(Array<T> *self, Array<T> *other) {
    for i32 i = 0; i < other.len(); i += 1 {
        self.push(other.elements[i]);
    }
}

fn Array::remove<T>(Array<T> *self, i32 index) -> T {
    if index < 0 || index >= self.size {
        return (T)nil;
    }

    T removedElement = self.elements[index];

    for i32 i = index; i < self.size - 1; i += 1 {
        self.elements[i] = self.elements[i + 1];
    }

    self.size -= 1;

    if self.size < self.capacity / 4 {
        self.resize(self.capacity / 2);
    }

    return removedElement;
}

// Print all of the elements of this Array<T>
fn Array::dbg<T>(Array<T> *self) {
    for i32 i = 0; i < self.len(); i += 1 {
        io::dbg(self.elements[i]);
    }
}

fn Array::map<T>(Array<T> *self, fn *mapping) {
    Array<T> *new_self = Array::with_capacity(self.len());

    for i32 i = 0; i < self.len(); i += 1 {
        new_self.push(mapping(self.elements[i]));
    }

    return new_self;
}

fn Array::free<T>(Array<T> *self) {
    free(self.elements);
    free((void *)(*self));
    free(self);
}
