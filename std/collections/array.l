use std/libc/mem;

generic T;
global pub;

struct Array {
    i32 size;
    i32 capacity;
    T *elements;
};

fn Array::len(Array self) {
    return self.size;
}

fn Array::init(Array *self, i32 initialCapacity) -> Array * {
    self.elements = (T *)malloc(initialCapacity * #size(T));
    self.size = 0;
    self.capacity = initialCapacity;

    return self;
}

fn Array::new() -> Array {
    Array *self = malloc(#size(Array *));
    *self = malloc(#size(Array));

    self.init(2);
    return *self;
}

fn Array::resize(Array *self, i32 newCapacity) {
    self.elements = (T *)realloc(self.elements, newCapacity * #size(T));
    self.capacity = newCapacity;
}

fn Array::push(Array *self, T element) {
    if self.size >= self.capacity {
        self.resize(self.capacity * 2);
    }

    self.elements[self.size] = element;
    self.size += 1;
}

fn Array::free(Array *self) {
    free(self.elements);
    self.elements = nil;
    self.size = 0;
    self.capacity = 0;
}
