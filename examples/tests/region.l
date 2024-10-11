use std/collections/array;
use std/io;

struct Object {
    void *data;
    i32 size;
};

fn Object::new(i32 size) {
    Object *obj = malloc(#size(Object));
    *obj = malloc(#size(Object));

    obj.data = malloc(size);
    obj.size = size;

    return obj;
}

fn Object::free(Object *self) {
    free(self.data);
    free((void *)(*self));
    free(self);
}

struct Region {
    Array<Object *> *heap;
};

fn Region::new() {
    Region *region = malloc(#size(Region));
    *region = malloc(#size(Region));
    region.heap = Array::new();

    return region;
}

fn Region::free(Region *self) {
    self.heap.free();
    free((void *)(*self));
    free(self);
}

fn Region::alloc(Region *self, i32 size) {
    Object *obj = Object::new(size);
    self.heap.push(obj);
    return obj.data;
}

fn Region::clean(Region *self) {
    while self.heap.len() {
        self.heap.pop().free();
    }
}

fn main() {
    Region *region = Region::new();
    defer region.free();

    i32 *num = region.alloc(#size(i32));
    *num = 42;

    i32 *arr = region.alloc(10 * #size(i32));
    arr[0] = 5;

    io::dbg(*num, arr[0], region.heap.len());
    region.clean();

    // Everything will be garbage data because it was freed
    io::dbg(*num, arr[0], region.heap.len());
}
