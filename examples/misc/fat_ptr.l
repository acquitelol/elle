use std/io;

fn new_array<T>(i32 length) {
    T *array = malloc(#size(i32) + #size(T) * length);
    *array = length;

    return array + #size(i32);
}

fn free_array<T>(T *array) {
    free(array - #size(i32));
}

fn len<T>(T *array) {
    return *(array - #size(i32));
}

fn loop<T>(T *array) {
    for (i32 i = 0; i < len(array); i += 1) {
        io::printf("array[{}] = {}", i, array[i]);
    }
}

fn main() {
    i32 *array = new_array<i32>(4);
    defer free_array(array);

    array[0] = 5;
    array[1] = 14;
    array[2] = 39;
    array[3] = 4;

    loop(array);
}
