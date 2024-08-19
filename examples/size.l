use std/io;

fn other(i32 *buf) {
    io::printf("(fn other)\n\t#size(buf) = %d\n", #size(buf));
}

fn main() {
    i32 buf[100];
    buf[0] = 123;

    io::printf(
        "(fn main)\n\t#size(buf) = %d\n\t#arrlen(buf) = %d\n",
        #size(buf),
        #arrlen(buf)
    );

    other(buf);
}
