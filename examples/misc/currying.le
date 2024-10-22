use std/collections/array;
use std/io;

struct Env {
    i32 a;
};

struct Function {
    Env *env;
    fn *func;
};

fn add(i32 a) {
    Env env = Env { a = a };
    fn *func = fn(Env *env, i32 b) -> env.a + b;

    return Function {
        func = func,
        env = &env
    };
}

fn main() {
    Function res = add(1);
    fn *curried = res.func;
    io::dbg((i32)curried(res.env, 2));
}
