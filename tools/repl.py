from subprocess import run
from time import time
from os import remove

lines = []

def repl():
    while True:
        line = input("⋆.ೃ࿔* -> ");

        code = f"""
        use std/io;
        use std/cast;
        use std/math;
        use std/vectors;

        fn main(){{
            {";".join(lines)}{";" if len(lines) > 0 else ""}
            io::println({line});
        }}""";

        lines.append(line);
        path = f".repl-{int(time())}.l";
        exec_path = f"./{path.replace(".l", "")}";

        with open(path, "w") as fp:
            fp.write(code);

        code = run(["ellec", "--hush", path]).returncode;
        remove(path);

        if code == 0:
            run([exec_path])
            remove(exec_path);

if __name__ == "__main__":
    repl()
