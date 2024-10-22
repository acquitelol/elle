from subprocess import run
from time import time
from os import remove

lines = []
SHORT_EXTENSION = ".le"

def repl():
    while True:
        line = input("⋆.ೃ࿔* -> ");

        code = f"""
        use std/io;
        use std/cast;
        use std/math;
        use std/vectors;

        fn main() {{
            {";".join(lines)}{";" if len(lines) > 0 else ""}
            io::println({line});
        }}""";

        lines.append(line);
        path = f".repl-{int(time())}{SHORT_EXTENSION}";
        exec_path = f"./{path.replace(SHORT_EXTENSION, "")}";

        with open(path, "w") as fp:
            fp.write(code);

        code = run(["ellec", "--hush", path]).returncode;
        remove(path);

        if code == 0:
            run([exec_path])
            remove(exec_path);

if __name__ == "__main__":
    repl()
