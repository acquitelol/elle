from subprocess import run
from time import time
from os import remove

lines = []
SHORT_EXTENSION = ".le"

def repl():
    while True:
        line = input("⋆.ೃ࿔* -> ");

        if line.endswith(";"):
            line = line[:-1]

        code = "\n".join(list(map(lambda x: x.replace(" " * 12, ""),
            f"""use std/io;
            use std/math;
            use std/cast;
            use std/vectors;
            use std/types;
            use std/split;
            use std/collections/array;

            fn main() {{
                {f";\n{" " * 16}".join(lines)}{";" if len(lines) > 0 else ""}
                io::println({line});
            }}"""
        .split("\n"))));

        if line == "<!dbg>":
            print(code)
            continue

        path = f".repl-{int(time())}{SHORT_EXTENSION}";
        exec_path = f"./{path.replace(SHORT_EXTENSION, "")}";

        with open(path, "w") as fp:
            fp.write(code);

        code = run(["ellec", "--hush", path]).returncode;
        remove(path);

        if code == 0:
            res = run([exec_path])
            remove(exec_path);

            if res.returncode == 0:
                lines.append(line);

if __name__ == "__main__":
    repl()
