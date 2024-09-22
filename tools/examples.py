from subprocess import run, Popen, PIPE
from os import listdir, remove, kill
from os.path import isfile, join
from time import sleep
from select import select
from sys import stdout, stdin

TIMEOUT = 2 # seconds
EXAMPLES_PATH = "./examples"

def read_from_process(process):
    """Reads from both stdout and stderr until both streams are empty."""
    reads = [process.stdout, process.stderr]

    while True:
        readable, _, _ = select(reads, [], [], 0.1)

        if not readable:
            if process.poll() is not None:
                break

        for stream in readable:
            output = stream.read(1024)
            if output:
                stdout.write(output)
                stdout.flush()

    for stream in reads:
        while True:
            output = stream.read(1024)
            if not output:
                break
            stdout.write(output)
            stdout.flush()

def examples():
    examples = sorted([f for f in listdir(EXAMPLES_PATH) if isfile(join(EXAMPLES_PATH, f))])

    for example in examples:
        run(["clear"])
        code = run(["ellec", "-Dtime", "-Clink-flags", "-lraylib", f"./examples/{example}"]).returncode;

        if code == 0:
            exec_path = f"./{example.replace(".l", "")}";

            elapsed = 0
            process = Popen(
                [exec_path, "some argv", "to ensure", "that it", "works!"],
                stdin=PIPE, stdout=PIPE, stderr=PIPE, text=True
            )

            try:
                while process.poll() is None:
                    read_from_process(process)

                    if stdin in select([stdin], [], [], 0.1)[0] and process.stdin:
                        user_input = input()
                        process.stdin.write(user_input + '\n')
                        process.stdin.flush()

                    sleep(0.01)
            except KeyboardInterrupt:
                process.terminate()

            remove(exec_path);
        else:
            print(f"Failed with a non-zero status code \"{code}\"");

            try:
                input()
            except KeyboardInterrupt:
                pass


# Intended to be run from root dir
# `python3 tools/examples.py`
if __name__ == "__main__":
    examples()
