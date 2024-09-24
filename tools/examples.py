from subprocess import run, Popen, PIPE
from os import listdir, remove, kill
from os.path import isfile, isdir, join
from time import sleep
from select import select
from sys import stdout, stdin, argv

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

def examples(kind):
    path = f"{EXAMPLES_PATH}/{kind}"
    examples = sorted([f for f in listdir(path) if isfile(join(path, f))])

    for example in examples:
        run(["clear"])
        code = run(["ellec", "-Dtime", "-Clink-flags", "-lraylib", f"./examples/{kind}/{example}"]).returncode;

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
    kinds = sorted([f for f in listdir(EXAMPLES_PATH) if
        isdir(join(EXAMPLES_PATH, f)) and f != "resources"]);

    if len(argv) < 2:
        print(f"No option was provided, exiting...")
        print(f"Possible options include: {', '.join(kinds)}")
        exit(1)

    if argv[1] == "help":
        print(f"To run examples, you should run the following:");
        print(f"\n\t{argv[0]} <option goes here>\n")
        print(f"Possible options include: {', '.join(kinds)}")
        print("You can CTRL+C to skip to the next example")
        print("CTRL+C twice in rapid succession to exit the program")
        exit(0)

    kinds.append("help");

    if not (argv[1] in kinds):
        print(f"Invalid option was provided: {argv[1]}")
        print(f"Possible options include: {', '.join(kinds)}")
        exit(1)

    examples(argv[1])
