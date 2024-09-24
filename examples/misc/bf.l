use std/io;

const i64 MEMORY_SIZE = 30_000;

fn skip(string *pc, i32 dir) -> string {
    i32 loop = 1;

    while loop > 0 {
        *pc += (string)dir;

        if *(*pc) == '[' {
            loop += dir;
        }

        if *(*pc) == ']' {
            loop -= dir;
        }
    }
}

fn interpret(string program) {
    char memory[MEMORY_SIZE];
    memset(memory, 0, MEMORY_SIZE * #size(char));

    string ptr = memory;
    string pc = program;

    while *pc {
        if *pc == '>' {
            ptr += (string)1;
        }

        if *pc == '<' {
            ptr -= (string)1;
        }

        if *pc == '+' {
            *ptr += 1;
        }

        if *pc == '-' {
            *ptr -= 1;
        }

        if *pc == '.' {
            io::print(*ptr);
        }

        if *pc == ',' {
            *ptr = io::getchar();
        }

        if *pc == '[' {
            if !*ptr {
                skip(&pc, 1);
            }
        }

        if *pc == ']' {
            if *ptr {
                skip(&pc, -1);
            }
        }

        pc += (string)1;
    }
}

fn main() {
    interpret(
        "--<-<<+[+[<+>--->->->-<<<]>]<<--.<++++++.<<-..<<.<+.>>.>>.<<<.+++.>>.>>-.<<<+."
    );

    io::print('\n');
}
