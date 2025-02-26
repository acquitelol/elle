use crate::misc::colors::*;

const DIVIDER_SIZE: usize = 50;

pub fn print_help(program: String) {
    println!("{}Welcome to the Elle compiler! (˶ᵔ ᵕ ᵔ˶){}", get_GREEN!(), get_RESET!());
    println!("{}", "―".repeat(DIVIDER_SIZE));
    println!("{}Usage: {program} [..options] <entry.le> [<file1.o>, <file2.o>...]{}", get_GREEN!(), get_RESET!());

    let help_message_options = vec![
        (
            "Options",
            vec![
                ("-h, --help", "Displays this help message"),
                (
                    "-o <output_path>",
                    "Emits the final result in <output_path>",
                ),
                (
                    "--hush, --silent",
                    "No longer tells you if a target was compiled successfully",
                ),
            ],
        ),
        (
            "Debug flags",
            vec![
                (
                    "-t, --time, --elapsed-time",
                    "Displays how long each compilation step takes",
                ),
                (
                    "--ssa, --emit-qbe, --emit-ssa",
                    "Emits the QBE IR file in the form of .ssa instead of an executable",
                ),
                (
                    "--asm, --emit-asm, --emit-s",
                    "Emits the Assembly file in the form of .s instead of an executable",
                ),
                (
                    "--ast, --emit-ast, --emit-tree",
                    "Prints the AST representation of the program to standard output",
                ),
            ],
        ),
        (
            "Warning Flags",
            vec![
                ("-Wall", "Enables all of the warnings the compiler provides"),
                (
                    "-Wstruct-fields-missing",
                    "Initialization a stack-allocated struct without all of its fields",
                ),
                (
                    "-Winvalid-alias",
                    "Attempting to set an alias attribute on a non-external function",
                ),
                (
                    "-Wvariadic-no-meta",
                    "Creating a variadic function without ElleMeta as the 0th argument",
                ),
                (
                    "-Wc-style-void",
                    "Creating a function with no arguments like 'fn foo(void) {}'",
                ),
                (
                    "-Wallocator-methods-missing",
                    "Setting an allocator which is missing some of the possible methods"
                )
            ],
        ),
        (
            "Utility Flags",
            vec![
                (
                    "--nosm, --no-string-module",
                    "Doesn't import the string module by default. May break things.",
                ),
                (
                    "--noalloc, --no-allocation",
                    "Disables Elle allocation. You won't be able to use most std methods.",
                ),
                (
                    "--nogc, --no-garbage-collector",
                    "Doesn't use garbage collection, switches default allocator to arenas",
                ),
                (
                    "--nofmt, --no-primitive-formatters",
                    "Doesn't import the primitive formatter module.",
                ),
                (
                    "--nostd, --no-stdlib",
                    "Doesn't link with the Elle runtime library during compilation.",
                ),
                (
                    "--noclr, --no-ansi",
                    "Disables ANSI color output (alternative to NO_COLOR=1)"
                )
            ]
        ),
        (
            "Compilation Flags",
            vec![
                (
                    "-c, --compile-only",
                    "Compiles but does not link anything. Produces an object file.",
                ),
                (
                    "-z, --link-flag <flag>",
                    "Allows you to pass the flag specified to the linker",
                ),
                (
                    "-Z, --link-path <path>",
                    "Allows you to pass a custom linker path, the default is \"cc\"",
                ),
                (
                    "-Q, --qbe-path <path>",
                    "Allows you to pass a custom QBE path, the default is \"qbe\"",
                ),
                (
                    "-S, --std-path <path>",
                    "Pass a custom std path, the default is \"/usr/local/include/elle\"",
                ),
                (
                    "-R, --runtime-path <path>",
                    "Pass a custom runtime path, the default is \"/usr/local/lib\"",
                ),
            ],
        ),
        (
            "Environment Variables",
            vec![
                (
                    "NO_COLOR",
                    "Disables colored output, see https://no-color.org/ (same as --noclr)"
                )
            ]
        )
    ];

    print_options(help_message_options);
}

fn print_options(options: Vec<(impl Into<String>, Vec<(&str, &str)>)>) {
    let max_option_length = options
        .iter()
        .map(|(_, options)| options.iter().map(|(opt, _)| opt.len()).max().unwrap_or(0))
        .max()
        .unwrap_or(0);

    for (title, options) in options {
        let formatted_title = format!("{}: ", title.into());
        println!(
            "\n{formatted_title}{}",
            "―".repeat(DIVIDER_SIZE - formatted_title.len())
        );

        for (option, description) in options {
            let spaces = " ".repeat(4 + max_option_length - option.len());
            println!(
                "{}    {opt}{}{spaces}{desc}",
                get_GREEN!(),
                get_RESET!(),
                opt = option,
                spaces = spaces,
                desc = description
            );
        }
    }
}
