use crate::misc::colors::*;

const DIVIDER_SIZE: usize = 50;

pub fn print_help(program: String) {
    println!("{GREEN}Welcome to the Elle compiler! (˶ᵔ ᵕ ᵔ˶){RESET}");
    println!("{}", "―".repeat(DIVIDER_SIZE));
    println!("{GREEN}Usage: {program} [..options] <file>{RESET}");

    let help_message_options = vec![
        (
            "Options",
            vec![
                ("-h, --help", "Displays this help message"),
                (
                    "-o <output_path>",
                    "Emits the final result in <output_path>",
                ),
            ],
        ),
        (
            "Debug flags",
            vec![
                (
                    "--elapsed-time, -Dtime",
                    "Displays how long each compilation step takes",
                ),
                (
                    "--emit-qbe, -Demit-qbe, -Demit-ssa",
                    "Emits the QBE IR file in the form of .ssa instead of an executable",
                ),
                (
                    "--emit-asm, -Demit-asm, -Demit-s",
                    "Emits the Assembly file in the form of .s instead of an executable",
                ),
            ],
        ),
        (
            "Warning Flags",
            vec![
                ("-Wall", "Enables all of the warnings the compiler provides"),
                (
                    "-Wimplicit-cast",
                    "Warns you when you implicitly cast a variable to another type",
                ),
                (
                    "-Wstruct-fields-missing",
                    "Warns you when you initialize a stack-allocated struct without all of its fields",
                ),
                (
                    "-Winvalid-alias",
                    "Warns you when you attempt to set an alias attribute on a non-external function",
                ),
                (
                    "-Wtoo-many-generics",
                    "Warns you when you import a module with more generic parameters than the module's requirements",
                ),
                (
                    "-Wmissing-generics",
                    "Warns you when you import a module with less generic parameters than the module's requirements",
                ),
            ],
        ),
        (
            "Compilation Flags",
            vec![
                (
                    "--link-flags, -Clink-flags, -Clinker-flags <flags>",
                    "Allows you to pass the flags specified to the linker",
                ),
                (
                    "--link-path, -Clink-path, -Clinker-path <path>",
                    "Allows you to pass a custom linker path, the default is \"cc\"",
                ),
                (
                    "--qbe-path, -Cqbe-path, -Cssa-path <path>",
                    "Allows you to pass a custom QBE installation path, the default is \"qbe\"",
                ),
            ],
        ),
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
                "{GREEN}    {opt}{RESET}{spaces}{desc}",
                opt = option,
                spaces = spaces,
                desc = description
            );
        }
    }
}
