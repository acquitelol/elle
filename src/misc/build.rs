use std::{fs, path::Path, process::Command};

use regex::Regex;

use crate::lexer::colors::{RED, RESET};

pub fn build(
    qbe_path: String,
    path_to_qbe_dist: String,
    output_path: String,
    emit_asm: bool,
    linker_flags: Option<String>,
    linker_path: String,
) -> bool {
    let path = Path::new(&path_to_qbe_dist).with_extension("s");
    let path_string = path.to_str().unwrap().to_string();

    let result = Command::new(qbe_path)
        .args(["-o", &path_string, &path_to_qbe_dist])
        .output()
        .expect(&format!("{RED}Failed to execute QBE."));

    if !result.stderr.is_empty() {
        println!(
            "{RED}ERROR: {}{RESET}",
            String::from_utf8(result.stderr).unwrap()
        );

        return false;
    }

    let mut file =
        fs::read_to_string(path.clone()).expect(&format!("{RED}Failed to read assembly file."));

    // Fix codegen issue for generating floating point numbers
    let floating_point_re = Regex::new(r"Lfp([0-9]+):").unwrap();
    file = floating_point_re.replace_all(&file, "_Lfp$1:").to_string();

    // Fix codegen issue when taking size for a buffer as an argument
    let buf_size_re = Regex::new(r"and\t(.*), #(.*), lsl (.*)").unwrap();
    file = buf_size_re
        .replace_all(&file, "lsl\t$1, $3\n\tand\t$1, #$2")
        .to_string();

    // Fix codegen issue when doing logical operations with strings on lhs or rhs
    let adrp_re = Regex::new(r"adrp\tw([0-9]+), _(.*)").unwrap();
    file = adrp_re.replace_all(&file, "adrp\tx$1, _$2").to_string();

    let add_re = Regex::new(r"add\tw([0-9]+), w([0-9]+), _(.*)").unwrap();
    file = add_re.replace_all(&file, "add x$1, x$2, _$3").to_string();

    fs::write(path, file).expect(&format!("{RED}Failed to write to file."));

    if emit_asm {
        fs::rename(
            path_string.clone(),
            Path::new(&output_path).with_extension("s"),
        )
        .expect(&format!(
            "{RED}Failed to rename {path_string} to {output_path}"
        ));

        return true;
    }

    let result = Command::new(linker_path)
        .args([
            "-o",
            &output_path,
            &path_string,
            &linker_flags.unwrap_or("".into()),
        ])
        .output()
        .expect(&format!("{RED}Failed to execute CC."));

    if !result.stderr.is_empty() {
        println!(
            "{RED}ERROR: {}{RESET}",
            String::from_utf8(result.stderr).unwrap()
        );

        return false;
    }

    return true;
}
