use std::{fs, path::Path, process::Command};

use crate::{
    misc::colors::{RED, RESET},
    EmitKind,
};

pub fn build(
    qbe_path: String,
    path_to_qbe_dist: String,
    output_path: String,
    emit_asm: bool,
    linker_flags: Option<String>,
    linker_path: String,
) -> EmitKind {
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

        return EmitKind::None;
    }

    if emit_asm {
        fs::rename(
            path_string.clone(),
            Path::new(&output_path).with_extension("s"),
        )
        .expect(&format!(
            "{RED}Failed to rename {path_string} to {output_path}"
        ));

        return EmitKind::AsmFile(output_path);
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

        return EmitKind::None;
    }

    return EmitKind::Executable(output_path);
}
