use std::{fs, path::Path, process::Command};

use crate::{
    misc::colors::{RED, RESET},
    EmitKind,
};

pub fn build(
    qbe_path: String,
    path_to_qbe_dist: String,
    mut output_path: String,
    emit_asm: bool,
    object_output: bool,
    linker_flags: Vec<Option<String>>,
    linker_path: String,
    object_files: Vec<String>,
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
            "{RED}Failed to rename {path_string} to {output_path}{RESET}"
        ));

        return EmitKind::AsmFile(output_path);
    }

    let mut args = linker_flags
        .iter()
        .filter(|x| x.is_some())
        .map(|x| x.as_ref().unwrap().as_str())
        .collect::<Vec<&str>>();

    let objects = object_files.join(" ");

    if object_output {
        output_path = Path::new(&output_path)
            .with_extension("o")
            .to_str()
            .unwrap()
            .to_string();
    }
    args.extend(vec!["-o", &output_path, &path_string, &objects]);

    if object_output {
        args.push("-c");
    }

    // let frame_addr_path = format!("{STD_LIB_PATH}/runtime/stack_top.s");
    // let frame_addr_out = Path::new(&output_path)
    //     .with_file_name("stack_top")
    //     .with_extension("o")
    //     .to_str()
    //     .unwrap()
    //     .to_string();

    // let _ = Command::new(linker_path.clone())
    //     .args(vec![&frame_addr_path, "-o", &frame_addr_out, "-c"])
    //     .output()
    //     .expect(&format!(
    //         "{RED}Failed to execute CC for {frame_addr_out}.{RESET}"
    //     ));

    // args.push(&frame_addr_out);

    let result = Command::new(linker_path)
        .args(args)
        .output()
        .expect(&format!(
            "{RED}Failed to execute CC for {path_string}.{RESET}"
        ));

    if !result.stderr.is_empty() {
        println!(
            "{RED}ERROR: {}{RESET}",
            String::from_utf8(result.stderr).unwrap()
        );

        return EmitKind::None;
    }

    if object_output {
        EmitKind::Object(output_path)
    } else {
        EmitKind::Executable(output_path)
    }
}
