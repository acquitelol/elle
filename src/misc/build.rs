use std::{fs, path::Path, process::Command};

use crate::{
    misc::{colors::*, constants::*}, EmitKind
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
    no_std: bool,
) -> EmitKind {
    let path = Path::new(&path_to_qbe_dist).with_extension("s");
    let path_string = path.to_str().unwrap().to_string();

    let result = Command::new(qbe_path)
        .args(["-o", &path_string, &path_to_qbe_dist])
        .output()
        .expect(&format!("{}Failed to execute QBE.", get_RED!()));

    if !result.stderr.is_empty() {
        eprintln!(
            "{}ERROR: {}{}",
            get_RED!(),
            String::from_utf8(result.stderr).unwrap(),
            get_RESET!()
        );

        return EmitKind::None;
    }

    if emit_asm {
        fs::rename(
            path_string.clone(),
            Path::new(&output_path).with_extension("s"),
        )
        .expect(&format!(
            "{}Failed to rename {path_string} to {output_path}{}",
            get_RED!(),
            get_RESET!()
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

    let lib_lookup = format!("-L{}", get_RUNTIME_PATH!());
    if !no_std && !object_output {
        // explicitly look for the runtime at this path in case
        // the user doesnt have rpath set or similar
        args.push(&lib_lookup);
        args.push("-lelle"); // must be prebuilt
    }

    let result = Command::new(linker_path)
        .args(args)
        .output()
        .expect(&format!(
            "{}Failed to execute CC for {path_string}.{}",
            get_RED!(),
            get_RESET!()
        ));

    if !result.stderr.is_empty() {
        eprintln!(
            "{}ERROR: {}{}",
            get_RED!(),
            String::from_utf8(result.stderr).unwrap(),
            get_RESET!()
        );

        return EmitKind::None;
    }

    if object_output {
        EmitKind::Object(output_path)
    } else {
        EmitKind::Executable(output_path)
    }
}
