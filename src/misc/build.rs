use std::{fs, path::Path, process::Command};

use crate::{
    misc::{colors::*, constants::*},
    os_arch_to_qbe_target, EmitKind,
};

pub fn build(
    qbe_path: &str,
    path_to_qbe_dist: &str,
    mut output_path: String,
    emit_asm: bool,
    object_output: bool,
    linker_flags: &[Option<String>],
    linker_path: String,
    object_files: &[String],
    no_std: bool,
) -> EmitKind {
    let path = Path::new(&path_to_qbe_dist).with_extension("s");
    let path_string = path.to_str().unwrap().to_string();

    let result = Command::new(qbe_path)
        .args([
            "-t",
            os_arch_to_qbe_target!(),
            "-o",
            &path_string,
            path_to_qbe_dist,
        ])
        .output()
        .unwrap_or_else(|err| panic!("{}Failed to execute QBE: {err}{}", get_RED!(), get_RESET!()));

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
        fs::rename(&path_string, Path::new(&output_path).with_extension("s")).unwrap_or_else(
            |err| {
                panic!(
                    "{}Failed to rename {path_string} to {output_path}: {err}{}",
                    get_RED!(),
                    get_RESET!()
                )
            },
        );

        return EmitKind::AsmFile(output_path);
    }

    if object_output {
        output_path = Path::new(&output_path)
            .with_extension("o")
            .to_str()
            .unwrap()
            .to_string();
    }

    let mut args = vec!["-o", &output_path, &path_string];

    if !object_files.is_empty() {
        for file in object_files {
            args.push(file.as_str());
        }
    }

    if object_output {
        args.push("-c");
    }

    let lib_lookup = format!("-L{}", get_RUNTIME_PATH!());
    if !no_std && !object_output {
        // explicitly look for the runtime at this path in case
        // the user doesnt have rpath set or similar
        args.push(&lib_lookup);
        args.push("-lelle"); // must be prebuilt
        args.push("-lm");
    }

    args.extend(
        linker_flags
            .iter()
            .filter(|x| x.is_some())
            .map(|x| x.as_ref().unwrap().as_str()),
    );

    let result = Command::new(linker_path)
        .args(args)
        .output()
        .unwrap_or_else(|err| {
            panic!(
                "{}Failed to execute CC for {path_string}: {err}{}",
                get_RED!(),
                get_RESET!()
            )
        });

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
