use std::path::PathBuf;

use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

pub fn get_diagnostics(primary_file: &PathBuf, output: &str) -> Vec<Diagnostic> {
    output
        .split("\n\n")
        .filter_map(|x| get_diagnostic(primary_file, x))
        .collect::<Vec<Diagnostic>>()
}

fn get_location(input: &str) -> Option<(String, u32, u32)> {
    let location_parts = input.split(":").collect::<Vec<&str>>();

    if location_parts.len() != 3 {
        eprintln!("Found invalid location {input}");
        return None;
    }

    let file = location_parts[0];
    let row = location_parts[1]
        .parse::<u32>()
        .unwrap_or(1)
        .saturating_sub(1);
    let col = location_parts[2]
        .parse::<u32>()
        .unwrap_or(1)
        .saturating_sub(1);

    Some((file.to_string(), row, col))
}

pub fn get_diagnostic(primary_file: &PathBuf, output: &str) -> Option<Diagnostic> {
    let lines = output.splitn(6, "\n").collect::<Vec<&str>>();

    if lines.len() != 6 {
        eprintln!("Found invalid diagnostic {output}");
        return None;
    }

    let severity = match lines[0] {
        "error" => Some(DiagnosticSeverity::ERROR),
        "warning" => Some(DiagnosticSeverity::WARNING),
        _ => {
            eprintln!("Invalid severity");
            None
        }
    };

    let start_location = get_location(lines[1])?;
    let end_location = get_location(lines[2])?;
    let alt_start_location = get_location(lines[3])?;
    let alt_end_location = get_location(lines[4])?;
    let message = lines[5];

    let range = if start_location.0 != end_location.0
        || primary_file != &PathBuf::from(start_location.0).with_extension("le")
    {
        Range {
            start: Position {
                line: alt_start_location.1,
                character: alt_start_location.2,
            },
            end: Position {
                line: alt_end_location.1,
                character: alt_end_location.2,
            },
        }
    } else {
        Range {
            start: Position {
                line: start_location.1,
                character: start_location.2,
            },
            end: Position {
                line: end_location.1,
                character: end_location.2,
            },
        }
    };

    Some(Diagnostic {
        range,
        message: message.trim().replace("\n", "\n\n").to_string(),
        severity,
        ..Default::default()
    })
}
