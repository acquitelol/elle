use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

pub fn get_diagnostics(output: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let lines = output.splitn(4, "\n").collect::<Vec<&str>>();

    if lines.len() != 4 {
        eprintln!("Found invalid diagnostic {output}");
        return diagnostics;
    }

    let severity = match lines[0] {
        "error" => Some(DiagnosticSeverity::ERROR),
        "warning" => Some(DiagnosticSeverity::WARNING),
        _ => {
            eprintln!("Invalid severity");
            None
        }
    };

    let location = lines[1];
    let length = lines[2].parse::<u32>().unwrap_or_else(|_| {
        eprintln!("Invalid location length");
        0
    });

    let message = lines[3];

    let location_parts = location.split(":").collect::<Vec<&str>>();

    if location_parts.len() != 3 {
        eprintln!("Found invalid location {location}");
        return diagnostics;
    }

    let _ = location_parts[0];
    let row = location_parts[1]
        .parse::<u32>()
        .unwrap_or(1)
        .saturating_sub(1);
    let mut col = location_parts[2]
        .parse::<u32>()
        .unwrap_or(1)
        .saturating_sub(1);

    dbg!(message.len(), message.trim().len());
    col -= (message.len() - message.trim().len()) as u32;

    diagnostics.push(Diagnostic {
        range: Range {
            start: Position {
                line: row,
                character: col - 2,
            },
            end: Position {
                line: row,
                character: col - 2 + length,
            },
        },
        message: message.trim().to_string(),
        severity,
        ..Default::default()
    });

    diagnostics
}
