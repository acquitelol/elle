use tower_lsp::lsp_types::{Hover, HoverContents, MarkedString, Position, Range};

use super::get_location::get_location;

pub fn get_hover_info(output: &str) -> Option<Hover> {
    let parts = output
        .split("\n")
        .filter(|x| !x.is_empty())
        .collect::<Vec<&str>>();

    if parts.len() != 4 {
        eprintln!("Found invalid hover info");
        dbg!(&parts, &output);
        return None;
    }

    let _ = parts[0];
    let start_location = get_location(parts[1])?;
    let end_location = get_location(parts[2])?;
    let res = parts[3];

    Some(Hover {
        contents: HoverContents::Scalar(MarkedString::String(format!("```elle\n{res}\n```"))),
        range: Some(Range {
            start: Position {
                line: start_location.1,
                character: start_location.2,
            },
            end: Position {
                line: end_location.1,
                character: end_location.2,
            },
        }),
    })
}
