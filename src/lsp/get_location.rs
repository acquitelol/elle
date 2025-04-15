pub fn get_location(input: &str) -> Option<(String, u32, u32)> {
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
