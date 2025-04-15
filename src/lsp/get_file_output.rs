use anyhow::Result;
use std::{env::set_current_dir, path::Path};
use tokio::process::Command;

use crate::misc::constants::SHORT_EXTENSION;

pub async fn get_file_output(path: &Path) -> Result<String> {
    if path
        .extension()
        .is_none_or(|ext| format!(".{}", ext.display()) != SHORT_EXTENSION)
    {
        return Err(anyhow::anyhow!("File must have the '.le' extension").into());
    }

    set_current_dir(path.parent().unwrap())
        .unwrap_or_else(|err| panic!("Failed to set the current dir: {}", err));

    let output = Command::new("ellec")
        .arg("-c")
        .arg("-x")
        .arg("-Wall")
        .arg("--noclr")
        .arg(path)
        .stderr(std::process::Stdio::piped())
        .output()
        .await?;

    dbg!(&output);
    Ok(String::from_utf8_lossy(&output.stderr).to_string())
}
