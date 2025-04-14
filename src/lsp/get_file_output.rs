use anyhow::Result;
use std::path::Path;
use tokio::process::Command;

use crate::misc::constants::SHORT_EXTENSION;

pub async fn get_file_output(path: &Path) -> Result<String> {
    if path
        .extension()
        .is_none_or(|ext| format!(".{}", ext.display()) != SHORT_EXTENSION)
    {
        return Err(anyhow::anyhow!("File must have the '.le' extension").into());
    }

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
