use zed_extension_api::{self as zed, Result};

struct ElleExtension;

impl zed::Extension for ElleExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let elle_lsp_cmd = worktree.which("ellec");
        let path = elle_lsp_cmd.ok_or_else(|| "ellec must be in your path".to_string())?;

        Ok(zed::Command {
            command: path,
            args: vec!["--lsp".into()],
            env: Default::default(),
        })
    }
}

zed::register_extension!(ElleExtension);
