use anyhow::Result;
use tower_lsp::{jsonrpc::Error, lsp_types::*, Client, LanguageServer};

use crate::lsp::{get_diagnostics::get_diagnostics, get_file_output::get_file_output};

pub struct Backend {
    client: Client,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client }
    }
}

impl Backend {
    pub async fn try_report_diagnostics(&self, uri: &Url) {
        if let Some(path) = uri.to_file_path().ok() {
            if let Ok(output) = get_file_output(&path).await {
                let diagnostics = get_diagnostics(&path, &output);
                dbg!(&diagnostics);

                self.client
                    .publish_diagnostics(uri.clone(), diagnostics, None)
                    .await;
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult, Error> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = &params.text_document.uri;
        dbg!("File opened", uri);
        self.try_report_diagnostics(uri).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = &params.text_document.uri;
        dbg!("File opened", uri);
        self.try_report_diagnostics(uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = &params.text_document.uri;
        dbg!("File modified", uri);
        self.try_report_diagnostics(uri).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>, Error> {
        dbg!(
            "Detected hover",
            params.text_document_position_params.position
        );

        Ok(None)
        // Ok(Some(Hover {
        //     contents: HoverContents::Scalar(MarkedString::String("hello world".into())),
        //     range: None,
        // }))
    }

    async fn shutdown(&self) -> Result<(), Error> {
        Ok(())
    }
}
