use crate::{
    add_all_builtin_functions, add_all_keywords, add_all_type_casts, add_function, add_keyword,
    add_type_cast, file_manager::FileManager,
};
use std::{path::PathBuf, sync::RwLock};
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer};

pub struct CranberryLsp {
    client: Client,
    pub file_manager: RwLock<FileManager>,
    pub basic_completions: Vec<CompletionItem>,
}

impl CranberryLsp {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            file_manager: RwLock::new(FileManager::new()),
            basic_completions: {
                let keywords = add_all_keywords!();
                let builtins = add_all_builtin_functions!();
                let type_casts = add_all_type_casts!();
                let extra = vec![CompletionItem {
                    label: "self".to_string(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some("class object".to_string()),
                    ..Default::default()
                }];

                [keywords, builtins, type_casts, extra].concat()
            },
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for CranberryLsp {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "Initialized cranberry-lsp")
            .await;

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
						open_close: Some(false),
                        change: Some(TextDocumentSyncKind::FULL),
                        will_save: Some(false),
                        will_save_wait_until: Some(false),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "cranberry-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "[cranberry-lsp] File Opened: {}",
                    PathBuf::from(params.text_document.uri.path())
                        .to_str()
                        .unwrap()
                ),
            )
            .await;

        let mut file_manager = match self.file_manager.write() {
            Ok(fm) => fm,
            Err(poisoned) => poisoned.into_inner(),
        };
        let doc = params.text_document;

        file_manager.open_file(doc.uri, doc.text);
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "[cranberry-lsp] File Closed: {}",
                    PathBuf::from(params.text_document.uri.path())
                        .to_str()
                        .unwrap()
                ),
            )
            .await;

        let mut file_manager = match self.file_manager.write() {
            Ok(fm) => fm,
            Err(poisoned) => poisoned.into_inner(),
        };
        let doc = params.text_document;

        file_manager.close_file(doc.uri);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "[cranberry-lsp] File Saved: {}",
                    PathBuf::from(params.text_document.uri.path())
                        .to_str()
                        .unwrap()
                ),
            )
            .await;

        let mut file_manager = match self.file_manager.write() {
            Ok(fm) => fm,
            Err(poisoned) => poisoned.into_inner(),
        };
        let doc = params.text_document;

        if let Some(file) = file_manager.get_file_mut(&doc.uri) {
            if let Some(new_content) = params.text {
                file.replace_all(new_content);
            }
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "[cranberry-lsp] File Changed: {}",
                    PathBuf::from(params.text_document.uri.path())
                        .to_str()
                        .unwrap()
                ),
            )
            .await;

        let content_changes = params.content_changes;
        let doc = params.text_document;

        self.client
            .log_message(MessageType::INFO, content_changes[0].text.clone())
            .await;

        let mut file_manager = match self.file_manager.write() {
            Ok(fm) => fm,
            Err(poisoned) => poisoned.into_inner(),
        };

        for change in content_changes {
            if let Some(file) = file_manager.get_file_mut(&doc.uri) {
                match change.range {
                    Some(range) => file.edit_file(change.text, range),
                    None => file.replace_all(change.text),
                }
            }
        }
    }

    // Handle completion requests
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut file_manager = match self.file_manager.write() {
            Ok(fm) => fm,
            Err(poisoned) => poisoned.into_inner(),
        };

        let doc = params.text_document_position.text_document;
        if let Some(file) = file_manager.get_file_mut(&doc.uri) {
            return Ok(Some(CompletionResponse::Array(
                [self.basic_completions.clone(), file.completion()].concat(),
            )));
        }

        Ok(Some(CompletionResponse::Array(
            self.basic_completions.clone(),
        )))
    }
}
