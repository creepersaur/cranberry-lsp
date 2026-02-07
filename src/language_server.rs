use crate::{
    add_all_builtin_functions, add_all_keywords, add_all_type_casts, add_function, add_keyword,
    add_type_cast, file_manager::FileManager, logger::LspLogger,
};
use std::{collections::HashSet, path::PathBuf};
use tokio::sync::RwLock;
use tower_lsp::{Client, LanguageServer, jsonrpc::Result, lsp_types::*};

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
                let extra = vec![];

                [keywords, builtins, type_casts, extra].concat()
            },
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for CranberryLsp {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "Initialized cranberry-lsp")
            .await;

        log::set_boxed_logger(Box::new(LspLogger {
            client: self.client.clone(),
        }))
        .unwrap();
        log::set_max_level(log::LevelFilter::Info);

        let root_uri = if let Some(folders) = params.workspace_folders.as_ref() {
            folders.first().map(|f| f.uri.clone())
        } else {
            params.root_uri.clone()
        };

        if let Some(uri) = root_uri {
            let path = uri.to_file_path().unwrap();
            let mut file_manager = self.file_manager.write().await;
            file_manager.set_root(path);
            file_manager.scan_src();
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                workspace: Some(WorkspaceServerCapabilities {
                    file_operations: Some(WorkspaceFileOperationsServerCapabilities {
                        did_create: Some(FileOperationRegistrationOptions {
                            filters: vec![FileOperationFilter {
                                scheme: Some("file".to_string()),
                                pattern: FileOperationPattern {
                                    glob: "**/*.cb".to_string(),
                                    matches: None,
                                    options: None,
                                },
                            }],
                        }),

                        // did_change: Some(FileOperationRegistrationOptions {
                        //     filters: vec![FileOperationFilter {
                        //         scheme: Some("file".to_string()),
                        //         pattern: FileOperationPattern {
                        //             glob: "**/*.cb".to_string(),
                        //             matches: None,
                        //             options: None,
                        //         },
                        //     }],
                        // }),
                        did_delete: Some(FileOperationRegistrationOptions {
                            filters: vec![FileOperationFilter {
                                scheme: Some("file".to_string()),
                                pattern: FileOperationPattern {
                                    glob: "**/*.cb".to_string(),
                                    matches: None,
                                    options: None,
                                },
                            }],
                        }),
                        ..Default::default()
                    }),
                    ..Default::default()
                }),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        will_save: Some(false),
                        will_save_wait_until: Some(false),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    resolve_provider: Some(true),
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

    // workspace: Some(WorkspaceClientCapabilities {
    //             did_change_watched_files: Some(DidChangeWatchedFilesClientCapabilities {
    //                 dynamic_registration: Some(false),
    //                 ..Default::default()
    //             }),
    //         }),

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

        let mut file_manager = self.file_manager.write().await;
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

        let mut file_manager = self.file_manager.write().await;
        let doc = params.text_document;

        file_manager.close_file(doc.uri);
    }

    async fn did_delete_files(&self, params: DeleteFilesParams) {
        for file in params.files {
            let mut file_manager = self.file_manager.write().await;

            self.client
                .log_message(
                    MessageType::INFO,
                    format!(
                        "[cranberry-lsp] File Deleted: {}",
                        PathBuf::from(&file.uri).to_str().unwrap()
                    ),
                )
                .await;

            file_manager.delete_file(Url::from_file_path(file.uri).unwrap());
        }
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

        let mut file_manager = self.file_manager.write().await;
        let doc = params.text_document;

        if let Some(file) = file_manager.get_file_mut(&doc.uri) {
            if let Some(new_content) = params.text {
                file.replace_all(new_content);
            }
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let content_changes = params.content_changes;
        let doc = params.text_document;

        let mut file_manager = self.file_manager.write().await;

        for change in content_changes {
            if let Some(file) = file_manager.get_file_mut(&doc.uri) {
                match change.range {
                    Some(range) => file.edit_file(&change.text, Some(range)),
                    None => file.replace_all(change.text),
                }
            }
        }
    }

    // Handle completion requests
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut file_manager = self.file_manager.write().await;

        let doc = params.text_document_position.text_document;
        let position = params.text_document_position.position;

        let mut other_file_completions = vec![];
        let mut project_symbols = vec![];

        for file in file_manager.get_files() {
            file.model.clear();

            if file.uri != doc.uri {
				file.model.build_model(&file.tree, &file.source_code);
                for x in file.completion(&Position::new(0, 0)) {
                    other_file_completions.push(x);
                    project_symbols.append(&mut file.model.global_scope.symbols);
                }
            }
        }

        if let Some(file) = file_manager.get_file_mut(&doc.uri) {
            file.model.global_scope.symbols.append(&mut project_symbols);
			file.model.build_model(&file.tree, &file.source_code);

            let completions = if let Some(context) = params.context {
                if context.trigger_kind == CompletionTriggerKind::TRIGGER_CHARACTER
                    && (context.trigger_character.as_deref() == Some(".")
                        || context.trigger_character.as_deref() == Some(":"))
                {
                    // Extract receiver and get member completions
                    let object = if context.trigger_character.as_deref() == Some(":") {
                        file.get_static_member_object(&file.source_code, &position)
                    } else {
                        file.get_member_object(&file.source_code, &position)
                    };
                    let members = file.member_access(&object);
                    let obj_type = file.get_object_type(&object, &position);

                    if let Some(obj) = obj_type {
                        [members, file.member_access(&obj)].concat()
                    } else {
                        members
                    }
                } else {
                    let mut seen = HashSet::new();
                    let filtered = [file.completion(&position), other_file_completions]
                        .concat()
                        .into_iter()
                        .filter(|comp| seen.insert(comp.label.clone()))
                        .collect();

                    // Fallback to basic or file completions
                    [filtered, self.basic_completions.clone()].concat()
                }
            } else {
                let mut seen = HashSet::new();
                let filtered = [file.completion(&position), other_file_completions]
                    .concat()
                    .into_iter()
                    .filter(|comp| seen.insert(comp.label.clone()))
                    .collect();

                // No context: general completion
                [filtered, self.basic_completions.clone()].concat()
            };

            let mut seen = HashSet::new();
            let filtered = completions
                .into_iter()
                .filter(|comp| seen.insert(comp.label.clone()))
                .collect();

            return Ok(Some(CompletionResponse::Array(filtered)));
        }

        let mut seen = HashSet::new();
        let filtered = other_file_completions
            .into_iter()
            .filter(|comp| seen.insert(comp.label.clone()))
            .collect();

        Ok(Some(CompletionResponse::Array(
            [filtered, self.basic_completions.clone()].concat(),
        )))
    }

    async fn completion_resolve(&self, mut completion: CompletionItem) -> Result<CompletionItem> {
        // log::info!("completion_resolve CALLED");
        if let Some(data) = &completion.data {
            if let Some(doc) = data.get("doc") {
                completion.documentation = Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc.as_str().unwrap().to_string(),
                }));
            }
        }

        Ok(completion)
    }
}
