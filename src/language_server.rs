use crate::{
    add_all_builtin_functions, add_all_constants, add_all_keywords, add_all_type_casts,
    add_constant, add_function, add_keyword, add_type_cast, file_manager::FileManager,
    language_model::Symbol, logger::LspLogger,
};
use std::{collections::HashSet, path::PathBuf};
use tokio::sync::RwLock;
use tower_lsp::{
    Client, LanguageServer,
    jsonrpc::Result,
    lsp_types::{
        // request::{GotoDeclarationParams, GotoDeclarationResponse},
        *,
    },
};

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
                let constants = add_all_constants!();
                let extra = vec![];

                [keywords, builtins, type_casts, constants, extra].concat()
            },
        }
    }

    pub fn build_project_and_file(
        file_manager: &mut FileManager,
        url: &Url,
        mut other_file_completions: Option<&mut Vec<CompletionItem>>,
    ) {
        let mut project_symbols = vec![];

        for file in file_manager.get_files() {
            file.model.clear();

            if file.uri != *url {
                file.model.build_model(&file.tree, &file.source_code);
                for x in file.completion(&Position::new(0, 0)) {
                    if let Some(ref mut other_file_completions) = other_file_completions {
                        other_file_completions.push(x);
                    }
                    project_symbols.append(&mut file.model.global_scope.symbols);
                }
            }
        }

        if let Some(file) = file_manager.get_file_mut(&url) {
            file.model.global_scope.symbols.append(&mut project_symbols);
            file.model.build_model(&file.tree, &file.source_code);
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
                definition_provider: Some(OneOf::Left(true)),
                declaration_provider: Some(DeclarationCapability::Simple(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
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

        Self::build_project_and_file(&mut file_manager, &doc.uri, None);
    }

    // Handle completion requests
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut file_manager = self.file_manager.write().await;

        let doc = params.text_document_position.text_document;
        let position = params.text_document_position.position;

        let mut other_file_completions = vec![];

        Self::build_project_and_file(
            &mut file_manager,
            &doc.uri,
            Some(&mut other_file_completions),
        );

        if let Some(file) = file_manager.get_file_mut(&doc.uri) {
            let completions = if let Some(context) = params.context {
                if context.trigger_kind == CompletionTriggerKind::TRIGGER_CHARACTER
                    && (context.trigger_character.as_deref() == Some(".")
                        || context.trigger_character.as_deref() == Some(":"))
                {
                    // Extract receiver and get member completions
                    let object = if context.trigger_character.as_deref() == Some(":") {
                        file.get_static_member_object(&position)
                    } else {
                        file.get_member_object(&position)
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: String::from("Hello"),
            }),
            range: Some(Range {
                start: params.text_document_position_params.position,
                end: Position {
                    line: params.text_document_position_params.position.line,
                    character: params.text_document_position_params.position.character + 5,
                },
            }),
        }))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let mut hints = vec![];

        let url = params.text_document.uri;

        let mut file_manager = self.file_manager.write().await;

        if let Some(file) = file_manager.get_file_mut(&url) {
            for symbol in file.model.global_scope.flatten_symbols() {
                if let Symbol::Variable {
                    optional_type,
                    end_position,
                    type_defined,
                    ..
                } = symbol
                {
                    if let Some(optional_type) = optional_type
                        && !*type_defined
                    {
                        if let Some(end_position) = end_position {
                            hints.push(InlayHint {
                                position: end_position.clone(),
                                label: InlayHintLabel::String(format!(": {optional_type}")),
                                kind: None,
                                text_edits: None,
                                tooltip: Some(InlayHintTooltip::String(
                                    "Inlay tooltip".to_string(),
                                )),
                                padding_left: None,
                                padding_right: None,
                                data: None,
                            });
                        }
                    }
                }
            }
        }

        Ok(Some(hints))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let position = params.text_document_position_params.position;
        let doc = params.text_document_position_params.text_document;

        let mut file_manager = self.file_manager.write().await;
        if let Some(file) = file_manager.get_file_mut(&doc.uri) {
            let hover_word = file.get_word_at_position(&position);
            let word = match hover_word {
                Some(word) => word,
                None => return Ok(None),
            };

            let symbols = file.model.global_scope.flatten_symbols();

            for i in symbols.iter().rev() {
                if let Symbol::Variable {
                    name,
                    file,
                    start_position,
                    end_position,
                    ..
                } = i
                    && name == &word
                {
                    if let Some(end) = end_position
                        && let Some(start) = start_position
                        && let Some(uri) = file
                    {
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri: uri.clone(),
                            range: Range {
                                start: *start,
                                end: *end,
                            },
                        })));
                    }
                }

				if let Symbol::Function {
                    name,
                    file,
                    start_position,
                    end_position,
                    ..
                } = i
                    && name == &word
                {
                    if let Some(end) = end_position
                        && let Some(start) = start_position
                        && let Some(uri) = file
                    {
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri: uri.clone(),
                            range: Range {
                                start: *start,
                                end: *end,
                            },
                        })));
                    }
                }

				if let Symbol::Class {
                    name,
                    file,
                    start_position,
                    end_position,
                    ..
                } = i
                    && name == &word
                {
                    if let Some(end) = end_position
                        && let Some(start) = start_position
                        && let Some(uri) = file
                    {
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri: uri.clone(),
                            range: Range {
                                start: *start,
                                end: *end,
                            },
                        })));
                    }
                }
            }
        }

        Ok(None)
    }
}
