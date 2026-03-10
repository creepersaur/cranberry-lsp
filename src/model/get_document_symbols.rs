use tower_lsp::lsp_types::{Location, Range, SymbolInformation, SymbolKind, Url};

use crate::model::language_model::Symbol;

pub fn get_document_symbols(url: Url, symbols: Vec<Symbol>) -> Vec<SymbolInformation> {
	let mut doc_symbols = vec![];

    for i in symbols.iter() {
        match i {
            Symbol::Variable {
                name,
                constant,
                file,
                start_position,
                end_position,
                ..
            } => {
                if let Some(file) = file
                    && file == &url
                {
                    doc_symbols.push(SymbolInformation {
                        name: name.clone(),
                        kind: if *constant {
                            SymbolKind::CONSTANT
                        } else {
                            SymbolKind::VARIABLE
                        },
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        location: Location::new(
                            url.clone(),
                            Range::new(start_position.unwrap(), end_position.unwrap()),
                        ),
                        container_name: None,
                    });
                }
            }

            Symbol::Function {
                name,
                file,
                scope_start_position,
                scope_end_position,
                ..
            } => {
                if let Some(file) = file
                    && file == &url
                {
                    doc_symbols.push(SymbolInformation {
                        name: name.clone(),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        location: Location::new(
                            url.clone(),
                            Range::new(scope_start_position.unwrap(), scope_end_position.unwrap()),
                        ),
                        container_name: None,
                    });
                }
            }

            Symbol::Class {
                name,
                constructor,
                file,
                start_position,
                scope_end_position,
                ..
            } => {
                if let Some(file) = file
                    && file == &url
                {
                    doc_symbols.push(SymbolInformation {
                        name: name.clone(),
                        kind: SymbolKind::CLASS,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        location: Location::new(
                            url.clone(),
                            Range::new(start_position.unwrap(), scope_end_position.unwrap()),
                        ),
                        container_name: None,
                    });

                    if let Some(constructor) = constructor {
                        if let Symbol::Constructor {
                            scope_start_position,
                            scope_end_position,
                            ..
                        } = &**constructor
                        {
                            doc_symbols.push(SymbolInformation {
                                name: String::from("constructor"),
                                kind: SymbolKind::CONSTRUCTOR,
                                tags: None,
                                #[allow(deprecated)]
                                deprecated: None,
                                location: Location::new(
                                    url.clone(),
                                    Range::new(
                                        scope_start_position.unwrap(),
                                        scope_end_position.unwrap(),
                                    ),
                                ),
                                container_name: None,
                            });
                        }
                    }
                }
            }

            _ => {}
        }
    }

    doc_symbols
}
