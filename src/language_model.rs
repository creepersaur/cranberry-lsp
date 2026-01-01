use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, InsertTextFormat, MarkupContent, MarkupKind,
};
use tree_sitter::{Node, Tree, TreeCursor};
pub enum Symbol {
    Class { name: String },
    Function { name: String },
    Variable { name: String },
}

#[derive(Default)]
pub struct Scope {
    children: Vec<Scope>,
    symbols: Vec<Symbol>,
}

impl Scope {
    pub fn add_symbol(&mut self, symbol: Symbol) {
        self.symbols.push(symbol);
    }

    pub fn add_child_scope(&mut self, scope: Scope) {
        self.children.push(scope);
    }
}

pub struct LanguageModel {
    global_scope: Scope,
    source_code: String,
}

impl LanguageModel {
    pub fn new() -> Self {
        Self {
            global_scope: Scope::default(),
            source_code: String::new(),
        }
    }

    pub fn build_model(&mut self, tree: &Tree, source_code: &str) {
        self.global_scope.children.clear();
        self.global_scope.symbols.clear();

        self.source_code = source_code.to_string();

        let mut cursor = tree.walk();
        if !cursor.goto_first_child() {
            return;
        }

        loop {
            let node = cursor.node();
            Self::evaluate_node(&mut cursor, node, &mut self.global_scope, source_code);

            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }

    pub fn get_completion_symbols(&mut self) -> Vec<CompletionItem> {
        self.global_scope
            .symbols
            .iter()
            .map(|symbol| match symbol {
                Symbol::Variable { name } => CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    ..Default::default()
                },

                Symbol::Function { name } => CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some("()".to_string()),
                    insert_text: Some(format!("{}($0)", &name)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                },

                Symbol::Class { name } => CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some(String::from("class")),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!(
                            "# Class `{}`\nConstruct this class using `{}()`\n```cb\nclass {} {}",
                            &name, &name, &name, "{\n\n}"
                        ),
                    })),
                    ..Default::default()
                },
            })
            .collect()
    }

    pub fn get_field<'a>(node: Node<'a>, name: &'a str) -> Node<'a> {
        node.child_by_field_name(name).unwrap()
    }

    pub fn try_get_field<'a>(node: Node<'a>, name: &'a str) -> Option<Node<'a>> {
        node.child_by_field_name(name)
    }

    pub fn get_text<'a>(node: Node<'a>, source_code: &str) -> String {
        source_code[node.start_byte()..node.end_byte()].to_string()
    }

    pub fn get_field_text<'a>(node: Node<'a>, name: &'a str, source_code: &str) -> String {
        Self::get_text(Self::get_field(node, name), source_code)
    }

    pub fn evaluate_node<'a>(
        cursor: &mut TreeCursor<'a>,
        node: Node<'a>,
        scope: &mut Scope,
        source_code: &str,
    ) {
        let kind = node.kind();

        match kind {
            "function_declaration" => {
                let name = Self::get_field_text(node, "name", source_code);

                scope.add_symbol(Symbol::Function { name });

                if let Some(block) = Self::try_get_field(node, "block") {
                    cursor.goto_descendant(block.id());
                    if !cursor.goto_first_child() {
                        cursor.goto_parent();
                        cursor.goto_parent();
                        return;
                    }

                    let mut inner_scope = Scope {
                        children: vec![],
                        symbols: vec![],
                    };

                    loop {
                        Self::evaluate_node(cursor, cursor.node(), &mut inner_scope, source_code);

                        if !cursor.goto_next_sibling() {
                            break;
                        }
                    }

                    scope.add_child_scope(inner_scope);
                    cursor.goto_parent();
                    cursor.goto_parent();
                }
            }

            "let_statement" => {
                let names: Vec<String> = node
                    .children_by_field_name("name", cursor)
                    .map(|x| Self::get_text(x, source_code))
                    .collect();

                for name in names {
                    scope.add_symbol(Symbol::Variable { name });
                }
            }

            _ => return,
        }
    }
}
