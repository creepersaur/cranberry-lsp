use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, InsertTextFormat, MarkupContent, MarkupKind,
};
use tree_sitter::{Node, Tree, TreeCursor};
pub enum Symbol {
    Class {
        name: String,
        function_names: Vec<String>,
    },
    Function {
        name: String,
        args: Vec<String>,
    },
    Variable {
        name: String,
    },
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
            Self::evaluate_node(node, &mut cursor, &mut self.global_scope, source_code);

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
                    label: format!("⎔ {name}"),
                    kind: Some(CompletionItemKind::VARIABLE),
					insert_text: Some(name.clone()),
                    ..Default::default()
                },

                Symbol::Function { name, args } => CompletionItem {
                    label: format!("⨐ {name}"),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format!("({})", args.join(", "))),
                    insert_text: Some(format!("{}($0)", &name)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                },

                Symbol::Class {
                    name,
                    function_names,
                } => CompletionItem {
                    label: format!("⊡ {name}"),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some(String::from("class")),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!(
                            r#"# Class `{}`
Construct this class using `{}()`
```cb
class {} {}
	{}
{}
```"#,
                            &name,
                            &name,
                            &name,
                            "{",
                            function_names
                                .iter()
                                .map(|x| format!("fn {}() {{ .... }}", x))
                                .collect::<Vec<String>>()
                                .join("\n"),
                            "}",
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
        node: Node<'a>,
        cursor: &mut TreeCursor<'a>,
        scope: &mut Scope,
        source_code: &str,
    ) {
        let kind = node.kind();

        match kind {
            "function_declaration" => {
                let name = Self::get_field_text(node, "name", source_code);

                let mut args = vec![];

                if let Some(parameters) = Self::try_get_field(node, "parameters") {
                    args = parameters
                        .children_by_field_name("param", &mut parameters.walk())
                        .map(|x| Self::get_text(x, source_code))
                        .collect();
                }

                scope.add_symbol(Symbol::Function { name, args });

                if let Some(block) = Self::try_get_field(node, "block") {
                    let mut b_cursor = block.walk();

                    if b_cursor.goto_first_child() {
                        let mut inner_scope = Scope {
                            children: vec![],
                            symbols: vec![],
                        };

                        loop {
                            Self::evaluate_node(
                                b_cursor.node(),
                                &mut b_cursor,
                                &mut inner_scope,
                                source_code,
                            );

                            if !b_cursor.goto_next_sibling() {
                                break;
                            }
                        }

                        scope.add_child_scope(inner_scope);
                    }
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

            "class_definition" => {
                let name = Self::get_field_text(node, "name", source_code);

                let function_names: Vec<String> = node
                    .children_by_field_name("method", cursor)
                    .map(|x| Self::get_field_text(x, "name", source_code))
                    .collect();

                scope.add_symbol(Symbol::Class {
                    name,
                    function_names,
                });
            }

            _ => return,
        }
    }
}
