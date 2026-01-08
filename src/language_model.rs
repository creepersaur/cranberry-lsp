use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, InsertTextFormat, MarkupContent, MarkupKind,
    Position,
};
use tree_sitter::{Node, Tree, TreeCursor};

#[derive(Debug)]
pub enum Symbol {
    Class {
        name: String,
        constructor: Option<Box<Symbol>>,
        functions: Vec<Symbol>,
        variables: Vec<Symbol>,
    },
    Function {
        name: String,
        args: Vec<String>,
    },
    Variable {
        name: String,
        optional_type: Option<String>,
    },
    Constructor {
        args: Vec<String>,
    },
}

#[derive(Default)]
pub struct Scope {
    pub children: Vec<Scope>,
    pub symbols: Vec<Symbol>,
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
    pub global_scope: Scope,
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

    pub fn get_completion_symbols(&mut self, scope: Option<&Scope>) -> Vec<CompletionItem> {
        scope
            .unwrap_or(&self.global_scope)
            .symbols
            .iter()
            .map(|symbol| match symbol {
                Symbol::Variable {
                    name,
                    optional_type,
                } => CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: optional_type.clone(),
                    insert_text: Some(name.clone()),
                    ..Default::default()
                },

                Symbol::Function { name, args } => CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(format!("({})", args.join(", "))),
                    insert_text: Some(format!("{}($0)", &name)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                },

                Symbol::Class {
                    name,
                    functions,
                    variables,
                    constructor,
                } => CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some(String::from("class")),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!(
                            r#"# Class `{}`
Construct this class using `{}({})`
```cb
class {} {}{}{}{}
{}
```"#,
                            &name,
                            &name,
                            if let Some(constructor) = constructor {
                                if let Symbol::Constructor { args } = constructor.as_ref() {
                                    args[1..].join(", ")
                                } else {
                                    String::new()
                                }
                            } else {
                                String::new()
                            },
                            &name,
                            "{",
                            if let Some(constructor) = constructor {
                                if let Symbol::Constructor { args } = constructor.as_ref() {
                                    if variables.len() > 0 || functions.len() > 0 {
                                        format!("\n\tconstructor({})\n", args.join(", "))
                                    } else {
                                        format!("\n\tconstructor({})", args.join(", "))
                                    }
                                } else {
                                    String::new()
                                }
                            } else {
                                String::new()
                            },
                            if variables.len() > 0 {
                                format!(
                                    "\n{}{}",
                                    variables
                                        .iter()
                                        .map(|x| {
                                            let Symbol::Variable {
                                                name,
                                                optional_type,
                                            } = x
                                            else {
                                                unreachable!("non-function in functions vec");
                                            };

                                            format!(
                                                "\tlet {name}{};",
                                                if let Some(t) = optional_type {
                                                    format!(": {t}")
                                                } else {
                                                    String::new()
                                                }
                                            )
                                        })
                                        .collect::<Vec<String>>()
                                        .join("\n"),
                                    if functions.len() > 0 { "\n" } else { "" }
                                )
                            } else {
                                String::new()
                            },
                            if functions.len() > 0 {
                                format!(
                                    "\n{}",
                                    functions
                                        .iter()
                                        .map(|x| {
                                            let Symbol::Function { name, args } = x else {
                                                unreachable!("non-function in functions vec");
                                            };

                                            format!(
                                                "\tfn {} {{ .... }}",
                                                format!("{name}({})", args.join(", "))
                                            )
                                        })
                                        .collect::<Vec<String>>()
                                        .join("\n")
                                )
                            } else {
                                if variables.len() < 1 && constructor.is_none() {
                                    "\n".to_string()
                                } else {
                                    String::new()
                                }
                            },
                            "}",
                        ),
                    })),
                    ..Default::default()
                },

                x => CompletionItem::new_simple(format!("{x:?}"), String::new()),
            })
            .collect()
    }

    pub fn get_member_symbols(&mut self, position: &Position) -> Vec<CompletionItem> {
        let line = self
            .source_code
            .lines()
            .nth(position.line as usize)
            .unwrap();
        let col = position.character as usize;
        let dot_pos = col.saturating_sub(1);

        // GO BACK AND GET OBJECT
        let mut start = dot_pos;
        while start > 0 {
            let ch = line.as_bytes()[start - 1];
            if !matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_') {
                break;
            }
            start -= 1;
        }

        let object = &line[start..dot_pos];
        for i in self.global_scope.symbols.iter() {
            if let Symbol::Class {
                name,
                functions,
                variables,
                ..
            } = i
            {
                if name != object {
                    continue;
                }
                let function_completion = functions
                    .iter()
                    .map(|x| match x {
                        Symbol::Function { name, args } => CompletionItem {
                            label: name.to_string(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some(format!("({})", args.join(", "))),
                            insert_text: Some(format!("{name}($0)")),
                            insert_text_format: Some(InsertTextFormat::SNIPPET),
                            ..Default::default()
                        },
                        _ => CompletionItem {
                            label: "NOT-FUNCTION".to_string(),
                            ..Default::default()
                        },
                    })
                    .collect::<Vec<_>>();

                let variable_completion = variables
                    .iter()
                    .map(|x| match x {
                        Symbol::Variable {
                            name,
                            optional_type,
                        } => CompletionItem {
                            label: name.to_string(),
                            kind: Some(CompletionItemKind::VARIABLE),
                            detail: optional_type.clone(),
                            ..Default::default()
                        },

                        _ => CompletionItem {
                            label: "NOT-VARIABLE".to_string(),
                            ..Default::default()
                        },
                    })
                    .collect();

                return [variable_completion, function_completion].concat();
            }
        }

        vec![]
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

    pub fn get_value_type<'a>(node: Node<'a>, source_code: &str, scope: &Scope) -> Option<String> {
        match node.kind() {
            "number" => Some("number".to_string()),
            "string" => Some("string".to_string()),
            "formatted_string" => Some("string".to_string()),
            "boolean" => Some("bool".to_string()),
            "nil" => Some("nil?".to_string()),
            "call_expression" => {
                let name_node = node.child_by_field_name("name").unwrap();
                let mut cursor = name_node.walk();
                cursor.goto_first_child();

                if cursor.node().kind() == "pascal_case_identifier" {
                    Some(Self::get_text(name_node, source_code))
                } else {
					Some("nil".to_string())
				}
            }

			"identifier" => {
				let mut cursor = node.walk();
                cursor.goto_first_child();

				let text = Self::get_text(node, source_code);
				if cursor.node().kind() == "pascal_case_identifier" {
                    Some(text)
                } else {
					for i in scope.symbols.iter() {
						if let Symbol::Variable { optional_type, .. } = i {
							return optional_type.clone();
						} else if let Symbol::Function { .. } = i {
							return Some("function".to_string())
						}
					}
					None
				}
			}

            _ => None,
        }
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

            "constructor" => {
                let mut args = vec![];

                if let Some(parameters) = Self::try_get_field(node, "parameters") {
                    args = parameters
                        .children_by_field_name("param", &mut parameters.walk())
                        .map(|x| Self::get_text(x, source_code))
                        .collect();
                }

                scope.add_symbol(Symbol::Constructor { args });
            }

            "let_statement" => {
                let names: Vec<String> = node
                    .children_by_field_name("name", &mut node.walk())
                    .map(|x| Self::get_text(x, source_code))
                    .collect();

                let values: Vec<_> = node
                    .children_by_field_name("value", &mut node.walk())
                    .map(|x| Self::get_value_type(x, source_code, scope))
                    .collect();

                let optional_type = if let Some(t) = Self::try_get_field(node, "type") {
                    Some(Self::get_text(t, source_code))
                } else {
                    None
                };

                if values.len() > 0 {
                    for i in 0..names.len() {
                        scope.add_symbol(Symbol::Variable {
                            name: names[i].clone(),
                            optional_type: values[i].clone().or(optional_type.clone()),
                        });
                    }
                } else {
                    for name in names {
                        scope.add_symbol(Symbol::Variable {
                            name,
                            optional_type: optional_type.clone(),
                        });
                    }
                }
            }

            "const_statement" => {
                let names: Vec<String> = node
                    .children_by_field_name("name", cursor)
                    .map(|x| Self::get_text(x, source_code))
                    .collect();

                let optional_type = if let Some(t) = Self::try_get_field(node, "type") {
                    Some(Self::get_text(t, source_code))
                } else {
                    None
                };

                for name in names {
                    scope.add_symbol(Symbol::Variable {
                        name,
                        optional_type: optional_type.clone(),
                    });
                }
            }

            "class_definition" => {
                let name = Self::get_field_text(node, "name", source_code);

                let mut class_scope = Scope::default();
                let mut variables = vec![];
                let mut functions = vec![];
                let mut constructor = None;

                let mut class_cursor = node.walk();
                class_cursor.goto_first_child();

                if class_cursor.goto_next_sibling() {
                    loop {
                        Self::evaluate_node(
                            class_cursor.node(),
                            &mut class_cursor,
                            &mut class_scope,
                            source_code,
                        );

                        if !class_cursor.goto_next_sibling() {
                            break;
                        }
                    }
                }

                for i in class_scope.symbols {
                    match i {
                        Symbol::Function { .. } => functions.push(i),
                        Symbol::Variable { .. } => variables.push(i),
                        Symbol::Constructor { .. } => constructor = Some(Box::new(i)),

                        _ => {}
                    }
                }

                scope.add_symbol(Symbol::Class {
                    name,
                    constructor,
                    functions,
                    variables,
                });
            }

            _ => return,
        }
    }
}
