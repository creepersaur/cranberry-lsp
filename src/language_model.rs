use std::collections::HashSet;

use serde_json::json;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, Documentation,
    InsertTextFormat, MarkupContent, MarkupKind, Position,
};
use tree_sitter::{Node, Tree, TreeCursor};

use crate::file_state::position_to_byte_offset_fast;

#[derive(Debug, Clone)]
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
        returns: HashSet<String>,
    },
    Variable {
        name: String,
        optional_type: Option<String>,
    },
    Constructor {
        args: Vec<String>,
    },
}

#[derive(Default, Debug, Clone)]
pub struct Scope {
    pub disable_autocomplete: bool,
    pub start: usize,
    pub end: usize,
    pub children: Vec<Scope>,
    pub symbols: Vec<Symbol>,
}

impl Scope {
    pub fn new(start_byte: usize, end_byte: usize) -> Self {
        Self {
            start: start_byte,
            end: end_byte,
            ..Default::default()
        }
    }

    pub fn new_disabled(start_byte: usize, end_byte: usize) -> Self {
        Self {
            start: start_byte,
            end: end_byte,
            disable_autocomplete: true,
            ..Default::default()
        }
    }

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

    pub fn clear(&mut self) {
        self.global_scope.children.clear();
        self.global_scope.symbols.clear();
    }

    pub fn build_model(&mut self, tree: &Tree, source_code: &str) {
        self.global_scope.end = tree.root_node().end_byte();

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

    pub fn get_completion_symbols(&self, scope_path: Vec<&Scope>) -> Vec<CompletionItem> {
        scope_path
            .iter()
            .map(|scope| {
                {
                    scope
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

                            Symbol::Function {
                                name,
                                args,
                                returns,
                            } => CompletionItem {
                                label: name.clone(),
                                kind: Some(CompletionItemKind::FUNCTION),
                                documentation: Some(if returns.is_empty() {
                                    Documentation::MarkupContent(MarkupContent {
                                        kind: MarkupKind::Markdown,
                                        value: format!(
                                            "```cranberry\n({}) -> nil\n```",
                                            args.join(", ")
                                        ),
                                    })
                                } else {
                                    Documentation::MarkupContent(MarkupContent {
                                        kind: MarkupKind::Markdown,
                                        value: format!(
                                            "```cranberry\n({}) -> {}\n```",
                                            args.join(", "),
                                            returns
                                                .iter()
                                                .map(|x| x.as_str())
                                                .collect::<Vec<&str>>()
                                                .join(" | ")
                                        ),
                                    })
                                }),
                                label_details: Some(CompletionItemLabelDetails {
                                    detail: Some(format!("({})", args.join(", "))),
                                    description: None,
                                }),
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
                                data: Some(json!({
                                    "doc": format!(
                                        r#"Construct this class using `{}({})`
```cranberry
let my_object = {}( ... )
```

## Declaration:

```cranberry
class {} {}{}{}{}
}}
```

[Learn More](https://creepersaur.github.io/CranberryDocs/basic_topics/classes/)
"#,
                                        &name,
                                        if let Some(constructor) = constructor {
                                            if let Symbol::Constructor { args } =
                                                constructor.as_ref()
                                            {
                                                args.join(", ")
                                            } else {
                                                String::new()
                                            }
                                        } else {
                                            String::new()
                                        },
                                        &name,
                                        &name,
                                        "{",
                                        if let Some(constructor) = constructor {
                                            if let Symbol::Constructor { args } =
                                                constructor.as_ref()
                                            {
                                                if variables.len() > 0 || functions.len() > 0 {
                                                    format!(
                                                        "\n\tconstructor({})\n",
                                                        args.join(", ")
                                                    )
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
                                                            unreachable!(
                                                                "non-function in functions vec"
                                                            );
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
                                                        let Symbol::Function { name, args, .. } = x
                                                        else {
                                                            unreachable!(
                                                                "non-function in functions vec"
                                                            );
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
                                    ),
                                })),
                                ..Default::default()
                            },

                            x => CompletionItem::new_simple(format!("{x:?}"), String::new()),
                        })
                        .collect::<Vec<_>>()
                }
            })
            .collect::<Vec<_>>()
            .concat()
    }

    pub fn get_class_members(
        &self,
        functions: &Vec<Symbol>,
        variables: &Vec<Symbol>,
    ) -> Vec<CompletionItem> {
        let function_completion = functions
            .iter()
            .map(|x| match x {
                Symbol::Function {
                    name,
                    args,
                    returns,
                } => CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    documentation: Some(if returns.is_empty() {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!("```cranberry\n({}) -> nil\n```", args.join(", ")),
                        })
                    } else {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!(
                                "```cranberry\n({}) -> {}\n```",
                                args.join(", "),
                                returns
                                    .iter()
                                    .map(|x| x.as_str())
                                    .collect::<Vec<&str>>()
                                    .join(" | ")
                            ),
                        })
                    }),
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some(format!(
                            "({})",
                            args.iter()
                                .filter_map(|x| if *x != "self" {
                                    Some(x.to_string())
                                } else {
                                    None
                                })
                                .collect::<Vec<String>>()
                                .join(", ")
                        )),
                        description: None,
                    }),
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
                    documentation: if let Some(optional_type) = optional_type {
                        Some(Documentation::String(optional_type.clone()))
                    } else {
                        None
                    },
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

    pub fn get_member_symbols(&mut self, object: &str) -> Vec<CompletionItem> {
        for i in self.global_scope.symbols.iter() {
            if let Symbol::Class {
                name,
                functions,
                variables,
                ..
            } = i
                && name == object
            {
                return self.get_class_members(functions, variables);
            }

            if let Symbol::Variable {
                name,
                optional_type,
            } = i
                && name == object
            {
                for i in self.global_scope.symbols.iter() {
                    if let Symbol::Class {
                        name,
                        functions,
                        variables,
                        ..
                    } = i
                        && Some(name) == optional_type.as_ref()
                    {
                        return self.get_class_members(functions, variables);
                    }
                }
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
            "nil" => Some("nil".to_string()),
            "call_expression" => {
                let name_node = node.child_by_field_name("name").unwrap();
                let mut cursor = name_node.walk();
                cursor.goto_first_child();

                if cursor.node().kind() == "pascal_case_identifier" {
                    Some(Self::get_text(name_node, source_code))
                } else {
                    let func_name = Self::get_text(name_node, source_code);

                    for i in scope.symbols.iter() {
                        if let Symbol::Function { name, returns, .. } = i
                            && name == &func_name
                        {
                            if returns.is_empty() {
                                return Some("nil".to_string());
                            } else {
                                return Some(returns.iter().collect::<Vec<_>>()[0].clone());
                            }
                        }
                    }

                    None
                }
            }
            "member_expression" => {
                let object = Self::get_field(node, "object");
                let member = Self::get_field(node, "member");

                let object_type = Self::get_value_type(object, source_code, scope)?;

                for symbol in scope.symbols.iter() {
                    if let Symbol::Class {
                        name,
                        functions,
                        variables,
                        ..
                    } = symbol
                        && name == &object_type
                    {
                        // Now check what kind of member access this is
                        let mut expr_type = "normal";

                        let member_name = if member.kind() == "call_expression" {
                            // hello.world()
                            expr_type = "call";
                            let method_name_node = member.child_by_field_name("name")?;
                            Self::get_text(method_name_node, source_code)
                        } else {
                            // hello.world
                            Self::get_text(member, source_code)
                        };

                        // Search for the member in functions
                        for func in functions {
                            if let Symbol::Function {
                                name,
                                args,
                                returns,
                            } = func
                                && name == &member_name
                            {
                                if expr_type == "call" {
                                    return if returns.is_empty() {
                                        Some("nil".to_string())
                                    } else {
                                        Some(returns.iter().collect::<Vec<_>>()[0].clone())
                                    };
                                } else {
                                    return Some(format!(
                                        "```cranberry
({}) -> {}
```",
                                        args.join(", "),
                                        if returns.is_empty() {
                                            "nil".to_string()
                                        } else {
                                            returns
                                                .iter()
                                                .map(|x| x.clone())
                                                .collect::<Vec<_>>()
                                                .join(" | ")
                                        }
                                    ));
                                }
                            }
                        }

                        // Search for the member in variables
                        for var in variables {
                            if let Symbol::Variable {
                                name,
                                optional_type,
                            } = var
                                && name == &member_name
                            {
                                return optional_type.clone();
                            }
                        }
                    }
                }

                None
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
                            return Some("function".to_string());
                        }
                    }
                    None
                }
            }

            _ => None,
        }
    }

    pub fn get_kind_descendants<'a>(node: Node<'a>, kind: &str) -> Vec<Node<'a>> {
        let mut cursor = node.walk();
        let mut nodes = vec![];

        for child in node.children(&mut cursor) {
            nodes.extend(Self::get_kind_descendants(child, kind));

            if child.kind() == kind {
                nodes.push(child);
            }
        }

        nodes
    }

    pub fn evaluate_node<'a>(
        node: Node<'a>,
        _cursor: &mut TreeCursor<'a>,
        scope: &mut Scope,
        source_code: &str,
    ) {
        let kind = node.kind();

        match kind {
            "function_declaration" => {
                let name = Self::get_field_text(node, "name", source_code);

                let mut returns = HashSet::new();
                let mut args = vec![];

                if let Some(parameters) = Self::try_get_field(node, "parameters") {
                    args = parameters
                        .children_by_field_name("param", &mut parameters.walk())
                        .map(|x| Self::get_text(x, source_code))
                        .collect();
                }

                if let Some(block) = Self::try_get_field(node, "block") {
                    let mut b_cursor = block.walk();

                    let start = b_cursor.node().start_byte();
                    let end = b_cursor.node().end_byte();

                    if !b_cursor.goto_first_child() {
                        return;
                    }

                    let mut inner_scope = Scope::new(start, end);

                    if b_cursor.node().kind() == "inline_block" {
                        if let Some(value) = Self::try_get_field(b_cursor.node(), "value") {
                            if let Some(ret) =
                                Self::get_value_type(value, source_code, &inner_scope)
                            {
                                returns.insert(ret);
                            } else {
                                returns.insert("???".to_string());
                            }
                        }
                    } else {
                        let return_statements =
                            Self::get_kind_descendants(b_cursor.node(), "return_statement");

                        for statements in return_statements {
                            if let Some(value) = statements.child_by_field_name("value") {
                                let return_type =
                                    Self::get_value_type(value, source_code, &inner_scope);

                                if let Some(ret) = return_type {
                                    returns.insert(ret);
                                } else {
                                    returns.insert("???".to_string());
                                }
                            }
                        }
                    }

                    if b_cursor.goto_first_child() {
                        for i in args.iter() {
                            inner_scope.add_symbol(Symbol::Variable {
                                name: i.clone(),
                                optional_type: None,
                            });
                        }

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

                scope.add_symbol(Symbol::Function {
                    name,
                    args,
                    returns,
                });
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

                if let Some(block) = Self::try_get_field(node, "block") {
                    scope.add_child_scope(Scope::new(block.start_byte(), block.end_byte()));
                }
            }

            "const_statement" | "let_statement" => {
                let names: Vec<String> = node
                    .children_by_field_name("name", &mut node.walk())
                    .map(|x| Self::get_text(x, source_code))
                    .collect();

                let values: Vec<_> = node
                    .children_by_field_name("value", &mut node.walk())
                    .map(|x| Self::get_value_type(x, source_code, scope))
                    .collect();

                let optional_types: Vec<_> = node
                    .children_by_field_name("type", &mut node.walk())
                    .map(|x| Self::get_value_type(x, source_code, scope))
                    .collect();

                if values.len() >= names.len() {
                    for i in 0..names.len() {
                        scope.add_symbol(Symbol::Variable {
                            name: names[i].clone(),
                            optional_type: values[i].clone().or(
                                if optional_types.len() == names.len() {
                                    optional_types[i].clone()
                                } else {
                                    None
                                },
                            ),
                        });
                    }
                } else {
                    for name in names {
                        scope.add_symbol(Symbol::Variable {
                            name,
                            optional_type: if optional_types.len() > 0 {
                                optional_types[0].clone()
                            } else {
                                None
                            },
                        });
                    }
                }
            }

            "class_definition" => {
                let name = Self::get_field_text(node, "name", source_code);

                let mut class_scope = Scope::new_disabled(node.start_byte(), node.end_byte());
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

                for i in class_scope.symbols.iter() {
                    match i {
                        Symbol::Function { .. } => functions.push(i.clone()),
                        Symbol::Variable { .. } => variables.push(i.clone()),
                        Symbol::Constructor { .. } => constructor = Some(Box::new(i.clone())),

                        _ => {}
                    }
                }

                scope.add_child_scope(class_scope.clone());

                scope.add_symbol(Symbol::Class {
                    name,
                    constructor,
                    functions,
                    variables,
                });
            }

            "explicit_scope" => {
                if let Some(block) = Self::try_get_field(node, "block") {
                    let mut b_cursor = block.walk();

                    let start = b_cursor.node().start_byte();
                    let end = b_cursor.node().end_byte();

                    if b_cursor.goto_first_child() {
                        let mut inner_scope = Scope::new(start, end);

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

            "for_statement" => {
                if let Some(block) = Self::try_get_field(node, "block") {
                    let mut inner_scope = Scope::new(block.start_byte(), block.end_byte());

                    inner_scope.add_symbol(Symbol::Variable {
                        name: Self::get_field_text(node, "var", source_code),
                        optional_type: None,
                    });

                    scope.add_child_scope(inner_scope);
                }
            }

            _ => return,
        }
    }

    pub fn accumulate_cursor_scope(
        &self,
        line_starts: &[usize],
        source_code: &str,
        position: &Position,
    ) -> Vec<&Scope> {
        let position_byte = position_to_byte_offset_fast(
            source_code,
            line_starts,
            position.line as usize,
            position.character,
        );

        let mut scope_path = vec![];

        if let Some(pos) = position_byte {
            if let Some(scope) =
                self.accumulate_inner_scope_from_byte(&self.global_scope, pos, &mut scope_path)
            {
                // Push the innermost containing scope too, but only if autocomplete isn't disabled.
                if !scope.disable_autocomplete {
                    scope_path.push(scope);
                }
            } else {
                // fallback to global scope when nothing matched
                scope_path.push(&self.global_scope);
            }
        } else {
            scope_path.push(&self.global_scope);
        }

        // remove duplicate scopes (by start,end)
        let mut seen = HashSet::new();
        scope_path.retain(|scope| {
            let key = (scope.start, scope.end);
            seen.insert(key)
        });

        scope_path
    }

    pub fn accumulate_inner_scope_from_byte<'a>(
        &self,
        scope: &'a Scope,
        position_byte: usize,
        scope_path: &mut Vec<&'a Scope>,
    ) -> Option<&'a Scope> {
        // Only proceed if this scope actually contains the position
        if scope.start <= position_byte && scope.end > position_byte {
            for child in scope.children.iter() {
                // recurse: only children that contain `position_byte` will return Some(child_scope)
                if let Some(child_scope) =
                    self.accumulate_inner_scope_from_byte(child, position_byte, scope_path)
                {
                    // we want the containing child scopes to be visible (unless disabled)
                    if !child_scope.disable_autocomplete {
                        scope_path.push(child_scope);
                    }
                }
            }

            // return this scope (the caller will decide whether to push it)
            return Some(scope);
        }

        // not in this scope
        None
    }
}

#[allow(unused)]
fn format_sexp(sexp: &str) -> String {
    let mut out = String::new();
    let mut indent: i32 = 0;
    let mut newline = false;

    for c in sexp.chars() {
        match c {
            '(' => {
                if newline {
                    out.push('\n');
                    out.push_str(&"  ".repeat(indent as usize));
                }
                out.push('(');
                indent += 1;
                newline = true;
            }
            ')' => {
                indent -= 1;
                out.push(')');
                newline = true;
            }
            ' ' => {
                out.push(' ');
            }
            _ => {
                if newline {
                    out.push('\n');
                    out.push_str(&"  ".repeat(indent as usize));
                    newline = false;
                }
                out.push(c);
            }
        }
    }

    out
}
