#[macro_export]
macro_rules! add_keyword {
    ($name:expr, $description:expr) => {
        CompletionItem {
            label: $name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            documentation: Some(Documentation::String($description.to_string())),
            ..Default::default()
        }
    };
}

#[macro_export]
macro_rules! add_function {
    ($name:expr, $details:expr) => {
        CompletionItem {
            label: $name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some($details.to_string()),
            insert_text: Some(format!("{}($0)", $name)),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        }
    };
}

#[macro_export]
macro_rules! add_type_cast {
    ($name:expr, $details:expr) => {
        CompletionItem {
            label: $name.to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some($details.to_string()),
            insert_text: Some(format!("{}($0)", $name)),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        }
    };
}

#[macro_export]
macro_rules! add_all_keywords {
    () => {
        vec![
            add_keyword!("fn", "function declaration"),
            add_keyword!("let", "variable declaration"),
            add_keyword!("const", "constant declaration"),
            add_keyword!("class", "class declaration"),
            add_keyword!("constructor", "constructor function"),
            add_keyword!("using", "import namespaces"),
            add_keyword!("namespace", "namespace declaration"),
            add_keyword!("if", "if statement"),
            add_keyword!("else", "else(if) statement"),
            add_keyword!("loop", "forever loop"),
            add_keyword!("while", "condition loop"),
            add_keyword!("for", "for loop"),
            add_keyword!("return", "return value?"),
            add_keyword!("break", "break value?"),
            add_keyword!("out", "output value?"),
            add_keyword!("continue", "continue iteration"),
            add_keyword!("switch", "match statement"),
            add_keyword!("in", "in <iteratable>"),
        ]
    };
}

#[macro_export]
macro_rules! add_all_builtin_functions {
    () => {
        vec![
            add_function!("println", "(...)"),
            add_function!("print", "(...)"),
            add_function!("error", "(message)"),
            add_function!("format", "(...) -> string"),
            add_function!("typeof", "(value, internal?) -> string"),
            add_function!("pcall", "(fn, ...) -> (bool, data)"),
            add_type_cast!("number", "(value) -> number"),
            add_type_cast!("string", "(value) -> string"),
        ]
    };
}

#[macro_export]
macro_rules! add_all_type_casts {
    () => {
        vec![
            add_type_cast!("bool", "(value) -> bool"),
            add_type_cast!("char", "(value) -> char"),
            add_type_cast!("list", "(value) -> list"),
            add_type_cast!("List", "(value) -> list"),
            add_type_cast!("Dict", "(value) -> dict"),
        ]
    };
}
