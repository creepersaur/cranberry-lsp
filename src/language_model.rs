use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat};
use tree_sitter::{Node, Tree};

pub struct ClassInfo {
    pub name: String,
}

pub struct FunctionInfo {
    pub name: String,
}

pub struct VariableInfo;

pub struct LanguageModel {
    pub classes: Vec<ClassInfo>,
    pub functions: Vec<FunctionInfo>,
    pub variables: Vec<VariableInfo>,
}

impl LanguageModel {
    pub fn new() -> Self {
        Self {
            classes: vec![],
            functions: vec![],
            variables: vec![],
        }
    }

    pub fn get_class_completion(&self) -> Vec<CompletionItem> {
        let completions = self
            .classes
            .iter()
            .map(|x| CompletionItem {
                label: x.name.clone(),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            })
            .collect();

        completions
    }

    pub fn get_function_completion(&self) -> Vec<CompletionItem> {
        let completions = self
            .functions
            .iter()
            .map(|x| CompletionItem {
                label: x.name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("()".to_string()),
                insert_text: Some(format!("{}($0)", &x.name)),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            })
            .collect();

        completions
    }

    pub fn build_model(&mut self, tree: &Tree, source_code: &str) {
        self.classes.clear();
        self.functions.clear();
        self.variables.clear();

        let mut cursor = tree.walk();
        if !cursor.goto_first_child() {
            return;
        }

        loop {
            let node = cursor.node();
            self.evaluate_node(node, source_code);

            if !cursor.goto_next_sibling() {
                break;
            }
        }
    }

    pub fn evaluate_node(&mut self, node: Node, source_code: &str) {
        let kind = node.kind();

        if kind == "class_definition" {
            let name_node = node.child_by_field_name("name").unwrap();
            let name = &source_code[name_node.start_byte()..name_node.end_byte()];

            self.classes.push(ClassInfo {
                name: String::from(name),
            });
        } else if kind == "function_declaration" {
            let name_node = node.child_by_field_name("name").unwrap();
            let name = &source_code[name_node.start_byte()..name_node.end_byte()];

            self.functions.push(FunctionInfo {
                name: String::from(name),
            });
        }
    }
}
