use tower_lsp::lsp_types::{CompletionItem, Range};
use tree_sitter::{InputEdit, Parser, Point, Tree};
use tree_sitter_cranberry;

use crate::language_model::LanguageModel;

pub struct FileState {
    parser: Parser,
    source_code: String,
    tree: Tree,
    line_starts: Vec<usize>,
    model: LanguageModel,
}

impl FileState {
    pub fn new(source_code: String) -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_cranberry::LANGUAGE.into())
            .expect("Error loading Cranberry grammar");

        let tree = parser.parse(&source_code, None).unwrap();
        let mut model = LanguageModel::new();
		model.build_model(&tree, &source_code);

        Self {
			line_starts: compute_line_starts(&source_code),
            source_code,
            parser,
            model,
            tree,
        }
    }

    pub fn replace_all(&mut self, new_source_code: String) {
        self.tree = self.parser.parse(&new_source_code, None).unwrap();
        self.source_code = new_source_code;
    }

    pub fn edit_file(&mut self, new_source_code: String, range: Range) {
        self.line_starts = compute_line_starts(&new_source_code);

        let start_byte = position_to_byte_offset_fast(
            &new_source_code,
            &self.line_starts,
            range.start.line as usize,
            range.start.character,
        )
        .unwrap();

        let new_end_byte = position_to_byte_offset_fast(
            &new_source_code,
            &self.line_starts,
            range.end.line as usize,
            range.end.character,
        )
        .unwrap();

        let start_position = Point::new(range.start.line as usize, range.start.character as usize);
        let new_end_position = Point::new(range.end.line as usize, range.end.character as usize);

        let edit_details = InputEdit {
            start_byte,
            old_end_byte: start_byte,
            new_end_byte,
            start_position,
            old_end_position: start_position,
            new_end_position,
        };

        self.source_code = new_source_code;
        self.tree.edit(&edit_details);

        if let Some(tree) = self.parser.parse(&self.source_code, Some(&self.tree)) {
            self.tree = tree;
        }
    }

    pub fn completion(&mut self) -> Vec<CompletionItem> {
        self.model.build_model(&self.tree, &self.source_code);

        [
            self.model.get_class_completion(),
            self.model.get_function_completion(),
        ]
        .concat()
    }
}

pub fn compute_line_starts(text: &str) -> Vec<usize> {
    let mut starts = Vec::with_capacity(text.len() / 40); // heuristic
    starts.push(0);

    let bytes = text.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\n' {
            starts.push(i + 1);
        }
        i += 1;
    }

    starts
}

#[inline]
pub fn position_to_byte_offset_fast(
    text: &str,
    line_starts: &[usize],
    line: usize,
    character: u32,
) -> Option<usize> {
    let line_start = *line_starts.get(line)?;

    // Find line end (or EOF)
    let line_end = line_starts.get(line + 1).copied().unwrap_or(text.len());

    let line_text = &text[line_start..line_end];

    let mut utf16 = 0u32;

    for (byte_idx, ch) in line_text.char_indices() {
        let len16 = ch.len_utf16() as u32;
        if utf16 + len16 > character {
            return Some(line_start + byte_idx);
        }
        utf16 += len16;
    }

    if utf16 == character {
        return Some(line_start + line_text.len());
    }

    None
}
