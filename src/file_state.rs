use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, Position, Range, Url,
};
use tree_sitter::{InputEdit, Parser, Point, Tree};
use tree_sitter_cranberry;

use crate::language_model::{LanguageModel, Symbol};

#[allow(unused)]
pub struct FileState {
    parser: Parser,
    pub uri: Url,
    pub source_code: String,
    tree: Tree,
    line_starts: Vec<usize>,
    model: LanguageModel,
}

impl FileState {
    pub fn new(source_code: String, uri: Url) -> Self {
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
            uri,
        }
    }

    pub fn replace_all(&mut self, new_source_code: String) {
        self.tree = self.parser.parse(&new_source_code, None).unwrap();
        self.source_code = new_source_code;
    }

    pub fn edit_file(&mut self, new_text: &str, range: Option<Range>) {
        // old state
        let old_source = self.source_code.clone();
        let old_line_starts = self.line_starts.clone();

        if let Some(range) = range {
            // compute start/end bytes in the OLD document
            let start_byte = position_to_byte_offset_fast(
                &old_source,
                &old_line_starts,
                range.start.line as usize,
                range.start.character,
            )
            .unwrap_or(0); // clamp defensively

            let old_end_byte = position_to_byte_offset_fast(
                &old_source,
                &old_line_starts,
                range.end.line as usize,
                range.end.character,
            )
            .unwrap_or(old_source.len()); // clamp

            // assemble new source by replacing [start_byte..old_end_byte) with new_text
            let mut new_source = String::with_capacity(
                old_source.len() - (old_end_byte - start_byte) + new_text.as_bytes().len(),
            );
            new_source.push_str(&old_source[..start_byte]);
            new_source.push_str(new_text);
            new_source.push_str(&old_source[old_end_byte..]);

            // recompute line starts for new document
            let new_line_starts = compute_line_starts(&new_source);

            // new_end_byte is the byte offset in the NEW document after the inserted text
            let new_end_byte = start_byte + new_text.as_bytes().len();

            // helper: convert byte offset in a document -> Point (line, utf16-col)
            fn byte_offset_to_point(
                text: &str,
                line_starts: &[usize],
                byte_offset: usize,
            ) -> Point {
                // find line (greatest index where line_start <= byte_offset)
                let line_idx =
                    match line_starts.binary_search(&(byte_offset + 1).saturating_sub(1)) {
                        Ok(i) => i,
                        Err(i) => i.saturating_sub(1),
                    }
                    .min(line_starts.len().saturating_sub(1));

                let line_start = line_starts[line_idx];
                let slice = &text[line_start..byte_offset.min(text.len())];

                // count utf16 code units in this slice to compute character (LSP uses UTF-16 column)
                let mut utf16 = 0usize;
                for ch in slice.chars() {
                    utf16 += ch.len_utf16();
                }
                Point::new(line_idx, utf16)
            }

            let start_position =
                Point::new(range.start.line as usize, range.start.character as usize);
            let old_end_position =
                Point::new(range.end.line as usize, range.end.character as usize);
            let new_end_position =
                byte_offset_to_point(&new_source, &new_line_starts, new_end_byte);

            let edit_details = InputEdit {
                start_byte: start_byte,
                old_end_byte: old_end_byte,
                new_end_byte: new_end_byte,
                start_position,
                old_end_position,
                new_end_position,
            };

            // tell tree-sitter about the edit (old doc coordinates)
            self.tree.edit(&edit_details);

            // finally replace our stored source and line starts with the new document
            self.source_code = new_source;
            self.line_starts = new_line_starts;
        } else {
            // replace whole document
            let new_source = new_text.to_string();
            let new_line_starts = compute_line_starts(&new_source);

            let edit_details = InputEdit {
                start_byte: 0,
                old_end_byte: old_source.len(),
                new_end_byte: new_source.len(),
                start_position: Point::new(0, 0),
                old_end_position: byte_offset_to_point(
                    &old_source,
                    &old_line_starts,
                    old_source.len(),
                ),
                new_end_position: byte_offset_to_point(
                    &new_source,
                    &new_line_starts,
                    new_source.len(),
                ),
            };

            self.tree.edit(&edit_details);
            self.source_code = new_source;
            self.line_starts = new_line_starts;
        }

        // reparse (use previous tree as input to incremental parse)
        if let Some(tree) = self.parser.parse(&self.source_code, Some(&self.tree)) {
            self.tree = tree;
        }
    }

    pub fn completion(&mut self, position: &Position) -> Vec<CompletionItem> {
        self.model.build_model(&self.tree, &self.source_code);

        let scope = {
            self.model
                .accumulate_cursor_scope(&self.line_starts, &self.source_code, position)
        };

        let items = self.model.get_completion_symbols(scope);

        items
            .iter()
            .map(|x| {
                if x.label == "self" {
                    CompletionItem {
                        label: "self".to_string(),
                        kind: Some(CompletionItemKind::CLASS),
                        label_details: Some(CompletionItemLabelDetails {
                            detail: None,
                            description: Some("class object".to_string()),
                        }),
                        ..Default::default()
                    }
                } else {
                    x.clone()
                }
            })
            .rev()
            .collect()
    }

    pub fn member_access(&mut self, object: &str) -> Vec<CompletionItem> {
        self.model.build_model(&self.tree, &self.source_code);

        self.model.get_member_symbols(object)
    }

    pub fn get_object_type(&mut self, object: &str, position: &Position) -> Option<String> {
        let scopes = {
            self.model
                .accumulate_cursor_scope(&self.line_starts, &self.source_code, position)
        };
        for scope in scopes {
            for i in scope.symbols.iter() {
                if let Symbol::Variable {
                    name,
                    optional_type,
                } = i
                    && name == object
                {
                    return optional_type.clone();
                }
                if let Symbol::Class { name, .. } = i
                    && name == object
                {
                    return Some(name.clone());
                }
                if let Symbol::Function { name, .. } = i
                    && name == object
                {
                    return Some(name.clone());
                }
            }
        }
        None
    }

    pub fn get_member_object(&self, source_code: &str, position: &Position) -> String {
        let line = source_code.lines().nth(position.line as usize).unwrap();
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

        line[start..dot_pos].to_string()
    }

    pub fn get_static_member_object(&self, source_code: &str, position: &Position) -> String {
        let line = source_code.lines().nth(position.line as usize).unwrap();
        let col = position.character as usize;
        let dot_pos = col.saturating_sub(1);

        // GO BACK AND GET OBJECT
        if &line[dot_pos - 1..dot_pos] != ":" {
            return String::new();
        }
        let mut start = dot_pos - 1;
        while start > 0 {
            let ch = line.as_bytes()[start - 1];
            if !matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_') {
                break;
            }
            start -= 1;
        }

        line[start..dot_pos - 1].to_string()
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
    let line_start = *line_starts.get(line)?; // return None if line out of range

    // Find line end (or EOF)
    let line_end = line_starts.get(line + 1).copied().unwrap_or(text.len());
    let line_text = &text[line_start..line_end];

    let mut utf16 = 0u32;

    for (byte_idx, ch) in line_text.char_indices() {
        let len16 = ch.len_utf16() as u32;
        // if the requested character index falls inside this `ch`, return its byte offset
        if utf16 + len16 > character {
            return Some(line_start + byte_idx);
        }
        utf16 += len16;
    }

    // If character is at or beyond the end of the line, clamp to end-of-line byte offset
    Some(line_start + line_text.len())
}

/// Convert a byte offset in `text` -> `Point { row, column }`.
/// - `line_starts` should be a list of byte indices where each line begins (first element usually 0).
/// - `byte_offset` will be clamped to `text.len()`.
pub fn byte_offset_to_point(text: &str, line_starts: &[usize], byte_offset: usize) -> Point {
    if line_starts.is_empty() {
        return Point::new(0, 0);
    }

    let byte_offset = byte_offset.min(text.len());

    // binary search: find greatest index i where line_starts[i] <= byte_offset
    let line = match line_starts.binary_search(&byte_offset) {
        Ok(i) => i,      // exact match: start of a line
        Err(0) => 0,     // before the first start (shouldn't happen if first = 0)
        Err(i) => i - 1, // belongs to previous line
    }
    .min(line_starts.len() - 1);

    let line_start = line_starts[line];
    let column = byte_offset.saturating_sub(line_start);

    Point::new(line, column)
}
