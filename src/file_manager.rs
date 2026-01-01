use crate::file_state::FileState;
use std::collections::HashMap;
use tower_lsp::lsp_types::Url;

pub struct FileManager {
    files: HashMap<Url, FileState>,
}

impl FileManager {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    pub fn open_file(&mut self, url: Url, text: String) {
        self.files.insert(url, FileState::new(text));
    }

    pub fn close_file(&mut self, url: Url) {
        self.files.remove(&url);
    }

    pub fn get_file_mut(&mut self, url: &Url) -> Option<&mut FileState> {
        self.files.get_mut(url)
    }

	#[allow(unused)]
    pub fn get_file(&mut self, url: &Url) -> Option<&FileState> {
        self.files.get(url)
    }
}
