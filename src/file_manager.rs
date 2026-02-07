use crate::file_state::FileState;
use log::error;
use std::{collections::HashMap, fs, path::PathBuf};
use tower_lsp::lsp_types::Url;

pub struct FileManager {
    pub files: HashMap<Url, FileState>,
    pub root: PathBuf,
}

impl FileManager {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            root: PathBuf::new(),
        }
    }

    pub fn set_root(&mut self, root: PathBuf) {
        self.root = root;
    }

    pub fn scan_src(&mut self) {
        let src = self.root.join("src");

        if !src.exists() {
            error!("`src/` directory does not exist");
            return;
        }

        self.scan_dir(&src);
    }

    pub fn scan_dir(&mut self, dir: &PathBuf) {
        let entries = match fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };

        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_dir() {
                self.scan_dir(&path);
                continue;
            }

            if path.extension().and_then(|e| e.to_str()) == Some("cb") {
                let text = match fs::read_to_string(&path) {
                    Ok(text) => text,
                    Err(e) => return error!("{e}"),
                };
                self.open_file(Url::from_file_path(path).unwrap(), text);
            }
        }
    }

    pub fn get_files(&mut self) -> std::collections::hash_map::ValuesMut<'_, Url, FileState> {
        self.files.values_mut()
    }

    pub fn open_file(&mut self, url: Url, text: String) {
        self.files.insert(url.clone(), FileState::new(text, url));
    }

	#[allow(unused)]
    pub fn close_file(&mut self, url: Url) {
        // self.files.remove(&url);
    }

	pub fn delete_file(&mut self, url: Url) {
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
