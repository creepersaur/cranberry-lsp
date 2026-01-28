use log::{Log, Record, Metadata};
use tower_lsp::{Client, lsp_types::MessageType};

pub struct LspLogger {
    pub client: Client,
}

impl Log for LspLogger {
    fn enabled(&self, _: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        if !self.enabled(record.metadata()) {
            return;
        }

        let msg = format!("[{}] {}", record.level(), record.args());
        let client = self.client.clone();

        tokio::spawn(async move {
            let _ = client
                .log_message(MessageType::LOG, msg)
                .await;
        });
    }

    fn flush(&self) {}
}
