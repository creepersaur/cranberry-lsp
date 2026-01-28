use language_server::CranberryLsp;
use std::sync::Arc;
use tower_lsp::{LspService, Server};

mod file_manager;
mod file_state;
mod language_model;
mod language_server;
mod macros;
mod logger;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let create_lsp = |client| Arc::new(CranberryLsp::new(client));
    let (service, socket) = LspService::new(create_lsp);
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}
