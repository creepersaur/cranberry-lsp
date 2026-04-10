use language_server::IgniteLsp;
use std::sync::Arc;
use tower_lsp::{LspService, Server};

mod files;
mod language_server;
mod logger;
mod macros;
mod model;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let create_lsp = |client| Arc::new(IgniteLsp::new(client));
    let (service, socket) = LspService::new(create_lsp);
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}
