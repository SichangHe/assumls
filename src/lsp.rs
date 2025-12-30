use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use serde_json::Value;
use tokio::io::{stdin, stdout};
use tokio::sync::mpsc::{UnboundedSender, unbounded_channel};
use tokio_gen_server::actor::ActorExt;
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server, async_trait};
use tracing::error;

use crate::index::{AssumptionIndex, AssumptionIndexRef, DiagnosticsMap, IndexCall, IndexReply};
use crate::model::{AssumptionDiagnostic, DiagSeverity};
use crate::parser::is_valid_assumption_name;

pub type RpcResult<T> = jsonrpc::Result<T>;

struct DiagnosticsSink {
    tx: UnboundedSender<DiagnosticsMap>,
}

impl DiagnosticsSink {
    fn new(client: Client) -> Self {
        let (tx, mut rx): (UnboundedSender<DiagnosticsMap>, _) = unbounded_channel();
        tokio::spawn(async move {
            let mut last = HashSet::new();
            while let Some(diags) = rx.recv().await {
                let mut seen = HashSet::new();
                for (path, ds) in diags {
                    let Ok(uri) = Url::from_file_path(&path) else {
                        continue;
                    };
                    let lsp_diags: Vec<Diagnostic> = ds
                        .iter()
                        .map(|d: &AssumptionDiagnostic| to_lsp_diag(d))
                        .collect();
                    let _ = client
                        .publish_diagnostics(uri.clone(), lsp_diags, None)
                        .await;
                    seen.insert(uri);
                }
                for uri in last.difference(&seen) {
                    let _ = client
                        .publish_diagnostics(uri.clone(), Vec::new(), None)
                        .await;
                }
                last = seen;
            }
        });
        Self { tx }
    }

    fn send(&self, diags: DiagnosticsMap) {
        let _ = self.tx.send(diags);
    }
}

/// LSP backend managing overlays, indexing, and client IO.
pub struct Backend {
    /// LSP client handle.
    client: Client,
    /// Actor reference for index operations.
    index: AssumptionIndexRef,
    /// Diagnostic publishing sink.
    diagnostics: DiagnosticsSink,
    /// Optional CLI-provided root override.
    fallback_root: Option<PathBuf>,
}

impl Backend {
    pub fn new(client: Client, index: AssumptionIndexRef, fallback_root: Option<PathBuf>) -> Self {
        let diagnostics = DiagnosticsSink::new(client.clone());
        Self {
            client,
            index,
            diagnostics,
            fallback_root,
        }
    }

    async fn refresh_with(&self, call: IndexCall) -> RpcResult<DiagnosticsMap> {
        let reply = self.index.call(call).await.map_err(Self::internal_error)?;
        match reply {
            IndexReply::Refreshed(diags) | IndexReply::Diagnostics(diags) => Ok(diags),
            _ => Ok(HashMap::new()),
        }
    }

    async fn set_root(&self, root: PathBuf) -> RpcResult<DiagnosticsMap> {
        self.refresh_with(IndexCall::Initialize { root }).await
    }

    async fn apply_overlay(&self, uri: &Url, text: String) -> RpcResult<DiagnosticsMap> {
        let Some(path) = uri.to_file_path().ok() else {
            return Ok(HashMap::new());
        };
        self.refresh_with(IndexCall::ApplyOverlay { path, text })
            .await
    }

    async fn drop_overlay(&self, uri: &Url) -> RpcResult<DiagnosticsMap> {
        let Some(path) = uri.to_file_path().ok() else {
            return Ok(HashMap::new());
        };
        self.refresh_with(IndexCall::DropOverlay { path }).await
    }

    async fn refresh(&self) -> RpcResult<DiagnosticsMap> {
        self.refresh_with(IndexCall::Refresh).await
    }

    fn capabilities() -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec!["@".into(), ":".into()]),
                ..CompletionOptions::default()
            }),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Left(true)),
            document_highlight_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    work_done_progress_options: Default::default(),
                    legend: SemanticTokensLegend {
                        token_types: vec![SemanticTokenType::MACRO, SemanticTokenType::VARIABLE],
                        token_modifiers: Vec::new(),
                    },
                    range: None,
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                }),
            ),
            ..ServerCapabilities::default()
        }
    }

    fn internal_error(err: anyhow::Error) -> jsonrpc::Error {
        jsonrpc::Error {
            code: jsonrpc::ErrorCode::InternalError,
            message: "internal error".into(),
            data: Some(Value::String(err.to_string())),
        }
    }
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> RpcResult<InitializeResult> {
        let root = self
            .fallback_root
            .clone()
            .or_else(|| params.root_uri.and_then(|uri| uri.to_file_path().ok()))
            .or_else(|| {
                params
                    .workspace_folders
                    .and_then(|mut folders| folders.pop())
                    .and_then(|folder| folder.uri.to_file_path().ok())
            })
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
        let diags = self.set_root(root).await?;
        self.diagnostics.send(diags);
        Ok(InitializeResult {
            capabilities: Self::capabilities(),
            server_info: Some(ServerInfo {
                name: "AssumLS".into(),
                version: Some("0.0.0".into()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let _ = self
            .client
            .log_message(MessageType::INFO, "AssumLS initialized")
            .await;
    }

    async fn shutdown(&self) -> RpcResult<()> {
        Ok(())
    }

    async fn hover(&self, params: HoverParams) -> RpcResult<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let reply = self
            .index
            .call(IndexCall::Hover { uri, position })
            .await
            .map_err(Self::internal_error)?;
        if let IndexReply::Hover(h) = reply {
            Ok(h)
        } else {
            Ok(None)
        }
    }

    async fn completion(&self, params: CompletionParams) -> RpcResult<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let reply = self
            .index
            .call(IndexCall::Completion { uri })
            .await
            .map_err(Self::internal_error)?;
        if let IndexReply::Completion(items) = reply {
            Ok(items)
        } else {
            Ok(None)
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> RpcResult<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let reply = self
            .index
            .call(IndexCall::Definition { uri, position })
            .await
            .map_err(Self::internal_error)?;
        if let IndexReply::Definition(locs) = reply {
            if locs.is_empty() {
                Ok(None)
            } else {
                Ok(Some(GotoDefinitionResponse::Array(locs)))
            }
        } else {
            Ok(None)
        }
    }

    async fn references(&self, params: ReferenceParams) -> RpcResult<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;
        let reply = self
            .index
            .call(IndexCall::References {
                uri,
                position,
                include_declaration,
            })
            .await
            .map_err(Self::internal_error)?;
        if let IndexReply::References(locs) = reply {
            if locs.is_empty() {
                Ok(None)
            } else {
                Ok(Some(locs))
            }
        } else {
            Ok(None)
        }
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> RpcResult<Option<Vec<DocumentHighlight>>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let reply = self
            .index
            .call(IndexCall::DocumentHighlight { uri, position })
            .await
            .map_err(Self::internal_error)?;
        if let IndexReply::DocumentHighlight(highlights) = reply {
            if highlights.is_empty() {
                Ok(None)
            } else {
                Ok(Some(highlights))
            }
        } else {
            Ok(None)
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> RpcResult<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let reply = self
            .index
            .call(IndexCall::SemanticTokens { uri })
            .await
            .map_err(Self::internal_error)?;
        if let IndexReply::SemanticTokens(tokens) = reply {
            Ok(tokens.map(SemanticTokensResult::Tokens))
        } else {
            Ok(None)
        }
    }

    async fn rename(&self, params: RenameParams) -> RpcResult<Option<WorkspaceEdit>> {
        if !is_valid_assumption_name(&params.new_name) {
            return Err(jsonrpc::Error {
                code: jsonrpc::ErrorCode::InvalidParams,
                message: "assumption names must be snake_case starting with a letter".into(),
                data: None,
            });
        }
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let reply = self
            .index
            .call(IndexCall::Rename {
                uri,
                position,
                new_name: params.new_name,
            })
            .await
            .map_err(Self::internal_error)?;
        if let IndexReply::Rename(edit) = reply {
            Ok(edit)
        } else {
            Ok(None)
        }
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        match self
            .apply_overlay(&params.text_document.uri, params.text_document.text)
            .await
        {
            Ok(diags) => self.diagnostics.send(diags),
            Err(err) => error!(error = %err, "index refresh failed"),
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            match self
                .apply_overlay(&params.text_document.uri, change.text)
                .await
            {
                Ok(diags) => self.diagnostics.send(diags),
                Err(err) => error!(error = %err, "index refresh failed"),
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        match self.drop_overlay(&params.text_document.uri).await {
            Ok(diags) => self.diagnostics.send(diags),
            Err(err) => error!(error = %err, "index refresh failed"),
        }
    }

    async fn did_save(&self, _params: DidSaveTextDocumentParams) {
        match self.refresh().await {
            Ok(diags) => self.diagnostics.send(diags),
            Err(err) => error!(error = %err, "index refresh failed"),
        }
    }
}

/// Run LSP server over stdio.
pub async fn run_stdio(fallback_root: Option<PathBuf>) -> RpcResult<()> {
    let (_actor_handle, index) = AssumptionIndex::new().spawn();
    let fallback = fallback_root.clone();
    let (service, socket) =
        LspService::new(move |client| Backend::new(client, index.clone(), fallback.clone()));
    Server::new(stdin(), stdout(), socket).serve(service).await;
    Ok(())
}

/// Convert internal diagnostic to an LSP diagnostic.
fn to_lsp_diag(diag: &AssumptionDiagnostic) -> Diagnostic {
    let is_unused = diag.message.contains("unused");
    Diagnostic {
        range: diag.range,
        severity: Some(match diag.severity {
            DiagSeverity::Error => DiagnosticSeverity::ERROR,
            DiagSeverity::Warning => {
                if is_unused {
                    DiagnosticSeverity::HINT
                } else {
                    DiagnosticSeverity::WARNING
                }
            }
        }),
        code: None,
        code_description: None,
        source: Some("AssumLS".into()),
        message: diag.message.clone(),
        related_information: None,
        tags: if is_unused {
            Some(vec![DiagnosticTag::UNNECESSARY])
        } else {
            None
        },
        data: None,
    }
}
