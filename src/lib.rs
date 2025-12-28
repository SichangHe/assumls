use core::convert::TryInto;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use serde_json::Value;
use tokio::io::{stdin, stdout};
use tokio::sync::Mutex;
use tokio::task::spawn_blocking;
use tokio_gen_server::actor::{Actor, ActorEnv, ActorExt, ActorRef};
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server, async_trait};
use walkdir::WalkDir;

type RpcResult<T> = jsonrpc::Result<T>;

#[derive(Debug, Clone)]
struct AssumptionDoc {
    name: String,
    heading: String,
    body: String,
}

#[derive(Debug, Clone)]
struct TagHit {
    name: String,
    range: Range,
}

#[derive(Debug, Default)]
struct IndexState {
    scope_docs: HashMap<PathBuf, HashMap<String, AssumptionDoc>>,
    file_scope: HashMap<PathBuf, PathBuf>,
    tags: HashMap<PathBuf, Vec<TagHit>>,
}

impl IndexState {
    fn docs_for_path(&self, path: &Path) -> Option<&HashMap<String, AssumptionDoc>> {
        let scope = self.file_scope.get(path)?;
        self.scope_docs.get(scope)
    }

    fn tag_at(&self, path: &Path, position: Position) -> Option<TagHit> {
        let hits = self.tags.get(path)?;
        hits.iter()
            .find(|hit| contains(&hit.range, position))
            .cloned()
    }

    fn hover(&self, path: &Path, position: Position) -> Option<Hover> {
        let tag = self.tag_at(path, position)?;
        let docs = self.docs_for_path(path)?;
        let doc = docs.get(&tag.name)?;
        let value = format!("## {}\n{}", doc.heading, doc.body);
        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value,
            }),
            range: Some(tag.range),
        })
    }

    fn completion(&self, path: &Path) -> Vec<CompletionItem> {
        let Some(docs) = self.docs_for_path(path) else {
            return Vec::new();
        };
        docs.values()
            .map(|doc| CompletionItem {
                label: doc.name.clone(),
                detail: Some(doc.heading.clone()),
                kind: Some(CompletionItemKind::TEXT),
                ..CompletionItem::default()
            })
            .collect()
    }

    fn rename(&self, path: &Path, position: Position, new_name: String) -> Option<WorkspaceEdit> {
        let tag = self.tag_at(path, position)?;
        let scope = self.file_scope.get(path)?;
        let edits: HashMap<Url, Vec<TextEdit>> = self
            .tags
            .iter()
            .filter(|(p, _)| self.file_scope.get(*p) == Some(scope))
            .flat_map(|(p, hits)| {
                let url = Url::from_file_path(p).ok()?;
                let doc_edits: Vec<TextEdit> = hits
                    .iter()
                    .filter(|hit| hit.name == tag.name)
                    .map(|hit| TextEdit {
                        range: hit.range,
                        new_text: new_name.clone(),
                    })
                    .collect();
                if doc_edits.is_empty() {
                    None
                } else {
                    Some((url, doc_edits))
                }
            })
            .collect();
        if edits.is_empty() {
            None
        } else {
            Some(WorkspaceEdit {
                changes: Some(edits),
                document_changes: None,
                change_annotations: None,
            })
        }
    }
}

#[derive(Debug)]
struct AssumptionIndex {
    state: Option<IndexState>,
}

impl AssumptionIndex {
    fn new() -> Self {
        Self { state: None }
    }
}

enum IndexCall {
    Refresh(PathBuf),
    Hover {
        uri: Url,
        position: Position,
    },
    Completion {
        uri: Url,
    },
    Rename {
        uri: Url,
        position: Position,
        new_name: String,
    },
}

enum IndexReply {
    Refreshed,
    Hover(Option<Hover>),
    Completion(Vec<CompletionItem>),
    Rename(Option<WorkspaceEdit>),
}

#[allow(clippy::manual_async_fn)]
impl Actor for AssumptionIndex {
    type Call = IndexCall;
    type Cast = ();
    type Reply = IndexReply;

    fn init(
        &mut self,
        _env: &mut ActorEnv<Self>,
    ) -> impl std::future::Future<Output = Result<()>> + Send {
        async { Ok(()) }
    }

    fn handle_cast(
        &mut self,
        _msg: Self::Cast,
        _env: &mut ActorEnv<Self>,
    ) -> impl std::future::Future<Output = Result<()>> + Send {
        async { Ok(()) }
    }

    fn handle_call(
        &mut self,
        msg: Self::Call,
        _env: &mut ActorEnv<Self>,
        reply_sender: tokio::sync::oneshot::Sender<Self::Reply>,
    ) -> impl std::future::Future<Output = Result<()>> + Send {
        async move {
            match msg {
                IndexCall::Refresh(root) => {
                    let state = build_index(root).await?;
                    self.state = Some(state);
                    let _ = reply_sender.send(IndexReply::Refreshed);
                }
                IndexCall::Hover { uri, position } => {
                    let hover = self.state.as_ref().and_then(|state| {
                        uri.to_file_path()
                            .ok()
                            .and_then(|p| state.hover(&p, position))
                    });
                    let _ = reply_sender.send(IndexReply::Hover(hover));
                }
                IndexCall::Completion { uri } => {
                    let items = self
                        .state
                        .as_ref()
                        .map(|state| {
                            uri.to_file_path()
                                .ok()
                                .map(|p| state.completion(&p))
                                .unwrap_or_default()
                        })
                        .unwrap_or_default();
                    let _ = reply_sender.send(IndexReply::Completion(items));
                }
                IndexCall::Rename {
                    uri,
                    position,
                    new_name,
                } => {
                    let edit = self.state.as_ref().and_then(|state| {
                        uri.to_file_path()
                            .ok()
                            .and_then(|p| state.rename(&p, position, new_name))
                    });
                    let _ = reply_sender.send(IndexReply::Rename(edit));
                }
            }
            Ok(())
        }
    }
}

struct Backend {
    client: Client,
    index: ActorRef<AssumptionIndex>,
    root: Mutex<Option<PathBuf>>,
}

impl Backend {
    fn new(client: Client, index: ActorRef<AssumptionIndex>) -> Self {
        Self {
            client,
            index,
            root: Mutex::new(None),
        }
    }

    async fn set_root(&self, root: PathBuf) {
        let mut guard = self.root.lock().await;
        *guard = Some(root);
    }

    fn capabilities() -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec!["@".into()]),
                ..CompletionOptions::default()
            }),
            rename_provider: Some(OneOf::Left(true)),
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
        let root = params
            .root_uri
            .and_then(|uri| uri.to_file_path().ok())
            .or_else(|| {
                params
                    .workspace_folders
                    .and_then(|mut folders| folders.pop())
                    .and_then(|folder| folder.uri.to_file_path().ok())
            })
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));
        self.set_root(root.clone()).await;
        self.index
            .call(IndexCall::Refresh(root))
            .await
            .map_err(Self::internal_error)?;
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
            Ok(Some(CompletionResponse::Array(items)))
        } else {
            Ok(None)
        }
    }

    async fn rename(&self, params: RenameParams) -> RpcResult<Option<WorkspaceEdit>> {
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
}

pub async fn run_stdio() -> RpcResult<()> {
    let (_actor_handle, index) = AssumptionIndex::new().spawn();
    let (service, socket) = LspService::new(|client| Backend::new(client, index.clone()));
    Server::new(stdin(), stdout(), socket).serve(service).await;
    Ok(())
}

fn contains(range: &Range, position: Position) -> bool {
    let start_before = position.line > range.start.line
        || (position.line == range.start.line && position.character >= range.start.character);
    let end_after = position.line < range.end.line
        || (position.line == range.end.line && position.character <= range.end.character);
    start_before && end_after
}

async fn build_index(root: PathBuf) -> Result<IndexState> {
    let state = spawn_blocking(move || collect_index(root))
        .await
        .map_err(anyhow::Error::from)??;
    Ok(state)
}

fn collect_index(root: PathBuf) -> Result<IndexState> {
    let mut state = IndexState::default();
    let mut scope_roots = HashSet::new();
    let mut assum_files = Vec::new();
    for entry in WalkDir::new(&root)
        .into_iter()
        .filter_entry(|e| !is_ignored(e.path()))
    {
        let entry = entry?;
        if entry.file_type().is_file() && entry.file_name() == "ASSUM.md" {
            assum_files.push(entry.path().to_path_buf());
        }
    }
    for path in assum_files {
        let docs = parse_assumptions(&path)?;
        let scope = path.parent().unwrap_or(&root).to_path_buf();
        scope_roots.insert(scope.clone());
        state.scope_docs.insert(
            scope,
            docs.into_iter()
                .map(|doc| (doc.name.clone(), doc))
                .collect(),
        );
    }
    for entry in WalkDir::new(&root)
        .into_iter()
        .filter_entry(|e| !is_ignored(e.path()))
    {
        let entry = entry?;
        if entry.file_type().is_dir() {
            continue;
        }
        let path = entry.path();
        if path.file_name() == Some(std::ffi::OsStr::new("ASSUM.md")) {
            continue;
        }
        let Some(scope) = find_scope(&scope_roots, path) else {
            continue;
        };
        let hits = scan_tags(path)?;
        state.file_scope.insert(path.to_path_buf(), scope);
        if !hits.is_empty() {
            state.tags.insert(path.to_path_buf(), hits);
        }
    }
    Ok(state)
}

fn parse_assumptions(path: &Path) -> Result<Vec<AssumptionDoc>> {
    let content =
        std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    let mut docs = Vec::new();
    let mut current: Option<(String, Vec<String>)> = None;
    for line in content.lines() {
        if let Some(rest) = line.strip_prefix('#') {
            let name = rest.trim().to_string();
            if let Some((heading, body)) = current.take() {
                docs.push(AssumptionDoc {
                    name: heading.clone(),
                    heading,
                    body: body.join("\n"),
                });
            }
            current = Some((name, Vec::new()));
        } else if let Some((_, body)) = current.as_mut() {
            body.push(line.to_string());
        }
    }
    if let Some((heading, body)) = current.take() {
        docs.push(AssumptionDoc {
            name: heading.clone(),
            heading,
            body: body.join("\n"),
        });
    }
    Ok(docs)
}

fn scan_tags(path: &Path) -> Result<Vec<TagHit>> {
    let content =
        std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    let mut hits = Vec::new();
    for (line_idx, line) in content.lines().enumerate() {
        let mut offset = 0;
        while let Some(pos) = line[offset..].find("@ASSUME ") {
            let start = offset + pos;
            let rest = &line[start + 8..];
            let name: String = rest
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '-')
                .collect();
            if name.is_empty() {
                offset = start + 8;
                continue;
            }
            let range = Range {
                start: Position {
                    line: line_idx as u32,
                    character: start as u32,
                },
                end: Position {
                    line: line_idx as u32,
                    character: (start + 8 + name.len()).try_into().unwrap(),
                },
            };
            hits.push(TagHit { name, range });
            offset = start + 8;
        }
    }
    Ok(hits)
}

fn find_scope(scope_roots: &HashSet<PathBuf>, path: &Path) -> Option<PathBuf> {
    let mut current = path.parent();
    while let Some(dir) = current {
        if scope_roots.contains(dir) {
            return Some(dir.to_path_buf());
        }
        current = dir.parent();
    }
    None
}

fn is_ignored(path: &Path) -> bool {
    let Some(name) = path.file_name().and_then(|s| s.to_str()) else {
        return false;
    };
    matches!(
        name,
        ".git" | "target" | ".vscode" | ".idea" | "node_modules"
    )
}
