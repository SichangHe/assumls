use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result};
use serde::Deserialize;
use tokio::task::spawn_blocking;
use tokio_gen_server::actor::{Actor, ActorEnv, ActorRef};
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionResponse, CompletionTextEdit, DocumentHighlight,
    DocumentHighlightKind, Documentation, Hover, HoverContents, Location, MarkupContent,
    MarkupKind, Position, Range, SemanticTokens, TextEdit, Url, WorkspaceEdit,
};
use tracing::{error, info};

use crate::model::{AssumptionDiagnostic, AssumptionDoc, DiagSeverity, TagHit};
use crate::parser::{
    contains, content_for, find_scope, parse_assumptions_content, scan_tags_content,
};

pub type DiagnosticsMap = HashMap<PathBuf, Vec<AssumptionDiagnostic>>;
type FileScopeMap = HashMap<PathBuf, PathBuf>;
type TagHitsMap = HashMap<PathBuf, Vec<TagHit>>;
type TagScanMaps = (FileScopeMap, TagHitsMap);

type RpcResult<T> = Result<T>;

/// Cached index of assumptions, tags, and diagnostics.
#[derive(Debug, Default)]
pub struct IndexState {
    /// Docs keyed by scope root and assumption name.
    scope_docs: HashMap<PathBuf, HashMap<String, AssumptionDoc>>,
    /// Duplicate definitions keyed by scope root.
    duplicate_defs: HashMap<PathBuf, Vec<AssumptionDoc>>,
    /// Mapping from file path to its scope root.
    file_scope: HashMap<PathBuf, PathBuf>,
    /// Tag occurrences per file.
    tags: HashMap<PathBuf, Vec<TagHit>>,
    /// Diagnostics per file.
    diagnostics: DiagnosticsMap,
    /// Scope roots containing ASSUM.md files.
    scope_roots: HashSet<PathBuf>,
}

impl IndexState {
    fn scope_for(&self, path: &Path) -> Option<PathBuf> {
        let norm = normalize_path(path.to_path_buf());
        self.file_scope
            .get(&norm)
            .cloned()
            .or_else(|| find_scope(&self.scope_roots, &norm))
    }

    fn docs_for_path(&self, path: &Path) -> Option<&HashMap<String, AssumptionDoc>> {
        let scope = self.scope_for(path)?;
        self.scope_docs.get(&scope)
    }

    fn tag_at(&self, path: &Path, position: Position) -> Option<TagHit> {
        let hits = self.tags.get(&normalize_path(path.to_path_buf()))?;
        hits.iter()
            .find(|hit| contains(&hit.range, position))
            .cloned()
    }

    pub fn hover(&self, path: &Path, position: Position) -> Option<Hover> {
        let tag = self.tag_at(path, position)?;
        let docs = self.docs_for_path(path)?;
        let doc = docs.get(&tag.name)?;
        let value = doc.body.clone();
        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value,
            }),
            range: Some(tag.range),
        })
    }

    pub fn completion(
        &self,
        path: &Path,
        ctx: Option<(Range, String)>,
    ) -> Option<CompletionResponse> {
        let mut typed = String::new();
        let mut replace_range = None;
        let include_prefix = ctx.is_none();
        if let Some((range, name)) = ctx {
            typed = name;
            replace_range = Some(range);
        }

        let mut items: Vec<CompletionItem> = if include_prefix {
            vec![CompletionItem {
                label: "ASSUME:".into(),
                insert_text: Some("ASSUME:".into()),
                detail: Some("Insert @ASSUME prefix.".into()),
                kind: Some(CompletionItemKind::KEYWORD),
                ..CompletionItem::default()
            }]
        } else {
            Vec::new()
        };

        let mut seen = HashSet::new();
        let docs: Vec<&AssumptionDoc> = if let Some(docs) = self.docs_for_path(path) {
            docs.values().collect()
        } else {
            self.scope_docs.values().flat_map(|m| m.values()).collect()
        };

        items.extend(docs.into_iter().filter_map(|doc| {
            if !typed.is_empty() && !doc.name.starts_with(&typed) {
                return None;
            }
            if !seen.insert(doc.name.clone()) {
                return None;
            }
            Some(CompletionItem {
                label: doc.name.clone(),
                insert_text: Some(doc.name.clone()),
                filter_text: Some(format!("@ASSUME:{}", doc.name)),
                detail: Some(doc.heading.clone()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc.body.clone(),
                })),
                kind: Some(CompletionItemKind::VARIABLE),
                text_edit: replace_range.as_ref().map(|range| {
                    CompletionTextEdit::Edit(TextEdit {
                        range: *range,
                        new_text: doc.name.clone(),
                    })
                }),
                ..CompletionItem::default()
            })
        }));
        if items.is_empty() {
            return None;
        }
        items.sort_by(|a, b| a.label.cmp(&b.label));
        Some(CompletionResponse::Array(items))
    }

    pub fn definition(&self, path: &Path, position: Position) -> Option<Vec<Location>> {
        let tag = self.tag_at(path, position)?;
        let doc = self
            .docs_for_path(path)
            .and_then(|docs| docs.get(&tag.name))?;
        let uri = Url::from_file_path(&doc.path).ok()?;
        Some(vec![Location {
            uri,
            range: doc.range,
        }])
    }

    pub fn references(
        &self,
        path: &Path,
        position: Position,
        include_declaration: bool,
    ) -> Option<Vec<Location>> {
        let tag = self.tag_at(path, position)?;
        let scope = self.scope_for(path)?;
        let decl = self
            .docs_for_path(path)
            .and_then(|docs| docs.get(&tag.name))
            .map(|doc| (normalize_path(doc.path.clone()), doc.range));
        let mut out = Vec::new();
        if include_declaration
            && let Some(doc) = self
                .docs_for_path(path)
                .and_then(|docs| docs.get(&tag.name))
            && let Ok(uri) = Url::from_file_path(&doc.path)
        {
            out.push(Location {
                uri,
                range: doc.range,
            });
        }
        for (p, hits) in self
            .tags
            .iter()
            .filter(|(p, _)| self.scope_for(p).as_ref() == Some(&scope))
        {
            let norm_path = normalize_path(p.clone());
            let Ok(uri) = Url::from_file_path(&norm_path) else {
                continue;
            };
            for hit in hits.iter().filter(|h| h.name == tag.name) {
                if let Some((decl_path, decl_range)) = &decl
                    && decl_path == &norm_path
                    && decl_range == &hit.range
                {
                    continue;
                }
                out.push(Location {
                    uri: uri.clone(),
                    range: hit.range,
                });
            }
        }
        Some(out)
    }

    pub fn highlights(&self, path: &Path, position: Position) -> Option<Vec<DocumentHighlight>> {
        let tag = self.tag_at(path, position)?;
        let mut out = Vec::new();
        out.push(DocumentHighlight {
            range: tag.range,
            kind: Some(DocumentHighlightKind::TEXT),
        });
        if tag.name_range != tag.range {
            out.push(DocumentHighlight {
                range: tag.name_range,
                kind: Some(DocumentHighlightKind::READ),
            });
        }
        Some(out)
    }

    pub fn semantic_tokens(&self, path: &Path) -> Option<SemanticTokens> {
        let mut hits = self.tags.get(&normalize_path(path.to_path_buf()))?.clone();
        hits.sort_by_key(|h| (h.range.start.line, h.range.start.character));
        let mut data: Vec<tower_lsp::lsp_types::SemanticToken> = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_start = 0u32;
        let mut first = true;
        for hit in hits {
            let keyword_len = hit
                .name_range
                .start
                .character
                .saturating_sub(hit.range.start.character);
            if keyword_len > 0 {
                let (dl, ds) = delta(
                    hit.range.start.line,
                    hit.range.start.character,
                    prev_line,
                    prev_start,
                    first,
                );
                push_token(&mut data, dl, ds, keyword_len, 0);
                prev_line = hit.range.start.line;
                prev_start = hit.range.start.character;
                first = false;
            }
            let name_len = hit
                .name_range
                .end
                .character
                .saturating_sub(hit.name_range.start.character);
            if name_len > 0 {
                let (dl, ds) = delta(
                    hit.name_range.start.line,
                    hit.name_range.start.character,
                    prev_line,
                    prev_start,
                    first,
                );
                push_token(&mut data, dl, ds, name_len, 1);
                prev_line = hit.name_range.start.line;
                prev_start = hit.name_range.start.character;
                first = false;
            }
        }
        Some(SemanticTokens {
            result_id: None,
            data,
        })
    }

    pub fn rename(
        &self,
        path: &Path,
        position: Position,
        new_name: String,
    ) -> Option<WorkspaceEdit> {
        let tag = self.tag_at(path, position)?;
        let scope = self.scope_for(path)?;
        let decl = self
            .docs_for_path(path)
            .and_then(|docs| docs.get(&tag.name))
            .map(|doc| (normalize_path(doc.path.clone()), doc.range));
        let mut edits: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        if let Some(docs) = self.docs_for_path(path)
            && let Some(doc) = docs.get(&tag.name)
            && let Ok(url) = Url::from_file_path(&doc.path)
        {
            edits.entry(url).or_default().push(TextEdit {
                range: doc.range,
                new_text: new_name.clone(),
            });
        }
        for (p, hits) in self
            .tags
            .iter()
            .filter(|(p, _)| self.scope_for(p).as_ref() == Some(&scope))
        {
            let norm_path = normalize_path(p.clone());
            let Some(url) = Url::from_file_path(&norm_path).ok() else {
                continue;
            };
            let tag_edits: Vec<TextEdit> = hits
                .iter()
                .filter(|hit| hit.name == tag.name)
                .filter(|hit| {
                    if let Some((decl_path, decl_range)) = &decl
                        && decl_path == &norm_path
                        && decl_range == &hit.range
                    {
                        return false;
                    }
                    true
                })
                .map(|hit| TextEdit {
                    range: hit.name_range,
                    new_text: new_name.clone(),
                })
                .collect();
            if tag_edits.is_empty() {
                continue;
            }
            edits.entry(url).or_default().extend(tag_edits);
        }
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

    pub fn diagnostics(&self) -> DiagnosticsMap {
        self.diagnostics.clone()
    }
}

fn delta(line: u32, start: u32, prev_line: u32, prev_start: u32, first: bool) -> (u32, u32) {
    if first {
        (line, start)
    } else if line == prev_line {
        (0, start.saturating_sub(prev_start))
    } else {
        (line.saturating_sub(prev_line), start)
    }
}

fn push_token(
    data: &mut Vec<tower_lsp::lsp_types::SemanticToken>,
    delta_line: u32,
    delta_start: u32,
    length: u32,
    token_type: u32,
) {
    data.push(tower_lsp::lsp_types::SemanticToken {
        delta_line,
        delta_start,
        length,
        token_type,
        token_modifiers_bitset: 0,
    });
}

/// Actor-owned index cache.
#[derive(Debug)]
pub struct AssumptionIndex {
    root: Option<PathBuf>,
    overlays: HashMap<PathBuf, String>,
    versions: HashMap<PathBuf, i32>,
    state: Option<IndexState>,
}

impl AssumptionIndex {
    pub fn new() -> Self {
        Self {
            root: None,
            overlays: HashMap::new(),
            versions: HashMap::new(),
            state: None,
        }
    }
    // @ASSUME:incremental_indexing
    async fn incremental_refresh(&mut self, changed_path: &Path) -> DiagnosticsMap {
        let Some(ref mut state) = self.state else {
            return self.refresh().await;
        };
        let changed_path = normalize_path(changed_path.to_path_buf());
        // Check if it's an ASSUM.md file
        if changed_path.file_name().and_then(|n| n.to_str()) == Some("ASSUM.md") {
            // Incrementally update definitions for this scope
            let content = if let Some(overlay) = self.overlays.get(&changed_path) {
                overlay.clone()
            } else {
                std::fs::read_to_string(&changed_path)
                    .ok()
                    .unwrap_or_default()
            };
            if let Ok(docs_vec) = parse_assumptions_content(&content, &changed_path) {
                let scope = changed_path
                    .parent()
                    .map(|p| normalize_path(p.to_path_buf()));
                if let Some(scope) = scope {
                    let docs_map: HashMap<String, AssumptionDoc> = docs_vec
                        .into_iter()
                        .map(|doc| (doc.name.clone(), doc))
                        .collect();
                    state.scope_docs.insert(scope, docs_map);
                }
            }
        } else {
            // Update tags for regular source file
            let content = if let Some(overlay) = self.overlays.get(&changed_path) {
                Some(overlay.clone())
            } else {
                std::fs::read_to_string(&changed_path).ok()
            };
            if let Some(content) = content {
                let tags = scan_tags_content(&content);
                state.tags.insert(changed_path.clone(), tags);
            } else {
                state.tags.remove(&changed_path);
            }
        }
        state.diagnostics()
    }
    fn completion_ctx(&self, path: &Path, position: Position) -> Option<(Range, String)> {
        let text = content_for(path, &self.overlays).ok()??;
        let line_idx = usize::try_from(position.line).ok()?;
        let char_idx = usize::try_from(position.character).ok()?;
        let line = text.lines().nth(line_idx)?;
        let prefix: String = line.chars().take(char_idx).collect();
        let marker = "@ASSUME:";
        let idx = match prefix.rfind(marker) {
            Some(i) => i,
            None => {
                info!(path = %path.display(), line = line_idx, character = char_idx, "completion_ctx missing marker");
                return None;
            }
        };
        let start_char = u32::try_from(idx + marker.len()).ok()?;
        Some((
            Range {
                start: Position {
                    line: position.line,
                    character: start_char,
                },
                end: position,
            },
            prefix[idx + marker.len()..].to_string(),
        ))
    }
}

impl Default for AssumptionIndex {
    fn default() -> Self {
        Self::new()
    }
}

pub enum IndexCall {
    /// Set workspace root and refresh.
    Initialize {
        root: PathBuf,
    },
    /// Apply overlay content for a file.
    ApplyOverlay {
        path: PathBuf,
        text: String,
        version: Option<i32>,
    },
    /// Drop overlay for a file.
    DropOverlay {
        path: PathBuf,
    },
    Refresh,
    Hover {
        uri: Url,
        position: Position,
    },
    Completion {
        uri: Url,
        position: Position,
    },
    Definition {
        uri: Url,
        position: Position,
    },
    References {
        uri: Url,
        position: Position,
        include_declaration: bool,
    },
    SemanticTokens {
        uri: Url,
    },
    DocumentHighlight {
        uri: Url,
        position: Position,
    },
    Rename {
        uri: Url,
        position: Position,
        new_name: String,
    },
    Diagnostics,
}

pub enum IndexReply {
    Refreshed(DiagnosticsMap),
    Hover(Option<Hover>),
    Completion(Option<CompletionResponse>),
    Definition(Vec<Location>),
    References(Vec<Location>),
    SemanticTokens(Option<SemanticTokens>),
    DocumentHighlight(Vec<DocumentHighlight>),
    Rename(Option<WorkspaceEdit>),
    Diagnostics(DiagnosticsMap),
}

#[allow(clippy::manual_async_fn)]
impl Actor for AssumptionIndex {
    type Call = IndexCall;
    type Cast = ();
    type Reply = IndexReply;

    fn init(
        &mut self,
        _env: &mut ActorEnv<Self>,
    ) -> impl std::future::Future<Output = RpcResult<()>> + Send {
        async { Ok(()) }
    }

    fn handle_cast(
        &mut self,
        _msg: Self::Cast,
        _env: &mut ActorEnv<Self>,
    ) -> impl std::future::Future<Output = RpcResult<()>> + Send {
        async { Ok(()) }
    }

    fn handle_call(
        &mut self,
        msg: Self::Call,
        _env: &mut ActorEnv<Self>,
        reply_sender: tokio::sync::oneshot::Sender<Self::Reply>,
    ) -> impl std::future::Future<Output = RpcResult<()>> + Send {
        async move {
            match msg {
                IndexCall::Initialize { root } => {
                    self.root = Some(root);
                    let diags = self.refresh().await;
                    let _ = reply_sender.send(IndexReply::Refreshed(diags));
                }
                IndexCall::ApplyOverlay {
                    path,
                    text,
                    version,
                } => {
                    let path = normalize_path(path);
                    let stale = match version {
                        Some(v) => match self.versions.get(&path).copied() {
                            Some(prev) if v <= prev => true,
                            _ => {
                                self.versions.insert(path.clone(), v);
                                false
                            }
                        },
                        None => false,
                    };
                    let diags = if stale {
                        info!(path = %path.display(), version, prev_version = self.versions.get(&path), "dropping out-of-order overlay");
                        self.state
                            .as_ref()
                            .map(|s| s.diagnostics())
                            .unwrap_or_default()
                    } else {
                        self.overlays.insert(path.clone(), text);
                        // @ASSUME:incremental_indexing
                        self.incremental_refresh(&path).await
                    };
                    let _ = reply_sender.send(IndexReply::Refreshed(diags));
                }
                IndexCall::DropOverlay { path } => {
                    let path = normalize_path(path);
                    self.overlays.remove(&path);
                    self.versions.remove(&path);
                    let diags = self.refresh().await;
                    let _ = reply_sender.send(IndexReply::Refreshed(diags));
                }
                IndexCall::Refresh => {
                    let diags = self.refresh().await;
                    let _ = reply_sender.send(IndexReply::Refreshed(diags));
                }
                IndexCall::Hover { uri, position } => {
                    let hover = self.state.as_ref().and_then(|state| {
                        uri.to_file_path()
                            .ok()
                            .and_then(|p| state.hover(&p, position))
                    });
                    let _ = reply_sender.send(IndexReply::Hover(hover));
                }
                IndexCall::Completion { uri, position } => {
                    let mut path_str = String::new();
                    let mut ctx_found = false;
                    let has_state = self.state.is_some();
                    let items = self.state.as_ref().and_then(|state| {
                        uri.to_file_path().ok().and_then(|p| {
                            path_str = p.display().to_string();
                            let ctx = self.completion_ctx(&p, position);
                            ctx_found = ctx.is_some();
                            state.completion(&p, ctx)
                        })
                    });
                    let count = items
                        .as_ref()
                        .map(|r| match r {
                            CompletionResponse::Array(v) => v.len(),
                            _ => 0,
                        })
                        .unwrap_or(0);
                    info!(
                        uri = %uri,
                        path = %path_str,
                        line = position.line,
                        character = position.character,
                        has_state,
                        ctx_found,
                        items = count,
                        "completion handled"
                    );
                    let _ = reply_sender.send(IndexReply::Completion(items));
                }
                IndexCall::Definition { uri, position } => {
                    let locs = self.state.as_ref().and_then(|state| {
                        uri.to_file_path()
                            .ok()
                            .and_then(|p| state.definition(&p, position))
                    });
                    let _ = reply_sender.send(IndexReply::Definition(locs.unwrap_or_default()));
                }
                IndexCall::References {
                    uri,
                    position,
                    include_declaration,
                } => {
                    let locs = self.state.as_ref().and_then(|state| {
                        uri.to_file_path()
                            .ok()
                            .and_then(|p| state.references(&p, position, include_declaration))
                    });
                    let _ = reply_sender.send(IndexReply::References(locs.unwrap_or_default()));
                }
                IndexCall::SemanticTokens { uri } => {
                    let tokens = self.state.as_ref().and_then(|state| {
                        uri.to_file_path()
                            .ok()
                            .and_then(|p| state.semantic_tokens(&p))
                    });
                    let _ = reply_sender.send(IndexReply::SemanticTokens(tokens));
                }
                IndexCall::DocumentHighlight { uri, position } => {
                    let highlights = self.state.as_ref().and_then(|state| {
                        uri.to_file_path()
                            .ok()
                            .and_then(|p| state.highlights(&p, position))
                    });
                    let _ = reply_sender.send(IndexReply::DocumentHighlight(
                        highlights.unwrap_or_default(),
                    ));
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
                IndexCall::Diagnostics => {
                    let diags = self
                        .state
                        .as_ref()
                        .map(|s| s.diagnostics())
                        .unwrap_or_default();
                    let _ = reply_sender.send(IndexReply::Diagnostics(diags));
                }
            }
            Ok(())
        }
    }
}

/// Build index on a blocking thread from root plus overlays.
pub async fn build_index(root: PathBuf, overlays: HashMap<PathBuf, String>) -> Result<IndexState> {
    ensure_tools_available()?;
    let state = spawn_blocking(move || collect_index(root, overlays)).await??;
    Ok(state)
}

/// Build IndexState synchronously via fd/rg respecting ignore rules.
fn collect_index(root: PathBuf, overlays: HashMap<PathBuf, String>) -> Result<IndexState> {
    let mut state = IndexState::default();
    let mut scope_roots = HashSet::new();
    let assum_files = find_assum_files(&root, &overlays)?;
    for path in assum_files {
        let Some(content) = content_for(&path, &overlays)? else {
            continue;
        };
        let path = normalize_path(path);
        let docs = parse_assumptions_content(&content, &path)?;
        let scope = normalize_path(path.parent().unwrap_or(&root).to_path_buf());
        scope_roots.insert(scope.clone());
        state.file_scope.insert(path.clone(), scope.clone());
        let mut seen = HashSet::new();
        let mut dupes = Vec::new();
        let mut doc_map = HashMap::new();
        for doc in docs {
            if !seen.insert(doc.name.clone()) {
                dupes.push(doc.clone());
            }
            doc_map.insert(doc.name.clone(), doc);
        }
        if !dupes.is_empty() {
            state.duplicate_defs.insert(scope.clone(), dupes);
        }
        let def_hits: Vec<TagHit> = doc_map
            .values()
            .map(|doc| TagHit {
                name: doc.name.clone(),
                range: doc.range,
                name_range: doc.range,
            })
            .collect();
        if !def_hits.is_empty() {
            state.tags.entry(path.clone()).or_default().extend(def_hits);
        }
        state.scope_docs.insert(scope, doc_map);
    }
    let overlay_paths: HashSet<PathBuf> = overlays.keys().cloned().map(normalize_path).collect();
    for (path, text) in overlays.iter() {
        if !path.starts_with(&root) {
            continue;
        }
        let path = normalize_path(path.clone());
        let Some(scope) = find_scope(&scope_roots, &path) else {
            continue;
        };
        let hits = scan_tags_content(text);
        state.file_scope.insert(path.clone(), scope.clone());
        if !hits.is_empty() {
            state.tags.entry(path.clone()).or_default().extend(hits);
        }
    }
    let (file_scopes, tag_hits) = tags_from_rg(&root, &scope_roots, &overlay_paths)?;
    state.file_scope.extend(file_scopes);
    for (path, hits) in tag_hits {
        state.tags.entry(path).or_default().extend(hits);
    }
    state.scope_roots = scope_roots;
    state.diagnostics = compute_diagnostics(&state);
    Ok(state)
}

/// Compute errors for duplicates/undefined and warnings for unused definitions.
fn compute_diagnostics(state: &IndexState) -> DiagnosticsMap {
    let mut diags: DiagnosticsMap = HashMap::new();
    for dupes in state.duplicate_defs.values() {
        for doc in dupes {
            push_diag(
                &mut diags,
                &doc.path,
                doc.range,
                format!("Duplicate definition of assumption `{}` in scope", doc.name),
                DiagSeverity::Error,
            );
        }
    }
    for (path, hits) in &state.tags {
        let docs = state.docs_for_path(path);
        for hit in hits {
            let defined = docs.map(|d| d.contains_key(&hit.name)).unwrap_or(false);
            if !defined {
                push_diag(
                    &mut diags,
                    path,
                    hit.range,
                    format!("assumption `{}` not defined in scope", hit.name),
                    DiagSeverity::Error,
                );
            }
        }
    }
    for (scope, docs) in &state.scope_docs {
        let mut used = HashSet::new();
        for (_path, hits) in state
            .tags
            .iter()
            .filter(|(p, _)| state.scope_for(p).as_ref() == Some(scope))
        {
            for hit in hits {
                if let Some(doc) = docs.get(&hit.name) {
                    let is_decl = normalize_path(doc.path.clone()) == normalize_path(_path.clone())
                        && doc.range == hit.range;
                    if !is_decl {
                        used.insert(hit.name.clone());
                    }
                }
            }
        }
        for (name, doc) in docs {
            if !used.contains(name) {
                push_diag(
                    &mut diags,
                    &doc.path,
                    doc.range,
                    format!("assumption `{}` unused", name),
                    DiagSeverity::Warning,
                );
            }
        }
    }
    diags
}

fn push_diag(
    diags: &mut DiagnosticsMap,
    path: &Path,
    range: Range,
    message: String,
    severity: DiagSeverity,
) {
    diags
        .entry(path.to_path_buf())
        .or_default()
        .push(AssumptionDiagnostic {
            path: path.to_path_buf(),
            range,
            message,
            severity,
        });
}

pub type AssumptionIndexRef = ActorRef<AssumptionIndex>;

impl AssumptionIndex {
    async fn refresh(&mut self) -> DiagnosticsMap {
        let Some(root) = self.root.clone() else {
            self.state = None;
            return HashMap::new();
        };
        match build_index(root, self.overlays.clone()).await {
            Ok(state) => {
                let diags = state.diagnostics();
                self.state = Some(state);
                diags
            }
            Err(err) => {
                error!(error = %err, "index refresh failed");
                HashMap::new()
            }
        }
    }
}

fn find_assum_files(root: &Path, overlays: &HashMap<PathBuf, String>) -> Result<HashSet<PathBuf>> {
    let mut files: HashSet<PathBuf> = overlays
        .keys()
        .filter(|p| p.starts_with(root))
        .filter(|p| p.file_name() == Some(std::ffi::OsStr::new("ASSUM.md")))
        .cloned()
        .map(normalize_path)
        .collect();
    let output = Command::new("fd")
        .arg("--color=never")
        .arg("--glob")
        .arg("ASSUM.md")
        .arg("--type")
        .arg("f")
        .arg("-a")
        .arg(".")
        .current_dir(root)
        .output()
        .with_context(|| format!("running fd in {}", root.display()))?;
    if !output.status.success() {
        anyhow::bail!("fd failed with status {}", output.status);
    }
    for line in String::from_utf8_lossy(&output.stdout).lines() {
        if line.is_empty() {
            continue;
        }
        let mut path = PathBuf::from(line);
        if path.is_relative() {
            path = root.join(path);
        }
        files.insert(normalize_path(path));
    }
    Ok(files)
}

fn tags_from_rg(
    root: &Path,
    scope_roots: &HashSet<PathBuf>,
    overlay_paths: &HashSet<PathBuf>,
) -> Result<TagScanMaps> {
    let mut file_scope: FileScopeMap = HashMap::new();
    let mut tags: TagHitsMap = HashMap::new();
    let output = Command::new("rg")
        .arg("--json")
        .arg("--no-heading")
        .arg("--color=never")
        .arg("@ASSUME:[A-Za-z0-9_]+")
        .arg(".")
        .current_dir(root)
        .output()
        .with_context(|| format!("running rg in {}", root.display()))?;
    // @ASSUME:rg_exit_code_one
    match output.status.code() {
        Some(0) | Some(1) => {}
        Some(code) => anyhow::bail!("rg failed with exit code {}", code),
        None => anyhow::bail!("rg terminated by signal"),
    }
    for line in String::from_utf8_lossy(&output.stdout).lines() {
        let Ok(msg) = serde_json::from_str::<RgMessage>(line) else {
            continue;
        };
        let RgMessage::Match { data } = msg else {
            continue;
        };
        let mut path = PathBuf::from(data.path.text);
        if path.is_relative() {
            path = root.join(path);
        }
        path = normalize_path(path);
        if overlay_paths.contains(&path) {
            continue;
        }
        let Some(scope) = find_scope(scope_roots, &path) else {
            continue;
        };
        let line_hits = scan_tags_content(&data.lines.text);
        if line_hits.is_empty() {
            continue;
        }
        let adjusted: Vec<TagHit> = line_hits
            .into_iter()
            .map(|mut hit| {
                let base: u32 = data.line_number.saturating_sub(1).try_into().unwrap();
                hit.range.start.line += base;
                hit.range.end.line += base;
                hit.name_range.start.line += base;
                hit.name_range.end.line += base;
                hit
            })
            .collect();
        file_scope.entry(path.clone()).or_insert(scope);
        tags.entry(path).or_default().extend(adjusted);
    }
    Ok((file_scope, tags))
}

fn ensure_tools_available() -> Result<()> {
    for tool in ["fd", "rg"] {
        let status = Command::new(tool)
            .arg("--version")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .with_context(|| format!("invoking {} --version", tool))?;
        if !status.success() {
            anyhow::bail!("{} is required but returned status {}", tool, status);
        }
    }
    Ok(())
}

pub fn normalize_path(path: PathBuf) -> PathBuf {
    use std::path::Component;

    let mut out = PathBuf::new();
    for comp in path.components() {
        match comp {
            Component::CurDir => {}
            Component::ParentDir => {
                out.pop();
            }
            other => out.push(other.as_os_str()),
        }
    }
    out
}

#[derive(Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
enum RgMessage {
    Match {
        data: RgMatch,
    },
    #[serde(other)]
    Other,
}

#[derive(Deserialize)]
struct RgMatch {
    path: RgText,
    lines: RgText,
    line_number: u64,
}

#[derive(Deserialize)]
struct RgText {
    text: String,
}
