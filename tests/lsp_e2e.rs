mod common;

use std::path::PathBuf;

use anyhow::Result;
use common::lsp_client::LspClient;
use insta::assert_json_snapshot;
use serde_json::{Value, json};
use tower_lsp::lsp_types::Url;

#[tokio::test]
async fn lsp_hover_rename_completion() -> Result<()> {
    let ws = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/lsp_ws");
    let file = ws.join("web/app.js");
    let uri = Url::from_file_path(&file).unwrap().to_string();
    let assum = ws.join("web/ASSUM.md");
    let assum_uri = Url::from_file_path(&assum).unwrap().to_string();
    let root_uri = Url::from_file_path(&ws).unwrap().to_string();
    let mut client = LspClient::spawn(&ws).await?;
    client
        .did_open(&file, std::fs::read_to_string(&file)?)
        .await?;
    client
        .did_open(&assum, std::fs::read_to_string(&assum)?)
        .await?;

    let hover = client.hover(&file, 0, 12).await?;

    let completion = client.completion(&file, 0, 11).await?;
    let items = completion
        .get("result")
        .and_then(|r| r.get("items"))
        .and_then(|i| i.as_array().cloned())
        .or_else(|| completion.get("result").and_then(|r| r.as_array().cloned()))
        .unwrap_or_default();
    let mut labels = items
        .iter()
        .filter_map(|item| item.get("label").and_then(|l| l.as_str()))
        .collect::<Vec<_>>();
    labels.sort();

    let highlights = client
        .request(
            "textDocument/documentHighlight",
            json!({
                "textDocument": {"uri": uri},
                "position": {"line": 0, "character": 3},
            }),
        )
        .await?;

    let semantic_tokens = client
        .request(
            "textDocument/semanticTokens/full",
            json!({
                "textDocument": {"uri": uri},
            }),
        )
        .await?;

    let references_from_use = client
        .request(
            "textDocument/references",
            json!({
                "textDocument": {"uri": uri},
                "position": {"line": 0, "character": 12},
                "context": {"includeDeclaration": true},
            }),
        )
        .await?;

    let references_from_definition = client
        .request(
            "textDocument/references",
            json!({
                "textDocument": {"uri": assum_uri},
                "position": {"line": 0, "character": 4},
                "context": {"includeDeclaration": true},
            }),
        )
        .await?;

    let references_web_only = client
        .request(
            "textDocument/references",
            json!({
                "textDocument": {"uri": assum_uri},
                "position": {"line": 3, "character": 20},
                "context": {"includeDeclaration": true},
            }),
        )
        .await?;

    let rename = client
        .request(
            "textDocument/rename",
            json!({
                "textDocument": {"uri": uri},
                "position": {"line": 0, "character": 12},
                "newName": "shared_web",
            }),
        )
        .await?;

    let _ = client.request("shutdown", json!({})).await?;
    client.notify("exit", json!({})).await?;

    let snapshot = json!({
        "hover": hover.get("result"),
        "completion_labels": labels,
        "highlights": sanitize_paths(highlights.get("result").cloned().unwrap_or(Value::Null), &root_uri),
        "semantic_tokens": semantic_tokens.get("result"),
        "references_from_use": sort_locations_value(sanitize_paths(
            references_from_use.get("result").cloned().unwrap_or(Value::Null),
            &root_uri,
        )),
        "references_from_definition": sort_locations_value(sanitize_paths(
            references_from_definition.get("result").cloned().unwrap_or(Value::Null),
            &root_uri,
        )),
        "references_web_only": sort_locations_value(sanitize_paths(
            references_web_only.get("result").cloned().unwrap_or(Value::Null),
            &root_uri,
        )),
        "rename": stabilize_rename(sanitize_paths(rename.get("result").cloned().unwrap_or(Value::Null), &root_uri)),
    });
    assert_json_snapshot!("lsp_hover_rename_completion", snapshot);
    Ok(())
}

fn stabilize_rename(value: Value) -> Value {
    let mut value = value;
    if let Some(map) = value.as_object_mut()
        && let Some(changes) = map.get_mut("changes").and_then(|v| v.as_object_mut())
    {
        let mut entries: Vec<(String, Value)> = std::mem::take(changes).into_iter().collect();
        entries.sort_by_key(|a| a.0.clone());
        let mut new_changes = serde_json::Map::new();
        for (k, mut v) in entries {
            if let Some(edits) = v.as_array_mut() {
                edits.sort_by_key(edit_sort_key);
            }
            new_changes.insert(k, v);
        }
        *changes = new_changes;
    }
    value
}

fn edit_sort_key(value: &Value) -> (i64, i64, i64, i64) {
    let start_line = value
        .get("range")
        .and_then(|r| r.get("start"))
        .and_then(|p| p.get("line"))
        .and_then(|l| l.as_i64())
        .unwrap_or_default();
    let start_char = value
        .get("range")
        .and_then(|r| r.get("start"))
        .and_then(|p| p.get("character"))
        .and_then(|l| l.as_i64())
        .unwrap_or_default();
    let end_line = value
        .get("range")
        .and_then(|r| r.get("end"))
        .and_then(|p| p.get("line"))
        .and_then(|l| l.as_i64())
        .unwrap_or_default();
    let end_char = value
        .get("range")
        .and_then(|r| r.get("end"))
        .and_then(|p| p.get("character"))
        .and_then(|l| l.as_i64())
        .unwrap_or_default();
    (start_line, start_char, end_line, end_char)
}

fn sanitize_paths(value: Value, root_uri: &str) -> Value {
    match value {
        Value::String(s) => Value::String(s.replace(root_uri, "<ROOT>")),
        Value::Array(items) => Value::Array(
            items
                .into_iter()
                .map(|v| sanitize_paths(v, root_uri))
                .collect(),
        ),
        Value::Object(map) => {
            let mut entries: Vec<(String, Value)> = map.into_iter().collect();
            entries.sort_by(|a, b| a.0.cmp(&b.0));
            Value::Object(
                entries
                    .into_iter()
                    .map(|(k, v)| {
                        let key = k.replace(root_uri, "<ROOT>");
                        (key, sanitize_paths(v, root_uri))
                    })
                    .collect(),
            )
        }
        other => other,
    }
}

fn sort_locations_value(mut value: Value) -> Value {
    if let Some(arr) = value.as_array_mut() {
        arr.sort_by_key(location_sort_key);
    }
    value
}

fn location_sort_key(value: &Value) -> (String, i64, i64, i64, i64) {
    let uri = value
        .get("uri")
        .and_then(|u| u.as_str())
        .unwrap_or_default()
        .to_string();
    let start_line = value
        .get("range")
        .and_then(|r| r.get("start"))
        .and_then(|p| p.get("line"))
        .and_then(|l| l.as_i64())
        .unwrap_or_default();
    let start_char = value
        .get("range")
        .and_then(|r| r.get("start"))
        .and_then(|p| p.get("character"))
        .and_then(|l| l.as_i64())
        .unwrap_or_default();
    let end_line = value
        .get("range")
        .and_then(|r| r.get("end"))
        .and_then(|p| p.get("line"))
        .and_then(|l| l.as_i64())
        .unwrap_or_default();
    let end_char = value
        .get("range")
        .and_then(|r| r.get("end"))
        .and_then(|p| p.get("character"))
        .and_then(|l| l.as_i64())
        .unwrap_or_default();
    (uri, start_line, start_char, end_line, end_char)
}
