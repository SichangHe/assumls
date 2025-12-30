use std::path::PathBuf;
use std::process::Stdio;
use std::time::Duration;

use anyhow::{Result, anyhow};
use insta::assert_json_snapshot;
use serde_json::{Value, json};
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::{ChildStdin, ChildStdout, Command};
use tokio::time::timeout;
use tower_lsp::lsp_types::Url;

async fn send_payload(writer: &mut ChildStdin, payload: Value) -> Result<()> {
    let text = payload.to_string();
    let header = format!("Content-Length: {}\r\n\r\n", text.len());
    writer.write_all(header.as_bytes()).await?;
    writer.write_all(text.as_bytes()).await?;
    writer.flush().await?;
    Ok(())
}

async fn request(writer: &mut ChildStdin, id: u64, method: &str, params: Value) -> Result<()> {
    let payload = json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params});
    send_payload(writer, payload).await
}

async fn notify(writer: &mut ChildStdin, method: &str, params: Value) -> Result<()> {
    let payload = json!({"jsonrpc": "2.0", "method": method, "params": params});
    send_payload(writer, payload).await
}

async fn read_message(reader: &mut BufReader<ChildStdout>) -> Result<Option<Value>> {
    let mut content_length = None;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).await? == 0 {
            return Ok(None);
        }
        if line == "\r\n" {
            break;
        }
        if let Some(rest) = line.strip_prefix("Content-Length:") {
            content_length = rest.trim().parse::<usize>().ok();
        }
    }
    let len = match content_length {
        Some(len) => len,
        None => return Ok(None),
    };
    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf).await?;
    let value = serde_json::from_slice(&buf)?;
    Ok(Some(value))
}

async fn read_response(reader: &mut BufReader<ChildStdout>, id: u64) -> Result<Value> {
    loop {
        let msg = read_message(reader)
            .await?
            .ok_or_else(|| anyhow!("stream closed"))?;
        if msg.get("id").and_then(|v| v.as_u64()) == Some(id) {
            return Ok(msg);
        }
    }
}

#[tokio::test]
async fn lsp_hover_rename_completion() -> Result<()> {
    let ws = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/lsp_ws");
    let file = ws.join("web/app.js");
    let uri = Url::from_file_path(&file).unwrap().to_string();
    let assum = ws.join("web/ASSUM.md");
    let assum_uri = Url::from_file_path(&assum).unwrap().to_string();
    let root_uri = Url::from_file_path(&ws).unwrap().to_string();
    let app_text = std::fs::read_to_string(&file)?;
    let assum_text = std::fs::read_to_string(&assum)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("assumls"));
    cmd.kill_on_drop(true);
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::inherit());
    let mut child = cmd.spawn()?;
    let mut stdin = child.stdin.take().ok_or_else(|| anyhow!("stdin missing"))?;
    let mut stdout = BufReader::new(
        child
            .stdout
            .take()
            .ok_or_else(|| anyhow!("stdout missing"))?,
    );

    request(
        &mut stdin,
        1,
        "initialize",
        json!({"rootUri": root_uri, "capabilities": {}}),
    )
    .await?;
    let _ = timeout(Duration::from_secs(5), read_response(&mut stdout, 1)).await??;

    notify(
        &mut stdin,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": uri,
                "languageId": "javascript",
                "version": 1,
                "text": app_text,
            }
        }),
    )
    .await?;

    notify(
        &mut stdin,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": assum_uri,
                "languageId": "markdown",
                "version": 1,
                "text": assum_text,
            }
        }),
    )
    .await?;

    request(
        &mut stdin,
        2,
        "textDocument/hover",
        json!({
            "textDocument": {"uri": uri},
            "position": {"line": 0, "character": 12},
        }),
    )
    .await?;
    let hover = timeout(Duration::from_secs(5), read_response(&mut stdout, 2)).await??;

    request(
        &mut stdin,
        3,
        "textDocument/completion",
        json!({
            "textDocument": {"uri": uri},
            "position": {"line": 0, "character": 0},
        }),
    )
    .await?;
    let completion = timeout(Duration::from_secs(5), read_response(&mut stdout, 3)).await??;
    let items = completion
        .get("result")
        .and_then(|r| r.get("items"))
        .and_then(|i| i.as_array())
        .cloned()
        .or_else(|| completion.get("result").and_then(|r| r.as_array().cloned()))
        .unwrap_or_default();
    let mut labels = items
        .iter()
        .filter_map(|item| item.get("label").and_then(|l| l.as_str()))
        .collect::<Vec<_>>();
    labels.sort();

    request(
        &mut stdin,
        4,
        "textDocument/documentHighlight",
        json!({
            "textDocument": {"uri": uri},
            "position": {"line": 0, "character": 3},
        }),
    )
    .await?;
    let highlights = timeout(Duration::from_secs(5), read_response(&mut stdout, 4)).await??;

    request(
        &mut stdin,
        5,
        "textDocument/semanticTokens/full",
        json!({
            "textDocument": {"uri": uri},
        }),
    )
    .await?;
    let semantic_tokens = timeout(Duration::from_secs(5), read_response(&mut stdout, 5)).await??;

    request(
        &mut stdin,
        6,
        "textDocument/references",
        json!({
            "textDocument": {"uri": uri},
            "position": {"line": 0, "character": 12},
            "context": {"includeDeclaration": true},
        }),
    )
    .await?;
    let references_from_use =
        timeout(Duration::from_secs(5), read_response(&mut stdout, 6)).await??;

    request(
        &mut stdin,
        7,
        "textDocument/references",
        json!({
            "textDocument": {"uri": assum_uri},
            "position": {"line": 0, "character": 4},
            "context": {"includeDeclaration": true},
        }),
    )
    .await?;
    let references_from_definition =
        timeout(Duration::from_secs(5), read_response(&mut stdout, 7)).await??;

    request(
        &mut stdin,
        8,
        "textDocument/references",
        json!({
            "textDocument": {"uri": assum_uri},
            "position": {"line": 3, "character": 20},
            "context": {"includeDeclaration": true},
        }),
    )
    .await?;
    let references_web_only =
        timeout(Duration::from_secs(5), read_response(&mut stdout, 8)).await??;

    request(
        &mut stdin,
        9,
        "textDocument/rename",
        json!({
            "textDocument": {"uri": uri},
            "position": {"line": 0, "character": 12},
            "newName": "shared_web",
        }),
    )
    .await?;
    let rename = timeout(Duration::from_secs(5), read_response(&mut stdout, 9)).await??;

    request(&mut stdin, 10, "shutdown", json!({})).await?;
    let _ = timeout(Duration::from_secs(5), read_response(&mut stdout, 10)).await??;
    notify(&mut stdin, "exit", json!({})).await?;

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
