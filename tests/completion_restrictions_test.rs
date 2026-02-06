mod common;

use std::path::PathBuf;
use std::time::Duration;

use anyhow::Result;
use common::lsp_client::LspClient;
use serde_json::{Value, json};
use tower_lsp::lsp_types::Url;

#[tokio::test]
async fn completion_restrictions() -> Result<()> {
    let ws = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/completion_restrictions");
    let file = ws.join("test.js");
    let uri = Url::from_file_path(&file).unwrap().to_string();
    let mut client = LspClient::spawn(&ws).await?;

    client
        .did_open(&file, std::fs::read_to_string(&file)?)
        .await?;

    tokio::time::sleep(Duration::from_millis(100)).await;

    let completion_no_marker = client
        .request(
            "textDocument/completion",
            json!({
                "textDocument": {"uri": &uri},
                "position": {"line": 0, "character": 10}
            }),
        )
        .await?;
    let labels_no_marker = extract_labels(&completion_no_marker);
    assert!(
        labels_no_marker.is_empty(),
        "Expected no completions when @ is not typed, got: {:?}",
        labels_no_marker
    );

    let completion_at_sign = client
        .request(
            "textDocument/completion",
            json!({
                "textDocument": {"uri": &uri},
                "position": {"line": 1, "character": 4}
            }),
        )
        .await?;
    let labels_at_sign = extract_labels(&completion_at_sign);
    assert!(
        labels_at_sign.contains(&"ASSUME:".to_string()),
        "Expected ASSUME: completion after @, got: {:?}",
        labels_at_sign
    );
    assert!(
        !labels_at_sign.contains(&"shared_name".to_string()),
        "Should not contain assumption names when only @ is typed, got: {:?}",
        labels_at_sign
    );

    let completion_after_marker = client
        .request(
            "textDocument/completion",
            json!({
                "textDocument": {"uri": &uri},
                "position": {"line": 2, "character": 12}
            }),
        )
        .await?;
    let labels_after_marker = extract_labels(&completion_after_marker);
    assert!(
        !labels_after_marker.contains(&"ASSUME:".to_string()),
        "Should not contain ASSUME: when @ASSUME: is already typed, got: {:?}",
        labels_after_marker
    );
    assert!(
        labels_after_marker.contains(&"shared_name".to_string()),
        "Expected assumption names after @ASSUME:, got: {:?}",
        labels_after_marker
    );

    let completion_partial = client
        .request(
            "textDocument/completion",
            json!({
                "textDocument": {"uri": &uri},
                "position": {"line": 3, "character": 17}
            }),
        )
        .await?;
    let labels_partial = extract_labels(&completion_partial);
    assert!(
        labels_partial.contains(&"shared_name".to_string()),
        "Expected shared_name in completions for partial match, got: {:?}",
        labels_partial
    );
    assert!(
        !labels_partial.contains(&"ASSUME:".to_string()),
        "Should not contain ASSUME: when already past @ASSUME:, got: {:?}",
        labels_partial
    );

    let _ = client.request("shutdown", json!({})).await?;
    client.notify("exit", json!({})).await?;

    Ok(())
}

fn extract_labels(completion: &Value) -> Vec<String> {
    completion
        .get("result")
        .and_then(|r| r.get("items"))
        .and_then(|i| i.as_array().cloned())
        .or_else(|| completion.get("result").and_then(|r| r.as_array().cloned()))
        .unwrap_or_default()
        .iter()
        .filter_map(|item| item.get("label").and_then(|l| l.as_str()))
        .map(|s| s.to_string())
        .collect()
}
