// @ASSUME:lsp_e2e_testing
mod common;

use std::path::PathBuf;

use anyhow::Result;
use common::lsp_client::LspClient;
use serde_json::json;
use tower_lsp::lsp_types::Url;

#[tokio::test]
async fn definitions_inherit_parents() -> Result<()> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/inheritance_test");
    let subdir_file = root.join("subdir/file.rs");
    let mut client = LspClient::spawn(&root).await?;
    for path in [
        root.join("ASSUM.md"),
        root.join("subdir/ASSUM.md"),
        subdir_file.clone(),
    ] {
        client
            .did_open(&path, std::fs::read_to_string(&path)?)
            .await?;
    }

    let definition = client
        .request(
            "textDocument/definition",
            json!({
                "textDocument": {"uri": Url::from_file_path(&subdir_file).unwrap()},
                "position": {"line": 0, "character": 15},
            }),
        )
        .await?;
    let defs = definition
        .get("result")
        .and_then(|r| r.as_array())
        .cloned()
        .unwrap_or_default();
    assert!(
        !defs.is_empty(),
        "Should resolve to parent scope definition"
    );
    Ok(())
}

#[tokio::test]
async fn references_follow_inheritance() -> Result<()> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/inheritance_test");
    let subdir_file = root.join("subdir/file.rs");
    let mut client = LspClient::spawn(&root).await?;
    for path in [
        root.join("ASSUM.md"),
        root.join("subdir/ASSUM.md"),
        subdir_file.clone(),
    ] {
        client
            .did_open(&path, std::fs::read_to_string(&path)?)
            .await?;
    }

    let parent_assum = root.join("ASSUM.md");
    let references = client
        .request(
            "textDocument/references",
            json!({
                "textDocument": {"uri": Url::from_file_path(&subdir_file).unwrap()},
                "position": {"line": 1, "character": 20},
                "context": {"includeDeclaration": true},
            }),
        )
        .await?;
    let locs = references
        .get("result")
        .and_then(|r| r.as_array())
        .cloned()
        .unwrap_or_default();
    assert!(!locs.is_empty());
    let has_parent = locs.iter().any(|loc| {
        loc["uri"].as_str().unwrap_or_default()
            == Url::from_file_path(&parent_assum).unwrap().to_string()
    });
    assert!(has_parent, "References should include parent declaration");

    let definition = client
        .request(
            "textDocument/definition",
            json!({
                "textDocument": {"uri": Url::from_file_path(&subdir_file).unwrap()},
                "position": {"line": 1, "character": 20},
            }),
        )
        .await?;
    let defs = definition
        .get("result")
        .and_then(|r| r.as_array())
        .cloned()
        .unwrap_or_default();
    assert!(
        defs.iter()
            .any(|loc| loc["uri"].as_str().unwrap_or_default()
                == Url::from_file_path(&parent_assum).unwrap().to_string())
    );
    Ok(())
}
