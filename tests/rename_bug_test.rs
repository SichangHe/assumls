// @ASSUME:lsp_e2e_testing
mod common;

use std::path::PathBuf;

use anyhow::Result;
use common::lsp_client::LspClient;
use serde_json::json;
use tower_lsp::lsp_types::Url;

#[tokio::test]
async fn test_rename_preserves_heading_prefix() -> Result<()> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/rename_test");
    let assum_path = root.join("ASSUM.md");
    let main_path = root.join("main.rs");
    let mut client = LspClient::spawn(&root).await?;
    client
        .did_open(&assum_path, std::fs::read_to_string(&assum_path)?)
        .await?;
    client
        .did_open(&main_path, std::fs::read_to_string(&main_path)?)
        .await?;

    let rename = client
        .request(
            "textDocument/rename",
            json!({
                "textDocument": {"uri": Url::from_file_path(&assum_path).unwrap()},
                "position": {"line": 0, "character": 4},
                "newName": "new_name",
            }),
        )
        .await?;

    let changes = rename["result"]["changes"].as_object().unwrap();
    let assum_uri = Url::from_file_path(&assum_path).unwrap().to_string();
    let main_uri = Url::from_file_path(&main_path).unwrap().to_string();
    let assum_edits = changes.get(&assum_uri).and_then(|v| v.as_array()).unwrap();
    assert_eq!(assum_edits.len(), 1);
    let def_edit = &assum_edits[0];
    assert_eq!(def_edit["newText"], "new_name");
    assert_eq!(def_edit["range"]["start"]["line"], 0);
    assert_eq!(def_edit["range"]["start"]["character"], 2);
    assert_eq!(def_edit["range"]["end"]["character"], 10);

    let original_content = std::fs::read_to_string(&assum_path)?;
    let line = original_content.lines().next().unwrap_or_default();
    let start = def_edit["range"]["start"]["character"].as_u64().unwrap() as usize;
    let end = def_edit["range"]["end"]["character"].as_u64().unwrap() as usize;
    let mut edited_line = String::new();
    edited_line.push_str(&line[..start]);
    edited_line.push_str(def_edit["newText"].as_str().unwrap());
    edited_line.push_str(&line[end..]);
    assert_eq!(edited_line, "# new_name");

    let main_edits = changes.get(&main_uri).and_then(|v| v.as_array()).unwrap();
    assert_eq!(main_edits.len(), 1);
    assert_eq!(main_edits[0]["newText"], "new_name");
    Ok(())
}

#[tokio::test]
async fn test_rename_updates_all_references() -> Result<()> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/rename_test/subdir");
    let file1 = root.join("file1.rs");
    let assum_path = root.join("ASSUM.md");
    let mut client = LspClient::spawn(&root).await?;
    client
        .did_open(&assum_path, std::fs::read_to_string(&assum_path)?)
        .await?;
    client
        .did_open(&file1, std::fs::read_to_string(&file1)?)
        .await?;

    let rename = client
        .request(
            "textDocument/rename",
            json!({
                "textDocument": {"uri": Url::from_file_path(&file1).unwrap()},
                "position": {"line": 0, "character": 15},
                "newName": "renamed_test",
            }),
        )
        .await?;

    let changes = rename["result"]["changes"].as_object().unwrap();
    let file1_uri = Url::from_file_path(&file1).unwrap().to_string();
    let assum_uri = Url::from_file_path(&assum_path).unwrap().to_string();
    let file1_edits = changes.get(&file1_uri).and_then(|v| v.as_array()).unwrap();
    assert_eq!(file1_edits.len(), 2);
    for edit in file1_edits {
        assert_eq!(edit["newText"], "renamed_test");
        assert_eq!(edit["range"]["start"]["character"], 11);
        assert_eq!(edit["range"]["end"]["character"], 26);
    }
    let assum_edits = changes.get(&assum_uri).and_then(|v| v.as_array()).unwrap();
    assert_eq!(assum_edits.len(), 1);
    assert_eq!(assum_edits[0]["newText"], "renamed_test");
    Ok(())
}
