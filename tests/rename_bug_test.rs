use std::collections::HashMap;
use std::path::PathBuf;

#[tokio::test]
async fn test_rename_preserves_heading_prefix() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/rename_test");
    let assum_path = root.join("ASSUM.md");

    let overlays = HashMap::new();
    let state = assumls::index::build_index(root.clone(), overlays)
        .await
        .unwrap();

    let position = tower_lsp::lsp_types::Position {
        line: 0,
        character: 4,
    };
    let rename_result = state.rename(&assum_path, position, "new_name".to_string());

    assert!(rename_result.is_some(), "Rename should return edit");
    let edit = rename_result.unwrap();
    let changes = edit.changes.as_ref().expect("Should have changes");

    let assum_uri = tower_lsp::lsp_types::Url::from_file_path(&assum_path).unwrap();
    let assum_edits = changes.get(&assum_uri).expect("Should edit ASSUM.md");
    assert_eq!(assum_edits.len(), 1);

    let def_edit = &assum_edits[0];
    assert_eq!(def_edit.new_text, "new_name");
    assert_eq!(def_edit.range.start.line, 0);
    assert_eq!(def_edit.range.start.character, 2);
    assert_eq!(def_edit.range.end.character, 10);

    let original_content = std::fs::read_to_string(&assum_path).unwrap();
    let line = original_content.lines().next().unwrap();
    let start = def_edit.range.start.character as usize;
    let end = def_edit.range.end.character as usize;
    let mut edited_line = String::new();
    edited_line.push_str(&line[..start]);
    edited_line.push_str(&def_edit.new_text);
    edited_line.push_str(&line[end..]);

    assert_eq!(edited_line, "# new_name");
}

#[tokio::test]
async fn test_rename_updates_all_references() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/rename_test/subdir");
    let file1 = root.join("file1.rs");

    let overlays = HashMap::new();
    let state = assumls::index::build_index(root, overlays).await.unwrap();

    let position = tower_lsp::lsp_types::Position {
        line: 0,
        character: 15,
    };
    let rename_result = state.rename(&file1, position, "renamed_test".to_string());

    assert!(rename_result.is_some());
    let edit = rename_result.unwrap();
    let changes = edit.changes.as_ref().unwrap();

    let file1_uri = tower_lsp::lsp_types::Url::from_file_path(&file1).unwrap();
    let file1_edits = changes.get(&file1_uri).expect("Should edit file1");

    assert_eq!(file1_edits.len(), 2);

    for edit in file1_edits {
        assert_eq!(edit.new_text, "renamed_test");
        assert_eq!(edit.range.start.character, 11);
        assert_eq!(edit.range.end.character, 26);
    }
}
