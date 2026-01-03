use std::collections::HashMap;
use std::path::PathBuf;
use tower_lsp::lsp_types::Position;

#[tokio::test]
async fn test_scope_inheritance() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/inheritance_test");
    let overlays = HashMap::new();
    let state = assumls::index::build_index(root, overlays).await.unwrap();
    let diags = state.diagnostics();
    assert!(
        diags.is_empty(),
        "Should have no errors - child should inherit parent assumptions: {:#?}",
        diags
    );
}

#[tokio::test]
async fn test_goto_definition_inherited() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/inheritance_test");
    let subdir_file = root.join("subdir/file.rs");
    let parent_assum = root.join("ASSUM.md");

    let overlays = HashMap::new();
    let state = assumls::index::build_index(root, overlays).await.unwrap();

    // Click on parent_assumption_used_in_subdir in subdir/file.rs line 1
    let position = Position {
        line: 1,
        character: 15,
    };
    let locations = state.definition(&subdir_file, position);

    assert!(
        locations.is_some(),
        "Should find definition for inherited assumption"
    );
    let locs = locations.unwrap();
    assert_eq!(locs.len(), 1, "Should have exactly one definition");

    let loc = &locs[0];
    assert_eq!(
        loc.uri.to_file_path().unwrap(),
        parent_assum,
        "Definition should be in parent ASSUM.md"
    );
}

#[tokio::test]
async fn test_find_references_inherited() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/inheritance_test");
    let parent_assum = root.join("ASSUM.md");
    let subdir_file = root.join("subdir/file.rs");

    let overlays = HashMap::new();
    let state = assumls::index::build_index(root, overlays).await.unwrap();

    // Find references from parent ASSUM.md for parent_assumption_used_in_subdir (line 3)
    let position = Position {
        line: 3,
        character: 5,
    };
    let locations = state.references(&parent_assum, position, true);

    assert!(locations.is_some(), "Should find references");
    let locs = locations.unwrap();

    // Should find: 1 definition in parent + 1 reference in subdir/file.rs
    assert!(
        !locs.is_empty(),
        "Should find at least the reference in subdir, got: {:?}",
        locs
    );

    let has_subdir_ref = locs
        .iter()
        .any(|loc| loc.uri.to_file_path().unwrap() == subdir_file);
    assert!(
        has_subdir_ref,
        "Should include reference from subdir/file.rs"
    );
}

#[tokio::test]
async fn test_no_unused_warning_for_inherited() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/inheritance_test");
    let parent_assum = root.join("ASSUM.md");

    let overlays = HashMap::new();
    let state = assumls::index::build_index(root, overlays).await.unwrap();
    let diags = state.diagnostics();

    // Check if parent_assumption_used_in_subdir has unused warning
    let parent_diags = diags.get(&parent_assum);
    if let Some(diags) = parent_diags {
        for diag in diags {
            assert!(
                !diag.message.contains("parent_assumption_used_in_subdir"),
                "parent_assumption_used_in_subdir should NOT be marked as unused: {}",
                diag.message
            );
        }
    }
}
