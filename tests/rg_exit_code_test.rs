use std::collections::HashMap;
use std::path::PathBuf;

#[tokio::test]
async fn test_empty_workspace_no_tags() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/rg_empty_test");

    let overlays = HashMap::new();
    let result = assumls::index::build_index(root, overlays).await;

    assert!(
        result.is_ok(),
        "Should succeed when no @ASSUME tags exist: {:?}",
        result.err()
    );

    let state = result.unwrap();
    let diags = state.diagnostics();

    assert!(
        !diags.is_empty(),
        "Should have diagnostics for unused definition"
    );
}

#[tokio::test]
async fn test_fully_empty_workspace() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/rg_no_assum_test");

    let overlays = HashMap::new();
    let result = assumls::index::build_index(root, overlays).await;

    assert!(
        result.is_ok(),
        "Should succeed in empty workspace: {:?}",
        result.err()
    );

    let state = result.unwrap();
    assert_eq!(state.diagnostics().len(), 0);
}
