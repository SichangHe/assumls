use std::collections::HashMap;
use std::path::PathBuf;

#[tokio::test]
async fn test_scope_inheritance() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/inheritance_test");
    let overlays = HashMap::new();
    let state = assumls::index::build_index(root, overlays).await.unwrap();
    let diags = state.diagnostics();
    // Currently FAILS: subdir/file.rs will show "undefined" for root_assumption
    // because child scope doesn't inherit from parent
    assert!(
        diags.is_empty(),
        "Should have no errors - child should inherit parent assumptions: {:#?}",
        diags
    );
}
