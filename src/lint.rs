use std::collections::HashMap;
use std::path::PathBuf;

use anyhow::Result;
use tracing::info;

use crate::index::build_index;
use crate::model::{AssumptionDiagnostic, DiagSeverity};

/// Run static lint against a workspace root and return exit code.
pub async fn run_lint(root: PathBuf) -> Result<i32> {
    let root_abs = root
        .canonicalize()
        .map(crate::index::normalize_path)
        .unwrap_or_else(|_| root.clone());
    info!(path = %root_abs.display(), "running lint");
    let state = build_index(root_abs.clone(), HashMap::new()).await?;
    let mut entries: Vec<_> = state.diagnostics().into_iter().collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    let mut had_error = false;
    for (path, mut items) in entries {
        items.sort_by(|a, b| {
            a.range
                .start
                .line
                .cmp(&b.range.start.line)
                .then_with(|| a.range.start.character.cmp(&b.range.start.character))
                .then_with(|| severity_rank(a.severity).cmp(&severity_rank(b.severity)))
                .then_with(|| a.message.cmp(&b.message))
        });
        for diag in items {
            if matches!(diag.severity, DiagSeverity::Error) {
                had_error = true;
            }
            let display_path = path.strip_prefix(&root_abs).unwrap_or(&path);
            print_diag(display_path, &diag);
        }
    }
    Ok(if had_error { 1 } else { 0 })
}

fn severity_rank(severity: DiagSeverity) -> u8 {
    match severity {
        DiagSeverity::Error => 0,
        DiagSeverity::Warning => 1,
    }
}

/// Print one diagnostic in compiler-style format.
fn print_diag(path: &std::path::Path, diag: &AssumptionDiagnostic) {
    let line = diag.range.start.line + 1;
    let col = diag.range.start.character + 1;
    let severity = match diag.severity {
        DiagSeverity::Error => "error",
        DiagSeverity::Warning => "warning",
    };
    println!(
        "{}:{}:{}: {}: {}",
        path.display(),
        line,
        col,
        severity,
        diag.message
    );
}
