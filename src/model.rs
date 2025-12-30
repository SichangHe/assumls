use std::path::PathBuf;
use tower_lsp::lsp_types::Range;

/// Diagnostic emitted for assumption issues.
#[derive(Debug, Clone)]
pub struct AssumptionDiagnostic {
    /// File where the issue occurs.
    pub path: PathBuf,
    /// Range highlighting the offending text.
    pub range: Range,
    /// Human-readable description of the issue.
    pub message: String,
    /// Diagnostic severity.
    pub severity: DiagSeverity,
}

/// Parsed assumption block captured from an ASSUM.md file.
#[derive(Debug, Clone)]
pub struct AssumptionDoc {
    /// Snake_case assumption identifier.
    pub name: String,
    /// Original heading text.
    pub heading: String,
    /// Body lines joined with newlines.
    pub body: String,
    /// Source file containing the definition.
    pub path: PathBuf,
    /// Range of the heading within the source.
    pub range: Range,
}

/// Occurrence of an @ASSUME tag in a file.
#[derive(Debug, Clone)]
pub struct TagHit {
    /// Referenced assumption name.
    pub name: String,
    /// Range covering only the name.
    pub range: Range,
}

/// Severity levels for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagSeverity {
    Error,
    Warning,
}

impl AssumptionDiagnostic {
    pub fn is_error(&self) -> bool {
        matches!(self.severity, DiagSeverity::Error)
    }
}
