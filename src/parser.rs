use std::collections::HashSet;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use tower_lsp::lsp_types::{Position, Range};

use crate::model::{AssumptionDoc, TagHit};

/// Parse H1 sections in ASSUM.md content into assumption records.
pub fn parse_assumptions_content(content: &str, path: &Path) -> Result<Vec<AssumptionDoc>> {
    let mut docs = Vec::new();
    let mut current: Option<(String, Range, Vec<String>)> = None;
    for (idx, line) in content.lines().enumerate() {
        if let Some(rest) = line.strip_prefix("# ") {
            if let Some((name, range, body)) = current.take() {
                docs.push(AssumptionDoc {
                    name: name.clone(),
                    heading: name,
                    body: body.join("\n"),
                    path: path.to_path_buf(),
                    range,
                });
            }
            let name = rest.trim().to_string();
            if !is_valid_assumption_name(&name) {
                eprintln!(
                    "invalid assumption name `{}` in {} at line {}",
                    name,
                    path.display(),
                    idx + 1
                );
                current = None;
                continue;
            }
            let range = Range {
                start: Position {
                    line: idx as u32,
                    character: 2,
                },
                end: Position {
                    line: idx as u32,
                    character: (2 + name.len()) as u32,
                },
            };
            current = Some((name, range, Vec::new()));
            continue;
        }
        if line.starts_with('#') {
            if let Some((name, range, body)) = current.take() {
                docs.push(AssumptionDoc {
                    name: name.clone(),
                    heading: name,
                    body: body.join("\n"),
                    path: path.to_path_buf(),
                    range,
                });
            }
            continue;
        }
        if let Some((_, _, body)) = current.as_mut() {
            body.push(line.to_string());
        }
    }
    if let Some((name, range, body)) = current.take() {
        docs.push(AssumptionDoc {
            name: name.clone(),
            heading: name,
            body: body.join("\n"),
            path: path.to_path_buf(),
            range,
        });
    }
    Ok(docs)
}

/// Find @ASSUME:<name> tags in a file buffer and return name ranges.
pub fn scan_tags_content(content: &str) -> Vec<TagHit> {
    let mut hits = Vec::new();
    for (line_idx, line) in content.lines().enumerate() {
        let mut offset = 0;
        while let Some(pos) = line[offset..].find("@ASSUME:") {
            let start = offset + pos;
            let name_start = start + 8;
            let rest = &line[name_start..];
            let name: String = rest
                .chars()
                .take_while(|c| is_assumption_char(*c))
                .collect();
            if name.is_empty() || !is_valid_assumption_name(&name) {
                offset = name_start;
                continue;
            }
            let range = Range {
                start: Position {
                    line: line_idx as u32,
                    character: name_start as u32,
                },
                end: Position {
                    line: line_idx as u32,
                    character: (name_start + name.len()) as u32,
                },
            };
            let name_len = name.len();
            hits.push(TagHit { name, range });
            offset = name_start + name_len;
        }
    }
    hits
}

/// Return text for a path, preferring overlay buffers when present.
pub fn content_for(
    path: &Path,
    overlays: &std::collections::HashMap<PathBuf, String>,
) -> Result<Option<String>> {
    if let Some(text) = overlays.get(path) {
        return Ok(Some(text.clone()));
    }
    match std::fs::read_to_string(path) {
        Ok(text) => Ok(Some(text)),
        Err(err) if err.kind() == ErrorKind::NotFound => Ok(None),
        Err(err) => Err(err).with_context(|| format!("reading {}", path.display())),
    }
}

/// Locate the nearest directory that contains an ASSUM.md root.
pub fn find_scope(scope_roots: &HashSet<PathBuf>, path: &Path) -> Option<PathBuf> {
    let mut current = path.parent();
    while let Some(dir) = current {
        if scope_roots.contains(dir) {
            return Some(dir.to_path_buf());
        }
        current = dir.parent();
    }
    None
}

/// Check whether a position lies inside a range (inclusive).
pub fn contains(range: &Range, position: Position) -> bool {
    let start_before = position.line > range.start.line
        || (position.line == range.start.line && position.character >= range.start.character);
    let end_after = position.line < range.end.line
        || (position.line == range.end.line && position.character <= range.end.character);
    start_before && end_after
}

/// Validate snake_case assumption identifiers.
pub fn is_valid_assumption_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_lowercase() {
        return false;
    }
    chars.all(is_assumption_char)
}

fn is_assumption_char(c: char) -> bool {
    c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_h1_blocks() {
        let content = "# foo_bar\nline a\n# second_one\nline b";
        let docs = parse_assumptions_content(content, Path::new("ASSUM.md")).unwrap();
        assert_eq!(docs.len(), 2);
        assert_eq!(docs[0].name, "foo_bar");
        assert_eq!(docs[0].body, "line a");
        assert_eq!(docs[1].name, "second_one");
        assert_eq!(docs[1].body, "line b");
    }

    #[test]
    fn skips_invalid_names() {
        let content = "# FooBar\nbody\n# ok_name\nbody2";
        let docs = parse_assumptions_content(content, Path::new("ASSUM.md")).unwrap();
        assert_eq!(docs.len(), 1);
        assert_eq!(docs[0].name, "ok_name");
        assert_eq!(docs[0].body, "body2");
    }

    #[test]
    fn scans_tags_with_name_only_range() {
        let hits = scan_tags_content("a @ASSUME:foo_bar rest");
        assert_eq!(hits.len(), 1);
        let hit = &hits[0];
        assert_eq!(hit.name, "foo_bar");
        assert_eq!(hit.range.start.line, 0);
        assert_eq!(hit.range.start.character, 10);
        assert_eq!(hit.range.end.character, 17);
    }

    #[test]
    fn finds_nearest_scope() {
        let mut scopes = HashSet::new();
        scopes.insert(PathBuf::from("/root/area"));
        scopes.insert(PathBuf::from("/root/area/nested"));
        let path = Path::new("/root/area/nested/file.rs");
        assert_eq!(
            find_scope(&scopes, path),
            Some(PathBuf::from("/root/area/nested"))
        );
    }

    #[test]
    fn contains_is_inclusive() {
        let range = Range {
            start: Position {
                line: 1,
                character: 3,
            },
            end: Position {
                line: 1,
                character: 8,
            },
        };
        assert!(contains(
            &range,
            Position {
                line: 1,
                character: 3
            }
        ));
        assert!(contains(
            &range,
            Position {
                line: 1,
                character: 8
            }
        ));
        assert!(!contains(
            &range,
            Position {
                line: 1,
                character: 2
            }
        ));
    }
}
