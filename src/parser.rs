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
            // @ASSUME:definition_range_excludes_prefix
            let range = Range {
                start: Position {
                    line: idx.try_into().unwrap(),
                    character: 2,
                },
                end: Position {
                    line: idx.try_into().unwrap(),
                    character: (2 + name.len()).try_into().unwrap(),
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
            let end_char = name_start + name.len();

            // @ASSUME:utf16_positions
            let range = Range {
                start: Position {
                    line: line_idx.try_into().unwrap(),
                    character: byte_offset_to_utf16(line, start),
                },
                end: Position {
                    line: line_idx.try_into().unwrap(),
                    character: byte_offset_to_utf16(line, end_char),
                },
            };
            let name_range = Range {
                start: Position {
                    line: line_idx.try_into().unwrap(),
                    character: byte_offset_to_utf16(line, name_start),
                },
                end: Position {
                    line: line_idx.try_into().unwrap(),
                    character: byte_offset_to_utf16(line, end_char),
                },
            };
            let name_len = name.len();
            hits.push(TagHit {
                name,
                range,
                name_range,
            });
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

/// Collect all ancestor scopes from nearest to root for inheritance.
/// @ASSUME:scope_resolution_nearest_parent
pub fn collect_ancestor_scopes(scope_roots: &HashSet<PathBuf>, path: &Path) -> Vec<PathBuf> {
    let mut scopes = Vec::new();
    let mut current = path.parent();
    while let Some(dir) = current {
        if scope_roots.contains(dir) {
            scopes.push(dir.to_path_buf());
        }
        current = dir.parent();
    }
    scopes
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

/// @ASSUME:utf16_positions
fn byte_offset_to_utf16(s: &str, byte_offset: usize) -> u32 {
    let substr = &s[..byte_offset.min(s.len())];
    // @ASSUME:position_overflow_safety
    substr
        .chars()
        .map(|c| c.len_utf16())
        .sum::<usize>()
        .try_into()
        .unwrap()
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
        // Use actual assumption from src/ASSUM.md to avoid check errors
        let hits = scan_tags_content("a @ASSUME:utf16_positions rest");
        assert_eq!(hits.len(), 1);
        let hit = &hits[0];
        assert_eq!(hit.name, "utf16_positions");
        assert_eq!(hit.range.start.line, 0);
        assert_eq!(hit.range.start.character, 2);
        assert_eq!(hit.range.end.character, 25);
        assert_eq!(hit.name_range.start.character, 10);
        assert_eq!(hit.name_range.end.character, 25);
    }

    #[test]
    fn finds_nearest_scope() {
        let mut scopes = HashSet::new();
        scopes.insert(PathBuf::from("/root"));
        scopes.insert(PathBuf::from("/root/subdir"));
        assert_eq!(
            collect_ancestor_scopes(&scopes, Path::new("/root/subdir/file.rs")),
            vec![PathBuf::from("/root/subdir"), PathBuf::from("/root")]
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
