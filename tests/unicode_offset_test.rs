#[test]
fn test_unicode_chars_byte_vs_utf16_offset() {
    // Chinese: 3 bytes/char → 1 UTF-16 unit/char
    // "@ASSUME:" at byte 16, UTF-16 position 8
    let content = "// 你好世界 @ASSUME:utf16_positions rest";
    let tags = assumls::parser::scan_tags_content(content);

    assert_eq!(tags.len(), 1);
    let tag = &tags[0];
    assert_eq!(tag.name, "utf16_positions");
    assert_eq!(tag.range.start.character, 8);
}

#[test]
fn test_ascii_chars_work_correctly() {
    let content = "// hello @ASSUME:test_def";
    let tags = assumls::parser::scan_tags_content(content);

    assert_eq!(tags.len(), 1);
    assert_eq!(tags[0].range.start.character, 9);
}

#[test]
fn test_emoji_in_comment() {
    // Emoji: 4 bytes → 2 UTF-16 units (surrogate pair)
    // "@ASSUME:" at byte 8, UTF-16 position 6
    let content = "// 🚀 @ASSUME:test_def";
    let tags = assumls::parser::scan_tags_content(content);

    assert_eq!(tags[0].range.start.character, 6);
}

#[test]
fn test_definition_with_unicode() {
    use std::path::Path;

    let content = "# test_中文_name\nDescription with Unicode.\n";
    let path = Path::new("ASSUM.md");

    let docs = assumls::parser::parse_assumptions_content(content, path).unwrap();
    assert_eq!(docs.len(), 0, "Should reject non-ASCII assumption names");

    let content2 = "# valid_name\n描述文字 Chinese description.\n";
    let docs2 = assumls::parser::parse_assumptions_content(content2, path).unwrap();
    assert_eq!(docs2.len(), 1);

    let doc = &docs2[0];
    assert_eq!(doc.range.start.character, 2);
    assert_eq!(doc.range.end.character, 12);
}

#[test]
fn test_hover_position_with_unicode() {
    use assumls::parser::contains;
    use tower_lsp::lsp_types::{Position, Range};

    let range = Range {
        start: Position {
            line: 0,
            character: 5,
        },
        end: Position {
            line: 0,
            character: 10,
        },
    };

    assert!(contains(
        &range,
        Position {
            line: 0,
            character: 7
        }
    ));
}
