# utf16_positions
LSP Position.character is UTF-16 code units, not UTF-8 bytes.

Chinese: 3 bytes → 1 unit. Emoji: 4 bytes → 2 units. ASCII: 1 byte = 1 unit.

# rg_exit_code_one
rg exit 1 = no matches (success), 2+ = error. Accept 0 and 1.

# position_overflow_safety
Use `try_into().unwrap()` not `as u32` - explicit panic w/ trace vs silent truncation.

# definition_range_excludes_prefix
`# assumption_name` range: [2, 2+len), excludes "# ". Allows rename w/o rewriting prefix.

# scope_resolution_nearest_parent
Files use nearest parent ASSUM.md. No inheritance - scopes are independent.

# incremental_indexing
Overlay updates only reparse changed file, not full workspace.

ASSUM.md → update definitions for that scope. Non-ASSUM.md → update tags only.
