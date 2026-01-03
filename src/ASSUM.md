# rg_exit_code_one
rg exit 1 = no matches (success), 2+ = error. Accept 0 and 1.

# position_overflow_safety
Use `try_into().unwrap()` not `as u32` - explicit panic w/ trace vs silent truncation.

# definition_range_excludes_prefix
`# assumption_name` range: [2, 2+len), excludes "# ". Allows rename w/o rewriting prefix.

# scope_resolution_nearest_parent
Files inherit assumptions from all ancestor ASSUM.md files.

Child scopes shadow parent definitions for the same assumption name.
Resolution searches from nearest to root - first match wins.


# incremental_indexing
Overlay updates only reparse changed file, not full workspace.

ASSUM.md → update definitions for that scope. Non-ASSUM.md → update tags only.

# unwrap_or_default_on_reply
LSP replies use `unwrap_or_default()` when state unavailable - returns empty results vs error.

Graceful degradation: client gets empty hovers/completions if index not ready.

# overlay_version_ordering
Drop out-of-order overlays by version number to handle async LSP protocol race conditions.

Editor may send v2 before v1 arrives. Keep highest version seen per file.
