// @ASSUME:root_assumption - should inherit from parent
// @ASSUME:parent_assumption_used_in_subdir
// @ASSUME:subdir_assumption - defined in this scope
// @ASSUME:shadowed_assumption - should use subdir version
fn subdir_file() {}
