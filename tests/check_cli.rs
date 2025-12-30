use anyhow::Result;
use assert_cmd::Command;
use insta::assert_snapshot;
use std::path::PathBuf;

fn ws(path: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(path)
}

#[test]
fn check_succeeds_on_valid_workspace() -> Result<()> {
    let assert = Command::new(assert_cmd::cargo::cargo_bin!("assumls"))
        .env("RUST_LOG", "off")
        .arg("check")
        .arg(ws("test_data/cli_valid"))
        .assert()
        .success();
    let out = assert.get_output();
    let body = format!(
        "stdout:\n{}stderr:\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert_snapshot!("check_ok", body);
    Ok(())
}

#[test]
fn check_fails_on_missing_definition() -> Result<()> {
    let assert = Command::new(assert_cmd::cargo::cargo_bin!("assumls"))
        .env("RUST_LOG", "off")
        .arg("check")
        .arg(ws("test_data/cli_invalid"))
        .assert()
        .failure();
    let out = assert.get_output();
    let body = format!(
        "status: {:?}\nstdout:\n{}stderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert_snapshot!("check_missing_def", body);
    Ok(())
}

#[test]
fn check_errors_when_tools_missing() -> Result<()> {
    let assert = Command::new(assert_cmd::cargo::cargo_bin!("assumls"))
        .env("PATH", "")
        .env("RUST_LOG", "off")
        .arg("check")
        .arg(ws("test_data/cli_valid"))
        .assert()
        .failure();
    let out = assert.get_output();
    let body = format!(
        "status: {:?}\nstdout:\n{}stderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert_snapshot!("check_missing_tools", body);
    Ok(())
}

#[test]
fn check_fails_on_duplicate_definition() -> Result<()> {
    let assert = Command::new(assert_cmd::cargo::cargo_bin!("assumls"))
        .env("RUST_LOG", "off")
        .arg("check")
        .arg(ws("test_data/cli_duplicate"))
        .assert()
        .failure();
    let out = assert.get_output();
    let body = format!(
        "status: {:?}\nstdout:\n{}stderr:\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    assert_snapshot!("check_duplicate_def", body);
    Ok(())
}
