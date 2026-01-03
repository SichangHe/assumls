// @ASSUME:lsp_e2e_testing
//! Shared helpers for LSP end-to-end tests.
#![allow(dead_code)]

use std::path::PathBuf;
use std::process::Stdio;
use std::time::Duration;

use anyhow::{Result, anyhow};
use serde_json::{Value, json};
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout, Command};
use tokio::time::timeout;
use tower_lsp::lsp_types::Url;

/// LSP client for E2E testing
pub struct LspClient {
    pub stdin: ChildStdin,
    pub stdout: BufReader<ChildStdout>,
    pub child: Child,
    next_id: u64,
}

impl LspClient {
    /// Spawn LSP server process and initialize
    pub async fn spawn(workspace: &PathBuf) -> Result<Self> {
        let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("assumls"));
        cmd.arg("lsp");
        cmd.kill_on_drop(true);
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::inherit());

        let mut child = cmd.spawn()?;
        let stdin = child.stdin.take().ok_or_else(|| anyhow!("stdin missing"))?;
        let stdout = BufReader::new(
            child
                .stdout
                .take()
                .ok_or_else(|| anyhow!("stdout missing"))?,
        );

        let mut client = Self {
            stdin,
            stdout,
            child,
            next_id: 1,
        };

        // Initialize
        let root_uri = Url::from_file_path(workspace).unwrap().to_string();
        let id = client.next_id;
        client.next_id += 1;

        let payload = json!({"jsonrpc": "2.0", "id": id, "method": "initialize", "params": {"rootUri": root_uri, "capabilities": {}}});
        client.send_payload(payload).await?;
        let _ = timeout(Duration::from_secs(5), client.read_response(id)).await??;

        Ok(client)
    }

    /// Send a request and wait for response
    pub async fn request(&mut self, method: &str, params: Value) -> Result<Value> {
        let id = self.next_id;
        self.next_id += 1;

        let payload = json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params});
        self.send_payload(payload).await?;

        timeout(Duration::from_secs(5), self.read_response(id)).await?
    }

    /// Send a notification (no response expected)
    pub async fn notify(&mut self, method: &str, params: Value) -> Result<()> {
        let payload = json!({"jsonrpc": "2.0", "method": method, "params": params});
        self.send_payload(payload).await
    }

    /// Open a file in the LSP server
    pub async fn did_open(&mut self, file: &PathBuf, text: String) -> Result<()> {
        let uri = Url::from_file_path(file).unwrap().to_string();
        self.notify(
            "textDocument/didOpen",
            json!({"textDocument": {"uri": uri, "text": text, "version": 1}}),
        )
        .await
    }

    /// Get hover at position
    pub async fn hover(&mut self, file: &PathBuf, line: u32, character: u32) -> Result<Value> {
        let uri = Url::from_file_path(file).unwrap().to_string();
        self.request(
            "textDocument/hover",
            json!({"textDocument": {"uri": uri}, "position": {"line": line, "character": character}}),
        )
        .await
    }

    /// Get completion at position
    pub async fn completion(&mut self, file: &PathBuf, line: u32, character: u32) -> Result<Value> {
        let uri = Url::from_file_path(file).unwrap().to_string();
        self.request(
            "textDocument/completion",
            json!({"textDocument": {"uri": uri}, "position": {"line": line, "character": character}}),
        )
        .await
    }

    /// Read next message from server (notification or response)
    pub async fn read_message(&mut self) -> Result<Option<Value>> {
        let mut content_length = None;
        loop {
            let mut line = String::new();
            if self.stdout.read_line(&mut line).await? == 0 {
                return Ok(None);
            }
            if line == "\r\n" {
                break;
            }
            if let Some(rest) = line.strip_prefix("Content-Length:") {
                content_length = rest.trim().parse::<usize>().ok();
            }
        }
        let len = content_length.ok_or_else(|| anyhow!("no content-length"))?;
        let mut buf = vec![0u8; len];
        self.stdout.read_exact(&mut buf).await?;
        let value = serde_json::from_slice(&buf)?;
        Ok(Some(value))
    }

    async fn send_payload(&mut self, payload: Value) -> Result<()> {
        let text = payload.to_string();
        let header = format!("Content-Length: {}\r\n\r\n", text.len());
        self.stdin.write_all(header.as_bytes()).await?;
        self.stdin.write_all(text.as_bytes()).await?;
        self.stdin.flush().await?;
        Ok(())
    }

    async fn read_response(&mut self, id: u64) -> Result<Value> {
        loop {
            let msg = self
                .read_message()
                .await?
                .ok_or_else(|| anyhow!("stream closed"))?;
            if msg.get("id").and_then(|v| v.as_u64()) == Some(id) {
                return Ok(msg);
            }
        }
    }
}
