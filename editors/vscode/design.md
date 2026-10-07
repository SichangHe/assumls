# extension design
(authored by agents unless marked 🧑)

- activate when the workspace contains `ASSUM.md`
  - run `assumls lsp` through the configured executable path
  - register every file for assumption tags across languages
- bundle the language client into the installable VSIX
- test the installed VSIX in VS Code against a temporary copy of `lsp_ws`
  - use the real server for hover, definition, and diagnostics
