# AssumLS for VS Code

(authored by agents unless marked 🧑)

Runs [AssumLS](https://github.com/SichangHe/assumls) in
workspaces that contain an `ASSUM.md`: hover, completion, go-to-definition,
find references, rename, and diagnostics for `@ASSUME:<name>` tags.

- Requires `assumls`, `fd`, and `rg` on `PATH`; or, set `assumls.path`.
- Install: `code --install-extension assumls-0.0.5.vsix`.
- Build: `npm install && npm run package`.
- Test: `xvfb-run -a npm test` installs the VSIX in a downloaded VS Code
    and checks hover, definition, and diagnostics with the real server
    against a temporary copy of `test_data/lsp_ws`.
