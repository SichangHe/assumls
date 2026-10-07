const { cpSync, mkdtempSync } = require("node:fs");
const { tmpdir } = require("node:os");
const { join, resolve } = require("node:path");
const { spawnSync } = require("node:child_process");
const { runTests, downloadAndUnzipVSCode, resolveCliArgsFromVSCodeExecutablePath } = require("@vscode/test-electron");
const { name, version } = require("../package.json");

/** Install and test the packaged extension with a disposable fixture. */
async function main() {
  const extension_dir = resolve(__dirname, "..");
  const fixture = mkdtempSync(join(tmpdir(), "assumls-vscode-"));
  cpSync(resolve(extension_dir, "../../test_data/lsp_ws"), fixture, { recursive: true });
  const user_data = mkdtempSync(join(tmpdir(), "assumls-vscode-user-"));
  const vscodeExecutablePath = await downloadAndUnzipVSCode();
  const extensions_dir = mkdtempSync(join(tmpdir(), "assumls-vscode-extensions-"));
  const [cli, ...args] = resolveCliArgsFromVSCodeExecutablePath(vscodeExecutablePath);
  const install = spawnSync(cli, [...args, `--extensions-dir=${extensions_dir}`, `--user-data-dir=${user_data}`, "--install-extension", join(extension_dir, `${name}-${version}.vsix`)], { stdio: "inherit" });
  if (install.status !== 0) throw new Error("VSIX installation failed.");
  await runTests({
    vscodeExecutablePath,
    extensionDevelopmentPath: join(__dirname, "empty_extension"),
    extensionTestsPath: join(__dirname, "suite.js"),
    launchArgs: [fixture, `--user-data-dir=${user_data}`, `--extensions-dir=${extensions_dir}`, "--no-sandbox", "--disable-gpu", "--disable-workspace-trust"],
  });
}
main().catch(error => { console.error(error); process.exit(1); });
