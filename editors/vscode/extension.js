// 🧑 "make vscode extension for all the other language servers I have"
const { workspace } = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

let client;

/** Start `assumls lsp` over stdio for every file in the workspace. */
exports.activate = async () => {
  const command = workspace.getConfiguration("assumls").get("path");
  client = new LanguageClient(
    "assumls",
    "AssumLS",
    { command, args: ["lsp"] },
    { documentSelector: [{ scheme: "file" }] },
  );
  await client.start();
};

exports.deactivate = () => client?.stop();
