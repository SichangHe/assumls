const assert = require("assert");
const vscode = require("vscode");

/** Hover, go-to-definition, and diagnostics reach VS Code from the real `assumls`. */
exports.run = async () => {
  const [root] = vscode.workspace.workspaceFolders;
  const main_rs = vscode.Uri.joinPath(root.uri, "main.rs");
  await vscode.window.showTextDocument(main_rs);
  await vscode.extensions.getExtension("SichangHe.assumls").activate();
  const in_core_shared = new vscode.Position(0, 15);
  const hovers = await vscode.commands.executeCommand(
    "vscode.executeHoverProvider",
    main_rs,
    in_core_shared,
  );
  const hover_text = hovers
    .flatMap((hover) => hover.contents)
    .map((content) => content.value)
    .join("\n");
  assert.match(hover_text, /Core behavior shared across runtimes\./);
  const [definition] = await vscode.commands.executeCommand(
    "vscode.executeDefinitionProvider",
    main_rs,
    in_core_shared,
  );
  assert.strictEqual(
    (definition.uri ?? definition.targetUri).path,
    vscode.Uri.joinPath(root.uri, "ASSUM.md").path,
  );
  const edit = new vscode.WorkspaceEdit();
  edit.insert(main_rs, new vscode.Position(0, 0), "// @ASSUME:not_defined\n");
  const diagnosed = new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      listener.dispose();
      reject(new Error("AssumLS did not publish diagnostics within 15 seconds."));
    }, 15000);
    const listener = vscode.languages.onDidChangeDiagnostics(() => {
      const diagnostics = vscode.languages.getDiagnostics(main_rs);
      if (diagnostics.some(diagnostic => diagnostic.message.includes("`not_defined`"))) {
        clearTimeout(timeout);
        listener.dispose();
        resolve(diagnostics);
      }
    });
  });
  await vscode.workspace.applyEdit(edit);
  await vscode.workspace.saveAll();
  const diagnostic = (await diagnosed).find(item => item.message.includes("`not_defined`"));
  assert.strictEqual(diagnostic.source, "AssumLS");
  assert.match(diagnostic.message, /`not_defined` not defined in scope/);
  console.log("AssumLS VSIX: hover, definition, and diagnostics verified with the real server.");
};
