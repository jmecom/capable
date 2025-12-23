const vscode = require("vscode");
const cp = require("child_process");

const CHECK_DELAY_MS = 200;
const timers = new Map();
const collections = new Map();

function activate(context) {
  const collection = vscode.languages.createDiagnosticCollection("capable");
  context.subscriptions.push(collection);
  collections.set("cap", collection);

  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument((doc) => {
      if (doc.languageId === "cap") {
        scheduleCheck(doc);
      }
    })
  );

  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument((doc) => {
      if (doc.languageId === "cap") {
        scheduleCheck(doc);
      }
    })
  );

  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument((event) => {
      const doc = event.document;
      if (doc.languageId === "cap") {
        scheduleCheck(doc);
      }
    })
  );

  for (const doc of vscode.workspace.textDocuments) {
    if (doc.languageId === "cap") {
      scheduleCheck(doc);
    }
  }
}

function deactivate() {
  for (const timer of timers.values()) {
    clearTimeout(timer);
  }
}

function scheduleCheck(doc) {
  const key = doc.uri.toString();
  if (timers.has(key)) {
    clearTimeout(timers.get(key));
  }
  timers.set(
    key,
    setTimeout(() => {
      timers.delete(key);
      runCheck(doc);
    }, CHECK_DELAY_MS)
  );
}

function resolveCapcPath() {
  const config = vscode.workspace.getConfiguration("capable");
  const capcPath = config.get("capcPath");
  if (capcPath && capcPath.length > 0) {
    return capcPath;
  }
  const legacy = config.get("serverPath");
  if (legacy && legacy.length > 0) {
    return legacy;
  }
  return "capc";
}

function runCheck(doc) {
  const capcPath = resolveCapcPath();
  const filePath = doc.uri.fsPath;
  cp.execFile(capcPath, ["check", filePath], (err, stdout, stderr) => {
    const diagnostics = [];
    if (err) {
      diagnostics.push(...parseDiagnostics(doc, stderr || stdout || ""));
    }
    const collection = collections.get("cap");
    if (collection) {
      collection.set(doc.uri, diagnostics);
    }
  });
}

function parseDiagnostics(doc, output) {
  const lines = output.split(/\r?\n/);
  const diags = [];
  let message = null;
  let line = null;
  let col = null;
  for (const text of lines) {
    const msgMatch = text.match(/×\s+(.*)$/);
    if (msgMatch) {
      message = msgMatch[1].trim();
    }
    const locMatch = text.match(/\[(.+):(\d+):(\d+)\]/);
    if (locMatch) {
      const file = locMatch[1];
      if (file && file !== doc.uri.fsPath) {
        continue;
      }
      line = parseInt(locMatch[2], 10);
      col = parseInt(locMatch[3], 10);
    }
  }
  if (message) {
    const startLine = Math.max((line || 1) - 1, 0);
    const startCol = Math.max((col || 1) - 1, 0);
    const range = new vscode.Range(
      new vscode.Position(startLine, startCol),
      new vscode.Position(startLine, startCol + 1)
    );
    diags.push(
      new vscode.Diagnostic(range, message, vscode.DiagnosticSeverity.Error)
    );
  }
  return diags;
}

module.exports = { activate, deactivate };
