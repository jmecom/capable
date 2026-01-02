#!/usr/bin/env python3
"""
Generate a simple static code browser for the Capable repo.

Usage:
  python3 tools/gen_code_browser.py
  python3 tools/gen_code_browser.py --out docs/code-browser
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
from pathlib import Path


SKIP_DIRS = {
    ".git",
    "target",
    ".hermit",
    "vscode/node_modules",
    "docs/code-browser",
}

INCLUDE_EXTS = {
    ".cap",
    ".rs",
    ".md",
    ".toml",
    ".txt",
    ".json",
    ".yml",
    ".yaml",
}

INCLUDE_FILES = {
    "Cargo.lock",
}


def should_skip_dir(path: Path) -> bool:
    rel = str(path).replace("\\", "/")
    for skip in SKIP_DIRS:
        if rel == skip or rel.startswith(skip + "/"):
            return True
    return False


def collect_files(root: Path) -> list[Path]:
    files: list[Path] = []
    for dirpath, dirnames, filenames in os.walk(root):
        rel_dir = Path(dirpath).relative_to(root)
        if should_skip_dir(rel_dir):
            dirnames[:] = []
            continue
        dirnames[:] = [
            d for d in dirnames if not should_skip_dir(rel_dir / d)
        ]
        for name in filenames:
            path = Path(dirpath) / name
            rel = path.relative_to(root)
            if name in INCLUDE_FILES or path.suffix in INCLUDE_EXTS:
                files.append(rel)
    return sorted(files)


def build_tree(paths: list[Path]) -> dict:
    root: dict = {"name": "", "children": {}, "path": None}
    for path in paths:
        parts = path.parts
        node = root
        for part in parts:
            node = node["children"].setdefault(
                part, {"name": part, "children": {}, "path": None}
            )
        node["path"] = str(path).replace("\\", "/")
    return root


def node_to_json(node: dict) -> dict:
    children = node["children"]
    items = [node_to_json(children[name]) for name in sorted(children.keys())]
    return {
        "name": node["name"],
        "path": node["path"],
        "children": items,
    }


def render_index(tree_json: dict) -> str:
    data = json.dumps(tree_json)
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Capable Code Browser</title>
  <style>
    :root {{
      --bg: #f6f3ef;
      --panel: #ffffff;
      --text: #1f1a16;
      --muted: #7a6f68;
      --accent: #2f6b5f;
      --border: #e0d8d0;
      --mono: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", monospace;
    }}
    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      font-family: "Iowan Old Style", "Palatino", "Book Antiqua", serif;
      color: var(--text);
      background: var(--bg);
    }}
    header {{
      padding: 12px 16px;
      border-bottom: 1px solid var(--border);
      background: linear-gradient(90deg, #f7ede2, #f0f4f2);
      font-weight: 700;
      letter-spacing: 0.3px;
    }}
    .layout {{
      display: grid;
      grid-template-columns: 320px 1fr;
      min-height: calc(100vh - 48px);
    }}
    nav {{
      border-right: 1px solid var(--border);
      background: var(--panel);
      padding: 12px;
      overflow: auto;
    }}
    main {{
      padding: 12px;
      overflow: auto;
    }}
    pre {{
      background: var(--panel);
      border: 1px solid var(--border);
      padding: 12px;
      white-space: pre;
      font-family: var(--mono);
      font-size: 13px;
      line-height: 1.4;
      overflow: auto;
    }}
    .node {{
      cursor: pointer;
      padding: 2px 4px;
      border-radius: 4px;
      color: var(--text);
    }}
    .node:hover {{
      background: #efe8e0;
    }}
    .dir {{
      font-weight: 600;
    }}
    .file {{
      color: var(--muted);
    }}
    .indent {{
      margin-left: 14px;
    }}
    .path {{
      color: var(--accent);
      font-size: 12px;
      margin-bottom: 8px;
      font-family: var(--mono);
    }}
    @media (max-width: 900px) {{
      .layout {{
        grid-template-columns: 1fr;
      }}
      nav {{
        border-right: none;
        border-bottom: 1px solid var(--border);
      }}
    }}
  </style>
</head>
<body>
  <header>Capable Code Browser</header>
  <div class="layout">
    <nav id="tree"></nav>
    <main>
      <div class="path" id="path">Select a file…</div>
      <pre id="content"></pre>
    </main>
  </div>
  <script>
    const treeData = {data};
    const treeEl = document.getElementById("tree");
    const contentEl = document.getElementById("content");
    const pathEl = document.getElementById("path");

    function renderNode(node, container) {{
      if (!node.children || node.children.length === 0) {{
        if (!node.path) return;
        const item = document.createElement("div");
        item.className = "node file";
        item.textContent = node.name;
        item.onclick = () => loadFile(node.path);
        container.appendChild(item);
        return;
      }}
      if (node.name) {{
        const dir = document.createElement("div");
        dir.className = "node dir";
        dir.textContent = node.name;
        container.appendChild(dir);
        container = wrapIndent(container);
      }}
      for (const child of node.children) {{
        renderNode(child, container);
      }}
    }}

    function wrapIndent(container) {{
      const wrap = document.createElement("div");
      wrap.className = "indent";
      container.appendChild(wrap);
      return wrap;
    }}

    function loadFile(path) {{
      pathEl.textContent = path;
      fetch("files/" + path)
        .then(resp => resp.text())
        .then(text => {{
          contentEl.textContent = text;
        }})
        .catch(err => {{
          contentEl.textContent = "Failed to load file: " + err;
        }});
    }}

    renderNode(treeData, treeEl);
  </script>
</body>
</html>
"""


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", default="docs/code-browser", help="output directory")
    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parents[1]
    out_root = (repo_root / args.out).resolve()
    files_root = out_root / "files"
    out_root.mkdir(parents=True, exist_ok=True)
    files_root.mkdir(parents=True, exist_ok=True)

    files = collect_files(repo_root)
    for rel in files:
        src = repo_root / rel
        dest = files_root / rel
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(src, dest)

    tree = node_to_json(build_tree(files))
    index_html = render_index(tree)
    (out_root / "index.html").write_text(index_html, encoding="utf-8")

    print(f"Wrote {out_root / 'index.html'}")


if __name__ == "__main__":
    main()
