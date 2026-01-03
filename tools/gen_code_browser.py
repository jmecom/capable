#!/usr/bin/env python3
"""
Generate a simple static code browser for Capable .cap files.

Usage:
  python3 tools/gen_code_browser.py
  python3 tools/gen_code_browser.py --out docs/code-browser
"""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path


SKIP_DIRS = {
    ".git",
    "target",
    ".hermit",
    "vscode/node_modules",
    "docs/code-browser",
}

INCLUDE_EXTS = {".cap"}
INCLUDE_ROOTS = [Path("stdlib/sys")]


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
        if rel_dir != Path("."):
            rel_str = str(rel_dir).replace("\\", "/")
            under_root = any(rel_str.startswith(str(base)) for base in INCLUDE_ROOTS)
            is_prefix = any(str(base).startswith(rel_str) for base in INCLUDE_ROOTS)
            if not (under_root or is_prefix):
                dirnames[:] = []
                continue
        dirnames[:] = [
            d for d in dirnames if not should_skip_dir(rel_dir / d)
        ]
        for name in filenames:
            path = Path(dirpath) / name
            rel = path.relative_to(root)
            if path.suffix in INCLUDE_EXTS:
                files.append(rel)
    return sorted(files)


def count_braces(line: str) -> tuple[int, int]:
    return line.count("{"), line.count("}")


def parse_cap_file(path: Path) -> dict:
    text = path.read_text(encoding="utf-8")
    lines = text.splitlines()
    module_name = path.stem
    module_docs: list[str] = []
    doc_buf: list[str] = []
    items: list[dict] = []
    current_impl: str | None = None
    impl_depth: int | None = None
    brace_depth = 0

    def flush_docs() -> list[str]:
        nonlocal doc_buf
        docs = doc_buf
        doc_buf = []
        return docs

    for raw in lines:
        line = raw.strip()
        if line.startswith("///"):
            doc_buf.append(line[3:].lstrip())
            continue

        if line.startswith("package ") and doc_buf and not module_docs:
            module_docs = flush_docs()
        if line.startswith("module "):
            parts = line.split()
            if len(parts) >= 2:
                module_name = parts[1]
            if doc_buf and not module_docs:
                module_docs = flush_docs()

        if line.startswith("impl "):
            parts = line.split()
            if len(parts) >= 2:
                current_impl = parts[1]
                impl_depth = brace_depth
            flush_docs()

        if "fn " in line and line.startswith(("pub fn", "fn")):
            docs = flush_docs()
            items.append(
                {
                    "kind": "method" if current_impl else "fn",
                    "impl": current_impl,
                    "sig": line.rstrip("{").strip(),
                    "docs": docs,
                }
            )
        elif "struct " in line and line.startswith(
            (
                "pub struct",
                "struct",
                "pub copy struct",
                "pub copy opaque struct",
                "pub copy capability struct",
                "pub linear capability struct",
                "pub capability struct",
            )
        ):
            docs = flush_docs()
            items.append(
                {
                    "kind": "struct",
                    "sig": line.rstrip("{").strip(),
                    "docs": docs,
                }
            )
        elif "enum " in line and line.startswith(("pub enum", "enum")):
            docs = flush_docs()
            items.append(
                {
                    "kind": "enum",
                    "sig": line.rstrip("{").strip(),
                    "docs": docs,
                }
            )

        opens, closes = count_braces(line)
        brace_depth += opens - closes
        if current_impl is not None and impl_depth is not None:
            if brace_depth <= impl_depth:
                current_impl = None
                impl_depth = None

    if doc_buf and not module_docs:
        module_docs = flush_docs()

    return {
        "path": str(path).replace("\\", "/"),
        "module": module_name,
        "docs": module_docs,
        "items": items,
        "source": text,
    }


def render_index(modules: list[dict]) -> str:
    data = json.dumps(modules)
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Capable Docs</title>
  <style>
    :root {{
      --ink: #baccd4;
      --muted: #77758a;
      --surface: #10121c;
      --panel: #1a1c28;
      --edge: #252834;
      --accent: #f57c6c;
      --accent-hover: #ff9a8f;
      --rule: 1px solid rgba(186, 204, 212, 0.1);
      --page-pad: 1.5rem;
      --mono: "IBM Plex Mono", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", monospace;
    }}
    @import "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;700&display=swap";
    * {{ box-sizing: border-box; }}
    html, body {{ height: 100%; }}
    body {{
      margin: 0;
      font-family: var(--mono);
      color: var(--ink);
      background: var(--surface);
      line-height: 1.6;
      font-size: 15px;
    }}
    #dot-grid {{
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      pointer-events: none;
      background-image:
        radial-gradient(rgba(186, 204, 212, 0.08) 0.7px, transparent 0.7px);
      background-size: 22px 22px;
      z-index: 0;
    }}
    header {{
      position: sticky;
      top: 0;
      z-index: 10;
      padding: 0.85rem var(--page-pad);
      border-bottom: var(--rule);
      background-color: rgba(16, 18, 28, 0.95);
      backdrop-filter: blur(10px);
      letter-spacing: 0.12rem;
      text-transform: uppercase;
      font-size: 0.85rem;
      color: var(--muted);
    }}
    .layout {{
      position: relative;
      z-index: 1;
      display: grid;
      grid-template-columns: 300px 1fr;
      min-height: calc(100vh - 52px);
    }}
    nav {{
      border-right: 1px solid var(--edge);
      background: var(--panel);
      padding: 1rem;
      overflow: auto;
      max-height: calc(100vh - 52px);
    }}
    main {{
      padding: 1.5rem var(--page-pad) 3rem;
      overflow: auto;
    }}
    pre {{
      background: #0e0f18;
      border: 1px solid var(--edge);
      padding: 1rem;
      white-space: pre;
      font-family: var(--mono);
      font-size: 13px;
      line-height: 1.5;
      overflow: auto;
    }}
    .node {{
      cursor: pointer;
      padding: 4px 6px;
      border-radius: 4px;
      color: var(--ink);
    }}
    .node:hover {{
      background: #171a24;
    }}
    .dir {{
      font-weight: 700;
    }}
    .file {{
      color: var(--muted);
    }}
    .indent {{
      margin-left: 12px;
    }}
    .path {{
      color: var(--accent);
      font-size: 12px;
      margin-bottom: 8px;
      font-family: var(--mono);
    }}
    h1 {{
      font-size: 1.6rem;
      margin: 0 0 1rem;
    }}
    h2 {{
      margin: 2rem 0 0.75rem;
      font-size: 1.25rem;
      font-weight: 700;
      color: var(--ink);
    }}
    .doc {{
      color: var(--ink);
      margin-bottom: 1rem;
    }}
    .item {{
      border-top: var(--rule);
      padding: 0.75rem 0;
    }}
    .sig {{
      color: var(--accent);
      font-weight: 600;
    }}
    .muted {{
      color: var(--muted);
    }}
    .source-toggle {{
      margin-top: 1rem;
      cursor: pointer;
      color: var(--accent);
    }}
    @media (max-width: 900px) {{
      .layout {{
        grid-template-columns: 1fr;
      }}
      nav {{
        border-right: none;
        border-bottom: 1px solid var(--edge);
      }}
    }}
  </style>
</head>
<body>
  <div id="dot-grid"></div>
  <header>Capable Docs</header>
  <div class="layout">
    <nav id="tree"></nav>
    <main>
      <div class="path" id="path">Select a module…</div>
      <div id="module"></div>
    </main>
  </div>
  <script>
    const modules = {data};
    const treeEl = document.getElementById("tree");
    const moduleEl = document.getElementById("module");
    const pathEl = document.getElementById("path");

    function renderModuleNav(mod) {{
      const item = document.createElement("div");
      item.className = "node file";
      item.textContent = mod.module;
      item.onclick = () => loadModule(mod);
      treeEl.appendChild(item);
    }}

    function docLines(lines) {{
      if (!lines || lines.length === 0) return "";
      return lines.map(l => `<div>${{l}}</div>`).join("");
    }}

    function loadModule(mod) {{
      pathEl.textContent = mod.path;
      moduleEl.innerHTML = "";
      const title = document.createElement("h1");
      title.textContent = mod.module;
      moduleEl.appendChild(title);

      if (mod.docs && mod.docs.length) {{
        const doc = document.createElement("div");
        doc.className = "doc";
        doc.innerHTML = docLines(mod.docs);
        moduleEl.appendChild(doc);
      }}

      const kinds = {{
        struct: "Structs",
        enum: "Enums",
        fn: "Functions",
        method: "Methods",
      }};
      for (const kind of ["struct", "enum", "fn", "method"]) {{
        const group = mod.items.filter(i => i.kind === kind);
        if (group.length === 0) continue;
        const h2 = document.createElement("h2");
        h2.textContent = kinds[kind];
        moduleEl.appendChild(h2);
        for (const item of group) {{
          const wrap = document.createElement("div");
          wrap.className = "item";
          const sig = document.createElement("div");
          sig.className = "sig";
          sig.textContent = item.sig;
          wrap.appendChild(sig);
          if (item.impl) {{
            const imp = document.createElement("div");
            imp.className = "muted";
            imp.textContent = `impl ${{item.impl}}`;
            wrap.appendChild(imp);
          }}
          if (item.docs && item.docs.length) {{
            const doc = document.createElement("div");
            doc.className = "doc";
            doc.innerHTML = docLines(item.docs);
            wrap.appendChild(doc);
          }}
          moduleEl.appendChild(wrap);
        }}
      }}

      const toggle = document.createElement("div");
      toggle.className = "source-toggle";
      toggle.textContent = "Show source";
      let shown = false;
      let pre;
      toggle.onclick = () => {{
        if (!shown) {{
          pre = document.createElement("pre");
          pre.textContent = mod.source;
          moduleEl.appendChild(pre);
          toggle.textContent = "Hide source";
          shown = true;
        }} else {{
          pre.remove();
          toggle.textContent = "Show source";
          shown = false;
        }}
      }};
      moduleEl.appendChild(toggle);
    }}

    modules.forEach(renderModuleNav);
    if (modules.length) {{
      loadModule(modules[0]);
    }}
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
    out_root.mkdir(parents=True, exist_ok=True)

    files = collect_files(repo_root)
    modules = [parse_cap_file(repo_root / rel) for rel in files]
    index_html = render_index(modules)
    (out_root / "index.html").write_text(index_html, encoding="utf-8")

    print(f"Wrote {out_root / 'index.html'}")


if __name__ == "__main__":
    main()
