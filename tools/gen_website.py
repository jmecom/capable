#!/usr/bin/env python3
"""
Generate the Capable programming language website.

Usage:
  python3 tools/gen_website.py
  python3 tools/gen_website.py --out website
"""
from __future__ import annotations

import argparse
import html
import json
import os
import re
from pathlib import Path


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

STDLIB_ROOT = Path("stdlib/sys")
TUTORIAL_PATH = Path("docs/TUTORIAL.md")

CAP_KEYWORDS = {
    "fn", "let", "if", "else", "while", "for", "return", "match", "struct",
    "enum", "impl", "pub", "use", "module", "package", "defer", "capability",
    "linear", "copy", "opaque", "extern", "true", "false", "in", "break",
    "continue", "drop", "unsafe", "safe",
}

CAP_TYPES = {
    "i32", "u8", "u32", "i64", "u64", "bool", "unit", "string", "Result",
    "Ok", "Err", "RootCap", "Console", "Args", "Stdin", "Alloc", "Buffer",
    "Slice", "MutSlice", "VecU8", "VecI32", "VecString", "ReadFS", "Filesystem",
    "Dir", "FileRead", "Net", "TcpListener", "TcpConn",
}


# ---------------------------------------------------------------------------
# Syntax highlighting for .cap code
# ---------------------------------------------------------------------------

def highlight_cap(code: str) -> str:
    """Syntax highlight Capable code."""

    def escape(s: str) -> str:
        return html.escape(s)

    # Tokenize with regex
    token_pattern = re.compile(
        r'(///.*?$|//.*?$)'           # comments
        r'|("(?:[^"\\\n]|\\.)*")'     # strings (no newlines)
        r'|(\b\d+\b)'                  # numbers
        r'|(\b[a-zA-Z_][a-zA-Z0-9_]*\b)'  # identifiers
        r'|([^\s\w]+)'                 # operators/punctuation
        r'|(\s+)',                     # whitespace
        re.MULTILINE
    )

    result = []
    for match in token_pattern.finditer(code):
        comment, string, number, ident, op, ws = match.groups()
        if comment:
            result.append(f'<span class="hl-comment">{escape(comment)}</span>')
        elif string:
            result.append(f'<span class="hl-string">{escape(string)}</span>')
        elif number:
            result.append(f'<span class="hl-number">{escape(number)}</span>')
        elif ident:
            if ident in CAP_KEYWORDS:
                result.append(f'<span class="hl-keyword">{escape(ident)}</span>')
            elif ident in CAP_TYPES:
                result.append(f'<span class="hl-type">{escape(ident)}</span>')
            else:
                result.append(escape(ident))
        elif op:
            result.append(escape(op))
        elif ws:
            result.append(ws)

    return ''.join(result)


# ---------------------------------------------------------------------------
# Markdown to HTML conversion
# ---------------------------------------------------------------------------

def markdown_to_html(md: str) -> str:
    """Convert markdown to HTML with syntax highlighting for cap code blocks."""
    lines = md.split('\n')
    html_parts = []
    in_code_block = False
    code_lang = ""
    code_lines = []
    in_list = False

    def close_list():
        nonlocal in_list
        if in_list:
            html_parts.append('</ul>')
            in_list = False

    def process_inline(text: str) -> str:
        # Bold
        text = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', text)
        # Italic
        text = re.sub(r'\*(.+?)\*', r'<em>\1</em>', text)
        # Inline code
        text = re.sub(r'`([^`]+)`', r'<code>\1</code>', text)
        # Links
        text = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'<a href="\2">\1</a>', text)
        return text

    i = 0
    while i < len(lines):
        line = lines[i]

        # Code block start/end
        if line.startswith('```'):
            if not in_code_block:
                close_list()
                in_code_block = True
                code_lang = line[3:].strip()
                code_lines = []
            else:
                code_content = '\n'.join(code_lines)
                if code_lang == 'cap':
                    highlighted = highlight_cap(code_content)
                    html_parts.append(f'<pre class="code-block"><code>{highlighted}</code></pre>')
                else:
                    html_parts.append(f'<pre class="code-block"><code>{html.escape(code_content)}</code></pre>')
                in_code_block = False
                code_lang = ""
            i += 1
            continue

        if in_code_block:
            code_lines.append(line)
            i += 1
            continue

        # Headings
        if line.startswith('# '):
            close_list()
            html_parts.append(f'<h1>{process_inline(line[2:])}</h1>')
        elif line.startswith('## '):
            close_list()
            html_parts.append(f'<h2>{process_inline(line[3:])}</h2>')
        elif line.startswith('### '):
            close_list()
            html_parts.append(f'<h3>{process_inline(line[4:])}</h3>')
        # Horizontal rule
        elif line.strip() == '---':
            close_list()
            html_parts.append('<hr>')
        # List items
        elif line.startswith('- '):
            if not in_list:
                html_parts.append('<ul>')
                in_list = True
            html_parts.append(f'<li>{process_inline(line[2:])}</li>')
        # Empty line
        elif line.strip() == '':
            close_list()
        # Paragraph
        else:
            close_list()
            if line.strip():
                html_parts.append(f'<p>{process_inline(line)}</p>')

        i += 1

    close_list()
    return '\n'.join(html_parts)


# ---------------------------------------------------------------------------
# Stdlib parsing (adapted from gen_code_browser.py)
# ---------------------------------------------------------------------------

def parse_cap_file(path: Path) -> dict:
    """Parse a .cap file and extract documentation."""
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
            items.append({
                "kind": "method" if current_impl else "fn",
                "impl": current_impl,
                "sig": line.rstrip("{").strip(),
                "docs": docs,
            })
        elif "struct " in line and line.startswith((
            "pub struct", "struct",
            "pub copy struct", "pub copy opaque struct",
            "pub copy capability struct", "pub linear capability struct",
            "pub capability struct",
        )):
            docs = flush_docs()
            items.append({
                "kind": "struct",
                "sig": line.rstrip("{").strip(),
                "docs": docs,
            })
        elif "enum " in line and line.startswith(("pub enum", "enum")):
            docs = flush_docs()
            items.append({
                "kind": "enum",
                "sig": line.rstrip("{").strip(),
                "docs": docs,
            })

        opens = line.count("{")
        closes = line.count("}")
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


def collect_stdlib_modules(repo_root: Path) -> list[dict]:
    """Collect all stdlib modules."""
    stdlib_path = repo_root / STDLIB_ROOT
    if not stdlib_path.exists():
        return []

    files = sorted(stdlib_path.glob("*.cap"))
    return [parse_cap_file(f) for f in files]


# ---------------------------------------------------------------------------
# HTML generation
# ---------------------------------------------------------------------------

HERO_EXAMPLE = '''module config_reader
use sys::system
use sys::fs

pub fn main(rc: RootCap) -> i32 {
  // Mint a filesystem capability scoped to ./config
  let fs = rc.mint_filesystem("./config")
  let dir = fs.root_dir()
  let file = dir.open_read("app.txt")

  match file.read_to_string() {
    Ok(contents) => {
      rc.mint_console().println(contents)
      return 0
    }
    Err(e) => { return 1 }
  }
}'''


def generate_html(tutorial_html: str, modules: list[dict]) -> str:
    """Generate the complete website HTML."""

    modules_json = json.dumps(modules)
    hero_highlighted = highlight_cap(HERO_EXAMPLE)

    return f'''<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Capable</title>
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  <link href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600;700&display=swap" rel="stylesheet">
  <style>
    :root {{
      --ink: #baccd4;
      --muted: #77758a;
      --surface: #10121c;
      --panel: #1a1c28;
      --edge: #252834;
      --accent: #f57c6c;
      --accent-hover: #ff9a8f;
      --mono: "IBM Plex Mono", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    }}

    * {{ box-sizing: border-box; margin: 0; padding: 0; }}
    html, body {{ height: 100%; }}

    body {{
      font-family: var(--mono);
      color: var(--ink);
      background: var(--surface);
      line-height: 1.6;
      font-size: 15px;
    }}

    #dot-grid {{
      position: fixed;
      top: 0; left: 0;
      width: 100%; height: 100%;
      pointer-events: none;
      z-index: 0;
    }}

    header {{
      position: sticky;
      top: 0;
      z-index: 100;
      display: flex;
      align-items: center;
      gap: 2rem;
      padding: 0 1.5rem;
      height: 52px;
      border-bottom: 1px solid rgba(186, 204, 212, 0.1);
      background: rgba(16, 18, 28, 0.95);
      backdrop-filter: blur(10px);
    }}

    .logo {{
      font-weight: 700;
      font-size: 1rem;
      letter-spacing: 0.05em;
      color: var(--ink);
    }}

    nav {{
      display: flex;
      gap: 0.25rem;
    }}

    .tab {{
      padding: 0.5rem 1rem;
      border: none;
      background: transparent;
      color: var(--muted);
      font-family: var(--mono);
      font-size: 0.85rem;
      cursor: pointer;
      border-radius: 4px;
      transition: all 0.15s;
    }}

    .tab:hover {{
      color: var(--ink);
      background: rgba(186, 204, 212, 0.05);
    }}

    .tab.active {{
      color: var(--accent);
      background: rgba(245, 124, 108, 0.1);
    }}

    .header-right {{
      margin-left: auto;
    }}

    .header-right a {{
      color: var(--muted);
      text-decoration: none;
      font-size: 0.85rem;
    }}

    .header-right a:hover {{
      color: var(--ink);
    }}

    main {{
      position: relative;
      z-index: 1;
      min-height: calc(100vh - 52px);
    }}

    .page {{
      display: none;
      padding: 2rem;
      max-width: 900px;
      margin: 0 auto;
    }}

    .page.active {{
      display: block;
    }}

    /* Home page */
    .hero {{
      margin-bottom: 3rem;
    }}

    .hero h1 {{
      font-size: 2.5rem;
      font-weight: 700;
      margin-bottom: 0.5rem;
    }}

    .hero .tagline {{
      color: var(--muted);
      font-size: 1.1rem;
      margin-bottom: 1.5rem;
    }}

    .hero-code {{
      margin-bottom: 3rem;
    }}

    .security-section {{
      margin-bottom: 3rem;
    }}

    .security-section h2 {{
      font-size: 1.4rem;
      margin-bottom: 1rem;
      color: var(--accent);
    }}

    .security-section > p {{
      font-size: 1rem;
      margin-bottom: 1.5rem;
      line-height: 1.7;
    }}

    .security-comparison {{
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 1.5rem;
      margin-bottom: 2rem;
    }}

    @media (max-width: 700px) {{
      .security-comparison {{
        grid-template-columns: 1fr;
      }}
    }}

    .comparison-block {{
      padding: 1.25rem;
      border-radius: 6px;
      border: 1px solid var(--edge);
    }}

    .comparison-block.problem {{
      background: rgba(255, 100, 100, 0.05);
      border-color: rgba(255, 100, 100, 0.2);
    }}

    .comparison-block.solution {{
      background: rgba(100, 255, 150, 0.05);
      border-color: rgba(100, 255, 150, 0.2);
    }}

    .comparison-block h4 {{
      font-size: 0.9rem;
      margin-bottom: 0.75rem;
      text-transform: uppercase;
      letter-spacing: 0.05em;
    }}

    .comparison-block.problem h4 {{
      color: #ff8a80;
    }}

    .comparison-block.solution h4 {{
      color: #69f0ae;
    }}

    .comparison-block p {{
      font-size: 0.9rem;
      color: var(--ink);
      line-height: 1.6;
    }}

    .enforcement {{
      background: var(--panel);
      border: 1px solid var(--edge);
      border-radius: 6px;
      padding: 1.5rem;
    }}

    .enforcement h3 {{
      font-size: 1rem;
      margin-bottom: 1rem;
      color: var(--ink);
    }}

    .enforcement-grid {{
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 1.5rem;
    }}

    @media (max-width: 700px) {{
      .enforcement-grid {{
        grid-template-columns: 1fr;
      }}
    }}

    .enforcement-item strong {{
      display: block;
      color: var(--accent);
      margin-bottom: 0.5rem;
      font-size: 0.9rem;
    }}

    .enforcement-item p {{
      font-size: 0.85rem;
      color: var(--muted);
      line-height: 1.6;
    }}

    .features {{
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 1.5rem;
      margin-bottom: 3rem;
    }}

    @media (max-width: 700px) {{
      .features {{
        grid-template-columns: 1fr;
      }}
    }}

    .feature {{
      padding: 1.25rem;
      background: var(--panel);
      border: 1px solid var(--edge);
      border-radius: 6px;
    }}

    .feature h3 {{
      color: var(--accent);
      font-size: 0.95rem;
      margin-bottom: 0.5rem;
    }}

    .feature p {{
      color: var(--muted);
      font-size: 0.9rem;
    }}

    /* Code blocks */
    pre, .code-block {{
      background: #0e0f18;
      border: 1px solid var(--edge);
      border-radius: 6px;
      padding: 1rem;
      overflow-x: auto;
      font-size: 13px;
      line-height: 1.5;
    }}

    code {{
      font-family: var(--mono);
    }}

    /* Syntax highlighting */
    .hl-keyword {{ color: #c792ea; }}
    .hl-type {{ color: #82aaff; }}
    .hl-string {{ color: #c3e88d; }}
    .hl-number {{ color: #f78c6c; }}
    .hl-comment {{ color: #546e7a; font-style: italic; }}

    /* Tutorial page */
    #tutorial-content {{
      line-height: 1.7;
    }}

    #tutorial-content h1 {{
      font-size: 2rem;
      margin-bottom: 1rem;
      padding-bottom: 0.5rem;
      border-bottom: 1px solid var(--edge);
    }}

    #tutorial-content h2 {{
      font-size: 1.4rem;
      margin-top: 2.5rem;
      margin-bottom: 1rem;
      color: var(--accent);
    }}

    #tutorial-content h3 {{
      font-size: 1.1rem;
      margin-top: 2rem;
      margin-bottom: 0.75rem;
    }}

    #tutorial-content p {{
      margin-bottom: 1rem;
    }}

    #tutorial-content ul {{
      margin-bottom: 1rem;
      padding-left: 1.5rem;
    }}

    #tutorial-content li {{
      margin-bottom: 0.25rem;
    }}

    #tutorial-content code {{
      background: rgba(245, 124, 108, 0.1);
      padding: 0.15em 0.4em;
      border-radius: 3px;
      font-size: 0.9em;
    }}

    #tutorial-content pre code {{
      background: transparent;
      padding: 0;
    }}

    #tutorial-content .code-block {{
      margin-bottom: 1.5rem;
    }}

    #tutorial-content hr {{
      border: none;
      border-top: 1px solid var(--edge);
      margin: 2rem 0;
    }}

    #tutorial-content a {{
      color: var(--accent);
    }}

    /* Stdlib page */
    .stdlib-layout {{
      display: grid;
      grid-template-columns: 250px 1fr;
      gap: 2rem;
      max-width: 1200px;
    }}

    .stdlib-nav {{
      position: sticky;
      top: 70px;
      max-height: calc(100vh - 90px);
      overflow-y: auto;
      padding: 1rem;
      background: var(--panel);
      border: 1px solid var(--edge);
      border-radius: 6px;
    }}

    .stdlib-nav h3 {{
      font-size: 0.8rem;
      text-transform: uppercase;
      letter-spacing: 0.1em;
      color: var(--muted);
      margin-bottom: 0.75rem;
    }}

    .module-link {{
      display: block;
      padding: 0.4rem 0.6rem;
      color: var(--ink);
      text-decoration: none;
      border-radius: 4px;
      font-size: 0.9rem;
      cursor: pointer;
    }}

    .module-link:hover {{
      background: rgba(186, 204, 212, 0.05);
    }}

    .module-link.active {{
      background: rgba(245, 124, 108, 0.1);
      color: var(--accent);
    }}

    .stdlib-content {{
      min-width: 0;
    }}

    .module-header {{
      margin-bottom: 1.5rem;
    }}

    .module-header h2 {{
      font-size: 1.5rem;
      margin-bottom: 0.5rem;
    }}

    .module-path {{
      color: var(--muted);
      font-size: 0.85rem;
    }}

    .module-docs {{
      margin-bottom: 1.5rem;
      padding: 1rem;
      background: var(--panel);
      border-radius: 6px;
    }}

    .item-section h3 {{
      font-size: 1rem;
      color: var(--muted);
      text-transform: uppercase;
      letter-spacing: 0.05em;
      margin: 2rem 0 1rem;
      padding-bottom: 0.5rem;
      border-bottom: 1px solid var(--edge);
    }}

    .item {{
      padding: 0.75rem 0;
      border-bottom: 1px solid rgba(186, 204, 212, 0.05);
    }}

    .item:last-child {{
      border-bottom: none;
    }}

    .item-sig {{
      color: var(--accent);
      font-weight: 500;
      font-size: 0.95rem;
    }}

    .item-impl {{
      color: var(--muted);
      font-size: 0.85rem;
      margin-top: 0.25rem;
    }}

    .item-docs {{
      margin-top: 0.5rem;
      font-size: 0.9rem;
    }}

    .source-toggle {{
      display: inline-block;
      margin-top: 1.5rem;
      padding: 0.5rem 1rem;
      background: var(--panel);
      border: 1px solid var(--edge);
      border-radius: 4px;
      color: var(--accent);
      font-family: var(--mono);
      font-size: 0.85rem;
      cursor: pointer;
    }}

    .source-toggle:hover {{
      background: rgba(245, 124, 108, 0.1);
    }}

    .source-code {{
      margin-top: 1rem;
    }}

    @media (max-width: 800px) {{
      .stdlib-layout {{
        grid-template-columns: 1fr;
      }}
      .stdlib-nav {{
        position: static;
        max-height: none;
      }}
    }}
  </style>
</head>
<body>
  <canvas id="dot-grid"></canvas>

  <header>
    <div class="logo">Capable</div>
    <nav>
      <button class="tab active" data-page="home">Home</button>
      <button class="tab" data-page="tutorial">Tutorial</button>
      <button class="tab" data-page="stdlib">Stdlib</button>
    </nav>
    <div class="header-right">
      <a href="https://github.com/jmecom/capable" target="_blank">GitHub</a>
    </div>
  </header>

  <main>
    <!-- Home Page -->
    <div id="home" class="page active">
      <div class="hero">
        <h1>Capable</h1>
        <p class="tagline">Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
      </div>

      <pre class="hero-code"><code>{hero_highlighted}</code></pre>

      <div class="security-section">
        <h2>Capability-Based Security</h2>
        <p>Capable is a systems language where <strong>authority is a value</strong>. Code cannot perform privileged operations&mdash;filesystem access, networking, console output&mdash;without explicitly receiving a capability token.</p>

        <div class="security-comparison">
          <div class="comparison-block problem">
            <h4>The Problem</h4>
            <p>In typical systems languages, the OS is ambiently available. Any code in your process can call <code>open()</code>, <code>connect()</code>, or <code>exec()</code>. When you pull in a dependency, you're trusting it&mdash;and all of its transitive dependencies&mdash;not to be malicious or compromised.</p>
          </div>
          <div class="comparison-block solution">
            <h4>The Solution</h4>
            <p>In Capable, safe code can only perform OS actions by calling <code>sys.*</code> functions, and those require capability arguments. A library cannot read your disk unless you pass it a <code>ReadFS</code>. It cannot phone home unless you pass it a <code>Net</code>. Authority flow is explicit and reviewable.</p>
          </div>
        </div>

        <div class="enforcement">
          <h3>Two-Layer Enforcement</h3>
          <div class="enforcement-grid">
            <div class="enforcement-item">
              <strong>Static (Compiler)</strong>
              <p>No capability value in scope? The code won't compile. Privileged functions only exist in <code>sys.*</code> and require capability arguments. Capability types are opaque&mdash;user code cannot forge them.</p>
            </div>
            <div class="enforcement-item">
              <strong>Dynamic (Runtime)</strong>
              <p>Capabilities encode attenuation. A <code>ReadFS("./config")</code> cannot escape its root directory. The runtime validates paths, normalizes traversals, and enforces scoped rights.</p>
            </div>
          </div>
        </div>
      </div>

      <div class="features">
        <div class="feature">
          <h3>Linear and Affine Types</h3>
          <p>Move-only types prevent use-after-move. Linear capabilities must be consumed on all code paths. Attenuation methods consume the original capability&mdash;you can't keep both the powerful and attenuated versions.</p>
        </div>
        <div class="feature">
          <h3>Systems Programming</h3>
          <p>No garbage collector. Explicit memory management. Compiles to native code via Cranelift. Safe code is memory-safe; unsafe code is clearly marked with <code>package unsafe</code>.</p>
        </div>
      </div>
    </div>

    <!-- Tutorial Page -->
    <div id="tutorial" class="page">
      <div id="tutorial-content">
        {tutorial_html}
      </div>
    </div>

    <!-- Stdlib Page -->
    <div id="stdlib" class="page">
      <div class="stdlib-layout">
        <div class="stdlib-nav">
          <h3>Modules</h3>
          <div id="module-list"></div>
        </div>
        <div class="stdlib-content" id="module-content">
          <p class="module-path">Select a module from the sidebar</p>
        </div>
      </div>
    </div>
  </main>

  <script>
    // Tab navigation
    const tabs = document.querySelectorAll('.tab');
    const pages = document.querySelectorAll('.page');

    tabs.forEach(tab => {{
      tab.addEventListener('click', () => {{
        const pageId = tab.dataset.page;

        tabs.forEach(t => t.classList.remove('active'));
        tab.classList.add('active');

        pages.forEach(p => {{
          p.classList.toggle('active', p.id === pageId);
        }});
      }});
    }});

    // Stdlib browser
    const modules = {modules_json};
    const moduleList = document.getElementById('module-list');
    const moduleContent = document.getElementById('module-content');

    function escapeHtml(text) {{
      const div = document.createElement('div');
      div.textContent = text;
      return div.innerHTML;
    }}

    function highlightCode(code) {{
      const keywords = new Set(['fn', 'let', 'if', 'else', 'while', 'for', 'return', 'match', 'struct', 'enum', 'impl', 'pub', 'use', 'module', 'package', 'defer', 'capability', 'linear', 'copy', 'opaque', 'extern', 'true', 'false', 'in', 'break', 'continue', 'drop', 'unsafe', 'safe']);
      const types = new Set(['i32', 'u8', 'u32', 'i64', 'u64', 'bool', 'unit', 'string', 'Result', 'Ok', 'Err', 'RootCap', 'Console', 'Args', 'Stdin', 'Alloc', 'Buffer', 'Slice', 'MutSlice', 'VecU8', 'VecI32', 'VecString', 'ReadFS', 'Filesystem', 'Dir', 'FileRead', 'Net', 'TcpListener', 'TcpConn']);

      return code.replace(/(\/\/\/.*?$|\/\/.*?$)|("(?:[^"\\\\]|\\\\.)*")|(\b\d+\b)|(\b[a-zA-Z_][a-zA-Z0-9_]*\b)/gm, (match, comment, str, num, ident) => {{
        if (comment) return `<span class="hl-comment">${{escapeHtml(comment)}}</span>`;
        if (str) return `<span class="hl-string">${{escapeHtml(str)}}</span>`;
        if (num) return `<span class="hl-number">${{escapeHtml(num)}}</span>`;
        if (ident) {{
          if (keywords.has(ident)) return `<span class="hl-keyword">${{ident}}</span>`;
          if (types.has(ident)) return `<span class="hl-type">${{ident}}</span>`;
          return ident;
        }}
        return match;
      }});
    }}

    function renderModule(mod) {{
      const kinds = {{ struct: 'Structs', enum: 'Enums', fn: 'Functions', method: 'Methods' }};
      let html = `
        <div class="module-header">
          <h2>${{escapeHtml(mod.module)}}</h2>
          <div class="module-path">${{escapeHtml(mod.path)}}</div>
        </div>
      `;

      if (mod.docs && mod.docs.length) {{
        html += `<div class="module-docs">${{mod.docs.map(d => `<div>${{escapeHtml(d)}}</div>`).join('')}}</div>`;
      }}

      for (const kind of ['struct', 'enum', 'fn', 'method']) {{
        const items = mod.items.filter(i => i.kind === kind);
        if (items.length === 0) continue;

        html += `<div class="item-section"><h3>${{kinds[kind]}}</h3>`;
        for (const item of items) {{
          html += `<div class="item">`;
          html += `<div class="item-sig">${{escapeHtml(item.sig)}}</div>`;
          if (item.impl) {{
            html += `<div class="item-impl">impl ${{escapeHtml(item.impl)}}</div>`;
          }}
          if (item.docs && item.docs.length) {{
            html += `<div class="item-docs">${{item.docs.map(d => escapeHtml(d)).join(' ')}}</div>`;
          }}
          html += `</div>`;
        }}
        html += `</div>`;
      }}

      html += `<button class="source-toggle" onclick="toggleSource(this)">Show Source</button>`;
      html += `<div class="source-code" style="display:none"><pre><code>${{highlightCode(mod.source)}}</code></pre></div>`;

      moduleContent.innerHTML = html;
    }}

    window.toggleSource = function(btn) {{
      const source = btn.nextElementSibling;
      if (source.style.display === 'none') {{
        source.style.display = 'block';
        btn.textContent = 'Hide Source';
      }} else {{
        source.style.display = 'none';
        btn.textContent = 'Show Source';
      }}
    }};

    // Build module list
    modules.forEach((mod, i) => {{
      const link = document.createElement('div');
      link.className = 'module-link' + (i === 0 ? ' active' : '');
      link.textContent = mod.module;
      link.onclick = () => {{
        document.querySelectorAll('.module-link').forEach(l => l.classList.remove('active'));
        link.classList.add('active');
        renderModule(mod);
      }};
      moduleList.appendChild(link);
    }});

    // Load first module
    if (modules.length > 0) {{
      renderModule(modules[0]);
    }}

    // Interactive dot grid animation
    (function() {{
      const canvas = document.getElementById('dot-grid');
      const ctx = canvas.getContext('2d');

      let width, height;
      let dots = [];
      let mouse = {{ x: -1000, y: -1000 }};
      const SPACING = 28;
      const DOT_RADIUS = 1;
      const INFLUENCE_RADIUS = 120;
      const BASE_COLOR = [186, 204, 212];
      const HOVER_COLOR = [245, 124, 108];

      function resize() {{
        width = canvas.width = window.innerWidth;
        height = canvas.height = window.innerHeight;
        initDots();
      }}

      function initDots() {{
        dots = [];
        const offsetX = (width % SPACING) / 2;
        const offsetY = (height % SPACING) / 2;
        for (let x = offsetX; x < width; x += SPACING) {{
          for (let y = offsetY; y < height; y += SPACING) {{
            dots.push({{ originX: x, originY: y, x: x, y: y }});
          }}
        }}
      }}

      function animate() {{
        ctx.clearRect(0, 0, width, height);

        for (const dot of dots) {{
          const dx = mouse.x - dot.originX;
          const dy = mouse.y - dot.originY;
          const dist = Math.sqrt(dx * dx + dy * dy);

          let color, alpha;

          if (dist < INFLUENCE_RADIUS) {{
            const t = 1 - dist / INFLUENCE_RADIUS;
            const force = t * 10;
            const angle = Math.atan2(dy, dx);
            dot.x = dot.originX - Math.cos(angle) * force;
            dot.y = dot.originY - Math.sin(angle) * force;
            color = HOVER_COLOR;
            alpha = 0.15 + t * 0.45;
          }} else {{
            dot.x += (dot.originX - dot.x) * 0.1;
            dot.y += (dot.originY - dot.y) * 0.1;
            color = BASE_COLOR;
            alpha = 0.12;
          }}

          ctx.beginPath();
          ctx.arc(dot.x, dot.y, DOT_RADIUS, 0, Math.PI * 2);
          ctx.fillStyle = `rgba(${{color[0]}}, ${{color[1]}}, ${{color[2]}}, ${{alpha}})`;
          ctx.fill();
        }}

        requestAnimationFrame(animate);
      }}

      canvas.style.pointerEvents = 'none';
      window.addEventListener('resize', resize);
      window.addEventListener('mousemove', (e) => {{
        mouse.x = e.clientX;
        mouse.y = e.clientY;
      }});
      window.addEventListener('mouseleave', () => {{
        mouse.x = -1000;
        mouse.y = -1000;
      }});

      resize();
      animate();
    }})();
  </script>
</body>
</html>
'''


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description="Generate Capable website")
    parser.add_argument("--out", default="website", help="Output directory")
    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parents[1]
    out_dir = (repo_root / args.out).resolve()
    out_dir.mkdir(parents=True, exist_ok=True)

    # Read and convert tutorial
    tutorial_path = repo_root / TUTORIAL_PATH
    if tutorial_path.exists():
        tutorial_md = tutorial_path.read_text(encoding="utf-8")
        tutorial_html = markdown_to_html(tutorial_md)
    else:
        tutorial_html = "<p>Tutorial not found.</p>"

    # Collect stdlib modules
    modules = collect_stdlib_modules(repo_root)

    # Generate HTML
    html = generate_html(tutorial_html, modules)

    out_file = out_dir / "index.html"
    out_file.write_text(html, encoding="utf-8")

    print(f"Generated: {out_file}")


if __name__ == "__main__":
    main()
