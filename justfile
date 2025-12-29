set shell := ["bash", "-uc"]

default:
  @just --list

fmt:
  cargo fmt

check:
  cargo check

test:
  cargo test

typecheck:
  cargo test -p capc --test typecheck

run-tests:
  cargo test -p capc --test run

run-config:
  cargo test -p capc --test run run_config_loader -- --nocapture

tree-sitter:
  tree-sitter generate -C tree-sitter-capable

snap:
  cargo insta test

parse file:
  cargo run -p capc -- parse {{file}}

check-file file:
  cargo run -p capc -- check {{file}}

build file:
  cargo run -p capc -- build {{file}}

run file:
  cargo run -p capc -- run {{file}}

audit file:
  cargo run -p capc -- audit {{file}}

extern-demo-build:
  make -C examples/extern_demo

extern-demo-run:
  cargo run -p capc -- run --link-search examples/extern_demo --link-lib extern_demo examples/extern_demo/extern_demo.cap

http-server:
  cargo run -p capc -- run examples/http_server/http_server.cap

lsp:
  cargo run -p caplsp
