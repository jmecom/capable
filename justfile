set shell := ["bash", "-uc"]

default:
  @just --list

fmt:
  cargo fmt

check:
  cargo check

test:
  cargo test

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
