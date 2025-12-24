use std::path::PathBuf;

use capc::parse_module;

fn load_program(name: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../tests/programs")
        .join(name);
    std::fs::read_to_string(path).expect("read program file")
}

#[test]
fn snapshot_basic_module() {
    let source = load_program("hello.cap");
    let module = parse_module(&source).expect("parse module");
    insta::assert_debug_snapshot!(module);
}

#[test]
fn snapshot_struct_and_match() {
    let source = load_program("fs_read.cap");
    let module = parse_module(&source).expect("parse module");
    insta::assert_debug_snapshot!(module);
}

#[test]
fn snapshot_struct_literal() {
    let source = load_program("struct_literal.cap");
    let module = parse_module(&source).expect("parse module");
    insta::assert_debug_snapshot!(module);
}

#[test]
fn snapshot_doc_comments() {
    let source = load_program("doc_comments.cap");
    let module = parse_module(&source).expect("parse module");
    insta::assert_debug_snapshot!(module);
}

