use std::path::PathBuf;

use capc::parse_module;

fn sanitize_debug_ids(input: &str) -> String {
    let mut result = String::new();
    let mut skip_lines = 0usize;
    for line in input.lines() {
        if skip_lines > 0 {
            skip_lines -= 1;
            continue;
        }
        if line.trim() == "id: ExprId(" {
            skip_lines = 2;
            continue;
        }
        result.push_str(line);
        result.push('\n');
    }
    result
}

fn assert_module_snapshot(name: &str, module: &capc::ast::Module) {
    let debug = format!("{module:#?}");
    insta::assert_snapshot!(name, sanitize_debug_ids(&debug));
}

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
    assert_module_snapshot("snapshot_basic_module", &module);
}

#[test]
fn snapshot_struct_and_match() {
    let source = load_program("fs_read.cap");
    let module = parse_module(&source).expect("parse module");
    assert_module_snapshot("snapshot_struct_and_match", &module);
}

#[test]
fn snapshot_struct_literal() {
    let source = load_program("struct_literal.cap");
    let module = parse_module(&source).expect("parse module");
    assert_module_snapshot("snapshot_struct_literal", &module);
}

#[test]
fn snapshot_doc_comments() {
    let source = load_program("doc_comments.cap");
    let module = parse_module(&source).expect("parse module");
    assert_module_snapshot("snapshot_doc_comments", &module);
}

#[test]
fn snapshot_generics_basic() {
    let source = load_program("generics_basic.cap");
    let module = parse_module(&source).expect("parse module");
    assert_module_snapshot("snapshot_generics_basic", &module);
}
