use std::path::PathBuf;

use capc::{load_stdlib, load_user_modules_transitive, parse_module, type_check_program};

fn load_program(name: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../tests/programs")
        .join(name);
    std::fs::read_to_string(path).expect("read program file")
}

#[test]
fn typecheck_ok() {
    let source = load_program("hello.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_fs_read_ok() {
    let source = load_program("fs_read.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_struct_literal_ok() {
    let source = load_program("struct_literal.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_if_while_ok() {
    let source = load_program("if_while.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_match_bool_ok() {
    let source = load_program("match_bool.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_match_expr_ok() {
    let source = load_program("match_expr.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_fs_traversal_kind_ok() {
    let source = load_program("fs_traversal_kind.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_with_helper_module() {
    let source = load_program("with_helper.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../tests/programs/with_helper.cap");
    let user_modules = load_user_modules_transitive(&path, &module).expect("load user modules");
    type_check_program(&module, &stdlib, &user_modules).expect("typecheck module");
}

#[test]
fn typecheck_missing_console_cap() {
    let source = load_program("should_fail_no_console.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("unknown value `c`"));
}

#[test]
fn typecheck_opaque_console_constructor_fails() {
    let source = load_program("should_fail_forge_console.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(
        err.to_string()
            .contains("cannot construct opaque type `sys.console.Console` outside module `sys.console`")
    );
}

#[test]
fn typecheck_console_wrong_type() {
    let source = load_program("should_fail_console_wrong_type.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    let text = err.to_string();
    assert!(text.contains("argument type mismatch"));
    assert!(text.contains("Path(\"sys.console.Console\""));
    assert!(text.contains("Builtin(I32)"));
}

#[test]
fn typecheck_mint_without_system() {
    let source = load_program("should_fail_mint_without_system.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    let text = err.to_string();
    assert!(text.contains("argument type mismatch"));
    assert!(text.contains("Path(\"sys.system.System\""));
    assert!(text.contains("Builtin(I32)"));
}

#[test]
fn typecheck_extern_requires_unsafe_package() {
    let source = load_program("extern_safe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("extern declarations require `package unsafe`"));
}

#[test]
fn typecheck_extern_unsafe_ok() {
    let source = load_program("extern_unsafe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_error_on_missing_return() {
    let source = r#"
module app

fn add(a: i32, b: i32) -> i32 {
  let c = a + b
}
"#;
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("missing return"));
}
