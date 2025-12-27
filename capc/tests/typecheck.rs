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
fn typecheck_console_as_alloc_fails() {
    let source = load_program("should_fail_console_as_alloc.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("unknown method `sys.console.Console__buffer_new`"));
}

#[test]
fn typecheck_alloc_as_console_fails() {
    let source = load_program("should_fail_alloc_as_console.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("unknown method `sys.buffer.Alloc__println`"));
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
    assert!(text.contains("method receiver must be a struct value"));
}

#[test]
fn typecheck_mint_without_system() {
    let source = load_program("should_fail_mint_without_system.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    let text = err.to_string();
    assert!(text.contains("method receiver must be a struct value"));
}

#[test]
fn typecheck_reserved_type_name_fails() {
    let source = load_program("should_fail_reserved_type_name.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("type name `string` is reserved"));
}

#[test]
fn typecheck_move_opaque_fails() {
    let source = load_program("should_fail_move_opaque.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `t`"));
}

#[test]
fn typecheck_dup_affine_field_fails() {
    let source = load_program("should_fail_dup_affine_field.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `h`"));
}

#[test]
fn typecheck_affine_nested_field_fails() {
    let source = load_program("should_fail_affine_nested_field.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `o`"));
}

#[test]
fn typecheck_affine_branch_merge_fails() {
    let source = load_program("should_fail_affine_branch_merge.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `c`"));
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
fn typecheck_extern_call_ok() {
    let source = load_program("extern_call.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_pointer_requires_unsafe_package() {
    let source = load_program("pointer_safe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("raw pointer types require `package unsafe`"));
}

#[test]
fn typecheck_pointer_unsafe_ok() {
    let source = load_program("pointer_unsafe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_slice_safe_ok() {
    let source = load_program("slice_safe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_slice_unsafe_ok() {
    let source = load_program("slice_unsafe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_buffer_safe_ok() {
    let source = load_program("buffer_safe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_slice_safe_read_ok() {
    let source = load_program("slice_safe_read.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_buffer_push_safe_ok() {
    let source = load_program("buffer_push_safe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_args_safe_ok() {
    let source = load_program("args_safe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_stdin_safe_ok() {
    let source = load_program("stdin_safe.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_string_helpers_ok() {
    let source = load_program("string_helpers.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_wc_stdin_ok() {
    let source = load_program("wc_stdin.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_wc_file_ok() {
    let source = load_program("wc_file.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_bytes_helpers_ok() {
    let source = load_program("bytes_helpers.cap");
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
    let msg = err.to_string();
    assert!(
        msg.contains("expected return") || msg.contains("missing return"),
        "expected error about missing/expected return, got: {msg}"
    );
}

#[test]
fn typecheck_impl_requires_self_param() {
    let source = r#"
module app

struct Pair { left: i32, right: i32 }

impl Pair {
  fn sum(x: Pair) -> i32 {
    return x.left + x.right
  }
}
"#;
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    let msg = err.to_string();
    assert!(
        msg.contains("first parameter must be self: Pair"),
        "expected error about missing self, got: {msg}"
    );
}

#[test]
fn typecheck_impl_self_type_mismatch() {
    let source = r#"
module app

struct Pair { left: i32, right: i32 }

impl Pair {
  fn sum(self: i32) -> i32 {
    return self
  }
}
"#;
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    let msg = err.to_string();
    assert!(
        msg.contains("first parameter must be self: Pair"),
        "expected error about self type, got: {msg}"
    );
}

#[test]
fn typecheck_private_method_across_modules() {
    let source = load_program("method_private_use.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../tests/programs/method_private_use.cap");
    let user_modules = load_user_modules_transitive(&path, &module).expect("load user modules");
    let err = type_check_program(&module, &stdlib, &user_modules).expect_err("expected type error");
    let msg = err.to_string();
    assert!(
        msg.contains("private"),
        "expected error about private method, got: {msg}"
    );
}

#[test]
fn typecheck_impl_method_name_should_be_unqualified() {
    let source = r#"
module app

struct Pair { left: i32, right: i32 }

impl Pair {
  fn Pair__sum(self: Pair) -> i32 {
    return self.left + self.right
  }
}
"#;
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    let msg = err.to_string();
    assert!(
        msg.contains("method name in impl should be unqualified"),
        "expected error about impl method name, got: {msg}"
    );
}

#[test]
fn typecheck_impl_wrong_module() {
    let source = load_program("impl_wrong_module.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../tests/programs/impl_wrong_module.cap");
    let user_modules = load_user_modules_transitive(&path, &module).expect("load user modules");
    let err = type_check_program(&module, &stdlib, &user_modules).expect_err("expected type error");
    let msg = err.to_string();
    assert!(
        msg.contains("impl blocks must be declared in the defining module"),
        "expected error about impl module, got: {msg}"
    );
}
