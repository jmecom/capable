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
fn typecheck_shadowing_fails() {
    let source = load_program("should_fail_shadowing.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("variable shadowing is not allowed"));
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
fn typecheck_match_bool_non_exhaustive_fails() {
    let source = load_program("should_fail_match_bool_non_exhaustive.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("non-exhaustive match on bool"));
}

#[test]
fn typecheck_match_enum_non_exhaustive_fails() {
    let source = load_program("should_fail_match_enum_non_exhaustive.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("non-exhaustive match, missing variants"));
}

#[test]
fn typecheck_match_enum_exhaustive_ok() {
    let source = load_program("should_pass_match_enum_exhaustive.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_match_result_non_exhaustive_fails() {
    let source = load_program("should_fail_match_result_non_exhaustive.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("non-exhaustive match on Result"));
}

#[test]
fn typecheck_result_unwrap_or_ok() {
    let source = load_program("should_pass_result_unwrap_or.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_result_unwrap_err_or_ok() {
    let source = load_program("should_pass_result_unwrap_err_or.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_result_unwrap_or_mismatch_fails() {
    let source = load_program("should_fail_result_unwrap_or_mismatch.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("unwrap_or type mismatch"));
}

#[test]
fn typecheck_match_expr_ok() {
    let source = load_program("match_expr.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_try_question_ok() {
    let source = load_program("should_pass_try_question.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_unit_return_infer_ok() {
    let source = load_program("should_pass_unit_return_infer.cap");
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
            .contains("cannot construct opaque/capability type `sys.console.Console` outside module `sys.console`")
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
fn typecheck_try_question_non_result_fails() {
    let source = load_program("should_fail_try_non_result.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("`?` operator can only be used in functions returning Result"));
}

#[test]
fn typecheck_try_question_err_mismatch_fails() {
    let source = load_program("should_fail_try_err_mismatch.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("mismatched error type for `?`"));
}

#[test]
fn typecheck_numeric_add_mismatch_fails() {
    let source = load_program("should_fail_numeric_add_mismatch.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("implicit numeric conversions are not allowed"));
}

#[test]
fn typecheck_numeric_cmp_mismatch_fails() {
    let source = load_program("should_fail_numeric_cmp_mismatch.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("implicit numeric conversions are not allowed"));
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
fn typecheck_affine_use_after_move_fails() {
    let source = load_program("should_fail_affine_use_after_move.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `c`"));
}

#[test]
fn typecheck_affine_call_moves_fails() {
    let source = load_program("should_fail_affine_call_moves.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `c`"));
}

#[test]
fn typecheck_affine_fs_caps_fail() {
    let source = load_program("should_fail_affine_fs_caps.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `fs`"));
}

#[test]
fn typecheck_linear_drop_ok() {
    let source = load_program("should_pass_linear_drop.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_linear_not_consumed_fails() {
    let source = load_program("should_fail_linear_not_consumed.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("linear value `t` not consumed"));
}

#[test]
fn typecheck_linear_drop_twice_fails() {
    let source = load_program("should_fail_linear_drop_twice.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `t`"));
}

#[test]
fn typecheck_linear_branch_merge_fails() {
    let source = load_program("should_fail_linear_branch_merge.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("linear value `t` must be consumed on all paths"));
}

#[test]
fn typecheck_linear_match_merge_fails() {
    let source = load_program("should_fail_linear_match_merge.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("linear value `t` must be consumed on all paths"));
}

#[test]
fn typecheck_linear_loop_move_fails() {
    let source = load_program("should_fail_linear_loop_move.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("move-only value `t` moved inside loop"));
}

#[test]
fn typecheck_copy_struct_move_field_fails() {
    let source = load_program("should_fail_copy_struct_move_field.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("copy struct cannot contain move-only fields"));
}

#[test]
fn typecheck_attenuation_keep_fs_fails() {
    let source = load_program("should_fail_attenuation_keep_fs.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `fs`"));
}

#[test]
fn typecheck_attenuation_keep_dir_fails() {
    let source = load_program("should_fail_attenuation_keep_dir.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `d`"));
}

#[test]
fn typecheck_attenuation_reuse_fileread_fails() {
    let source = load_program("should_fail_attenuation_reuse_fileread.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `f`"));
}

#[test]
fn typecheck_attenuation_untrusted_pass() {
    let source = load_program("attenuation_untrusted_pass.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../tests/programs/attenuation_untrusted_pass.cap");
    let user_modules = load_user_modules_transitive(&path, &module).expect("load user modules");
    type_check_program(&module, &stdlib, &user_modules).expect("typecheck module");
}

#[test]
fn typecheck_attenuation_untrusted_fail() {
    let source = load_program("attenuation_untrusted_fail.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../tests/programs/attenuation_untrusted_fail.cap");
    let user_modules = load_user_modules_transitive(&path, &module).expect("load user modules");
    let err = type_check_program(&module, &stdlib, &user_modules).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `d`"));
}

#[test]
fn typecheck_borrow_self_ok() {
    let source = load_program("should_pass_borrow_self.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_borrow_local_ok() {
    let source = load_program("should_pass_borrow_local.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_capability_return_ok() {
    let source = load_program("should_pass_capability_return.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_borrow_return_fails() {
    let source = load_program("should_fail_borrow_return.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("reference types cannot be returned"));
}

#[test]
fn typecheck_capability_borrow_return_fails() {
    let source = load_program("should_fail_capability_borrow_return.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("methods returning capabilities must take `self` by value"));
}

#[test]
fn typecheck_capability_borrow_return_result_fails() {
    let source = load_program("should_fail_capability_borrow_return_result.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("methods returning capabilities must take `self` by value"));
}

#[test]
fn typecheck_capability_borrow_return_helper_fails() {
    let source = load_program("should_fail_capability_borrow_return_helper.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("methods returning capabilities must take `self` by value"));
}

#[test]
fn typecheck_borrow_local_temp_fails() {
    let source = load_program("should_fail_borrow_local_temp.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("reference locals must be initialized from a local value"));
}

#[test]
fn typecheck_borrow_local_move_fails() {
    let source = load_program("should_fail_borrow_local_move.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("cannot pass a reference to a value parameter"));
}

#[test]
fn typecheck_borrow_local_assign_fails() {
    let source = load_program("should_fail_borrow_local_assign.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("cannot assign to a reference local"));
}

#[test]
fn typecheck_linear_return_ok() {
    let source = load_program("should_pass_linear_return.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
}

#[test]
fn typecheck_affine_match_merge_fails() {
    let source = load_program("should_fail_affine_match_merge.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("use of moved value `c`"));
}

#[test]
fn typecheck_affine_loop_move_fails() {
    let source = load_program("should_fail_affine_loop_move.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err.to_string().contains("move-only value `c` moved inside loop"));
}

#[test]
fn typecheck_affine_temp_field_fails() {
    let source = load_program("should_fail_affine_temp_field.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    let err = type_check_program(&module, &stdlib, &[]).expect_err("expected type error");
    assert!(err
        .to_string()
        .contains("cannot move affine field from non-local expression"));
}

#[test]
fn typecheck_affine_non_affine_field_reads_ok() {
    let source = load_program("should_pass_affine_non_affine_field_reads.cap");
    let module = parse_module(&source).expect("parse module");
    let stdlib = load_stdlib().expect("load stdlib");
    type_check_program(&module, &stdlib, &[]).expect("typecheck module");
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
