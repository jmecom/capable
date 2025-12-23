use capc::{parse_module, type_check};

#[test]
fn typecheck_ok() {
    let source = r#"
module app

fn add(a: i32, b: i32) -> i32 {
  let c = a + b
  return c
}
"#;
    let module = parse_module(source).expect("parse module");
    type_check(&module).expect("typecheck module");
}

#[test]
fn typecheck_error_on_missing_return() {
    let source = r#"
module app

fn add(a: i32, b: i32) -> i32 {
  let c = a + b
}
"#;
    let module = parse_module(source).expect("parse module");
    let err = type_check(&module).expect_err("expected type error");
    assert!(err.to_string().contains("missing return"));
}
