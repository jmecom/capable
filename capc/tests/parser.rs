use capc::parse_module;

#[test]
fn snapshot_basic_module() {
    let source = r#"
module app

pub fn add(a: i32, b: i32) -> i32 {
  let c = a + b
  return c
}
"#;
    let module = parse_module(source).expect("parse module");
    insta::assert_debug_snapshot!(module);
}

#[test]
fn snapshot_struct_and_match() {
    let source = r#"
module demo

struct Pair { left: i32, right: i32 }

fn test(flag: bool) -> unit {
  match flag {
    _ => { return () }
  }
}
"#;
    let module = parse_module(source).expect("parse module");
    insta::assert_debug_snapshot!(module);
}
