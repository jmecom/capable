use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn run_capc(args: &[&str]) -> (i32, String, String) {
    let exe = env!("CARGO_BIN_EXE_capc");
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let output = Command::new(exe)
        .current_dir(root)
        .args(args)
        .output()
        .expect("run capc");
    let code = output.status.code().unwrap_or(-1);
    (
        code,
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn make_temp_dir(test_name: &str) -> PathBuf {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time")
        .as_nanos();
    let pid = std::process::id();
    let dir = root
        .join("target")
        .join("capc-test-input")
        .join(format!("{test_name}-{pid}-{nanos}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

#[test]
fn safe_only_rejects_unsafe_package() {
    let (code, _stdout, stderr) = run_capc(&[
        "check",
        "--safe-only",
        "tests/programs/unsafe_pkg.cap",
    ]);
    assert_ne!(code, 0);
    assert!(stderr.contains("safe-only"), "stderr was: {stderr:?}");
}

#[test]
fn safe_only_allows_safe_package() {
    let (code, _stdout, stderr) = run_capc(&[
        "check",
        "--safe-only",
        "tests/programs/hello.cap",
    ]);
    assert_eq!(code, 0, "stderr was: {stderr:?}");
}

#[test]
fn audit_lists_unsafe_package() {
    let (code, stdout, stderr) = run_capc(&["audit", "tests/programs/unsafe_pkg.cap"]);
    assert_eq!(code, 0, "stderr was: {stderr:?}");
    assert!(stdout.contains("unsafe packages"));
    assert!(stdout.contains("unsafe_pkg"));
    assert!(stdout.contains("package unsafe"));
}

#[test]
fn audit_reports_extern_in_safe_package() {
    let (code, stdout, stderr) = run_capc(&["audit", "tests/programs/extern_safe.cap"]);
    assert_eq!(code, 0, "stderr was: {stderr:?}");
    assert!(stdout.contains("extern declarations"));
}

#[test]
fn extern_missing_symbol_reports_link_error() {
    let (code, _stdout, stderr) = run_capc(&["build", "tests/programs/extern_missing.cap"]);
    assert_ne!(code, 0);
    assert!(stderr.contains("link failed"));
    assert!(stderr.contains("missing_symbol"), "stderr was: {stderr:?}");
}

#[test]
fn imported_parse_error_reports_imported_file() {
    let dir = make_temp_dir("imported-parse-error");
    let entry = dir.join("main.cap");
    let helper = dir.join("helper.cap");
    fs::write(
        &entry,
        "package safe\nmodule main\nuse helper\n\npub fn main() -> i32 {\n  return helper::value()\n}\n",
    )
    .expect("write entry");
    fs::write(
        &helper,
        "package safe\nmodule helper\n\npub fn value() -> i32 {\n  return (\n}\n",
    )
    .expect("write helper");

    let entry = entry.to_string_lossy().to_string();
    let (code, _stdout, stderr) = run_capc(&["check", &entry]);
    assert_ne!(code, 0);
    assert!(stderr.contains("helper.cap"), "stderr was: {stderr:?}");
}

#[test]
fn imported_type_error_reports_imported_file() {
    let dir = make_temp_dir("imported-type-error");
    let entry = dir.join("main.cap");
    let helper = dir.join("helper.cap");
    fs::write(
        &entry,
        "package safe\nmodule main\nuse helper\n\npub fn main() -> i32 {\n  return helper::value()\n}\n",
    )
    .expect("write entry");
    fs::write(
        &helper,
        "package safe\nmodule helper\n\npub fn value() -> i32 {\n  return true\n}\n",
    )
    .expect("write helper");

    let entry = entry.to_string_lossy().to_string();
    let (code, _stdout, stderr) = run_capc(&["check", &entry]);
    assert_ne!(code, 0);
    assert!(stderr.contains("helper.cap"), "stderr was: {stderr:?}");
}
