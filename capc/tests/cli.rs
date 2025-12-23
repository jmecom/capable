use std::path::PathBuf;
use std::process::Command;

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
