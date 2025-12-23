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

fn make_out_dir(test_name: &str) -> PathBuf {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time")
        .as_nanos();
    let pid = std::process::id();
    let dir = root
        .join("target")
        .join("capc-test-out")
        .join(format!("{test_name}-{pid}-{nanos}"));
    std::fs::create_dir_all(&dir).expect("create out dir");
    dir
}

#[test]
fn run_fs_traversal_denied() {
    let out_dir = make_out_dir("fs_traversal_denied");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/fs_traversal_denied.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("denied"), "stdout was: {stdout:?}");
}

#[test]
fn run_fs_traversal_kind() {
    let out_dir = make_out_dir("fs_traversal_kind");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/fs_traversal_kind.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("invalid"), "stdout was: {stdout:?}");
}

#[test]
fn run_match_expr() {
    let out_dir = make_out_dir("match_expr");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/match_expr.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("yes"), "stdout was: {stdout:?}");
}

#[test]
fn run_malloc_demo() {
    let out_dir = make_out_dir("malloc_demo");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "examples/malloc_demo/malloc_demo.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("malloc ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_slice_unsafe() {
    let out_dir = make_out_dir("slice_unsafe");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/slice_unsafe.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("slice ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_buffer_unsafe() {
    let out_dir = make_out_dir("buffer_unsafe");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/buffer_unsafe.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("buffer ok"), "stdout was: {stdout:?}");
    assert!(stdout.contains("slice ok"), "stdout was: {stdout:?}");
}
