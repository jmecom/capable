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

#[test]
fn run_buffer_safe() {
    let out_dir = make_out_dir("buffer_safe");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/buffer_safe.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("buffer ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_buffer_push_safe() {
    let out_dir = make_out_dir("buffer_push_safe");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/buffer_push_safe.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("push ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_slice_safe_read() {
    let out_dir = make_out_dir("slice_safe_read");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/slice_safe_read.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("slice read ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_args_safe() {
    let out_dir = make_out_dir("args_safe");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/args_safe.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("args ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_stdin_safe() {
    let out_dir = make_out_dir("stdin_safe");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/stdin_safe.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("stdin ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_string_helpers() {
    let out_dir = make_out_dir("string_helpers");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/string_helpers.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("string ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_wc_stdin() {
    let out_dir = make_out_dir("wc_stdin");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/wc_stdin.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("0 0 0"), "stdout was: {stdout:?}");
}

#[test]
fn run_wc_file() {
    let out_dir = make_out_dir("wc_file");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/wc_file.cap",
        "--",
        "tests/programs/hello.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("9 22 152"), "stdout was: {stdout:?}");
}

#[test]
fn run_bytes_helpers() {
    let out_dir = make_out_dir("bytes_helpers");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/bytes_helpers.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("bytes ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_vec_helpers() {
    let out_dir = make_out_dir("vec_helpers");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/vec_helpers.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("vec ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_string_split() {
    let out_dir = make_out_dir("string_split");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/string_split.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("split ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_unit_return() {
    let out_dir = make_out_dir("unit_return");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/unit_return.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("unit return ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_unit_match_arm() {
    let out_dir = make_out_dir("unit_match_arm");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/unit_match_arm.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("unit match arm ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_unit_match_multi() {
    let out_dir = make_out_dir("unit_match_multi");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/unit_match_multi.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("unit match multi ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_result_unit_ok() {
    let out_dir = make_out_dir("result_unit_ok");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/result_unit_ok.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("result unit ok - got Ok(unit)"), "stdout was: {stdout:?}");
}

#[test]
fn run_unit_match_bind() {
    let out_dir = make_out_dir("unit_match_bind");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/unit_match_bind.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("unit match bind ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_early_return_block() {
    let out_dir = make_out_dir("early_return_block");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/early_return_block.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("early return test ok"), "stdout was: {stdout:?}");
    assert!(!stdout.contains("SHOULD NOT PRINT"), "stdout was: {stdout:?}");
}

#[test]
fn run_early_return_while() {
    let out_dir = make_out_dir("early_return_while");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/early_return_while.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("early return while test ok"), "stdout was: {stdout:?}");
    assert!(!stdout.contains("SHOULD NOT PRINT"), "stdout was: {stdout:?}");
}

#[test]
fn run_scoping_let_block() {
    let out_dir = make_out_dir("scoping_let_block");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/scoping_let_block.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("scoping let block test ok"), "stdout was: {stdout:?}");
}

#[test]
fn run_scoping_assign() {
    let out_dir = make_out_dir("scoping_assign");
    let out_dir = out_dir.to_str().expect("utf8 out dir");
    let (code, stdout, _stderr) = run_capc(&[
        "run",
        "--out-dir",
        out_dir,
        "tests/programs/scoping_assign.cap",
    ]);
    assert_eq!(code, 0);
    assert!(stdout.contains("scoping assign test ok"), "stdout was: {stdout:?}");
}

