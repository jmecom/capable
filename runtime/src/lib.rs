use std::collections::HashMap;
use std::ffi::OsStr;
use std::io::{self, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Component, Path, PathBuf};
use std::sync::{LazyLock, Mutex};

use getrandom::getrandom;
use libc;

pub type Handle = u64;

static READ_FS: LazyLock<Mutex<HashMap<Handle, ReadFsState>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static FILESYSTEMS: LazyLock<Mutex<HashMap<Handle, FilesystemState>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static DIRS: LazyLock<Mutex<HashMap<Handle, DirState>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static FILE_READS: LazyLock<Mutex<HashMap<Handle, FileReadState>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static ROOT_CAPS: LazyLock<Mutex<HashMap<Handle, ()>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static CONSOLES: LazyLock<Mutex<HashMap<Handle, ()>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static ARGS_CAPS: LazyLock<Mutex<HashMap<Handle, ()>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static STDIN_CAPS: LazyLock<Mutex<HashMap<Handle, ()>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static NET_CAPS: LazyLock<Mutex<HashMap<Handle, ()>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static TCP_LISTENERS: LazyLock<Mutex<HashMap<Handle, TcpListener>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static TCP_CONNS: LazyLock<Mutex<HashMap<Handle, TcpStream>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static SLICES: LazyLock<Mutex<HashMap<Handle, SliceState>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static BUFFERS: LazyLock<Mutex<HashMap<Handle, Vec<u8>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static VECS_U8: LazyLock<Mutex<HashMap<Handle, Vec<u8>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static VECS_I32: LazyLock<Mutex<HashMap<Handle, Vec<i32>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static VECS_STRING: LazyLock<Mutex<HashMap<Handle, Vec<String>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static ARGS: LazyLock<Vec<String>> = LazyLock::new(|| std::env::args().collect());

#[derive(Debug, Clone)]
struct ReadFsState {
    root: PathBuf,
}

#[derive(Debug, Clone)]
struct FilesystemState {
    root: PathBuf,
}

#[derive(Debug, Clone)]
struct DirState {
    root: PathBuf,
    rel: PathBuf,
}

#[derive(Debug, Clone)]
struct FileReadState {
    root: PathBuf,
    rel: PathBuf,
}

#[repr(C)]
pub struct CapString {
    bytes: Handle,
}

#[derive(Copy, Clone, Debug)]
struct SliceState {
    ptr: usize,
    len: usize,
}

#[derive(Copy, Clone, Debug)]
enum VecErr {
    OutOfRange = 0,
    Empty = 1,
}

fn new_handle() -> Handle {
    let mut buf = [0u8; 8];
    loop {
        if let Err(err) = getrandom(&mut buf) {
            panic!("failed to generate capability handle: {err}");
        }
        let value = u64::from_le_bytes(buf);
        if value != 0 {
            return value;
        }
    }
}

fn insert_handle<T>(
    table: &LazyLock<Mutex<HashMap<Handle, T>>>,
    handle: Handle,
    value: T,
    label: &'static str,
) {
    let mut table = table.lock().expect(label);
    table.insert(handle, value);
}

fn take_handle<T>(
    table: &LazyLock<Mutex<HashMap<Handle, T>>>,
    handle: Handle,
    label: &'static str,
) -> Option<T> {
    let mut table = table.lock().expect(label);
    table.remove(&handle)
}

fn has_handle<T>(
    table: &LazyLock<Mutex<HashMap<Handle, T>>>,
    handle: Handle,
    label: &'static str,
) -> bool {
    with_table(table, label, |table| table.contains_key(&handle))
}

fn with_table<T, R>(
    table: &LazyLock<Mutex<HashMap<Handle, T>>>,
    label: &'static str,
    f: impl FnOnce(&mut HashMap<Handle, T>) -> R,
) -> R {
    let mut table = table.lock().expect(label);
    f(&mut table)
}

fn to_slice_handle_bytes(bytes: Vec<u8>) -> Handle {
    let bytes = bytes.into_boxed_slice();
    let len = bytes.len();
    let ptr = Box::into_raw(bytes) as *mut u8;
    let handle = new_handle();
    with_table(&SLICES, "slice table", |table| {
        table.insert(
            handle,
            SliceState {
                ptr: ptr as usize,
                len,
            },
        );
    });
    handle
}

fn to_slice_handle(value: String) -> Handle {
    to_slice_handle_bytes(value.into_bytes())
}

fn write_handle_result(
    out_ok: *mut Handle,
    out_err: *mut i32,
    result: Result<Handle, FsErr>,
) -> u8 {
    unsafe {
        if !out_ok.is_null() {
            *out_ok = 0;
        }
        if !out_err.is_null() {
            *out_err = 0;
        }
    }
    match result {
        Ok(handle) => {
            unsafe {
                if !out_ok.is_null() {
                    *out_ok = handle;
                }
            }
            0
        }
        Err(err) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = err as i32;
                }
            }
            1
        }
    }
}

fn write_handle_result_code(
    out_ok: *mut Handle,
    out_err: *mut i32,
    result: Result<Handle, i32>,
) -> u8 {
    unsafe {
        if !out_ok.is_null() {
            *out_ok = 0;
        }
        if !out_err.is_null() {
            *out_err = 0;
        }
    }
    match result {
        Ok(handle) => {
            unsafe {
                if !out_ok.is_null() {
                    *out_ok = handle;
                }
            }
            0
        }
        Err(err) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = err;
                }
            }
            1
        }
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_console(_sys: Handle) -> Handle {
    if !has_handle(&ROOT_CAPS, _sys, "root cap table") {
        return 0;
    }
    let handle = new_handle();
    insert_handle(&CONSOLES, handle, (), "console table");
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_args(_sys: Handle) -> Handle {
    if !has_handle(&ROOT_CAPS, _sys, "root cap table") {
        return 0;
    }
    let handle = new_handle();
    insert_handle(&ARGS_CAPS, handle, (), "args table");
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_stdin(_sys: Handle) -> Handle {
    if !has_handle(&ROOT_CAPS, _sys, "root cap table") {
        return 0;
    }
    let handle = new_handle();
    insert_handle(&STDIN_CAPS, handle, (), "stdin table");
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_net(_sys: Handle) -> Handle {
    if !has_handle(&ROOT_CAPS, _sys, "root cap table") {
        return 0;
    }
    let handle = new_handle();
    insert_handle(&NET_CAPS, handle, (), "net table");
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_readfs(
    _sys: Handle,
    root: *const CapString,
) -> Handle {
    if !has_handle(&ROOT_CAPS, _sys, "root cap table") {
        return 0;
    }
    let root = unsafe { read_cap_string(root) };
    let root_path = match root {
        Some(path) => normalize_root(Path::new(&path)),
        None => normalize_root(Path::new("")),
    };
    let Some(root_path) = root_path else {
        return 0;
    };
    let handle = new_handle();
    insert_handle(&READ_FS, handle, ReadFsState { root: root_path }, "readfs table");
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_filesystem(
    _sys: Handle,
    root: *const CapString,
) -> Handle {
    if !has_handle(&ROOT_CAPS, _sys, "root cap table") {
        return 0;
    }
    let root = unsafe { read_cap_string(root) };
    let root_path = match root {
        Some(path) => normalize_root(Path::new(&path)),
        None => normalize_root(Path::new("")),
    };
    let Some(root_path) = root_path else {
        return 0;
    };
    let handle = new_handle();
    insert_handle(
        &FILESYSTEMS,
        handle,
        FilesystemState { root: root_path },
        "filesystem table",
    );
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_root_dir(fs: Handle) -> Handle {
    let state = take_handle(&FILESYSTEMS, fs, "filesystem table");
    let Some(state) = state else {
        return 0;
    };
    let handle = new_handle();
    insert_handle(
        &DIRS,
        handle,
        DirState {
            root: state.root,
            rel: PathBuf::new(),
        },
        "dir table",
    );
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_filesystem_close(fs: Handle) {
    take_handle(&FILESYSTEMS, fs, "filesystem table");
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_subdir(
    dir: Handle,
    name: *const CapString,
) -> Handle {
    let name = unsafe { read_cap_string(name) };
    let state = take_handle(&DIRS, dir, "dir table");
    let (Some(state), Some(name)) = (state, name) else {
        return 0;
    };
    let Some(name_rel) = normalize_relative(Path::new(&name)) else {
        return 0;
    };
    let combined = state.rel.join(name_rel);
    let Some(rel) = normalize_relative(&combined) else {
        return 0;
    };
    let handle = new_handle();
    insert_handle(
        &DIRS,
        handle,
        DirState {
            root: state.root,
            rel,
        },
        "dir table",
    );
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_open_read(
    dir: Handle,
    name: *const CapString,
) -> Handle {
    let name = unsafe { read_cap_string(name) };
    let state = take_handle(&DIRS, dir, "dir table");
    let (Some(state), Some(name)) = (state, name) else {
        return 0;
    };
    let Some(name_rel) = normalize_relative(Path::new(&name)) else {
        return 0;
    };
    let combined = state.rel.join(name_rel);
    let Some(rel) = normalize_relative(&combined) else {
        return 0;
    };
    let handle = new_handle();
    insert_handle(
        &FILE_READS,
        handle,
        FileReadState {
            root: state.root,
            rel,
        },
        "file read table",
    );
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_dir_close(dir: Handle) {
    take_handle(&DIRS, dir, "dir table");
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_exists(
    fs: Handle,
    path: *const CapString,
) -> u8 {
    let path = unsafe { read_cap_string(path) };
    let state = take_handle(&READ_FS, fs, "readfs table");
    let (Some(state), Some(path)) = (state, path) else {
        return 0;
    };
    let Some(relative) = normalize_relative(Path::new(&path)) else {
        return 0;
    };
    let full = state.root.join(relative);
    match full.canonicalize() {
        Ok(path) => u8::from(path.starts_with(&state.root) && path.exists()),
        Err(_) => 0,
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_read_bytes(
    fs: Handle,
    path: *const CapString,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    let path = unsafe { read_cap_string(path) };
    let state = take_handle(&READ_FS, fs, "readfs table");
    let (Some(state), Some(path)) = (state, path) else {
        return write_handle_result(out_ok, out_err, Err(FsErr::PermissionDenied));
    };
    let Some(relative) = normalize_relative(Path::new(&path)) else {
        return write_handle_result(out_ok, out_err, Err(FsErr::InvalidPath));
    };
    let full = match resolve_rooted_path(&state.root, &relative) {
        Ok(path) => path,
        Err(err) => return write_handle_result(out_ok, out_err, Err(err)),
    };
    match std::fs::read(&full) {
        Ok(bytes) => {
            let handle = new_handle();
            with_table(&VECS_U8, "vec u8 table", |table| {
                table.insert(handle, bytes);
            });
            write_handle_result(out_ok, out_err, Ok(handle))
        }
        Err(err) => write_handle_result(out_ok, out_err, Err(map_fs_err(err))),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_list_dir(
    fs: Handle,
    path: *const CapString,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    let path = unsafe { read_cap_string(path) };
    let state = take_handle(&READ_FS, fs, "readfs table");
    let (Some(state), Some(path)) = (state, path) else {
        return write_handle_result(out_ok, out_err, Err(FsErr::PermissionDenied));
    };
    let Some(relative) = normalize_relative(Path::new(&path)) else {
        return write_handle_result(out_ok, out_err, Err(FsErr::InvalidPath));
    };
    let full = match resolve_rooted_path(&state.root, &relative) {
        Ok(path) => path,
        Err(err) => return write_handle_result(out_ok, out_err, Err(err)),
    };
    let entries = match std::fs::read_dir(&full) {
        Ok(entries) => entries,
        Err(err) => return write_handle_result(out_ok, out_err, Err(map_fs_err(err))),
    };
    let mut names = Vec::new();
    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(err) => return write_handle_result(out_ok, out_err, Err(map_fs_err(err))),
        };
        names.push(entry.file_name().to_string_lossy().to_string());
    }
    let handle = new_handle();
    with_table(&VECS_STRING, "vec string table", |table| {
        table.insert(handle, names);
    });
    write_handle_result(out_ok, out_err, Ok(handle))
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_dir_exists(
    dir: Handle,
    name: *const CapString,
) -> u8 {
    let name = unsafe { read_cap_string(name) };
    let state = take_handle(&DIRS, dir, "dir table");
    let (Some(state), Some(name)) = (state, name) else {
        return 0;
    };
    let Some(name_rel) = normalize_relative(Path::new(&name)) else {
        return 0;
    };
    let combined = state.rel.join(name_rel);
    match resolve_rooted_path(&state.root, &combined) {
        Ok(path) => u8::from(path.exists()),
        Err(_) => 0,
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_dir_read_bytes(
    dir: Handle,
    name: *const CapString,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    let name = unsafe { read_cap_string(name) };
    let state = take_handle(&DIRS, dir, "dir table");
    let (Some(state), Some(name)) = (state, name) else {
        return write_handle_result(out_ok, out_err, Err(FsErr::PermissionDenied));
    };
    let Some(name_rel) = normalize_relative(Path::new(&name)) else {
        return write_handle_result(out_ok, out_err, Err(FsErr::InvalidPath));
    };
    let combined = state.rel.join(name_rel);
    let full = match resolve_rooted_path(&state.root, &combined) {
        Ok(path) => path,
        Err(err) => return write_handle_result(out_ok, out_err, Err(err)),
    };
    match std::fs::read(&full) {
        Ok(bytes) => {
            let handle = new_handle();
            with_table(&VECS_U8, "vec u8 table", |table| {
                table.insert(handle, bytes);
            });
            write_handle_result(out_ok, out_err, Ok(handle))
        }
        Err(err) => write_handle_result(out_ok, out_err, Err(map_fs_err(err))),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_dir_list_dir(
    dir: Handle,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    let state = take_handle(&DIRS, dir, "dir table");
    let Some(state) = state else {
        return write_handle_result(out_ok, out_err, Err(FsErr::PermissionDenied));
    };
    let full = match resolve_rooted_path(&state.root, &state.rel) {
        Ok(path) => path,
        Err(err) => return write_handle_result(out_ok, out_err, Err(err)),
    };
    let entries = match std::fs::read_dir(&full) {
        Ok(entries) => entries,
        Err(err) => return write_handle_result(out_ok, out_err, Err(map_fs_err(err))),
    };
    let mut names = Vec::new();
    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(err) => return write_handle_result(out_ok, out_err, Err(map_fs_err(err))),
        };
        names.push(entry.file_name().to_string_lossy().to_string());
    }
    let handle = new_handle();
    with_table(&VECS_STRING, "vec string table", |table| {
        table.insert(handle, names);
    });
    write_handle_result(out_ok, out_err, Ok(handle))
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_join(
    out: *mut CapString,
    a: *const CapString,
    b: *const CapString,
) {
    let (Some(a), Some(b)) = (unsafe { read_cap_string(a) }, unsafe { read_cap_string(b) }) else {
        unsafe {
            if !out.is_null() {
                (*out).bytes = 0;
            }
        }
        return;
    };
    let joined = Path::new(&a).join(&b);
    let handle = to_slice_handle(joined.to_string_lossy().to_string());
    unsafe {
        if !out.is_null() {
            (*out).bytes = handle;
        }
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_assert(_sys: Handle, cond: u8) {
    if !has_handle(&CONSOLES, _sys, "console table") {
        return;
    }
    if cond == 0 {
        eprintln!("assertion failed");
        std::process::exit(1);
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_console_print(_console: Handle, s: *const CapString) {
    if !has_handle(&CONSOLES, _console, "console table") {
        return;
    }
    let handle = unsafe { if s.is_null() { 0 } else { (*s).bytes } };
    let Some(state) = slice_state(handle) else {
        return;
    };
    unsafe { write_bytes(state.ptr as *const u8, state.len, false) };
}

#[no_mangle]
pub extern "C" fn capable_rt_console_println(_console: Handle, s: *const CapString) {
    if !has_handle(&CONSOLES, _console, "console table") {
        return;
    }
    let handle = unsafe { if s.is_null() { 0 } else { (*s).bytes } };
    let Some(state) = slice_state(handle) else {
        return;
    };
    unsafe { write_bytes(state.ptr as *const u8, state.len, true) };
}

#[no_mangle]
pub extern "C" fn capable_rt_console_print_i32(_console: Handle, value: i32) {
    if !has_handle(&CONSOLES, _console, "console table") {
        return;
    }
    let mut stdout = io::stdout().lock();
    let _ = write!(stdout, "{value}");
    let _ = stdout.flush();
}

#[no_mangle]
pub extern "C" fn capable_rt_console_println_i32(_console: Handle, value: i32) {
    if !has_handle(&CONSOLES, _console, "console table") {
        return;
    }
    let mut stdout = io::stdout().lock();
    let _ = writeln!(stdout, "{value}");
    let _ = stdout.flush();
}

#[no_mangle]
pub extern "C" fn capable_rt_math_add_wrap_i32(a: i32, b: i32) -> i32 {
    a.wrapping_add(b)
}

#[no_mangle]
pub extern "C" fn capable_rt_math_sub_wrap_i32(a: i32, b: i32) -> i32 {
    a.wrapping_sub(b)
}

#[no_mangle]
pub extern "C" fn capable_rt_math_mul_wrap_i32(a: i32, b: i32) -> i32 {
    a.wrapping_mul(b)
}

#[no_mangle]
pub extern "C" fn capable_rt_math_add_wrap_u32(a: u32, b: u32) -> u32 {
    a.wrapping_add(b)
}

#[no_mangle]
pub extern "C" fn capable_rt_math_sub_wrap_u32(a: u32, b: u32) -> u32 {
    a.wrapping_sub(b)
}

#[no_mangle]
pub extern "C" fn capable_rt_math_mul_wrap_u32(a: u32, b: u32) -> u32 {
    a.wrapping_mul(b)
}

#[no_mangle]
pub extern "C" fn capable_rt_math_add_wrap_u8(a: u8, b: u8) -> u8 {
    a.wrapping_add(b)
}

#[no_mangle]
pub extern "C" fn capable_rt_math_sub_wrap_u8(a: u8, b: u8) -> u8 {
    a.wrapping_sub(b)
}

#[no_mangle]
pub extern "C" fn capable_rt_math_mul_wrap_u8(a: u8, b: u8) -> u8 {
    a.wrapping_mul(b)
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_read_to_string(
    fs: Handle,
    path: *const CapString,
    out_ok: *mut CapString,
    out_err: *mut i32,
) -> u8 {
    let path = unsafe { read_cap_string(path) };
    let state = take_handle(&READ_FS, fs, "readfs table");

    let Some(state) = state else {
        return write_result(out_ok, out_err, Err(FsErr::PermissionDenied));
    };
    let Some(path) = path else {
        return write_result(out_ok, out_err, Err(FsErr::InvalidPath));
    };
    let relative = match normalize_relative(Path::new(&path)) {
        Some(path) => path,
        None => return write_result(out_ok, out_err, Err(FsErr::InvalidPath)),
    };
    let full = state.root.join(relative);
    let full = match full.canonicalize() {
        Ok(path) => path,
        Err(err) => return write_result(out_ok, out_err, Err(map_fs_err(err))),
    };
    if !full.starts_with(&state.root) {
        return write_result(out_ok, out_err, Err(FsErr::InvalidPath));
    }

    match std::fs::read_to_string(&full) {
        Ok(contents) => write_result(out_ok, out_err, Ok(contents)),
        Err(err) => write_result(out_ok, out_err, Err(map_fs_err(err))),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_readfs_close(fs: Handle) {
    take_handle(&READ_FS, fs, "readfs table");
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_file_read_to_string(
    file: Handle,
    out_ok: *mut CapString,
    out_err: *mut i32,
) -> u8 {
    let state = take_handle(&FILE_READS, file, "file read table");

    let Some(state) = state else {
        return write_result(out_ok, out_err, Err(FsErr::PermissionDenied));
    };
    let full = state.root.join(state.rel);
    let full = match full.canonicalize() {
        Ok(path) => path,
        Err(err) => return write_result(out_ok, out_err, Err(map_fs_err(err))),
    };
    if !full.starts_with(&state.root) {
        return write_result(out_ok, out_err, Err(FsErr::InvalidPath));
    }

    match std::fs::read_to_string(&full) {
        Ok(contents) => write_result(out_ok, out_err, Ok(contents)),
        Err(err) => write_result(out_ok, out_err, Err(map_fs_err(err))),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_file_read_close(file: Handle) {
    take_handle(&FILE_READS, file, "file read table");
}

#[no_mangle]
pub extern "C" fn capable_rt_net_connect(
    net: Handle,
    host: *const CapString,
    port: i32,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    if !has_handle(&NET_CAPS, net, "net table") {
        return write_handle_result_code(out_ok, out_err, Err(NetErr::IoError as i32));
    }
    let host = unsafe { read_cap_string(host) };
    let Some(host) = host else {
        return write_handle_result_code(out_ok, out_err, Err(NetErr::InvalidAddress as i32));
    };
    if host.is_empty() || port <= 0 || port > u16::MAX as i32 {
        return write_handle_result_code(out_ok, out_err, Err(NetErr::InvalidAddress as i32));
    }
    match TcpStream::connect((host.as_str(), port as u16)) {
        Ok(stream) => {
            let handle = new_handle();
            insert_handle(&TCP_CONNS, handle, stream, "tcp conn table");
            write_handle_result_code(out_ok, out_err, Ok(handle))
        }
        Err(err) => write_handle_result_code(out_ok, out_err, Err(map_net_err(err) as i32)),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_net_listen(
    net: Handle,
    host: *const CapString,
    port: i32,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    if !has_handle(&NET_CAPS, net, "net table") {
        return write_handle_result_code(out_ok, out_err, Err(NetErr::IoError as i32));
    }
    let host = unsafe { read_cap_string(host) };
    let Some(host) = host else {
        return write_handle_result_code(out_ok, out_err, Err(NetErr::InvalidAddress as i32));
    };
    if host.is_empty() || port <= 0 || port > u16::MAX as i32 {
        return write_handle_result_code(out_ok, out_err, Err(NetErr::InvalidAddress as i32));
    }
    match TcpListener::bind((host.as_str(), port as u16)) {
        Ok(listener) => {
            let handle = new_handle();
            insert_handle(&TCP_LISTENERS, handle, listener, "tcp listener table");
            write_handle_result_code(out_ok, out_err, Ok(handle))
        }
        Err(err) => write_handle_result_code(out_ok, out_err, Err(map_net_err(err) as i32)),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_net_accept(
    listener: Handle,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    let listener = take_handle(&TCP_LISTENERS, listener, "tcp listener table");
    let Some(listener) = listener else {
        return write_handle_result_code(out_ok, out_err, Err(NetErr::IoError as i32));
    };
    match listener.accept() {
        Ok((stream, _addr)) => {
            let handle = new_handle();
            insert_handle(&TCP_CONNS, handle, stream, "tcp conn table");
            write_handle_result_code(out_ok, out_err, Ok(handle))
        }
        Err(err) => write_handle_result_code(out_ok, out_err, Err(map_net_err(err) as i32)),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_net_read_to_string(
    conn: Handle,
    out_ok: *mut CapString,
    out_err: *mut i32,
) -> u8 {
    let result = with_table(&TCP_CONNS, "tcp conn table", |table| {
        let Some(stream) = table.get_mut(&conn) else {
            return Err(NetErr::IoError);
        };
        let mut buffer = String::new();
        match stream.read_to_string(&mut buffer) {
            Ok(_) => Ok(buffer),
            Err(err) => Err(map_net_err(err)),
        }
    });
    write_string_result(out_ok, out_err, result.map_err(|err| err as i32))
}

#[no_mangle]
pub extern "C" fn capable_rt_net_read(
    conn: Handle,
    max_size: i32,
    out_ok: *mut CapString,
    out_err: *mut i32,
) -> u8 {
    let result = with_table(&TCP_CONNS, "tcp conn table", |table| {
        let Some(stream) = table.get_mut(&conn) else {
            return Err(NetErr::IoError);
        };
        let max = max_size.max(0) as usize;
        let mut buffer = vec![0u8; max];
        match stream.read(&mut buffer) {
            Ok(n) => {
                buffer.truncate(n);
                match String::from_utf8(buffer) {
                    Ok(s) => Ok(s),
                    Err(_) => Err(NetErr::InvalidData),
                }
            }
            Err(err) => Err(map_net_err(err)),
        }
    });
    write_string_result(out_ok, out_err, result.map_err(|err| err as i32))
}

#[no_mangle]
pub extern "C" fn capable_rt_net_write(
    conn: Handle,
    data: *const CapString,
    out_err: *mut i32,
) -> u8 {
    let data = unsafe { read_cap_string(data) };
    let Some(data) = data else {
        unsafe {
            if !out_err.is_null() {
                *out_err = NetErr::InvalidData as i32;
            }
        }
        return 1;
    };
    with_table(&TCP_CONNS, "tcp conn table", |table| {
        if !out_err.is_null() {
            unsafe {
                *out_err = 0;
            }
        }
        let Some(stream) = table.get_mut(&conn) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = NetErr::IoError as i32;
                }
            }
            return 1;
        };
        if let Err(err) = stream.write_all(data.as_bytes()) {
            unsafe {
                if !out_err.is_null() {
                    *out_err = map_net_err(err) as i32;
                }
            }
            return 1;
        }
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_net_close(conn: Handle) {
    take_handle(&TCP_CONNS, conn, "tcp conn table");
}

#[no_mangle]
pub extern "C" fn capable_rt_net_listener_close(listener: Handle) {
    take_handle(&TCP_LISTENERS, listener, "tcp listener table");
}

#[no_mangle]
pub extern "C" fn capable_rt_start() -> i32 {
    let sys = new_handle();
    insert_handle(&ROOT_CAPS, sys, (), "root cap table");
    unsafe { capable_main(sys) }
}

#[no_mangle]
pub extern "C" fn capable_rt_malloc(_sys: Handle, size: i32) -> *mut u8 {
    if size <= 0 {
        return std::ptr::null_mut();
    }
    unsafe { libc::malloc(size as usize) as *mut u8 }
}

#[no_mangle]
pub extern "C" fn capable_rt_free(_sys: Handle, ptr: *mut u8) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        libc::free(ptr as *mut libc::c_void);
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_cast_u8_to_u32(_sys: Handle, ptr: *mut u8) -> *mut u32 {
    ptr as *mut u32
}

#[no_mangle]
pub extern "C" fn capable_rt_cast_u32_to_u8(_sys: Handle, ptr: *mut u32) -> *mut u8 {
    ptr as *mut u8
}

#[no_mangle]
pub extern "C" fn capable_rt_alloc_default(_sys: Handle) -> Handle {
    new_handle()
}

#[no_mangle]
pub extern "C" fn capable_rt_slice_from_ptr(_sys: Handle, ptr: *mut u8, len: i32) -> Handle {
    let len = len.max(0) as usize;
    let handle = new_handle();
    with_table(&SLICES, "slice table", |table| {
        table.insert(
            handle,
            SliceState {
                ptr: ptr as usize,
                len,
            },
        );
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_mut_slice_from_ptr(_sys: Handle, ptr: *mut u8, len: i32) -> Handle {
    let len = len.max(0) as usize;
    let handle = new_handle();
    with_table(&SLICES, "slice table", |table| {
        table.insert(
            handle,
            SliceState {
                ptr: ptr as usize,
                len,
            },
        );
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_slice_len(slice: Handle) -> i32 {
    with_table(&SLICES, "slice table", |table| {
        table
            .get(&slice)
            .map(|state| state.len.min(i32::MAX as usize) as i32)
            .unwrap_or(0)
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_slice_at(slice: Handle, index: i32) -> u8 {
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => return 0,
    };
    with_table(&SLICES, "slice table", |table| {
        let Some(state) = table.get(&slice) else {
            return 0;
        };
        if state.ptr == 0 || idx >= state.len {
            return 0;
        }
        let ptr = state.ptr as *mut u8;
        unsafe { *ptr.add(idx) }
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_mut_slice_at(slice: Handle, index: i32) -> u8 {
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => return 0,
    };
    with_table(&SLICES, "slice table", |table| {
        let Some(state) = table.get(&slice) else {
            return 0;
        };
        if state.ptr == 0 || idx >= state.len {
            return 0;
        }
        let ptr = state.ptr as *mut u8;
        unsafe { *ptr.add(idx) }
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_buffer_new(
    _alloc: Handle,
    initial_len: i32,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    if initial_len < 0 {
        unsafe {
            if !out_err.is_null() {
                *out_err = 0;
            }
        }
        return 1;
    }
    let len = initial_len as usize;
    let mut data = Vec::new();
    if data.try_reserve(len).is_err() {
        unsafe {
            if !out_err.is_null() {
                *out_err = 0;
            }
        }
        return 1;
    }
    data.resize(len, 0);
    let handle = new_handle();
    with_table(&BUFFERS, "buffer table", |table| {
        table.insert(handle, data);
    });
    unsafe {
        if !out_ok.is_null() {
            *out_ok = handle;
        }
    }
    0
}

#[no_mangle]
pub extern "C" fn capable_rt_buffer_new_default(
    initial_len: i32,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    capable_rt_buffer_new(0, initial_len, out_ok, out_err)
}

#[no_mangle]
pub extern "C" fn capable_rt_buffer_len(buffer: Handle) -> i32 {
    with_table(&BUFFERS, "buffer table", |table| {
        table
            .get(&buffer)
            .map(|data| data.len().min(i32::MAX as usize) as i32)
            .unwrap_or(0)
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_buffer_push(
    buffer: Handle,
    value: u8,
    out_err: *mut i32,
) -> u8 {
    with_table(&BUFFERS, "buffer table", |table| {
        let Some(data) = table.get_mut(&buffer) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        if data.try_reserve(1).is_err() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        }
        data.push(value);
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_buffer_extend(
    buffer: Handle,
    slice: Handle,
    out_err: *mut i32,
) -> u8 {
    let slice_state = with_table(&SLICES, "slice table", |table| table.get(&slice).copied());
    let Some(slice_state) = slice_state else {
        unsafe {
            if !out_err.is_null() {
                *out_err = 0;
            }
        }
        return 1;
    };
    let bytes = unsafe { std::slice::from_raw_parts(slice_state.ptr as *const u8, slice_state.len) };
    with_table(&BUFFERS, "buffer table", |table| {
        let Some(data) = table.get_mut(&buffer) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        if data.try_reserve(bytes.len()).is_err() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        }
        data.extend_from_slice(bytes);
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_buffer_is_empty(buffer: Handle) -> u8 {
    with_table(&BUFFERS, "buffer table", |table| {
        u8::from(table.get(&buffer).map(|data| data.is_empty()).unwrap_or(true))
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_buffer_free(_alloc: Handle, buffer: Handle) {
    with_table(&BUFFERS, "buffer table", |table| {
        table.remove(&buffer);
    });
}

#[no_mangle]
pub extern "C" fn capable_rt_buffer_as_slice(buffer: Handle) -> Handle {
    let Some((ptr, len)) = with_table(&BUFFERS, "buffer table", |table| {
        let Some(data) = table.get(&buffer) else {
            return None;
        };
        let ptr = if data.is_empty() { 0 } else { data.as_ptr() as usize };
        Some((ptr, data.len()))
    }) else {
        return 0;
    };
    let handle = new_handle();
    with_table(&SLICES, "slice table", |table| {
        table.insert(handle, SliceState { ptr, len });
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_buffer_as_mut_slice(buffer: Handle) -> Handle {
    let Some((ptr, len)) = with_table(&BUFFERS, "buffer table", |table| {
        let Some(data) = table.get_mut(&buffer) else {
            return None;
        };
        let ptr = if data.is_empty() { 0 } else { data.as_mut_ptr() as usize };
        Some((ptr, data.len()))
    }) else {
        return 0;
    };
    let handle = new_handle();
    with_table(&SLICES, "slice table", |table| {
        table.insert(handle, SliceState { ptr, len });
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_slice_copy(
    slice: Handle,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    let slice_state = with_table(&SLICES, "slice table", |table| table.get(&slice).copied());
    let Some(slice_state) = slice_state else {
        unsafe {
            if !out_ok.is_null() {
                *out_ok = 0;
            }
            if !out_err.is_null() {
                *out_err = 0;
            }
        }
        return 1;
    };
    let bytes =
        unsafe { std::slice::from_raw_parts(slice_state.ptr as *const u8, slice_state.len) };
    let handle = to_slice_handle_bytes(bytes.to_vec());
    unsafe {
        if !out_ok.is_null() {
            *out_ok = handle;
        }
        if !out_err.is_null() {
            *out_err = 0;
        }
    }
    0
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_new(_alloc: Handle) -> Handle {
    let handle = new_handle();
    with_table(&VECS_U8, "vec u8 table", |table| {
        table.insert(handle, Vec::new());
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_len(vec: Handle) -> i32 {
    with_table(&VECS_U8, "vec u8 table", |table| {
        table
            .get(&vec)
            .map(|data| data.len().min(i32::MAX as usize) as i32)
            .unwrap_or(0)
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_get(
    vec: Handle,
    index: i32,
    out_ok: *mut u8,
    out_err: *mut i32,
) -> u8 {
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        }
    };
    with_table(&VECS_U8, "vec u8 table", |table| {
        let Some(data) = table.get(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        };
        let Some(value) = data.get(idx) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        };
        unsafe {
            if !out_ok.is_null() {
                *out_ok = *value;
            }
        }
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_set(
    vec: Handle,
    index: i32,
    value: u8,
    out_err: *mut i32,
) -> u8 {
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        }
    };
    with_table(&VECS_U8, "vec u8 table", |table| {
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        };
        if idx >= data.len() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        }
        data[idx] = value;
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_push(
    vec: Handle,
    value: u8,
    out_err: *mut i32,
) -> u8 {
    with_table(&VECS_U8, "vec u8 table", |table| {
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        if data.try_reserve(1).is_err() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        }
        data.push(value);
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_extend(
    vec: Handle,
    other: Handle,
    out_err: *mut i32,
) -> u8 {
    with_table(&VECS_U8, "vec u8 table", |table| {
        let Some(other_data) = table.get(&other).cloned() else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        if data.try_reserve(other_data.len()).is_err() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        }
        data.extend_from_slice(&other_data);
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_filter(vec: Handle, value: u8) -> Handle {
    let filtered = with_table(&VECS_U8, "vec u8 table", |table| {
        table
            .get(&vec)
            .map(|data| data.iter().copied().filter(|b| *b == value).collect::<Vec<_>>())
    });
    let Some(filtered) = filtered else {
        return 0;
    };
    let handle = new_handle();
    with_table(&VECS_U8, "vec u8 table", |table| {
        table.insert(handle, filtered);
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_map_add(vec: Handle, delta: u8) -> Handle {
    let mapped = with_table(&VECS_U8, "vec u8 table", |table| {
        table
            .get(&vec)
            .map(|data| data.iter().map(|b| b.wrapping_add(delta)).collect::<Vec<_>>())
    });
    let Some(mapped) = mapped else {
        return 0;
    };
    let handle = new_handle();
    with_table(&VECS_U8, "vec u8 table", |table| {
        table.insert(handle, mapped);
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_slice(
    vec: Handle,
    start: i32,
    len: i32,
    out_ok: *mut Handle,
    out_err: *mut i32,
) -> u8 {
    let start = match usize::try_from(start) {
        Ok(start) => start,
        Err(_) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        }
    };
    let len = match usize::try_from(len) {
        Ok(len) => len,
        Err(_) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        }
    };
    let slice_state = with_table(&VECS_U8, "vec u8 table", |table| {
        let Some(data) = table.get(&vec) else {
            return None;
        };
        if start > data.len() || start.saturating_add(len) > data.len() {
            return None;
        }
        let ptr = if len == 0 { 0 } else { unsafe { data.as_ptr().add(start) as usize } };
        Some(SliceState { ptr, len })
    });
    let Some(slice_state) = slice_state else {
        unsafe {
            if !out_err.is_null() {
                *out_err = VecErr::OutOfRange as i32;
            }
        }
        return 1;
    };
    let handle = new_handle();
    with_table(&SLICES, "slice table", |table| {
        table.insert(handle, slice_state);
    });
    unsafe {
        if !out_ok.is_null() {
            *out_ok = handle;
        }
    }
    0
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_pop(
    vec: Handle,
    out_ok: *mut u8,
    out_err: *mut i32,
) -> u8 {
    with_table(&VECS_U8, "vec u8 table", |table| {
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::Empty as i32;
                }
            }
            return 1;
        };
        let Some(value) = data.pop() else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::Empty as i32;
                }
            }
            return 1;
        };
        unsafe {
            if !out_ok.is_null() {
                *out_ok = value;
            }
        }
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_as_slice(vec: Handle) -> Handle {
    let Some((ptr, len)) = with_table(&VECS_U8, "vec u8 table", |table| {
        let Some(data) = table.get(&vec) else {
            return None;
        };
        let ptr = if data.is_empty() { 0 } else { data.as_ptr() as usize };
        Some((ptr, data.len()))
    }) else {
        return 0;
    };
    let handle = new_handle();
    with_table(&SLICES, "slice table", |table| {
        table.insert(handle, SliceState { ptr, len });
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_u8_free(_alloc: Handle, vec: Handle) {
    with_table(&VECS_U8, "vec u8 table", |table| {
        table.remove(&vec);
    });
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_new(_alloc: Handle) -> Handle {
    let handle = new_handle();
    with_table(&VECS_I32, "vec i32 table", |table| {
        table.insert(handle, Vec::new());
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_len(vec: Handle) -> i32 {
    with_table(&VECS_I32, "vec i32 table", |table| {
        table
            .get(&vec)
            .map(|data| data.len().min(i32::MAX as usize) as i32)
            .unwrap_or(0)
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_get(
    vec: Handle,
    index: i32,
    out_ok: *mut i32,
    out_err: *mut i32,
) -> u8 {
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        }
    };
    with_table(&VECS_I32, "vec i32 table", |table| {
        let Some(data) = table.get(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        };
        let Some(value) = data.get(idx) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        };
        unsafe {
            if !out_ok.is_null() {
                *out_ok = *value;
            }
        }
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_set(
    vec: Handle,
    index: i32,
    value: i32,
    out_err: *mut i32,
) -> u8 {
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        }
    };
    with_table(&VECS_I32, "vec i32 table", |table| {
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        };
        if idx >= data.len() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::OutOfRange as i32;
                }
            }
            return 1;
        }
        data[idx] = value;
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_push(
    vec: Handle,
    value: i32,
    out_err: *mut i32,
) -> u8 {
    with_table(&VECS_I32, "vec i32 table", |table| {
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        if data.try_reserve(1).is_err() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        }
        data.push(value);
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_extend(
    vec: Handle,
    other: Handle,
    out_err: *mut i32,
) -> u8 {
    with_table(&VECS_I32, "vec i32 table", |table| {
        let Some(other_data) = table.get(&other).cloned() else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        if data.try_reserve(other_data.len()).is_err() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        }
        data.extend_from_slice(&other_data);
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_filter(vec: Handle, value: i32) -> Handle {
    let filtered = with_table(&VECS_I32, "vec i32 table", |table| {
        table
            .get(&vec)
            .map(|data| data.iter().copied().filter(|b| *b == value).collect::<Vec<_>>())
    });
    let Some(filtered) = filtered else {
        return 0;
    };
    let handle = new_handle();
    with_table(&VECS_I32, "vec i32 table", |table| {
        table.insert(handle, filtered);
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_map_add(vec: Handle, delta: i32) -> Handle {
    let mapped = with_table(&VECS_I32, "vec i32 table", |table| {
        table
            .get(&vec)
            .map(|data| data.iter().map(|b| b.wrapping_add(delta)).collect::<Vec<_>>())
    });
    let Some(mapped) = mapped else {
        return 0;
    };
    let handle = new_handle();
    with_table(&VECS_I32, "vec i32 table", |table| {
        table.insert(handle, mapped);
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_pop(
    vec: Handle,
    out_ok: *mut i32,
    out_err: *mut i32,
) -> u8 {
    with_table(&VECS_I32, "vec i32 table", |table| {
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::Empty as i32;
                }
            }
            return 1;
        };
        let Some(value) = data.pop() else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = VecErr::Empty as i32;
                }
            }
            return 1;
        };
        unsafe {
            if !out_ok.is_null() {
                *out_ok = value;
            }
        }
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_i32_free(_alloc: Handle, vec: Handle) {
    with_table(&VECS_I32, "vec i32 table", |table| {
        table.remove(&vec);
    });
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_new(_alloc: Handle) -> Handle {
    let handle = new_handle();
    with_table(&VECS_STRING, "vec string table", |table| {
        table.insert(handle, Vec::new());
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_new_default() -> Handle {
    capable_rt_vec_string_new(0)
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_len(vec: Handle) -> i32 {
    with_table(&VECS_STRING, "vec string table", |table| {
        table
            .get(&vec)
            .map(|data| data.len().min(i32::MAX as usize) as i32)
            .unwrap_or(0)
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_get(
    vec: Handle,
    index: i32,
    out_ok: *mut CapString,
    out_err: *mut i32,
) -> u8 {
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => {
            return write_string_result(out_ok, out_err, Err(VecErr::OutOfRange as i32));
        }
    };
    with_table(&VECS_STRING, "vec string table", |table| {
        let Some(data) = table.get(&vec) else {
            return write_string_result(out_ok, out_err, Err(VecErr::OutOfRange as i32));
        };
        let Some(value) = data.get(idx) else {
            return write_string_result(out_ok, out_err, Err(VecErr::OutOfRange as i32));
        };
        write_string_result(out_ok, out_err, Ok(value.clone()))
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_push(
    vec: Handle,
    value: *const CapString,
    out_err: *mut i32,
) -> u8 {
    let value = unsafe { read_cap_string(value) };
    let Some(value) = value else {
        unsafe {
            if !out_err.is_null() {
                *out_err = 0;
            }
        }
        return 1;
    };
    with_table(&VECS_STRING, "vec string table", |table| {
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        if data.try_reserve(1).is_err() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        }
        data.push(value);
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_extend(
    vec: Handle,
    other: Handle,
    out_err: *mut i32,
) -> u8 {
    with_table(&VECS_STRING, "vec string table", |table| {
        let Some(other_data) = table.get(&other).cloned() else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        let Some(data) = table.get_mut(&vec) else {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        };
        if data.try_reserve(other_data.len()).is_err() {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        }
        data.extend(other_data);
        0
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_pop(
    vec: Handle,
    out_ok: *mut CapString,
    out_err: *mut i32,
) -> u8 {
    with_table(&VECS_STRING, "vec string table", |table| {
        let Some(data) = table.get_mut(&vec) else {
            return write_string_result(out_ok, out_err, Err(VecErr::Empty as i32));
        };
        let Some(value) = data.pop() else {
            return write_string_result(out_ok, out_err, Err(VecErr::Empty as i32));
        };
        write_string_result(out_ok, out_err, Ok(value))
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_free(_alloc: Handle, vec: Handle) {
    with_table(&VECS_STRING, "vec string table", |table| {
        table.remove(&vec);
    });
}

#[no_mangle]
pub extern "C" fn capable_rt_args_len(_sys: Handle) -> i32 {
    if !has_handle(&ARGS_CAPS, _sys, "args table") {
        return 0;
    }
    ARGS.len().min(i32::MAX as usize) as i32
}

#[no_mangle]
pub extern "C" fn capable_rt_args_at(
    _sys: Handle,
    index: i32,
    out_ok: *mut CapString,
    out_err: *mut i32,
) -> u8 {
    if !has_handle(&ARGS_CAPS, _sys, "args table") {
        unsafe {
            if !out_err.is_null() {
                *out_err = 0;
            }
        }
        return 1;
    }
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = 0;
                }
            }
            return 1;
        }
    };
    let Some(arg) = ARGS.get(idx) else {
        unsafe {
            if !out_err.is_null() {
                *out_err = 0;
            }
        }
        return 1;
    };
    write_string_result(out_ok, out_err, Ok(arg.clone()))
}

#[no_mangle]
pub extern "C" fn capable_rt_read_stdin_to_string(
    _sys: Handle,
    out_ok: *mut CapString,
    out_err: *mut i32,
) -> u8 {
    if !has_handle(&STDIN_CAPS, _sys, "stdin table") {
        return write_string_result(out_ok, out_err, Err(0));
    }
    let mut input = String::new();
    let result = io::stdin().read_to_string(&mut input);
    match result {
        Ok(_) => write_string_result(out_ok, out_err, Ok(input)),
        Err(_) => write_string_result(out_ok, out_err, Err(0)),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_string_eq(
    left: *const CapString,
    right: *const CapString,
) -> i8 {
    let left_handle = unsafe { if left.is_null() { 0 } else { (*left).bytes } };
    let right_handle = unsafe { if right.is_null() { 0 } else { (*right).bytes } };
    let left_state = slice_state(left_handle);
    let right_state = slice_state(right_handle);
    let (Some(left_state), Some(right_state)) = (left_state, right_state) else {
        return 0;
    };
    if left_state.len != right_state.len {
        return 0;
    }
    if left_state.len == 0 {
        return 1;
    }
    if left_state.ptr == 0 || right_state.ptr == 0 {
        return 0;
    }
    let s1 =
        unsafe { std::slice::from_raw_parts(left_state.ptr as *const u8, left_state.len) };
    let s2 =
        unsafe { std::slice::from_raw_parts(right_state.ptr as *const u8, right_state.len) };
    i8::from(s1 == s2)
}

#[no_mangle]
pub extern "C" fn capable_rt_bytes_is_whitespace(value: u8) -> u8 {
    match value {
        b' ' | b'\t' | b'\n' | b'\r' => 1,
        _ => 0,
    }
}

unsafe fn write_bytes(ptr: *const u8, len: usize, newline: bool) {
    if ptr.is_null() {
        return;
    }
    let slice = std::slice::from_raw_parts(ptr, len);
    let mut stdout = io::stdout().lock();
    let _ = stdout.write_all(slice);
    if newline {
        let _ = stdout.write_all(b"\n");
    }
    let _ = stdout.flush();
}

#[derive(Copy, Clone, Debug)]
enum FsErr {
    NotFound = 0,
    PermissionDenied = 1,
    InvalidPath = 2,
    IoError = 3,
}

#[derive(Copy, Clone, Debug)]
enum NetErr {
    InvalidAddress = 0,
    IoError = 1,
    InvalidData = 2,
}

fn map_fs_err(err: std::io::Error) -> FsErr {
    use std::io::ErrorKind;
    match err.kind() {
        ErrorKind::NotFound => FsErr::NotFound,
        ErrorKind::PermissionDenied => FsErr::PermissionDenied,
        ErrorKind::InvalidInput => FsErr::InvalidPath,
        _ => FsErr::IoError,
    }
}

fn map_net_err(err: std::io::Error) -> NetErr {
    use std::io::ErrorKind;
    match err.kind() {
        ErrorKind::InvalidInput | ErrorKind::AddrNotAvailable | ErrorKind::AddrInUse => {
            NetErr::InvalidAddress
        }
        ErrorKind::InvalidData => NetErr::InvalidData,
        _ => NetErr::IoError,
    }
}

fn resolve_rooted_path(root: &Path, rel: &Path) -> Result<PathBuf, FsErr> {
    let full = root.join(rel);
    let full = full.canonicalize().map_err(map_fs_err)?;
    if !full.starts_with(root) {
        return Err(FsErr::InvalidPath);
    }
    Ok(full)
}

fn write_result(out_ok: *mut CapString, out_err: *mut i32, result: Result<String, FsErr>) -> u8 {
    unsafe {
        if !out_ok.is_null() {
            (*out_ok).bytes = 0;
        }
        if !out_err.is_null() {
            *out_err = 0;
        }
    }
    match result {
        Ok(contents) => {
            let handle = to_slice_handle(contents);
            unsafe {
                if !out_ok.is_null() {
                    (*out_ok).bytes = handle;
                }
            }
            0
        }
        Err(err) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = err as i32;
                }
            }
            1
        }
    }
}

fn write_string_result(
    out_ok: *mut CapString,
    out_err: *mut i32,
    result: Result<String, i32>,
) -> u8 {
    unsafe {
        if !out_ok.is_null() {
            (*out_ok).bytes = 0;
        }
        if !out_err.is_null() {
            *out_err = 0;
        }
    }
    match result {
        Ok(s) => {
            let handle = to_slice_handle(s);
            unsafe {
                if !out_ok.is_null() {
                    (*out_ok).bytes = handle;
                }
            }
            0
        }
        Err(err) => {
            unsafe {
                if !out_err.is_null() {
                    *out_err = err;
                }
            }
            1
        }
    }
}

fn slice_state(handle: Handle) -> Option<SliceState> {
    with_table(&SLICES, "slice table", |table| table.get(&handle).copied())
}

fn read_slice_string(handle: Handle) -> Option<String> {
    if handle == 0 {
        return Some(String::new());
    }
    let state = slice_state(handle)?;
    if state.ptr == 0 {
        return Some(String::new());
    }
    let bytes = unsafe { std::slice::from_raw_parts(state.ptr as *const u8, state.len) };
    std::str::from_utf8(bytes).ok().map(|s| s.to_string())
}

unsafe fn read_cap_string(ptr: *const CapString) -> Option<String> {
    if ptr.is_null() {
        return None;
    }
    read_slice_string((*ptr).bytes)
}

fn normalize_root(root: &Path) -> Option<PathBuf> {
    let path = if root.is_absolute() {
        root.to_path_buf()
    } else {
        match std::env::current_dir() {
            Ok(cwd) => cwd.join(root),
            Err(_) => root.to_path_buf(),
        }
    };
    match path.canonicalize() {
        Ok(canon) => Some(canon),
        Err(_) => None,
    }
}

fn normalize_relative(path: &Path) -> Option<PathBuf> {
    let mut out = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Prefix(_) | Component::RootDir => return None,
            Component::CurDir => {}
            Component::ParentDir => {
                if !out.pop() {
                    return None;
                }
            }
            Component::Normal(part) => out.push(part),
        }
    }
    if out.as_os_str() == OsStr::new("") {
        Some(PathBuf::new())
    } else {
        Some(out)
    }
}

extern "C" {
    fn capable_main(sys: Handle) -> i32;
}

#[cfg(test)]
mod tests {
    use super::{normalize_relative, normalize_root};
    use std::path::Path;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn normalize_relative_allows_simple_paths() {
        let path = normalize_relative(Path::new("a/b.txt")).expect("path");
        assert_eq!(path.to_string_lossy(), "a/b.txt");
    }

    #[test]
    fn normalize_relative_cleans_dot_segments() {
        let path = normalize_relative(Path::new("a/./b")).expect("path");
        assert_eq!(path.to_string_lossy(), "a/b");
        let path = normalize_relative(Path::new("a/../b")).expect("path");
        assert_eq!(path.to_string_lossy(), "b");
    }

    #[test]
    fn normalize_relative_rejects_escape() {
        assert!(normalize_relative(Path::new("../secret")).is_none());
        assert!(normalize_relative(Path::new("a/../../b")).is_none());
    }

    #[test]
    fn normalize_relative_rejects_absolute() {
        assert!(normalize_relative(Path::new("/abs/path")).is_none());
    }

    #[test]
    fn normalize_root_rejects_missing_paths() {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        let name = format!("capable-missing-{nanos}");
        let path = std::env::temp_dir().join(name);
        assert_eq!(normalize_root(&path), None);
    }

    #[test]
    fn normalize_root_accepts_existing_path() {
        let cwd = std::env::current_dir().expect("cwd");
        assert!(normalize_root(&cwd).is_some());
    }
}
