use std::collections::HashMap;
use std::ffi::OsStr;
use std::io::{self, Write};
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{LazyLock, Mutex};

pub type Handle = u64;

static NEXT_HANDLE: AtomicU64 = AtomicU64::new(1);
static READ_FS: LazyLock<Mutex<HashMap<Handle, ReadFsState>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

#[derive(Debug, Clone)]
struct ReadFsState {
    root: PathBuf,
}

fn new_handle() -> Handle {
    NEXT_HANDLE.fetch_add(1, Ordering::Relaxed)
}

#[no_mangle]
pub extern "C" fn capable_rt_system_console(_sys: Handle) -> Handle {
    new_handle()
}

#[no_mangle]
pub extern "C" fn capable_rt_system_fs_read(
    _sys: Handle,
    root_ptr: *const u8,
    root_len: usize,
) -> Handle {
    let root = unsafe { read_str(root_ptr, root_len) };
    let root_path = match root {
        Some(path) => normalize_root(Path::new(&path)),
        None => normalize_root(Path::new("")),
    };
    let handle = new_handle();
    let mut table = READ_FS.lock().expect("readfs table");
    table.insert(handle, ReadFsState { root: root_path });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_console_print(_console: Handle, ptr: *const u8, len: usize) {
    unsafe { write_bytes(ptr, len, false) };
}

#[no_mangle]
pub extern "C" fn capable_rt_console_println(_console: Handle, ptr: *const u8, len: usize) {
    unsafe { write_bytes(ptr, len, true) };
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_read_to_string(
    fs: Handle,
    path_ptr: *const u8,
    path_len: usize,
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
) -> u8 {
    let path = unsafe { read_str(path_ptr, path_len) };
    let state = {
        let table = READ_FS.lock().expect("readfs table");
        table.get(&fs).cloned()
    };

    let Some(state) = state else {
        return write_result(out_ptr, out_len, out_err, Err(FsErr::PermissionDenied));
    };
    let Some(path) = path else {
        return write_result(out_ptr, out_len, out_err, Err(FsErr::InvalidPath));
    };
    let relative = match normalize_relative(Path::new(&path)) {
        Some(path) => path,
        None => return write_result(out_ptr, out_len, out_err, Err(FsErr::InvalidPath)),
    };
    let full = state.root.join(relative);
    let full = match full.canonicalize() {
        Ok(path) => path,
        Err(err) => return write_result(out_ptr, out_len, out_err, Err(map_fs_err(err))),
    };
    if !full.starts_with(&state.root) {
        return write_result(out_ptr, out_len, out_err, Err(FsErr::PermissionDenied));
    }

    match std::fs::read_to_string(&full) {
        Ok(contents) => write_result(out_ptr, out_len, out_err, Ok(contents)),
        Err(err) => write_result(out_ptr, out_len, out_err, Err(map_fs_err(err))),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_start() -> i32 {
    let sys = new_handle();
    unsafe { capable_main(sys) }
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

fn map_fs_err(err: std::io::Error) -> FsErr {
    use std::io::ErrorKind;
    match err.kind() {
        ErrorKind::NotFound => FsErr::NotFound,
        ErrorKind::PermissionDenied => FsErr::PermissionDenied,
        ErrorKind::InvalidInput => FsErr::InvalidPath,
        _ => FsErr::IoError,
    }
}

fn write_result(
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
    result: Result<String, FsErr>,
) -> u8 {
    unsafe {
        if !out_ptr.is_null() {
            *out_ptr = std::ptr::null();
        }
        if !out_len.is_null() {
            *out_len = 0;
        }
        if !out_err.is_null() {
            *out_err = 0;
        }
    }
    match result {
        Ok(contents) => {
            let bytes = contents.into_bytes().into_boxed_slice();
            let len = bytes.len() as u64;
            let ptr = Box::into_raw(bytes) as *const u8;
            unsafe {
                if !out_ptr.is_null() {
                    *out_ptr = ptr;
                }
                if !out_len.is_null() {
                    *out_len = len;
                }
                if !out_err.is_null() {
                    *out_err = 0;
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

unsafe fn read_str(ptr: *const u8, len: usize) -> Option<String> {
    if ptr.is_null() {
        return None;
    }
    let bytes = std::slice::from_raw_parts(ptr, len);
    std::str::from_utf8(bytes).ok().map(|s| s.to_string())
}

fn normalize_root(root: &Path) -> PathBuf {
    let path = if root.is_absolute() {
        root.to_path_buf()
    } else {
        match std::env::current_dir() {
            Ok(cwd) => cwd.join(root),
            Err(_) => root.to_path_buf(),
        }
    };
    normalize_path(&path)
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

fn normalize_path(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Prefix(prefix) => out.push(prefix.as_os_str()),
            Component::RootDir => out.push(Component::RootDir.as_os_str()),
            Component::CurDir => {}
            Component::ParentDir => {
                let _ = out.pop();
            }
            Component::Normal(part) => out.push(part),
        }
    }
    out
}

extern "C" {
    fn capable_main(sys: Handle) -> i32;
}
