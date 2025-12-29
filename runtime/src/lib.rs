use std::collections::HashMap;
use std::ffi::OsStr;
use std::io::{self, Read, Write};
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

fn with_table<T, R>(
    table: &LazyLock<Mutex<HashMap<Handle, T>>>,
    label: &'static str,
    f: impl FnOnce(&mut HashMap<Handle, T>) -> R,
) -> R {
    let mut table = table.lock().expect(label);
    f(&mut table)
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_console(_sys: Handle) -> Handle {
    new_handle()
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_args(_sys: Handle) -> Handle {
    new_handle()
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_stdin(_sys: Handle) -> Handle {
    new_handle()
}

#[no_mangle]
pub extern "C" fn capable_rt_mint_readfs(
    _sys: Handle,
    root_ptr: *const u8,
    root_len: usize,
) -> Handle {
    let root = unsafe { read_str(root_ptr, root_len) };
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
    root_ptr: *const u8,
    root_len: usize,
) -> Handle {
    let root = unsafe { read_str(root_ptr, root_len) };
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
pub extern "C" fn capable_rt_fs_subdir(
    dir: Handle,
    name_ptr: *const u8,
    name_len: usize,
) -> Handle {
    let name = unsafe { read_str(name_ptr, name_len) };
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
    name_ptr: *const u8,
    name_len: usize,
) -> Handle {
    let name = unsafe { read_str(name_ptr, name_len) };
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
pub extern "C" fn capable_rt_assert(_sys: Handle, cond: u8) {
    if cond == 0 {
        eprintln!("assertion failed");
        std::process::exit(1);
    }
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
pub extern "C" fn capable_rt_console_print_i32(_console: Handle, value: i32) {
    let mut stdout = io::stdout().lock();
    let _ = write!(stdout, "{value}");
    let _ = stdout.flush();
}

#[no_mangle]
pub extern "C" fn capable_rt_console_println_i32(_console: Handle, value: i32) {
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
    path_ptr: *const u8,
    path_len: usize,
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
) -> u8 {
    let path = unsafe { read_str(path_ptr, path_len) };
    let state = take_handle(&READ_FS, fs, "readfs table");

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
        return write_result(out_ptr, out_len, out_err, Err(FsErr::InvalidPath));
    }

    match std::fs::read_to_string(&full) {
        Ok(contents) => write_result(out_ptr, out_len, out_err, Ok(contents)),
        Err(err) => write_result(out_ptr, out_len, out_err, Err(map_fs_err(err))),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_file_read_to_string(
    file: Handle,
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
) -> u8 {
    let state = take_handle(&FILE_READS, file, "file read table");

    let Some(state) = state else {
        return write_result(out_ptr, out_len, out_err, Err(FsErr::PermissionDenied));
    };
    let full = state.root.join(state.rel);
    let full = match full.canonicalize() {
        Ok(path) => path,
        Err(err) => return write_result(out_ptr, out_len, out_err, Err(map_fs_err(err))),
    };
    if !full.starts_with(&state.root) {
        return write_result(out_ptr, out_len, out_err, Err(FsErr::InvalidPath));
    }

    match std::fs::read_to_string(&full) {
        Ok(contents) => write_result(out_ptr, out_len, out_err, Ok(contents)),
        Err(err) => write_result(out_ptr, out_len, out_err, Err(map_fs_err(err))),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_fs_file_read_close(file: Handle) {
    take_handle(&FILE_READS, file, "file read table");
}

#[no_mangle]
pub extern "C" fn capable_rt_start() -> i32 {
    let sys = new_handle();
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
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
) -> u8 {
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => {
            return write_string_result(out_ptr, out_len, out_err, Err(VecErr::OutOfRange as i32));
        }
    };
    with_table(&VECS_STRING, "vec string table", |table| {
        let Some(data) = table.get(&vec) else {
            return write_string_result(out_ptr, out_len, out_err, Err(VecErr::OutOfRange as i32));
        };
        let Some(value) = data.get(idx) else {
            return write_string_result(out_ptr, out_len, out_err, Err(VecErr::OutOfRange as i32));
        };
        write_string_result(out_ptr, out_len, out_err, Ok(value.clone()))
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_push(
    vec: Handle,
    ptr: *const u8,
    len: usize,
    out_err: *mut i32,
) -> u8 {
    let value = unsafe { read_str(ptr, len) };
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
pub extern "C" fn capable_rt_vec_string_pop(
    vec: Handle,
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
) -> u8 {
    with_table(&VECS_STRING, "vec string table", |table| {
        let Some(data) = table.get_mut(&vec) else {
            return write_string_result(out_ptr, out_len, out_err, Err(VecErr::Empty as i32));
        };
        let Some(value) = data.pop() else {
            return write_string_result(out_ptr, out_len, out_err, Err(VecErr::Empty as i32));
        };
        write_string_result(out_ptr, out_len, out_err, Ok(value))
    })
}

#[no_mangle]
pub extern "C" fn capable_rt_vec_string_free(_alloc: Handle, vec: Handle) {
    with_table(&VECS_STRING, "vec string table", |table| {
        table.remove(&vec);
    });
}

#[no_mangle]
pub extern "C" fn capable_rt_string_split_whitespace(
    ptr: *const u8,
    len: usize,
) -> Handle {
    let value = unsafe { read_str(ptr, len) };
    let mut vec = Vec::new();
    if let Some(value) = value {
        vec.extend(value.split_whitespace().map(|s| s.to_string()));
    }
    let handle = new_handle();
    with_table(&VECS_STRING, "vec string table", |table| {
        table.insert(handle, vec);
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_string_split(
    ptr: *const u8,
    len: usize,
    delim: u8,
) -> Handle {
    let value = unsafe { read_str(ptr, len) };
    let mut vec = Vec::new();
    if let Some(value) = value {
        let bytes = value.as_bytes();
        let mut start = 0usize;
        for (idx, byte) in bytes.iter().enumerate() {
            if *byte == delim {
                let part = &value[start..idx];
                vec.push(part.to_string());
                start = idx + 1;
            }
        }
        let part = &value[start..];
        vec.push(part.to_string());
    }
    let handle = new_handle();
    with_table(&VECS_STRING, "vec string table", |table| {
        table.insert(handle, vec);
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_string_split_lines(ptr: *const u8, len: usize) -> Handle {
    let value = unsafe { read_str(ptr, len) };
    let mut vec = Vec::new();
    if let Some(value) = value {
        for line in value.split('\n') {
            let line = line.strip_suffix('\r').unwrap_or(line);
            vec.push(line.to_string());
        }
    }
    let handle = new_handle();
    with_table(&VECS_STRING, "vec string table", |table| {
        table.insert(handle, vec);
    });
    handle
}

#[no_mangle]
pub extern "C" fn capable_rt_string_starts_with(
    ptr: *const u8,
    len: usize,
    prefix_ptr: *const u8,
    prefix_len: usize,
) -> u8 {
    let value = unsafe { read_str(ptr, len) };
    let prefix = unsafe { read_str(prefix_ptr, prefix_len) };
    match (value, prefix) {
        (Some(value), Some(prefix)) => u8::from(value.starts_with(&prefix)),
        _ => 0,
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_args_len(_sys: Handle) -> i32 {
    ARGS.len().min(i32::MAX as usize) as i32
}

#[no_mangle]
pub extern "C" fn capable_rt_args_at(
    _sys: Handle,
    index: i32,
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
) -> u8 {
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
    unsafe {
        if !out_ptr.is_null() {
            *out_ptr = arg.as_ptr();
        }
        if !out_len.is_null() {
            *out_len = arg.len() as u64;
        }
    }
    0
}

#[no_mangle]
pub extern "C" fn capable_rt_read_stdin_to_string(
    _sys: Handle,
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
) -> u8 {
    let mut input = String::new();
    let result = io::stdin().read_to_string(&mut input);
    match result {
        Ok(_) => write_string_result(out_ptr, out_len, out_err, Ok(input)),
        Err(_) => write_string_result(out_ptr, out_len, out_err, Err(0)),
    }
}

#[no_mangle]
pub extern "C" fn capable_rt_string_len(ptr: *const u8, len: usize) -> i32 {
    let _ = ptr;
    len.min(i32::MAX as usize) as i32
}

#[no_mangle]
pub extern "C" fn capable_rt_string_byte_at(
    ptr: *const u8,
    len: usize,
    index: i32,
) -> u8 {
    let idx = match usize::try_from(index) {
        Ok(idx) => idx,
        Err(_) => return 0,
    };
    if ptr.is_null() || idx >= len {
        return 0;
    }
    unsafe { *ptr.add(idx) }
}

#[no_mangle]
pub extern "C" fn capable_rt_string_as_slice(ptr: *const u8, len: usize) -> Handle {
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

/// Write a Result[String, i32] payload using the ResultString ABI (u64 length).
fn write_string_result(
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
    result: Result<String, i32>,
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
        Ok(s) => {
            let len = s.len();
            let ptr = s.as_ptr();
            std::mem::forget(s);
            unsafe {
                if !out_ptr.is_null() {
                    *out_ptr = ptr;
                }
                if !out_len.is_null() {
                    *out_len = len as u64;
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

unsafe fn read_str(ptr: *const u8, len: usize) -> Option<String> {
    if ptr.is_null() {
        return None;
    }
    let bytes = std::slice::from_raw_parts(ptr, len);
    std::str::from_utf8(bytes).ok().map(|s| s.to_string())
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
