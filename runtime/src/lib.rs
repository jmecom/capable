use std::io::{self, Write};
use std::sync::atomic::{AtomicU64, Ordering};

pub type Handle = u64;

static NEXT_HANDLE: AtomicU64 = AtomicU64::new(1);

fn new_handle() -> Handle {
    NEXT_HANDLE.fetch_add(1, Ordering::Relaxed)
}

#[no_mangle]
pub extern "C" fn capable_rt_system_console(_sys: Handle) -> Handle {
    new_handle()
}

#[no_mangle]
pub extern "C" fn capable_rt_system_fs_read(_sys: Handle, _root_ptr: *const u8, _root_len: usize) -> Handle {
    new_handle()
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
    _fs: Handle,
    _path_ptr: *const u8,
    _path_len: usize,
    out_ptr: *mut *const u8,
    out_len: *mut u64,
    out_err: *mut i32,
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
    1
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

extern "C" {
    fn capable_main(sys: Handle) -> i32;
}
