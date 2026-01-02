//! Runtime intrinsic registry.
//!
//! Any stdlib function listed here is treated as an intrinsic: its `.cap` body
//! is ignored, and codegen emits a direct call to the runtime symbol. If a
//! function is not listed here, the Capable implementation is used instead.
//! See `stdlib/README.md` for the stdlib-facing explanation.

use std::collections::HashMap;

use cranelift_codegen::ir::Type;

use crate::abi::AbiType;

use super::{FnInfo, FnSig};

pub fn register_runtime_intrinsics(ptr_ty: Type) -> HashMap<String, FnInfo> {
    let mut map = HashMap::new();
    // System + args.
    let system_console = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let system_fs_read = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Handle,
    };
    let system_filesystem = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Handle,
    };
    // Filesystem.
    let fs_root_dir = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let fs_subdir = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Handle,
    };
    let fs_open_read = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Handle,
    };
    let fs_read_to_string = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let fs_read_to_string_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr, AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32))],
        ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let fs_read_bytes = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let fs_read_bytes_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Ptr,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let fs_list_dir = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let fs_list_dir_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Ptr,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let fs_exists = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Bool,
    };
    let fs_readfs_close = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Unit,
    };
    let fs_filesystem_close = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Unit,
    };
    let fs_dir_close = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Unit,
    };
    let fs_file_read_to_string = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let fs_file_read_to_string_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32))],
        ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let fs_file_read_close = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Unit,
    };
    let fs_dir_list_dir = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let fs_dir_list_dir_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let fs_join = FnSig {
        params: vec![AbiType::Ptr, AbiType::Ptr],
        ret: AbiType::Ptr,
    };
    let fs_join_abi = FnSig {
        params: vec![AbiType::Ptr, AbiType::Ptr, AbiType::Ptr],
        ret: AbiType::Unit,
    };
    // Console.
    let console_println = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Unit,
    };
    let console_print = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Unit,
    };
    let console_print_i32 = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Unit,
    };
    // Math.
    let math_i32 = FnSig {
        params: vec![AbiType::I32, AbiType::I32],
        ret: AbiType::I32,
    };
    let math_u32 = FnSig {
        params: vec![AbiType::U32, AbiType::U32],
        ret: AbiType::U32,
    };
    let math_u8 = FnSig {
        params: vec![AbiType::U8, AbiType::U8],
        ret: AbiType::U8,
    };
    // Buffer + slices.
    let mem_malloc = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Ptr,
    };
    let mem_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Unit,
    };
    let mem_cast = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Ptr,
    };
    let mem_alloc_default = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let system_mint_args = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let system_mint_stdin = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let system_mint_net = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    // Net.
    let net_listen = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let net_listen_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Ptr,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let net_connect = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let net_connect_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Ptr,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let net_read_to_string = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let net_read_to_string_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32))],
        ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let net_read = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let net_read_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32))],
        ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let net_write = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let net_write_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Ptr,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let net_accept = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let net_accept_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let net_close = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Unit,
    };
    let net_listener_close = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Unit,
    };
    let args_at = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let args_at_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32))],
        ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    // Buffer + slices.
    let mem_slice_from_ptr = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr, AbiType::I32],
        ret: AbiType::Handle,
    };
    let mem_slice_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };
    let mem_slice_at = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::U8,
    };
    // Vecs.
    let mem_buffer_new = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let mem_buffer_new_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let mem_buffer_new_default = FnSig {
        params: vec![AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let mem_buffer_new_default_abi = FnSig {
        params: vec![
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let mem_buffer_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };
    let mem_buffer_push = FnSig {
        params: vec![AbiType::Handle, AbiType::U8],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let mem_buffer_push_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::U8,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let mem_buffer_extend = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let mem_buffer_extend_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let mem_buffer_is_empty = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Bool,
    };
    let mem_buffer_as_slice = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let mem_buffer_as_mut_slice = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let mem_slice_copy = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let mem_slice_copy_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let mem_buffer_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Unit,
    };
    let vec_new = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let vec_new_default = FnSig {
        params: vec![],
        ret: AbiType::Handle,
    };
    let vec_u8_get = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::U8), Box::new(AbiType::I32)),
    };
    let vec_u8_get_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::U8), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::U8), Box::new(AbiType::I32)),
    };
    let vec_u8_set = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::U8],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_set_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::U8,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_push = FnSig {
        params: vec![AbiType::Handle, AbiType::U8],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_push_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::U8,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_extend = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_extend_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_filter = FnSig {
        params: vec![AbiType::Handle, AbiType::U8],
        ret: AbiType::Handle,
    };
    let vec_u8_map_add = FnSig {
        params: vec![AbiType::Handle, AbiType::U8],
        ret: AbiType::Handle,
    };
    let vec_u8_slice = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let vec_u8_slice_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let vec_u8_pop = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::U8), Box::new(AbiType::I32)),
    };
    let vec_u8_pop_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::U8), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::U8), Box::new(AbiType::I32)),
    };
    let vec_u8_as_slice = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let vec_u8_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Unit,
    };
    let vec_i32_get = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::I32), Box::new(AbiType::I32)),
    };
    let vec_i32_get_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::I32), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::I32), Box::new(AbiType::I32)),
    };
    let vec_i32_set = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_set_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_push = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_push_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_extend = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_extend_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_filter = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Handle,
    };
    let vec_i32_map_add = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Handle,
    };
    let vec_i32_pop = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::I32), Box::new(AbiType::I32)),
    };
    let vec_i32_pop_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::I32), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::I32), Box::new(AbiType::I32)),
    };
    let vec_i32_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Unit,
    };
    let vec_string_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };
    let vec_string_get = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let vec_string_get_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32))],
        ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let vec_string_push = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_string_push_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Ptr,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_string_extend = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_string_extend_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_string_pop = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let vec_string_pop_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32))],
        ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
    };
    let vec_string_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Unit,
    };
    // Vec lengths.
    let vec_u8_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };
    let vec_i32_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };

    // === System + args ===
    map.insert(
        "sys.system.RootCap__mint_console".to_string(),
        FnInfo {
            sig: system_console,
            abi_sig: None,
            symbol: "capable_rt_mint_console".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.RootCap__mint_readfs".to_string(),
        FnInfo {
            sig: system_fs_read,
            abi_sig: None,
            symbol: "capable_rt_mint_readfs".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.RootCap__mint_filesystem".to_string(),
        FnInfo {
            sig: system_filesystem,
            abi_sig: None,
            symbol: "capable_rt_mint_filesystem".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.RootCap__mint_args".to_string(),
        FnInfo {
            sig: system_mint_args,
            abi_sig: None,
            symbol: "capable_rt_mint_args".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.RootCap__mint_stdin".to_string(),
        FnInfo {
            sig: system_mint_stdin,
            abi_sig: None,
            symbol: "capable_rt_mint_stdin".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.RootCap__mint_net".to_string(),
        FnInfo {
            sig: system_mint_net,
            abi_sig: None,
            symbol: "capable_rt_mint_net".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.args.Args__len".to_string(),
        FnInfo {
            sig: FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::I32,
            },
            abi_sig: None,
            symbol: "capable_rt_args_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.args.Args__at".to_string(),
        FnInfo {
            sig: args_at,
            abi_sig: Some(args_at_abi),
            symbol: "capable_rt_args_at".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    // === Stdin ===
    map.insert(
        "sys.stdin.Stdin__read_to_string".to_string(),
        FnInfo {
            sig: FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            },
            abi_sig: Some(FnSig {
                params: vec![AbiType::Handle, AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32))],
                ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            }),
            symbol: "capable_rt_read_stdin_to_string".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    // === Net ===
    map.insert(
        "sys.net.Net__listen".to_string(),
        FnInfo {
            sig: net_listen,
            abi_sig: Some(net_listen_abi),
            symbol: "capable_rt_net_listen".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.net.Net__connect".to_string(),
        FnInfo {
            sig: net_connect,
            abi_sig: Some(net_connect_abi),
            symbol: "capable_rt_net_connect".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.net.TcpListener__accept".to_string(),
        FnInfo {
            sig: net_accept,
            abi_sig: Some(net_accept_abi),
            symbol: "capable_rt_net_accept".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.net.TcpListener__close".to_string(),
        FnInfo {
            sig: net_listener_close,
            abi_sig: None,
            symbol: "capable_rt_net_listener_close".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.net.TcpConn__read_to_string".to_string(),
        FnInfo {
            sig: net_read_to_string,
            abi_sig: Some(net_read_to_string_abi),
            symbol: "capable_rt_net_read_to_string".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.net.TcpConn__read".to_string(),
        FnInfo {
            sig: net_read,
            abi_sig: Some(net_read_abi),
            symbol: "capable_rt_net_read".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.net.TcpConn__write".to_string(),
        FnInfo {
            sig: net_write,
            abi_sig: Some(net_write_abi),
            symbol: "capable_rt_net_write".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.net.TcpConn__close".to_string(),
        FnInfo {
            sig: net_close,
            abi_sig: None,
            symbol: "capable_rt_net_close".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    // === Alloc ===
    map.insert(
        "sys.system.RootCap__mint_alloc_default".to_string(),
        FnInfo {
            sig: mem_alloc_default,
            abi_sig: None,
            symbol: "capable_rt_alloc_default".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    // === Console ===
    map.insert(
        "sys.console.Console__println".to_string(),
        FnInfo {
            sig: console_println,
            abi_sig: None,
            symbol: "capable_rt_console_println".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.Console__print".to_string(),
        FnInfo {
            sig: console_print,
            abi_sig: None,
            symbol: "capable_rt_console_print".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.Console__print_i32".to_string(),
        FnInfo {
            sig: console_print_i32.clone(),
            abi_sig: None,
            symbol: "capable_rt_console_print_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.Console__println_i32".to_string(),
        FnInfo {
            sig: console_print_i32,
            abi_sig: None,
            symbol: "capable_rt_console_println_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.Console__assert".to_string(),
        FnInfo {
            sig: FnSig {
                params: vec![AbiType::Handle, AbiType::Bool, AbiType::Ptr],
                ret: AbiType::Unit,
            },
            abi_sig: None,
            symbol: "capable_rt_assert".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    // === Math ===
    map.insert(
        "sys.math.add_wrap_i32".to_string(),
        FnInfo {
            sig: math_i32.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_add_wrap_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.sub_wrap_i32".to_string(),
        FnInfo {
            sig: math_i32.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_sub_wrap_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.mul_wrap_i32".to_string(),
        FnInfo {
            sig: math_i32,
            abi_sig: None,
            symbol: "capable_rt_math_mul_wrap_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.add_wrap_u32".to_string(),
        FnInfo {
            sig: math_u32.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_add_wrap_u32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.sub_wrap_u32".to_string(),
        FnInfo {
            sig: math_u32.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_sub_wrap_u32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.mul_wrap_u32".to_string(),
        FnInfo {
            sig: math_u32,
            abi_sig: None,
            symbol: "capable_rt_math_mul_wrap_u32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.add_wrap_u8".to_string(),
        FnInfo {
            sig: math_u8.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_add_wrap_u8".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.sub_wrap_u8".to_string(),
        FnInfo {
            sig: math_u8.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_sub_wrap_u8".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.mul_wrap_u8".to_string(),
        FnInfo {
            sig: math_u8,
            abi_sig: None,
            symbol: "capable_rt_math_mul_wrap_u8".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    // === Filesystem ===
    map.insert(
        "sys.fs.ReadFS__read_to_string".to_string(),
        FnInfo {
            sig: fs_read_to_string,
            abi_sig: Some(fs_read_to_string_abi),
            symbol: "capable_rt_fs_read_to_string".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.ReadFS__read_bytes".to_string(),
        FnInfo {
            sig: fs_read_bytes.clone(),
            abi_sig: Some(fs_read_bytes_abi.clone()),
            symbol: "capable_rt_fs_read_bytes".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.ReadFS__list_dir".to_string(),
        FnInfo {
            sig: fs_list_dir,
            abi_sig: Some(fs_list_dir_abi),
            symbol: "capable_rt_fs_list_dir".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.ReadFS__exists".to_string(),
        FnInfo {
            sig: fs_exists.clone(),
            abi_sig: None,
            symbol: "capable_rt_fs_exists".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.ReadFS__close".to_string(),
        FnInfo {
            sig: fs_readfs_close,
            abi_sig: None,
            symbol: "capable_rt_fs_readfs_close".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Filesystem__root_dir".to_string(),
        FnInfo {
            sig: fs_root_dir,
            abi_sig: None,
            symbol: "capable_rt_fs_root_dir".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Filesystem__close".to_string(),
        FnInfo {
            sig: fs_filesystem_close,
            abi_sig: None,
            symbol: "capable_rt_fs_filesystem_close".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Dir__subdir".to_string(),
        FnInfo {
            sig: fs_subdir,
            abi_sig: None,
            symbol: "capable_rt_fs_subdir".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Dir__open_read".to_string(),
        FnInfo {
            sig: fs_open_read,
            abi_sig: None,
            symbol: "capable_rt_fs_open_read".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Dir__read_bytes".to_string(),
        FnInfo {
            sig: fs_read_bytes,
            abi_sig: Some(fs_read_bytes_abi),
            symbol: "capable_rt_fs_dir_read_bytes".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Dir__list_dir".to_string(),
        FnInfo {
            sig: fs_dir_list_dir,
            abi_sig: Some(fs_dir_list_dir_abi),
            symbol: "capable_rt_fs_dir_list_dir".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Dir__exists".to_string(),
        FnInfo {
            sig: fs_exists,
            abi_sig: None,
            symbol: "capable_rt_fs_dir_exists".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Dir__close".to_string(),
        FnInfo {
            sig: fs_dir_close,
            abi_sig: None,
            symbol: "capable_rt_fs_dir_close".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.FileRead__read_to_string".to_string(),
        FnInfo {
            sig: fs_file_read_to_string,
            abi_sig: Some(fs_file_read_to_string_abi),
            symbol: "capable_rt_fs_file_read_to_string".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.FileRead__close".to_string(),
        FnInfo {
            sig: fs_file_read_close,
            abi_sig: None,
            symbol: "capable_rt_fs_file_read_close".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.join".to_string(),
        FnInfo {
            sig: fs_join,
            abi_sig: Some(fs_join_abi),
            symbol: "capable_rt_fs_join".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    // === Buffer + slices ===
    map.insert(
        "sys.buffer.new".to_string(),
        FnInfo {
            sig: mem_buffer_new_default,
            abi_sig: Some(mem_buffer_new_default_abi),
            symbol: "capable_rt_buffer_new_default".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__buffer_new".to_string(),
        FnInfo {
            sig: mem_buffer_new,
            abi_sig: Some(mem_buffer_new_abi),
            symbol: "capable_rt_buffer_new".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__buffer_free".to_string(),
        FnInfo {
            sig: mem_buffer_free,
            abi_sig: None,
            symbol: "capable_rt_buffer_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__malloc".to_string(),
        FnInfo {
            sig: mem_malloc,
            abi_sig: None,
            symbol: "capable_rt_malloc".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__free".to_string(),
        FnInfo {
            sig: mem_free,
            abi_sig: None,
            symbol: "capable_rt_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__cast_u8_to_u32".to_string(),
        FnInfo {
            sig: mem_cast.clone(),
            abi_sig: None,
            symbol: "capable_rt_cast_u8_to_u32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__cast_u32_to_u8".to_string(),
        FnInfo {
            sig: mem_cast,
            abi_sig: None,
            symbol: "capable_rt_cast_u32_to_u8".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__slice_from_ptr".to_string(),
        FnInfo {
            sig: mem_slice_from_ptr.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_from_ptr".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__mut_slice_from_ptr".to_string(),
        FnInfo {
            sig: mem_slice_from_ptr,
            abi_sig: None,
            symbol: "capable_rt_mut_slice_from_ptr".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__len".to_string(),
        FnInfo {
            sig: mem_buffer_len,
            abi_sig: None,
            symbol: "capable_rt_buffer_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__push".to_string(),
        FnInfo {
            sig: mem_buffer_push,
            abi_sig: Some(mem_buffer_push_abi),
            symbol: "capable_rt_buffer_push".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__extend".to_string(),
        FnInfo {
            sig: mem_buffer_extend,
            abi_sig: Some(mem_buffer_extend_abi),
            symbol: "capable_rt_buffer_extend".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__is_empty".to_string(),
        FnInfo {
            sig: mem_buffer_is_empty,
            abi_sig: None,
            symbol: "capable_rt_buffer_is_empty".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__as_slice".to_string(),
        FnInfo {
            sig: mem_buffer_as_slice,
            abi_sig: None,
            symbol: "capable_rt_buffer_as_slice".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__as_mut_slice".to_string(),
        FnInfo {
            sig: mem_buffer_as_mut_slice,
            abi_sig: None,
            symbol: "capable_rt_buffer_as_mut_slice".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.copy_slice".to_string(),
        FnInfo {
            sig: mem_slice_copy,
            abi_sig: Some(mem_slice_copy_abi),
            symbol: "capable_rt_slice_copy".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Slice__len".to_string(),
        FnInfo {
            sig: mem_slice_len,
            abi_sig: None,
            symbol: "capable_rt_slice_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Slice__at".to_string(),
        FnInfo {
            sig: mem_slice_at.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_at".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.MutSlice__at".to_string(),
        FnInfo {
            sig: mem_slice_at,
            abi_sig: None,
            symbol: "capable_rt_mut_slice_at".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    // === Vec ===
    map.insert(
        "sys.buffer.Alloc__vec_u8_new".to_string(),
        FnInfo {
            sig: vec_new.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_u8_new".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_u8_free".to_string(),
        FnInfo {
            sig: vec_u8_free,
            abi_sig: None,
            symbol: "capable_rt_vec_u8_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_i32_new".to_string(),
        FnInfo {
            sig: vec_new.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_i32_new".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_i32_free".to_string(),
        FnInfo {
            sig: vec_i32_free,
            abi_sig: None,
            symbol: "capable_rt_vec_i32_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_string_new".to_string(),
        FnInfo {
            sig: vec_new,
            abi_sig: None,
            symbol: "capable_rt_vec_string_new".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.vec_string_new".to_string(),
        FnInfo {
            sig: vec_new_default,
            abi_sig: None,
            symbol: "capable_rt_vec_string_new_default".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_string_free".to_string(),
        FnInfo {
            sig: vec_string_free,
            abi_sig: None,
            symbol: "capable_rt_vec_string_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__len".to_string(),
        FnInfo {
            sig: vec_u8_len,
            abi_sig: None,
            symbol: "capable_rt_vec_u8_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__get".to_string(),
        FnInfo {
            sig: vec_u8_get,
            abi_sig: Some(vec_u8_get_abi),
            symbol: "capable_rt_vec_u8_get".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__set".to_string(),
        FnInfo {
            sig: vec_u8_set,
            abi_sig: Some(vec_u8_set_abi),
            symbol: "capable_rt_vec_u8_set".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__push".to_string(),
        FnInfo {
            sig: vec_u8_push,
            abi_sig: Some(vec_u8_push_abi),
            symbol: "capable_rt_vec_u8_push".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__extend".to_string(),
        FnInfo {
            sig: vec_u8_extend,
            abi_sig: Some(vec_u8_extend_abi),
            symbol: "capable_rt_vec_u8_extend".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__filter".to_string(),
        FnInfo {
            sig: vec_u8_filter,
            abi_sig: None,
            symbol: "capable_rt_vec_u8_filter".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__map_add".to_string(),
        FnInfo {
            sig: vec_u8_map_add,
            abi_sig: None,
            symbol: "capable_rt_vec_u8_map_add".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__slice".to_string(),
        FnInfo {
            sig: vec_u8_slice,
            abi_sig: Some(vec_u8_slice_abi),
            symbol: "capable_rt_vec_u8_slice".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__pop".to_string(),
        FnInfo {
            sig: vec_u8_pop,
            abi_sig: Some(vec_u8_pop_abi),
            symbol: "capable_rt_vec_u8_pop".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__as_slice".to_string(),
        FnInfo {
            sig: vec_u8_as_slice,
            abi_sig: None,
            symbol: "capable_rt_vec_u8_as_slice".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__len".to_string(),
        FnInfo {
            sig: vec_i32_len,
            abi_sig: None,
            symbol: "capable_rt_vec_i32_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__get".to_string(),
        FnInfo {
            sig: vec_i32_get,
            abi_sig: Some(vec_i32_get_abi),
            symbol: "capable_rt_vec_i32_get".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__set".to_string(),
        FnInfo {
            sig: vec_i32_set,
            abi_sig: Some(vec_i32_set_abi),
            symbol: "capable_rt_vec_i32_set".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__push".to_string(),
        FnInfo {
            sig: vec_i32_push,
            abi_sig: Some(vec_i32_push_abi),
            symbol: "capable_rt_vec_i32_push".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__extend".to_string(),
        FnInfo {
            sig: vec_i32_extend,
            abi_sig: Some(vec_i32_extend_abi),
            symbol: "capable_rt_vec_i32_extend".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__filter".to_string(),
        FnInfo {
            sig: vec_i32_filter,
            abi_sig: None,
            symbol: "capable_rt_vec_i32_filter".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__map_add".to_string(),
        FnInfo {
            sig: vec_i32_map_add,
            abi_sig: None,
            symbol: "capable_rt_vec_i32_map_add".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__pop".to_string(),
        FnInfo {
            sig: vec_i32_pop,
            abi_sig: Some(vec_i32_pop_abi),
            symbol: "capable_rt_vec_i32_pop".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecString__len".to_string(),
        FnInfo {
            sig: vec_string_len,
            abi_sig: None,
            symbol: "capable_rt_vec_string_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecString__get".to_string(),
        FnInfo {
            sig: vec_string_get,
            abi_sig: Some(vec_string_get_abi),
            symbol: "capable_rt_vec_string_get".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecString__push".to_string(),
        FnInfo {
            sig: vec_string_push,
            abi_sig: Some(vec_string_push_abi),
            symbol: "capable_rt_vec_string_push".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecString__extend".to_string(),
        FnInfo {
            sig: vec_string_extend,
            abi_sig: Some(vec_string_extend_abi),
            symbol: "capable_rt_vec_string_extend".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecString__pop".to_string(),
        FnInfo {
            sig: vec_string_pop,
            abi_sig: Some(vec_string_pop_abi),
            symbol: "capable_rt_vec_string_pop".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    // === Bytes ===
    map.insert(
        "sys.bytes.u8__is_whitespace".to_string(),
        FnInfo {
            sig: FnSig {
                params: vec![AbiType::U8],
                ret: AbiType::Bool,
            },
            abi_sig: None,
            symbol: "capable_rt_bytes_is_whitespace".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );

    let _ = ptr_ty;
    map
}
