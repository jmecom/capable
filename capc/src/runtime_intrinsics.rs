use std::collections::HashMap;
use std::sync::OnceLock;

use crate::abi::AbiType;

#[derive(Debug, Clone)]
pub(crate) struct RuntimeFnSig {
    pub params: Vec<AbiType>,
    pub ret: AbiType,
}

#[derive(Debug, Clone)]
pub(crate) struct RuntimeBinding {
    pub module: &'static str,
    pub func: &'static str,
    pub symbol: &'static str,
    pub sig: RuntimeFnSig,
    pub abi_sig: Option<RuntimeFnSig>,
}

fn sig(params: Vec<AbiType>, ret: AbiType) -> RuntimeFnSig {
    RuntimeFnSig { params, ret }
}

fn binding(
    module: &'static str,
    func: &'static str,
    symbol: &'static str,
    sig: RuntimeFnSig,
) -> RuntimeBinding {
    RuntimeBinding {
        module,
        func,
        symbol,
        sig,
        abi_sig: None,
    }
}

fn binding_with_abi(
    module: &'static str,
    func: &'static str,
    symbol: &'static str,
    sig: RuntimeFnSig,
    abi_sig: RuntimeFnSig,
) -> RuntimeBinding {
    RuntimeBinding {
        module,
        func,
        symbol,
        sig,
        abi_sig: Some(abi_sig),
    }
}

fn runtime_binding_list() -> Vec<RuntimeBinding> {
    vec![
        binding(
            "sys.system",
            "RootCap__mint_console",
            "capable_rt_mint_console",
            sig(vec![AbiType::Handle], AbiType::Handle),
        ),
        binding_with_abi(
            "sys.system",
            "RootCap__mint_readfs",
            "capable_rt_mint_readfs",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Handle),
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Handle),
        ),
        binding(
            "sys.system",
            "RootCap__mint_filesystem",
            "capable_rt_mint_filesystem",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Handle),
        ),
        binding(
            "sys.system",
            "RootCap__mint_args",
            "capable_rt_mint_args",
            sig(vec![AbiType::Handle], AbiType::Handle),
        ),
        binding(
            "sys.system",
            "RootCap__mint_stdin",
            "capable_rt_mint_stdin",
            sig(vec![AbiType::Handle], AbiType::Handle),
        ),
        binding(
            "sys.system",
            "RootCap__mint_net",
            "capable_rt_mint_net",
            sig(vec![AbiType::Handle], AbiType::Handle),
        ),
        binding(
            "sys.system",
            "RootCap__mint_alloc_default",
            "capable_rt_alloc_default",
            sig(vec![AbiType::Handle], AbiType::Handle),
        ),
        binding(
            "sys.args",
            "Args__len",
            "capable_rt_args_len",
            sig(vec![AbiType::Handle], AbiType::I32),
        ),
        binding_with_abi(
            "sys.args",
            "Args__at",
            "capable_rt_args_at",
            sig(
                vec![AbiType::Handle, AbiType::I32],
                AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::I32,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.stdin",
            "Stdin__read_to_string_with_alloc",
            "capable_rt_read_stdin_to_string",
            sig(
                vec![AbiType::Handle, AbiType::Handle],
                AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.net",
            "Net__listen",
            "capable_rt_net_listen",
            sig(
                vec![AbiType::Handle, AbiType::Ptr, AbiType::I32],
                AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::I32,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.net",
            "Net__connect",
            "capable_rt_net_connect",
            sig(
                vec![AbiType::Handle, AbiType::Ptr, AbiType::I32],
                AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::I32,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.net",
            "TcpListener__accept",
            "capable_rt_net_accept",
            sig(
                vec![AbiType::Handle],
                AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
        ),
        binding(
            "sys.net",
            "TcpListener__close",
            "capable_rt_net_listener_close",
            sig(vec![AbiType::Handle], AbiType::Unit),
        ),
        binding_with_abi(
            "sys.net",
            "TcpConn__read_to_string_with_alloc",
            "capable_rt_net_read_to_string",
            sig(
                vec![AbiType::Handle, AbiType::Handle],
                AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.net",
            "TcpConn__read_with_alloc",
            "capable_rt_net_read",
            sig(
                vec![AbiType::Handle, AbiType::Handle, AbiType::I32],
                AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::I32,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.net",
            "TcpConn__write",
            "capable_rt_net_write",
            sig(
                vec![AbiType::Handle, AbiType::Ptr],
                AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
            ),
        ),
        binding(
            "sys.net",
            "TcpConn__close",
            "capable_rt_net_close",
            sig(vec![AbiType::Handle], AbiType::Unit),
        ),
        binding(
            "sys.buffer",
            "default_alloc",
            "capable_rt_default_alloc",
            sig(vec![], AbiType::Handle),
        ),
        binding(
            "sys.console",
            "Console__println",
            "capable_rt_console_println",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Unit),
        ),
        binding(
            "sys.console",
            "Console__print",
            "capable_rt_console_print",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Unit),
        ),
        binding(
            "sys.console",
            "Console__print_i32",
            "capable_rt_console_print_i32",
            sig(vec![AbiType::Handle, AbiType::I32], AbiType::Unit),
        ),
        binding(
            "sys.console",
            "Console__println_i32",
            "capable_rt_console_println_i32",
            sig(vec![AbiType::Handle, AbiType::I32], AbiType::Unit),
        ),
        binding(
            "sys.console",
            "Console__assert",
            "capable_rt_assert",
            sig(vec![AbiType::Handle, AbiType::Bool, AbiType::Ptr], AbiType::Unit),
        ),
        binding(
            "sys.math",
            "add_wrap_i32",
            "capable_rt_math_add_wrap_i32",
            sig(vec![AbiType::I32, AbiType::I32], AbiType::I32),
        ),
        binding(
            "sys.math",
            "sub_wrap_i32",
            "capable_rt_math_sub_wrap_i32",
            sig(vec![AbiType::I32, AbiType::I32], AbiType::I32),
        ),
        binding(
            "sys.math",
            "mul_wrap_i32",
            "capable_rt_math_mul_wrap_i32",
            sig(vec![AbiType::I32, AbiType::I32], AbiType::I32),
        ),
        binding(
            "sys.math",
            "add_wrap_u32",
            "capable_rt_math_add_wrap_u32",
            sig(vec![AbiType::U32, AbiType::U32], AbiType::U32),
        ),
        binding(
            "sys.math",
            "sub_wrap_u32",
            "capable_rt_math_sub_wrap_u32",
            sig(vec![AbiType::U32, AbiType::U32], AbiType::U32),
        ),
        binding(
            "sys.math",
            "mul_wrap_u32",
            "capable_rt_math_mul_wrap_u32",
            sig(vec![AbiType::U32, AbiType::U32], AbiType::U32),
        ),
        binding(
            "sys.math",
            "add_wrap_u8",
            "capable_rt_math_add_wrap_u8",
            sig(vec![AbiType::U8, AbiType::U8], AbiType::U8),
        ),
        binding(
            "sys.math",
            "sub_wrap_u8",
            "capable_rt_math_sub_wrap_u8",
            sig(vec![AbiType::U8, AbiType::U8], AbiType::U8),
        ),
        binding(
            "sys.math",
            "mul_wrap_u8",
            "capable_rt_math_mul_wrap_u8",
            sig(vec![AbiType::U8, AbiType::U8], AbiType::U8),
        ),
        binding_with_abi(
            "sys.fs",
            "ReadFS__read_to_string_with_alloc",
            "capable_rt_fs_read_to_string",
            sig(
                vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.fs",
            "ReadFS__read_bytes_with_alloc",
            "capable_rt_fs_read_bytes",
            sig(
                vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.fs",
            "ReadFS__list_dir_with_alloc",
            "capable_rt_fs_list_dir",
            sig(
                vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
        ),
        binding(
            "sys.fs",
            "ReadFS__exists",
            "capable_rt_fs_exists",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Bool),
        ),
        binding(
            "sys.fs",
            "ReadFS__is_dir",
            "capable_rt_fs_is_dir",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Bool),
        ),
        binding(
            "sys.fs",
            "ReadFS__close",
            "capable_rt_fs_readfs_close",
            sig(vec![AbiType::Handle], AbiType::Unit),
        ),
        binding(
            "sys.fs",
            "Filesystem__root_dir",
            "capable_rt_fs_root_dir",
            sig(vec![AbiType::Handle], AbiType::Handle),
        ),
        binding(
            "sys.fs",
            "Filesystem__close",
            "capable_rt_fs_filesystem_close",
            sig(vec![AbiType::Handle], AbiType::Unit),
        ),
        binding(
            "sys.fs",
            "Dir__subdir",
            "capable_rt_fs_subdir",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Handle),
        ),
        binding(
            "sys.fs",
            "Dir__open_read",
            "capable_rt_fs_open_read",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Handle),
        ),
        binding_with_abi(
            "sys.fs",
            "Dir__read_bytes_with_alloc",
            "capable_rt_fs_dir_read_bytes",
            sig(
                vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.fs",
            "Dir__read_to_string_with_alloc",
            "capable_rt_fs_dir_read_to_string",
            sig(
                vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.fs",
            "Dir__list_dir_with_alloc",
            "capable_rt_fs_dir_list_dir",
            sig(
                vec![AbiType::Handle, AbiType::Handle],
                AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            ),
        ),
        binding(
            "sys.fs",
            "Dir__exists",
            "capable_rt_fs_dir_exists",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Bool),
        ),
        binding(
            "sys.fs",
            "Dir__is_dir",
            "capable_rt_fs_dir_is_dir",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Bool),
        ),
        binding_with_abi(
            "sys.fs",
            "Dir__create_dir_all",
            "capable_rt_fs_dir_create_dir_all",
            sig(
                vec![AbiType::Handle, AbiType::Ptr],
                AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
            ),
        ),
        binding_with_abi(
            "sys.fs",
            "Dir__write_string",
            "capable_rt_fs_dir_write_string",
            sig(
                vec![AbiType::Handle, AbiType::Ptr, AbiType::Ptr],
                AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
            ),
        ),
        binding(
            "sys.fs",
            "Dir__close",
            "capable_rt_fs_dir_close",
            sig(vec![AbiType::Handle], AbiType::Unit),
        ),
        binding_with_abi(
            "sys.fs",
            "FileRead__read_to_string_with_alloc",
            "capable_rt_fs_file_read_to_string",
            sig(
                vec![AbiType::Handle, AbiType::Handle],
                AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
            sig(
                vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            ),
        ),
        binding(
            "sys.fs",
            "FileRead__close",
            "capable_rt_fs_file_read_close",
            sig(vec![AbiType::Handle], AbiType::Unit),
        ),
        binding_with_abi(
            "sys.fs",
            "join_with_alloc",
            "capable_rt_fs_join",
            sig(
                vec![AbiType::Handle, AbiType::Ptr, AbiType::Ptr],
                AbiType::Ptr,
            ),
            sig(
                vec![AbiType::Ptr, AbiType::Handle, AbiType::Ptr, AbiType::Ptr],
                AbiType::Unit,
            ),
        ),
        binding(
            "sys.buffer",
            "Alloc__malloc",
            "capable_rt_malloc",
            sig(vec![AbiType::Handle, AbiType::I32], AbiType::Ptr),
        ),
        binding(
            "sys.buffer",
            "Alloc__free",
            "capable_rt_free",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Unit),
        ),
        binding(
            "sys.buffer",
            "Alloc__cast_u8_to_u32",
            "capable_rt_cast_u8_to_u32",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Ptr),
        ),
        binding(
            "sys.buffer",
            "Alloc__cast_u32_to_u8",
            "capable_rt_cast_u32_to_u8",
            sig(vec![AbiType::Handle, AbiType::Ptr], AbiType::Ptr),
        ),
        binding(
            "sys.bytes",
            "u8__is_whitespace",
            "capable_rt_bytes_is_whitespace",
            sig(vec![AbiType::U8], AbiType::Bool),
        ),
    ]
}

fn runtime_binding_index() -> &'static HashMap<String, RuntimeBinding> {
    static INDEX: OnceLock<HashMap<String, RuntimeBinding>> = OnceLock::new();
    INDEX.get_or_init(|| {
        runtime_binding_list()
            .into_iter()
            .map(|binding| (format!("{}.{}", binding.module, binding.func), binding))
            .collect()
    })
}

pub(crate) fn runtime_bindings() -> &'static HashMap<String, RuntimeBinding> {
    runtime_binding_index()
}

pub(crate) fn runtime_binding(module: &str, func: &str) -> Option<&'static RuntimeBinding> {
    runtime_binding_index().get(&format!("{module}.{func}"))
}

pub(crate) fn is_runtime_intrinsic(module: &str, func: &str) -> bool {
    runtime_binding(module, func).is_some()
}
