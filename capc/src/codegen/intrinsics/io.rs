use std::collections::HashMap;

use crate::abi::AbiType;

use super::{runtime_fn, FnInfo, FnSig};

pub(super) fn register_io_intrinsics(map: &mut HashMap<String, FnInfo>) {
    map.insert(
        "sys.system.RootCap__mint_console".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_mint_console",
        ),
    );
    map.insert(
        "sys.system.RootCap__mint_readfs".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_mint_readfs",
        ),
    );
    map.insert(
        "sys.system.RootCap__mint_filesystem".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_mint_filesystem",
        ),
    );
    map.insert(
        "sys.system.RootCap__mint_args".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_mint_args",
        ),
    );
    map.insert(
        "sys.system.RootCap__mint_stdin".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_mint_stdin",
        ),
    );
    map.insert(
        "sys.system.RootCap__mint_net".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_mint_net",
        ),
    );

    map.insert(
        "sys.args.Args__len".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::I32,
            },
            None,
            "capable_rt_args_len",
        ),
    );
    map.insert(
        "sys.args.Args__at".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::I32],
                ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::I32,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            }),
            "capable_rt_args_at",
        ),
    );

    map.insert(
        "sys.stdin.Stdin__read_to_string_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle],
                ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            }),
            "capable_rt_read_stdin_to_string",
        ),
    );

    map.insert(
        "sys.net.Net__listen".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr, AbiType::I32],
                ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::I32,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            }),
            "capable_rt_net_listen",
        ),
    );
    map.insert(
        "sys.net.Net__connect".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr, AbiType::I32],
                ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::I32,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            }),
            "capable_rt_net_connect",
        ),
    );
    map.insert(
        "sys.net.TcpListener__accept".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            }),
            "capable_rt_net_accept",
        ),
    );
    map.insert(
        "sys.net.TcpListener__close".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_net_listener_close",
        ),
    );
    map.insert(
        "sys.net.TcpConn__read_to_string_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle],
                ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            }),
            "capable_rt_net_read_to_string",
        ),
    );
    map.insert(
        "sys.net.TcpConn__read_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle, AbiType::I32],
                ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::I32,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            }),
            "capable_rt_net_read",
        ),
    );
    map.insert(
        "sys.net.TcpConn__write".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
            }),
            "capable_rt_net_write",
        ),
    );
    map.insert(
        "sys.net.TcpConn__close".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_net_close",
        ),
    );

    map.insert(
        "sys.console.Console__println".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_console_println",
        ),
    );
    map.insert(
        "sys.console.Console__print".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_console_print",
        ),
    );
    map.insert(
        "sys.console.Console__print_i32".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::I32],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_console_print_i32",
        ),
    );
    map.insert(
        "sys.console.Console__println_i32".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::I32],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_console_println_i32",
        ),
    );
    map.insert(
        "sys.console.Console__assert".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Bool, AbiType::Ptr],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_assert",
        ),
    );

    map.insert(
        "sys.fs.ReadFS__read_to_string_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            }),
            "capable_rt_fs_read_to_string",
        ),
    );
    map.insert(
        "sys.fs.ReadFS__read_bytes_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            }),
            "capable_rt_fs_read_bytes",
        ),
    );
    map.insert(
        "sys.fs.ReadFS__list_dir_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            }),
            "capable_rt_fs_list_dir",
        ),
    );
    map.insert(
        "sys.fs.ReadFS__exists".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Bool,
            },
            None,
            "capable_rt_fs_exists",
        ),
    );
    map.insert(
        "sys.fs.ReadFS__close".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_fs_readfs_close",
        ),
    );
    map.insert(
        "sys.fs.Filesystem__root_dir".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_fs_root_dir",
        ),
    );
    map.insert(
        "sys.fs.Filesystem__close".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_fs_filesystem_close",
        ),
    );
    map.insert(
        "sys.fs.Dir__subdir".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_fs_subdir",
        ),
    );
    map.insert(
        "sys.fs.Dir__open_read".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_fs_open_read",
        ),
    );
    map.insert(
        "sys.fs.Dir__read_bytes_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            }),
            "capable_rt_fs_dir_read_bytes",
        ),
    );
    map.insert(
        "sys.fs.Dir__read_to_string_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::Ptr,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            }),
            "capable_rt_fs_dir_read_to_string",
        ),
    );
    map.insert(
        "sys.fs.Dir__list_dir_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle],
                ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
            }),
            "capable_rt_fs_dir_list_dir",
        ),
    );
    map.insert(
        "sys.fs.Dir__exists".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Bool,
            },
            None,
            "capable_rt_fs_dir_exists",
        ),
    );
    map.insert(
        "sys.fs.Dir__close".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_fs_dir_close",
        ),
    );
    map.insert(
        "sys.fs.FileRead__read_to_string_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Handle],
                ret: AbiType::Result(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            },
            Some(FnSig {
                params: vec![
                    AbiType::Handle,
                    AbiType::Handle,
                    AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
                ],
                ret: AbiType::ResultOut(Box::new(AbiType::Ptr), Box::new(AbiType::I32)),
            }),
            "capable_rt_fs_file_read_to_string",
        ),
    );
    map.insert(
        "sys.fs.FileRead__close".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_fs_file_read_close",
        ),
    );
    map.insert(
        "sys.fs.join_with_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr, AbiType::Ptr],
                ret: AbiType::Ptr,
            },
            Some(FnSig {
                params: vec![AbiType::Ptr, AbiType::Handle, AbiType::Ptr, AbiType::Ptr],
                ret: AbiType::Unit,
            }),
            "capable_rt_fs_join",
        ),
    );
}
