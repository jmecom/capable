pub(crate) fn is_runtime_intrinsic(module: &str, func: &str) -> bool {
    matches!(
        (module, func),
        (
            "sys.system",
            "RootCap__mint_console"
                | "RootCap__mint_readfs"
                | "RootCap__mint_filesystem"
                | "RootCap__mint_args"
                | "RootCap__mint_stdin"
                | "RootCap__mint_net"
                | "RootCap__mint_alloc_default"
        ) | ("sys.args", "Args__len" | "Args__at")
            | ("sys.stdin", "Stdin__read_to_string_with_alloc")
            | (
                "sys.net",
                "Net__listen"
                    | "Net__connect"
                    | "TcpListener__accept"
                    | "TcpListener__close"
                    | "TcpConn__read_to_string_with_alloc"
                    | "TcpConn__read_with_alloc"
                    | "TcpConn__write"
                    | "TcpConn__close"
            )
            | ("sys.buffer", "default_alloc")
            | (
                "sys.console",
                "Console__println"
                    | "Console__print"
                    | "Console__print_i32"
                    | "Console__println_i32"
                    | "Console__assert"
            )
            | (
                "sys.math",
                "add_wrap_i32"
                    | "sub_wrap_i32"
                    | "mul_wrap_i32"
                    | "add_wrap_u32"
                    | "sub_wrap_u32"
                    | "mul_wrap_u32"
                    | "add_wrap_u8"
                    | "sub_wrap_u8"
                    | "mul_wrap_u8"
            )
            | (
                "sys.fs",
                "ReadFS__read_to_string_with_alloc"
                    | "ReadFS__read_bytes_with_alloc"
                    | "ReadFS__list_dir_with_alloc"
                    | "ReadFS__exists"
                    | "ReadFS__close"
                    | "Filesystem__root_dir"
                    | "Filesystem__close"
                    | "Dir__subdir"
                    | "Dir__open_read"
                    | "Dir__read_bytes_with_alloc"
                    | "Dir__read_to_string_with_alloc"
                    | "Dir__list_dir_with_alloc"
                    | "Dir__exists"
                    | "Dir__close"
                    | "FileRead__read_to_string_with_alloc"
                    | "FileRead__close"
                    | "join_with_alloc"
            )
            | (
                "sys.buffer",
                "Alloc__malloc"
                    | "Alloc__free"
                    | "Alloc__cast_u8_to_u32"
                    | "Alloc__cast_u32_to_u8"
            )
            | ("sys.bytes", "u8__is_whitespace")
    )
}
