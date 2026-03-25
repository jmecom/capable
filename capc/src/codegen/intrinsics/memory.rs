use std::collections::HashMap;

use crate::abi::AbiType;

use super::{runtime_fn, FnInfo, FnSig};

pub(super) fn register_memory_intrinsics(map: &mut HashMap<String, FnInfo>) {
    map.insert(
        "sys.system.RootCap__mint_alloc_default".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_alloc_default",
        ),
    );
    map.insert(
        "sys.buffer.default_alloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![],
                ret: AbiType::Handle,
            },
            None,
            "capable_rt_default_alloc",
        ),
    );
    map.insert(
        "sys.buffer.Alloc__malloc".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::I32],
                ret: AbiType::Ptr,
            },
            None,
            "capable_rt_malloc",
        ),
    );
    map.insert(
        "sys.buffer.Alloc__free".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Unit,
            },
            None,
            "capable_rt_free",
        ),
    );
    map.insert(
        "sys.buffer.Alloc__cast_u8_to_u32".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Ptr,
            },
            None,
            "capable_rt_cast_u8_to_u32",
        ),
    );
    map.insert(
        "sys.buffer.Alloc__cast_u32_to_u8".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::Handle, AbiType::Ptr],
                ret: AbiType::Ptr,
            },
            None,
            "capable_rt_cast_u32_to_u8",
        ),
    );
    map.insert(
        "sys.bytes.u8__is_whitespace".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::U8],
                ret: AbiType::Bool,
            },
            None,
            "capable_rt_bytes_is_whitespace",
        ),
    );

    map.insert(
        "sys.math.add_wrap_i32".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::I32, AbiType::I32],
                ret: AbiType::I32,
            },
            None,
            "capable_rt_math_add_wrap_i32",
        ),
    );
    map.insert(
        "sys.math.sub_wrap_i32".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::I32, AbiType::I32],
                ret: AbiType::I32,
            },
            None,
            "capable_rt_math_sub_wrap_i32",
        ),
    );
    map.insert(
        "sys.math.mul_wrap_i32".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::I32, AbiType::I32],
                ret: AbiType::I32,
            },
            None,
            "capable_rt_math_mul_wrap_i32",
        ),
    );
    map.insert(
        "sys.math.add_wrap_u32".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::U32, AbiType::U32],
                ret: AbiType::U32,
            },
            None,
            "capable_rt_math_add_wrap_u32",
        ),
    );
    map.insert(
        "sys.math.sub_wrap_u32".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::U32, AbiType::U32],
                ret: AbiType::U32,
            },
            None,
            "capable_rt_math_sub_wrap_u32",
        ),
    );
    map.insert(
        "sys.math.mul_wrap_u32".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::U32, AbiType::U32],
                ret: AbiType::U32,
            },
            None,
            "capable_rt_math_mul_wrap_u32",
        ),
    );
    map.insert(
        "sys.math.add_wrap_u8".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::U8, AbiType::U8],
                ret: AbiType::U8,
            },
            None,
            "capable_rt_math_add_wrap_u8",
        ),
    );
    map.insert(
        "sys.math.sub_wrap_u8".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::U8, AbiType::U8],
                ret: AbiType::U8,
            },
            None,
            "capable_rt_math_sub_wrap_u8",
        ),
    );
    map.insert(
        "sys.math.mul_wrap_u8".to_string(),
        runtime_fn(
            FnSig {
                params: vec![AbiType::U8, AbiType::U8],
                ret: AbiType::U8,
            },
            None,
            "capable_rt_math_mul_wrap_u8",
        ),
    );
}
