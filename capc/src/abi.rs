//! ABI-level type shapes shared between type checking and codegen.

/// ABI shape for a type used in codegen.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AbiType {
    Unit,
    I32,
    U32,
    U8,
    Bool,
    Handle,
    Ptr,
    Result(Box<AbiType>, Box<AbiType>),
    /// ABI-only return lowering for `Result<T, E>` where out params are used.
    ResultOut(Box<AbiType>, Box<AbiType>),
}
