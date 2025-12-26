// High-level Intermediate Representation (HIR)
//
// HIR is a fully type-checked and resolved version of the AST. It is produced
// by the typechecker through the lowering process and consumed by codegen.
//
// Key differences from AST:
// - All calls have resolved symbols (no path resolution needed)
// - Method calls are desugared to regular calls with receiver as first arg
// - Local variables use LocalId instead of strings
// - All types are fully resolved

use crate::ast::{BinaryOp, Literal, Span, UnaryOp};
use crate::typeck::Ty;

/// Unique identifier for a local variable within a function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub usize);

/// A fully resolved and type-checked module
#[derive(Debug, Clone)]
pub struct HirModule {
    pub name: String,
    pub functions: Vec<HirFunction>,
    pub structs: Vec<HirStruct>,
    pub enums: Vec<HirEnum>,
}

/// A function in HIR
#[derive(Debug, Clone)]
pub struct HirFunction {
    pub name: String,
    pub params: Vec<HirParam>,
    pub ret_ty: Ty,
    pub body: HirBlock,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirParam {
    pub local_id: LocalId,
    pub name: String,
    pub ty: Ty,
}

/// A struct declaration in HIR
#[derive(Debug, Clone)]
pub struct HirStruct {
    pub name: String,
    pub fields: Vec<HirField>,
    pub is_pub: bool,
    pub is_opaque: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirField {
    pub name: String,
    pub ty: Ty,
}

/// An enum declaration in HIR
#[derive(Debug, Clone)]
pub struct HirEnum {
    pub name: String,
    pub variants: Vec<HirEnumVariant>,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirEnumVariant {
    pub name: String,
    pub payload: Option<Ty>,
}

/// A block of statements
#[derive(Debug, Clone)]
pub struct HirBlock {
    pub stmts: Vec<HirStmt>,
    pub span: Span,
}

/// A statement in HIR
#[derive(Debug, Clone)]
pub enum HirStmt {
    Let(HirLetStmt),
    Assign(HirAssignStmt),
    Return(HirReturnStmt),
    If(HirIfStmt),
    While(HirWhileStmt),
    Expr(HirExprStmt),
}

#[derive(Debug, Clone)]
pub struct HirLetStmt {
    pub local_id: LocalId,
    pub name: String,
    pub ty: Ty,
    pub expr: HirExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirAssignStmt {
    pub local_id: LocalId,
    pub name: String,
    pub expr: HirExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirReturnStmt {
    pub expr: Option<HirExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirIfStmt {
    pub cond: HirExpr,
    pub then_block: HirBlock,
    pub else_block: Option<HirBlock>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirWhileStmt {
    pub cond: HirExpr,
    pub body: HirBlock,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirExprStmt {
    pub expr: HirExpr,
    pub span: Span,
}

/// An expression in HIR
#[derive(Debug, Clone)]
pub enum HirExpr {
    Literal(HirLiteral),
    Local(HirLocal),
    EnumVariant(HirEnumVariantExpr),
    Call(HirCall),
    FieldAccess(HirFieldAccess),
    StructLiteral(HirStructLiteral),
    Unary(HirUnary),
    Binary(HirBinary),
    Match(HirMatch),
}

impl HirExpr {
    pub fn span(&self) -> Span {
        match self {
            HirExpr::Literal(e) => e.span,
            HirExpr::Local(e) => e.span,
            HirExpr::EnumVariant(e) => e.span,
            HirExpr::Call(e) => e.span,
            HirExpr::FieldAccess(e) => e.span,
            HirExpr::StructLiteral(e) => e.span,
            HirExpr::Unary(e) => e.span,
            HirExpr::Binary(e) => e.span,
            HirExpr::Match(e) => e.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirLiteral {
    pub value: Literal,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirLocal {
    pub local_id: LocalId,
    pub name: String,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirEnumVariantExpr {
    /// The enum type this variant belongs to (e.g., "sys.fs.TraversalKind")
    pub enum_ty: Ty,
    /// The variant name (e.g., "File")
    pub variant_name: String,
    /// TODO: Add payload support when needed
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirCall {
    pub callee: ResolvedCallee,
    pub args: Vec<HirExpr>,
    pub ret_ty: Ty,
    pub span: Span,
}

/// A fully resolved function callee
#[derive(Debug, Clone)]
pub enum ResolvedCallee {
    /// A user-defined or stdlib function
    Function {
        /// Module path (e.g., "sys.string")
        module: String,
        /// Function name (e.g., "len")
        name: String,
        /// Full symbol for linking (e.g., "capable_sys_string_len")
        symbol: String,
    },
    /// A runtime intrinsic
    Intrinsic(IntrinsicId),
}

/// Runtime intrinsic functions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicId {
    // Add runtime intrinsics as needed
}

#[derive(Debug, Clone)]
pub struct HirFieldAccess {
    pub object: Box<HirExpr>,
    pub object_ty: Ty,
    pub field_name: String,
    pub field_ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirStructLiteral {
    pub struct_ty: Ty,
    pub fields: Vec<HirStructLiteralField>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirStructLiteralField {
    pub name: String,
    pub expr: HirExpr,
}

#[derive(Debug, Clone)]
pub struct HirUnary {
    pub op: UnaryOp,
    pub expr: Box<HirExpr>,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirBinary {
    pub op: BinaryOp,
    pub left: Box<HirExpr>,
    pub right: Box<HirExpr>,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirMatch {
    pub expr: Box<HirExpr>,
    pub expr_ty: Ty,
    pub arms: Vec<HirMatchArm>,
    pub result_ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirMatchArm {
    pub pattern: HirPattern,
    pub body: HirBlock,
}

#[derive(Debug, Clone)]
pub enum HirPattern {
    Wildcard,
    /// Path to an enum variant (fully resolved)
    Variant {
        enum_ty: Ty,
        variant_name: String,
        binding: Option<(LocalId, String)>,
    },
    /// Binding introduces a new local variable
    Binding(LocalId, String),
    Literal(Literal),
}
