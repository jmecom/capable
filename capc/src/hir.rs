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

use crate::abi::AbiType;
use crate::ast::{BinaryOp, Literal, Span, UnaryOp};
use crate::typeck::Ty;

/// Unique identifier for a local variable within a function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub usize);

/// HIR type pairing the resolved type with its ABI shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirType {
    pub ty: Ty,
    pub abi: AbiType,
}

/// A fully resolved and type-checked module
#[derive(Debug, Clone)]
pub struct HirModule {
    pub name: String,
    pub functions: Vec<HirFunction>,
    pub extern_functions: Vec<HirExternFunction>,
    pub structs: Vec<HirStruct>,
    pub enums: Vec<HirEnum>,
}

/// A complete program with entry module and dependencies
#[derive(Debug, Clone)]
pub struct HirProgram {
    pub entry: HirModule,
    pub user_modules: Vec<HirModule>,
    pub stdlib: Vec<HirModule>,
}

/// A function in HIR
#[derive(Debug, Clone)]
pub struct HirFunction {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<HirParam>,
    pub ret_ty: HirType,
    pub body: HirBlock,
}

#[derive(Debug, Clone)]
pub struct HirExternFunction {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<HirParam>,
    pub ret_ty: HirType,
}

#[derive(Debug, Clone)]
pub struct HirParam {
    pub local_id: LocalId,
    pub ty: HirType,
}

/// A struct declaration in HIR
#[derive(Debug, Clone)]
pub struct HirStruct {
    pub name: String,
    pub type_params: Vec<String>,
    pub fields: Vec<HirField>,
    pub is_opaque: bool,
}

#[derive(Debug, Clone)]
pub struct HirField {
    pub name: String,
    pub ty: HirType,
}

/// An enum declaration in HIR
#[derive(Debug, Clone)]
pub struct HirEnum {
    pub name: String,
    pub type_params: Vec<String>,
    pub variants: Vec<HirEnumVariant>,
}

#[derive(Debug, Clone)]
pub struct HirEnumVariant {
    pub name: String,
    pub payload: Option<HirType>,
}

/// A block of statements
#[derive(Debug, Clone)]
pub struct HirBlock {
    pub stmts: Vec<HirStmt>,
}

/// A statement in HIR
#[derive(Debug, Clone)]
pub enum HirStmt {
    Let(HirLetStmt),
    Assign(HirAssignStmt),
    Return(HirReturnStmt),
    Break(HirBreakStmt),
    Continue(HirContinueStmt),
    If(HirIfStmt),
    While(HirWhileStmt),
    For(HirForStmt),
    Expr(HirExprStmt),
}

impl HirStmt {
    pub fn span(&self) -> Span {
        match self {
            HirStmt::Let(s) => s.span,
            HirStmt::Assign(s) => s.span,
            HirStmt::Return(s) => s.span,
            HirStmt::Break(s) => s.span,
            HirStmt::Continue(s) => s.span,
            HirStmt::If(s) => s.span,
            HirStmt::While(s) => s.span,
            HirStmt::For(s) => s.span,
            HirStmt::Expr(s) => s.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirLetStmt {
    pub local_id: LocalId,
    pub ty: HirType,
    pub expr: HirExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirAssignStmt {
    pub local_id: LocalId,
    pub expr: HirExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirReturnStmt {
    pub expr: Option<HirExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirBreakStmt {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirContinueStmt {
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
pub struct HirForStmt {
    pub var_id: LocalId,
    pub start: HirExpr,
    pub end: HirExpr,
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
    Try(HirTry),
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
            HirExpr::Try(e) => e.span,
        }
    }

    pub fn ty(&self) -> &HirType {
        match self {
            HirExpr::Literal(e) => &e.ty,
            HirExpr::Local(e) => &e.ty,
            HirExpr::EnumVariant(e) => &e.enum_ty,
            HirExpr::Call(e) => &e.ret_ty,
            HirExpr::FieldAccess(e) => &e.field_ty,
            HirExpr::StructLiteral(e) => &e.struct_ty,
            HirExpr::Unary(e) => &e.ty,
            HirExpr::Binary(e) => &e.ty,
            HirExpr::Match(e) => &e.result_ty,
            HirExpr::Try(e) => &e.ok_ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirLiteral {
    pub value: Literal,
    pub ty: HirType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirLocal {
    pub local_id: LocalId,
    pub ty: HirType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirEnumVariantExpr {
    /// The enum type this variant belongs to (e.g., "sys.fs.TraversalKind")
    pub enum_ty: HirType,
    /// The variant name (e.g., "File")
    pub variant_name: String,
    /// Optional payload for variant constructors like Ok(value) or Err(err)
    pub payload: Option<Box<HirExpr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirCall {
    pub callee: ResolvedCallee,
    pub type_args: Vec<Ty>,
    pub args: Vec<HirExpr>,
    pub ret_ty: HirType,
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
    Drop,
}

#[derive(Debug, Clone)]
pub struct HirFieldAccess {
    pub object: Box<HirExpr>,
    pub field_name: String,
    pub field_ty: HirType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirStructLiteral {
    pub struct_ty: HirType,
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
    pub ty: HirType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirBinary {
    pub op: BinaryOp,
    pub left: Box<HirExpr>,
    pub right: Box<HirExpr>,
    pub ty: HirType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirMatch {
    pub expr: Box<HirExpr>,
    pub arms: Vec<HirMatchArm>,
    pub result_ty: HirType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirTry {
    pub expr: Box<HirExpr>,
    pub ok_ty: HirType,
    pub ret_ty: HirType,
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
        variant_name: String,
        binding: Option<LocalId>,
    },
    /// Binding introduces a new local variable
    Binding(LocalId),
    Literal(Literal),
}
