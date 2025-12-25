use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }
}

pub type Ident = Spanned<String>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PackageSafety {
    Safe,
    Unsafe,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub package: PackageSafety,
    pub name: Path,
    pub uses: Vec<UseDecl>,
    pub items: Vec<Item>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDecl {
    pub path: Path,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Function(Function),
    ExternFunction(ExternFunction),
    Struct(StructDecl),
    Enum(EnumDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Block,
    pub is_pub: bool,
    pub doc: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternFunction {
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret: Type,
    pub is_pub: bool,
    pub doc: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDecl {
    pub name: Ident,
    pub fields: Vec<Field>,
    pub is_pub: bool,
    pub is_opaque: bool,
    pub doc: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDecl {
    pub name: Ident,
    pub variants: Vec<EnumVariant>,
    pub is_pub: bool,
    pub doc: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: Ident,
    pub payload: Option<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Let(LetStmt),
    Assign(AssignStmt),
    Return(ReturnStmt),
    If(IfStmt),
    While(WhileStmt),
    Expr(ExprStmt),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStmt {
    pub name: Ident,
    pub ty: Option<Type>,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnStmt {
    pub expr: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStmt {
    pub cond: Expr,
    pub then_block: Block,
    pub else_block: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignStmt {
    pub name: Ident,
    pub expr: Expr,
    pub span: Span,
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Let(s) => s.span,
            Stmt::Assign(s) => s.span,
            Stmt::Return(s) => s.span,
            Stmt::If(s) => s.span,
            Stmt::While(s) => s.span,
            Stmt::Expr(s) => s.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Literal(LiteralExpr),
    Path(Path),
    Call(CallExpr),
    MethodCall(MethodCallExpr),
    FieldAccess(FieldAccessExpr),
    StructLiteral(StructLiteralExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Match(MatchExpr),
    Grouping(GroupingExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructLiteralExpr {
    pub path: Path,
    pub fields: Vec<StructLiteralField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructLiteralField {
    pub name: Ident,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LiteralExpr {
    pub value: Literal,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    U8(u8),
    String(String),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<Ident>,
    pub span: Span,
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for seg in &self.segments {
            if !first {
                write!(f, ".")?;
            }
            first = false;
            write!(f, "{}", seg.item)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccessExpr {
    pub object: Box<Expr>,
    pub field: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodCallExpr {
    pub receiver: Box<Expr>,
    pub method: Ident,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchExpr {
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Wildcard(Span),
    Path(Path),
    Binding(Ident),
    Literal(Literal),
    Call {
        path: Path,
        binding: Option<Ident>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Path { path: Path, args: Vec<Type>, span: Span },
    Ptr { target: Box<Type>, span: Span },
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Path { span, .. } => *span,
            Type::Ptr { span, .. } => *span,
        }
    }
}

impl Expr {
    /// Converts an expression to a Path if possible.
    /// This handles converting FieldAccess chains and single Paths.
    /// Used for resolving module-qualified names and enum variants.
    pub fn to_path(&self) -> Option<Path> {
        match self {
            Expr::Path(path) => Some(path.clone()),
            Expr::FieldAccess(field_access) => {
                // Recursively convert the object to a path, then append the field
                let mut base_path = field_access.object.to_path()?;
                base_path.segments.push(field_access.field.clone());
                base_path.span = Span::new(base_path.span.start, field_access.field.span.end);
                Some(base_path)
            }
            _ => None,
        }
    }
}
