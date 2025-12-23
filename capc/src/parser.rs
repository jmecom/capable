use crate::ast::*;
use crate::error::ParseError;
use crate::lexer::{lex, to_ident, Token, TokenKind};

pub fn parse_module(source: &str) -> Result<Module, ParseError> {
    let tokens = lex(source);
    let mut parser = Parser::new(tokens, source.len());
    parser.parse_module()
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    eof_span: Span,
}

impl Parser {
    fn new(tokens: Vec<Token>, source_len: usize) -> Self {
        let eof_span = Span::new(source_len, source_len);
        Self {
            tokens,
            index: 0,
            eof_span,
        }
    }

    fn parse_module(&mut self) -> Result<Module, ParseError> {
        let mut package = PackageSafety::Safe;
        let start_span = self.peek_span();
        if self.peek_kind() == Some(TokenKind::Package) {
            let pkg_span = self.bump().unwrap().span;
            let safety = match self.bump_kind()? {
                TokenKind::Safe => PackageSafety::Safe,
                TokenKind::Unsafe => PackageSafety::Unsafe,
                other => {
                    return Err(self.error_at(
                        pkg_span,
                        format!("expected `safe` or `unsafe`, found {other:?}"),
                    ));
                }
            };
            package = safety;
        }

        self.expect(TokenKind::Module)?;
        let name = self.parse_path()?;

        let mut uses = Vec::new();
        while self.peek_kind() == Some(TokenKind::Use) {
            uses.push(self.parse_use()?);
        }

        let mut items = Vec::new();
        while self.peek_kind().is_some() {
            items.push(self.parse_item()?);
        }

        let end_span = if let Some(token) = self.tokens.last() {
            token.span
        } else {
            self.eof_span
        };

        Ok(Module {
            package,
            name,
            uses,
            items,
            span: Span::new(start_span.start, end_span.end),
        })
    }

    fn parse_use(&mut self) -> Result<UseDecl, ParseError> {
        let start = self.expect(TokenKind::Use)?.span.start;
        let path = self.parse_path()?;
        let span = Span::new(start, path.span.end);
        self.maybe_consume(TokenKind::Semi);
        Ok(UseDecl { path, span })
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        let is_pub = self.maybe_consume(TokenKind::Pub).is_some();
        let is_opaque = self.maybe_consume(TokenKind::Opaque).is_some();
        if self.peek_kind() == Some(TokenKind::Extern) {
            if is_opaque {
                return Err(self.error_current(
                    "opaque applies only to struct declarations".to_string(),
                ));
            }
            return Ok(Item::ExternFunction(self.parse_extern_function(is_pub)?));
        }
        match self.peek_kind() {
            Some(TokenKind::Fn) => {
                if is_opaque {
                    return Err(self.error_current(
                        "opaque applies only to struct declarations".to_string(),
                    ));
                }
                Ok(Item::Function(self.parse_function(is_pub)?))
            }
            Some(TokenKind::Struct) => Ok(Item::Struct(self.parse_struct(is_pub, is_opaque)?)),
            Some(TokenKind::Enum) => {
                if is_opaque {
                    return Err(self.error_current(
                        "opaque applies only to struct declarations".to_string(),
                    ));
                }
                Ok(Item::Enum(self.parse_enum(is_pub)?))
            }
            Some(other) => Err(self.error_current(format!(
                "expected item, found {other:?}"
            ))),
            None => Err(self.error_current("unexpected end of input".to_string())),
        }
    }

    fn parse_extern_function(&mut self, is_pub: bool) -> Result<ExternFunction, ParseError> {
        let start = self.expect(TokenKind::Extern)?.span.start;
        self.expect(TokenKind::Fn)?;
        let name = self.expect_ident()?;
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        if self.peek_kind() != Some(TokenKind::RParen) {
            loop {
                let param_name = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let ty = self.parse_type()?;
                params.push(Param {
                    name: param_name,
                    ty,
                });
                if self.maybe_consume(TokenKind::Comma).is_some() {
                    continue;
                }
                break;
            }
        }
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(ret.span().end, |t| t.span.end);
        Ok(ExternFunction {
            name,
            params,
            ret,
            is_pub,
            span: Span::new(start, end),
        })
    }

    fn parse_function(&mut self, is_pub: bool) -> Result<Function, ParseError> {
        let start = self.expect(TokenKind::Fn)?.span.start;
        let name = self.expect_ident()?;
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        if self.peek_kind() != Some(TokenKind::RParen) {
            loop {
                let param_name = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let ty = self.parse_type()?;
                params.push(Param {
                    name: param_name,
                    ty,
                });
                if self.maybe_consume(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        let body = self.parse_block()?;
        let span = Span::new(start, body.span.end);
        Ok(Function {
            name,
            params,
            ret,
            body,
            is_pub,
            span,
        })
    }

    fn parse_struct(&mut self, is_pub: bool, is_opaque: bool) -> Result<StructDecl, ParseError> {
        let start = self.expect(TokenKind::Struct)?.span.start;
        let name = self.expect_ident()?;
        let mut fields = Vec::new();
        let end = if self.peek_kind() == Some(TokenKind::LBrace) {
            self.bump();
            if self.peek_kind() != Some(TokenKind::RBrace) {
                loop {
                    let field_name = self.expect_ident()?;
                    self.expect(TokenKind::Colon)?;
                    let ty = self.parse_type()?;
                    fields.push(Field {
                        name: field_name,
                        ty,
                    });
                    if self.maybe_consume(TokenKind::Comma).is_none() {
                        break;
                    }
                }
            }
            let end = self.expect(TokenKind::RBrace)?.span.end;
            if is_opaque && !fields.is_empty() {
                return Err(self.error_at(
                    Span::new(start, end),
                    "opaque struct cannot declare fields".to_string(),
                ));
            }
            end
        } else {
            name.span.end
        };
        Ok(StructDecl {
            name,
            fields,
            is_pub,
            is_opaque,
            span: Span::new(start, end),
        })
    }

    fn parse_enum(&mut self, is_pub: bool) -> Result<EnumDecl, ParseError> {
        let start = self.expect(TokenKind::Enum)?.span.start;
        let name = self.expect_ident()?;
        self.expect(TokenKind::LBrace)?;
        let mut variants = Vec::new();
        if self.peek_kind() != Some(TokenKind::RBrace) {
            loop {
                let variant_name = self.expect_ident()?;
                let variant_start = variant_name.span.start;
                let payload = if self.peek_kind() == Some(TokenKind::LParen) {
                    self.bump();
                    let ty = self.parse_type()?;
                    self.expect(TokenKind::RParen)?;
                    Some(ty)
                } else {
                    None
                };
                let end = payload
                    .as_ref()
                    .map_or(variant_name.span.end, |ty| ty.span().end);
                variants.push(EnumVariant {
                    name: variant_name,
                    payload,
                    span: Span::new(variant_start, end),
                });
                if self.maybe_consume(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(EnumDecl {
            name,
            variants,
            is_pub,
            span: Span::new(start, end),
        })
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start = self.expect(TokenKind::LBrace)?.span.start;
        let mut stmts = Vec::new();
        while self.peek_kind() != Some(TokenKind::RBrace) {
            if self.peek_kind().is_none() {
                return Err(self.error_current("unexpected end of input in block".to_string()));
            }
            stmts.push(self.parse_stmt()?);
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Block {
            stmts,
            span: Span::new(start, end),
        })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Let) => Ok(Stmt::Let(self.parse_let()?)),
            Some(TokenKind::Return) => Ok(Stmt::Return(self.parse_return()?)),
            Some(TokenKind::If) => Ok(Stmt::If(self.parse_if()?)),
            Some(TokenKind::While) => Ok(Stmt::While(self.parse_while()?)),
            _ => Ok(Stmt::Expr(self.parse_expr_stmt()?)),
        }
    }

    fn parse_let(&mut self) -> Result<LetStmt, ParseError> {
        let start = self.expect(TokenKind::Let)?.span.start;
        let name = self.expect_ident()?;
        let ty = if self.maybe_consume(TokenKind::Colon).is_some() {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr()?;
        let end = self.maybe_consume(TokenKind::Semi).map_or(expr.span().end, |t| t.span.end);
        Ok(LetStmt {
            name,
            ty,
            expr,
            span: Span::new(start, end),
        })
    }

    fn parse_return(&mut self) -> Result<ReturnStmt, ParseError> {
        let start = self.expect(TokenKind::Return)?.span.start;
        let expr = match self.peek_kind() {
            Some(TokenKind::Semi) | Some(TokenKind::RBrace) => None,
            Some(_) => Some(self.parse_expr()?),
            None => None,
        };
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(self.peek_span().end, |t| t.span.end);
        Ok(ReturnStmt {
            expr,
            span: Span::new(start, end),
        })
    }

    fn parse_if(&mut self) -> Result<IfStmt, ParseError> {
        let start = self.expect(TokenKind::If)?.span.start;
        let cond = self.parse_expr()?;
        let then_block = self.parse_block()?;
        let else_block = if self.peek_kind() == Some(TokenKind::Else) {
            self.bump();
            Some(self.parse_block()?)
        } else {
            None
        };
        let end = else_block
            .as_ref()
            .map_or(then_block.span.end, |b| b.span.end);
        Ok(IfStmt {
            cond,
            then_block,
            else_block,
            span: Span::new(start, end),
        })
    }

    fn parse_while(&mut self) -> Result<WhileStmt, ParseError> {
        let start = self.expect(TokenKind::While)?.span.start;
        let cond = self.parse_expr()?;
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(WhileStmt {
            cond,
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_expr_stmt(&mut self) -> Result<ExprStmt, ParseError> {
        let expr = self.parse_expr()?;
        let expr_span = expr.span();
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(expr_span.end, |t| t.span.end);
        Ok(ExprStmt {
            expr,
            span: Span::new(expr_span.start, end),
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_prefix()?;

        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::OrOr) => BinaryOp::Or,
                Some(TokenKind::AndAnd) => BinaryOp::And,
                Some(TokenKind::EqEq) => BinaryOp::Eq,
                Some(TokenKind::NotEq) => BinaryOp::Neq,
                Some(TokenKind::Lt) => BinaryOp::Lt,
                Some(TokenKind::Lte) => BinaryOp::Lte,
                Some(TokenKind::Gt) => BinaryOp::Gt,
                Some(TokenKind::Gte) => BinaryOp::Gte,
                Some(TokenKind::Plus) => BinaryOp::Add,
                Some(TokenKind::Minus) => BinaryOp::Sub,
                Some(TokenKind::Star) => BinaryOp::Mul,
                Some(TokenKind::Slash) => BinaryOp::Div,
                Some(TokenKind::LParen) => {
                    lhs = self.finish_call(lhs)?;
                    continue;
                }
                _ => break,
            };

            let (l_bp, r_bp) = infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            self.bump();
            let rhs = self.parse_expr_bp(r_bp)?;
            let span = Span::new(lhs.span().start, rhs.span().end);
            lhs = Expr::Binary(BinaryExpr {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                span,
            });
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Minus) => {
                let start = self.bump().unwrap().span.start;
                let expr = self.parse_expr_bp(7)?;
                Ok(Expr::Unary(UnaryExpr {
                    op: UnaryOp::Neg,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                }))
            }
            Some(TokenKind::Bang) => {
                let start = self.bump().unwrap().span.start;
                let expr = self.parse_expr_bp(7)?;
                Ok(Expr::Unary(UnaryExpr {
                    op: UnaryOp::Not,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                }))
            }
            Some(TokenKind::Match) => self.parse_match(),
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Int) => {
                let token = self.bump().unwrap();
                let value = token.text.parse::<i64>().map_err(|_| {
                    self.error_at(token.span, "invalid integer literal".to_string())
                })?;
                if let Some(next) = self.peek_token(0) {
                    if next.kind == TokenKind::Ident
                        && next.text == "u8"
                        && next.span.start == token.span.end
                    {
                        let suffix = self.bump().unwrap();
                        if !(0..=255).contains(&value) {
                            return Err(self.error_at(
                                Span::new(token.span.start, suffix.span.end),
                                "u8 literal out of range".to_string(),
                            ));
                        }
                        return Ok(Expr::Literal(LiteralExpr {
                            value: Literal::U8(value as u8),
                            span: Span::new(token.span.start, suffix.span.end),
                        }));
                    }
                }
                Ok(Expr::Literal(LiteralExpr {
                    value: Literal::Int(value),
                    span: token.span,
                }))
            }
            Some(TokenKind::Str) => {
                let token = self.bump().unwrap();
                let value = unescape_string(&token.text).map_err(|message| {
                    self.error_at(token.span, format!("invalid string literal: {message}"))
                })?;
                Ok(Expr::Literal(LiteralExpr {
                    value: Literal::String(value),
                    span: token.span,
                }))
            }
            Some(TokenKind::True) => {
                let token = self.bump().unwrap();
                Ok(Expr::Literal(LiteralExpr {
                    value: Literal::Bool(true),
                    span: token.span,
                }))
            }
            Some(TokenKind::False) => {
                let token = self.bump().unwrap();
                Ok(Expr::Literal(LiteralExpr {
                    value: Literal::Bool(false),
                    span: token.span,
                }))
            }
            Some(TokenKind::LParen) => {
                let start = self.bump().unwrap().span.start;
                if self.peek_kind() == Some(TokenKind::RParen) {
                    let end = self.bump().unwrap().span.end;
                    Ok(Expr::Literal(LiteralExpr {
                        value: Literal::Unit,
                        span: Span::new(start, end),
                    }))
                } else {
                    let expr = self.parse_expr()?;
                    let end = self.expect(TokenKind::RParen)?.span.end;
                    Ok(Expr::Grouping(GroupingExpr {
                        expr: Box::new(expr),
                        span: Span::new(start, end),
                    }))
                }
            }
            Some(TokenKind::Ident) => {
                let path = self.parse_path()?;
                if self.peek_kind() == Some(TokenKind::LBrace) {
                    self.parse_struct_literal(path)
                } else {
                    Ok(Expr::Path(path))
                }
            }
            Some(other) => Err(self.error_current(format!(
                "unexpected token in expression: {other:?}"
            ))),
            None => Err(self.error_current("unexpected end of input".to_string())),
        }
    }

    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(TokenKind::Match)?.span.start;
        let expr = self.parse_expr()?;
        self.expect(TokenKind::LBrace)?;
        let mut arms = Vec::new();
        while self.peek_kind() != Some(TokenKind::RBrace) {
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::FatArrow)?;
            let body = self.parse_block()?;
            let end = body.span.end;
            arms.push(MatchArm {
                pattern,
                body,
                span: Span::new(start, end),
            });
            if self.maybe_consume(TokenKind::Comma).is_some() {
                continue;
            }
            if self.peek_kind() == Some(TokenKind::RBrace) {
                break;
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Expr::Match(MatchExpr {
            expr: Box::new(expr),
            arms,
            span: Span::new(start, end),
        }))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Int) => {
                let token = self.bump().unwrap();
                let value = token.text.parse::<i64>().map_err(|_| {
                    self.error_at(token.span, "invalid integer literal".to_string())
                })?;
                Ok(Pattern::Literal(Literal::Int(value)))
            }
            Some(TokenKind::True) => {
                self.bump();
                Ok(Pattern::Literal(Literal::Bool(true)))
            }
            Some(TokenKind::False) => {
                self.bump();
                Ok(Pattern::Literal(Literal::Bool(false)))
            }
            Some(TokenKind::Ident) => {
                let path = self.parse_path()?;
                if self.peek_kind() == Some(TokenKind::LParen) {
                    let start = path.span.start;
                    self.bump();
                    let binding = if self.peek_kind() == Some(TokenKind::Ident) {
                        Some(self.expect_ident()?)
                    } else {
                        None
                    };
                    let end = self.expect(TokenKind::RParen)?.span.end;
                    Ok(Pattern::Call {
                        path,
                        binding,
                        span: Span::new(start, end),
                    })
                } else {
                    Ok(Pattern::Path(path))
                }
            }
            Some(TokenKind::Underscore) => {
                let span = self.bump().unwrap().span;
                Ok(Pattern::Wildcard(span))
            }
            _ => Err(self.error_current("unexpected token in pattern".to_string())),
        }
    }

    fn parse_path(&mut self) -> Result<Path, ParseError> {
        let first = self.expect_ident()?;
        let start = first.span.start;
        let mut segments = vec![first];
        while self.peek_kind() == Some(TokenKind::Dot) {
            self.bump();
            segments.push(self.expect_ident()?);
        }
        let end = segments.last().map(|s| s.span.end).unwrap_or(start);
        Ok(Path {
            segments,
            span: Span::new(start, end),
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        if self.peek_kind() == Some(TokenKind::Star) {
            let start = self.bump().unwrap().span.start;
            let target = self.parse_type()?;
            let span = Span::new(start, target.span().end);
            return Ok(Type::Ptr {
                target: Box::new(target),
                span,
            });
        }

        let path = self.parse_path()?;
        let mut args = Vec::new();
        let mut end = path.span.end;
        if self.peek_kind() == Some(TokenKind::LBracket) {
            self.bump();
            if self.peek_kind() != Some(TokenKind::RBracket) {
                loop {
                    args.push(self.parse_type()?);
                    if self.maybe_consume(TokenKind::Comma).is_none() {
                        break;
                    }
                }
            }
            end = self.expect(TokenKind::RBracket)?.span.end;
        }
        let span = Span::new(path.span.start, end);
        Ok(Type::Path { path, args, span })
    }

    fn parse_struct_literal(&mut self, path: Path) -> Result<Expr, ParseError> {
        let start = path.span.start;
        self.expect(TokenKind::LBrace)?;
        let mut fields = Vec::new();
        if self.peek_kind() != Some(TokenKind::RBrace) {
            loop {
                let name = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let expr = self.parse_expr()?;
                let end = expr.span().end;
                fields.push(StructLiteralField {
                    name,
                    expr,
                    span: Span::new(start, end),
                });
                if self.maybe_consume(TokenKind::Comma).is_some() {
                    if self.peek_kind() == Some(TokenKind::RBrace) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Expr::StructLiteral(StructLiteralExpr {
            path,
            fields,
            span: Span::new(start, end),
        }))
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let start = callee.span().start;
        self.expect(TokenKind::LParen)?;
        let mut args = Vec::new();
        if self.peek_kind() != Some(TokenKind::RParen) {
            loop {
                args.push(self.parse_expr()?);
                if self.maybe_consume(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        let end = self.expect(TokenKind::RParen)?.span.end;
        Ok(Expr::Call(CallExpr {
            callee: Box::new(callee),
            args,
            span: Span::new(start, end),
        }))
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        match self.peek_kind() {
            Some(k) if k == kind => Ok(self.bump().unwrap()),
            Some(other) => Err(self.error_current(format!(
                "expected {kind:?}, found {other:?}"
            ))),
            None => Err(self.error_current("unexpected end of input".to_string())),
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Ident) => Ok(to_ident(&self.bump().unwrap())),
            Some(other) => Err(self.error_current(format!(
                "expected identifier, found {other:?}"
            ))),
            None => Err(self.error_current("unexpected end of input".to_string())),
        }
    }

    fn bump(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.index).cloned();
        if token.is_some() {
            self.index += 1;
        }
        token
    }

    fn bump_kind(&mut self) -> Result<TokenKind, ParseError> {
        self.bump()
            .map(|t| t.kind)
            .ok_or_else(|| self.error_current("unexpected end of input".to_string()))
    }

    fn maybe_consume(&mut self, kind: TokenKind) -> Option<Token> {
        if self.peek_kind() == Some(kind) {
            self.bump()
        } else {
            None
        }
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.tokens.get(self.index).map(|t| t.kind.clone())
    }

    fn peek_span(&self) -> Span {
        self.tokens
            .get(self.index)
            .map(|t| t.span)
            .unwrap_or(self.eof_span)
    }

    fn error_current(&self, message: String) -> ParseError {
        let span = self.peek_span();
        ParseError::new(message, span)
    }

    fn error_at(&self, span: Span, message: String) -> ParseError {
        ParseError::new(message, span)
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.index + offset)
    }
}

fn infix_binding_power(op: &BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Or => (1, 2),
        BinaryOp::And => (3, 4),
        BinaryOp::Eq | BinaryOp::Neq => (5, 6),
        BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => (7, 8),
        BinaryOp::Add | BinaryOp::Sub => (9, 10),
        BinaryOp::Mul | BinaryOp::Div => (11, 12),
    }
}

fn unescape_string(text: &str) -> Result<String, String> {
    let mut chars = text.chars();
    if chars.next() != Some('"') || text.len() < 2 {
        return Err("missing quotes".to_string());
    }
    let mut out = String::new();
    let mut escape = false;
    for ch in chars {
        if escape {
            let escaped = match ch {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                other => {
                    return Err(format!("unsupported escape \\{other}"));
                }
            };
            out.push(escaped);
            escape = false;
        } else if ch == '\\' {
            escape = true;
        } else if ch == '"' {
            break;
        } else {
            out.push(ch);
        }
    }
    Ok(out)
}

trait SpanExt {
    fn span(&self) -> Span;
}

impl SpanExt for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Literal(lit) => lit.span,
            Expr::Path(path) => path.span,
            Expr::Call(call) => call.span,
            Expr::StructLiteral(lit) => lit.span,
            Expr::Unary(unary) => unary.span,
            Expr::Binary(binary) => binary.span,
            Expr::Match(m) => m.span,
            Expr::Grouping(g) => g.span,
        }
    }
}
