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
    fn peek_kind_raw(&self) -> Option<TokenKind> {
        self.tokens.get(self.index).map(|t| t.kind.clone())
    }

    fn peek_span_raw(&self) -> Span {
        self.tokens
            .get(self.index)
            .map(|t| t.span)
            .unwrap_or(self.eof_span)
    }

    fn take_doc_comments(&mut self) -> Option<String> {
        let mut docs = Vec::new();
        loop {
            match self.peek_kind_raw() {
                Some(TokenKind::DocLine) => {
                    let token = self.bump().unwrap();
                    let text = token.text.trim_start_matches("///");
                    docs.push(text.trim_start().to_string());
                }
                _ => break,
            }
        }
        if docs.is_empty() {
            None
        } else {
            Some(docs.join("\n").trim().to_string())
        }
    }

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
        let start_span = self.peek_span_raw();
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
        while self.peek_kind_raw().is_some() {
            let doc = self.take_doc_comments();
            if self.peek_kind_raw().is_none() {
                break;
            }
            items.push(self.parse_item(doc)?);
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

    fn parse_item(&mut self, doc: Option<String>) -> Result<Item, ParseError> {
        let mut is_pub = false;
        let mut is_linear = false;
        let mut is_copy = false;
        let mut is_opaque = false;
        let mut is_capability = false;
        loop {
            match self.peek_kind() {
                Some(TokenKind::Pub) => {
                    if is_pub {
                        return Err(self.error_current(
                            "duplicate `pub` modifier".to_string(),
                        ));
                    }
                    self.bump();
                    is_pub = true;
                }
                Some(TokenKind::Linear) => {
                    if is_linear {
                        return Err(self.error_current(
                            "duplicate `linear` modifier".to_string(),
                        ));
                    }
                    self.bump();
                    is_linear = true;
                }
                Some(TokenKind::Copy) => {
                    if is_copy {
                        return Err(self.error_current(
                            "duplicate `copy` modifier".to_string(),
                        ));
                    }
                    self.bump();
                    is_copy = true;
                }
                Some(TokenKind::Opaque) => {
                    if is_opaque {
                        return Err(self.error_current(
                            "duplicate `opaque` modifier".to_string(),
                        ));
                    }
                    self.bump();
                    is_opaque = true;
                }
                Some(TokenKind::Capability) => {
                    if is_capability {
                        return Err(self.error_current(
                            "duplicate `capability` modifier".to_string(),
                        ));
                    }
                    self.bump();
                    is_capability = true;
                }
                _ => break,
            }
        }
        if is_linear && is_copy {
            return Err(self.error_current(
                "cannot combine `linear` and `copy` modifiers".to_string(),
            ));
        }
        if self.peek_kind() == Some(TokenKind::Extern) {
            if is_opaque || is_linear || is_copy || is_capability {
                return Err(self.error_current(
                    "linear/copy/opaque/capability applies only to struct declarations".to_string(),
                ));
            }
            return Ok(Item::ExternFunction(self.parse_extern_function(is_pub, doc)?));
        }
        match self.peek_kind() {
            Some(TokenKind::Fn) => {
                if is_opaque || is_linear || is_copy || is_capability {
                    return Err(self.error_current(
                        "linear/copy/opaque/capability applies only to struct declarations"
                            .to_string(),
                    ));
                }
                Ok(Item::Function(self.parse_function(is_pub, doc)?))
            }
            Some(TokenKind::Struct) => Ok(Item::Struct(self.parse_struct(
                is_pub,
                is_opaque,
                is_linear,
                is_copy,
                is_capability,
                doc,
            )?)),
            Some(TokenKind::Enum) => {
                if is_opaque || is_linear || is_copy || is_capability {
                    return Err(self.error_current(
                        "linear/copy/opaque/capability applies only to struct declarations"
                            .to_string(),
                    ));
                }
                Ok(Item::Enum(self.parse_enum(is_pub, doc)?))
            }
            Some(TokenKind::Impl) => {
                if is_pub {
                    return Err(self.error_current(
                        "impl blocks cannot be marked pub".to_string(),
                    ));
                }
                if is_opaque || is_linear || is_copy || is_capability {
                    return Err(self.error_current(
                        "linear/copy/opaque/capability applies only to struct declarations"
                            .to_string(),
                    ));
                }
                Ok(Item::Impl(self.parse_impl_block(doc)?))
            }
            Some(other) => Err(self.error_current(format!(
                "expected item, found {other:?}"
            ))),
            None => Err(self.error_current("unexpected end of input".to_string())),
        }
    }

    fn parse_impl_block(&mut self, impl_doc: Option<String>) -> Result<ImplBlock, ParseError> {
        let start = self.expect(TokenKind::Impl)?.span.start;
        let type_params = self.parse_type_params()?;
        let target = self.parse_type()?;
        self.expect(TokenKind::LBrace)?;
        let mut methods = Vec::new();
        while self.peek_kind() != Some(TokenKind::RBrace) {
            let doc = self.take_doc_comments();
            let is_pub = self.maybe_consume(TokenKind::Pub).is_some();
            if self.peek_kind() != Some(TokenKind::Fn) {
                return Err(self.error_current(
                    "expected method declaration in impl block".to_string(),
                ));
            }
            methods.push(self.parse_function(is_pub, doc)?);
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(ImplBlock {
            target,
            methods,
            type_params,
            doc: impl_doc,
            span: Span::new(start, end),
        })
    }

    fn parse_extern_function(&mut self, is_pub: bool, doc: Option<String>) -> Result<ExternFunction, ParseError> {
        let start = self.expect(TokenKind::Extern)?.span.start;
        self.expect(TokenKind::Fn)?;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        if self.peek_kind() != Some(TokenKind::RParen) {
            loop {
                let param_name = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let ty = self.parse_type()?;
                params.push(Param {
                    name: param_name,
                    ty: Some(ty),
                });
                if self.maybe_consume(TokenKind::Comma).is_some() {
                    continue;
                }
                break;
            }
        }
        let rparen = self.expect(TokenKind::RParen)?;
        let ret = if self.maybe_consume(TokenKind::Arrow).is_some() {
            self.parse_type()?
        } else {
            unit_type_at(Span::new(rparen.span.end, rparen.span.end))
        };
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(ret.span().end, |t| t.span.end);
        Ok(ExternFunction {
            name,
            type_params,
            params,
            ret,
            is_pub,
            doc,
            span: Span::new(start, end),
        })
    }

    fn parse_function(&mut self, is_pub: bool, doc: Option<String>) -> Result<Function, ParseError> {
        let start = self.expect(TokenKind::Fn)?.span.start;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        if self.peek_kind() != Some(TokenKind::RParen) {
            loop {
                let param_name = self.expect_ident()?;
                let ty = if self.maybe_consume(TokenKind::Colon).is_some() {
                    Some(self.parse_type()?)
                } else if param_name.item == "self" {
                    None
                } else {
                    return Err(self.error_current(
                        "expected ':' after parameter name".to_string(),
                    ));
                };
                params.push(Param {
                    name: param_name,
                    ty,
                });
                if self.maybe_consume(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        let rparen = self.expect(TokenKind::RParen)?;
        let ret = if self.maybe_consume(TokenKind::Arrow).is_some() {
            self.parse_type()?
        } else {
            unit_type_at(Span::new(rparen.span.end, rparen.span.end))
        };
        let body = self.parse_block()?;
        let span = Span::new(start, body.span.end);
        Ok(Function {
            name,
            type_params,
            params,
            ret,
            body,
            is_pub,
            doc,
            span,
        })
    }

    fn parse_struct(
        &mut self,
        is_pub: bool,
        is_opaque: bool,
        is_linear: bool,
        is_copy: bool,
        is_capability: bool,
        doc: Option<String>,
    ) -> Result<StructDecl, ParseError> {
        let start = self.expect(TokenKind::Struct)?.span.start;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
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
                    if self.peek_kind() == Some(TokenKind::RBrace) {
                        break;
                    }
                }
            }
            let end = self.expect(TokenKind::RBrace)?.span.end;
            if is_capability && !fields.is_empty() {
                return Err(self.error_at(
                    Span::new(start, end),
                    "capability struct cannot declare fields".to_string(),
                ));
            }
            end
        } else {
            name.span.end
        };
        Ok(StructDecl {
            name,
            type_params,
            fields,
            is_pub,
            is_opaque,
            is_linear,
            is_copy,
            is_capability,
            doc,
            span: Span::new(start, end),
        })
    }

    fn parse_enum(&mut self, is_pub: bool, doc: Option<String>) -> Result<EnumDecl, ParseError> {
        let start = self.expect(TokenKind::Enum)?.span.start;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
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
                if self.peek_kind() == Some(TokenKind::RBrace) {
                    break;
                }
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(EnumDecl {
            name,
            type_params,
            variants,
            is_pub,
            doc,
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
            Some(TokenKind::Break) => Ok(Stmt::Break(self.parse_break()?)),
            Some(TokenKind::Continue) => Ok(Stmt::Continue(self.parse_continue()?)),
            Some(TokenKind::Defer) => Ok(Stmt::Defer(self.parse_defer()?)),
            Some(TokenKind::If) => self.parse_if_stmt(),
            Some(TokenKind::While) => Ok(Stmt::While(self.parse_while()?)),
            Some(TokenKind::For) => self.parse_for_stmt(),
            Some(TokenKind::Ident) => {
                if self.peek_token(1).is_some_and(|t| t.kind == TokenKind::Eq) {
                    Ok(Stmt::Assign(self.parse_assign()?))
                } else {
                    Ok(Stmt::Expr(self.parse_expr_stmt()?))
                }
            }
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

    fn parse_assign(&mut self) -> Result<AssignStmt, ParseError> {
        let name = self.expect_ident()?;
        let start = name.span.start;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr()?;
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(expr.span().end, |t| t.span.end);
        Ok(AssignStmt {
            name,
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

    fn parse_break(&mut self) -> Result<BreakStmt, ParseError> {
        let token = self.expect(TokenKind::Break)?;
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(token.span.end, |t| t.span.end);
        Ok(BreakStmt {
            span: Span::new(token.span.start, end),
        })
    }

    fn parse_continue(&mut self) -> Result<ContinueStmt, ParseError> {
        let token = self.expect(TokenKind::Continue)?;
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(token.span.end, |t| t.span.end);
        Ok(ContinueStmt {
            span: Span::new(token.span.start, end),
        })
    }

    fn parse_defer(&mut self) -> Result<DeferStmt, ParseError> {
        let start = self.expect(TokenKind::Defer)?.span.start;
        let expr = self.parse_expr()?;
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(expr.span().end, |t| t.span.end);
        Ok(DeferStmt {
            expr,
            span: Span::new(start, end),
        })
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let if_token = self.expect(TokenKind::If)?;
        let start = if_token.span.start;
        if self.peek_kind() == Some(TokenKind::Let) {
            self.bump();
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::Eq)?;
            let expr = self.parse_expr_no_struct()?;
            let then_block = self.parse_block()?;
            let else_block = if self.peek_kind() == Some(TokenKind::Else) {
                self.bump();
                if self.peek_kind() == Some(TokenKind::If) {
                    let else_if = self.parse_if_stmt()?;
                    let span = else_if.span();
                    Some(Block {
                        stmts: vec![else_if],
                        span,
                    })
                } else {
                    Some(self.parse_block()?)
                }
            } else {
                None
            };
            let end = else_block
                .as_ref()
                .map_or(then_block.span.end, |b| b.span.end);
            let else_body = else_block.unwrap_or(Block {
                stmts: Vec::new(),
                span: Span::new(end, end),
            });
            let match_expr = MatchExpr {
                expr: Box::new(expr),
                arms: vec![
                    MatchArm {
                        pattern,
                        body: then_block,
                        span: Span::new(start, end),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard(Span::new(end, end)),
                        body: else_body,
                        span: Span::new(start, end),
                    },
                ],
                span: Span::new(start, end),
                match_span: if_token.span,
            };
            return Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Match(match_expr),
                span: Span::new(start, end),
            }));
        }

        // Use parse_expr_no_struct because `{` after condition starts the then-block, not a struct literal
        let cond = self.parse_expr_no_struct()?;
        let then_block = self.parse_block()?;
        let else_block = if self.peek_kind() == Some(TokenKind::Else) {
            self.bump();
            if self.peek_kind() == Some(TokenKind::If) {
                let else_if = self.parse_if_stmt()?;
                let span = else_if.span();
                Some(Block {
                    stmts: vec![else_if],
                    span,
                })
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };
        let end = else_block
            .as_ref()
            .map_or(then_block.span.end, |b| b.span.end);
        Ok(Stmt::If(IfStmt {
            cond,
            then_block,
            else_block,
            span: Span::new(start, end),
        }))
    }

    fn parse_while(&mut self) -> Result<WhileStmt, ParseError> {
        let start = self.expect(TokenKind::While)?.span.start;
        // Use parse_expr_no_struct because `{` after condition starts the loop body, not a struct literal
        let cond = self.parse_expr_no_struct()?;
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(WhileStmt {
            cond,
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_for_after(&mut self, start: usize) -> Result<ForStmt, ParseError> {
        let var = self.expect_ident()?;
        self.expect(TokenKind::In)?;
        let range_start = self.parse_range_bound()?;
        self.expect(TokenKind::DotDot)?;
        let range_end = self.parse_range_bound()?;
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(ForStmt {
            var,
            start: range_start,
            end: range_end,
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
        let for_token = self.expect(TokenKind::For)?;
        let start = for_token.span.start;
        if self.peek_kind() == Some(TokenKind::LBrace) {
            let body = self.parse_block()?;
            let end = body.span.end;
            let cond = Expr::Literal(LiteralExpr {
                value: Literal::Bool(true),
                span: for_token.span,
            });
            return Ok(Stmt::While(WhileStmt {
                cond,
                body,
                span: Span::new(start, end),
            }));
        }
        Ok(Stmt::For(self.parse_for_after(start)?))
    }

    /// Parse a simple expression for range bounds (no struct literals allowed)
    fn parse_range_bound(&mut self) -> Result<Expr, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Int) => {
                let token = self.bump().unwrap();
                let value = token.text.parse::<i64>().map_err(|_| {
                    self.error_at(token.span, "invalid integer literal".to_string())
                })?;
                Ok(Expr::Literal(LiteralExpr {
                    value: Literal::Int(value),
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
            Some(TokenKind::Ident) => {
                // Parse just a simple path (no struct literal)
                let first_ident = self.expect_ident()?;
                let start = first_ident.span.start;
                let mut segments = vec![first_ident];

                while self.peek_kind() == Some(TokenKind::ColonColon) {
                    self.bump();
                    let segment = self.expect_ident()?;
                    segments.push(segment);
                }

                let end = segments.last().unwrap().span.end;
                Ok(Expr::Path(Path {
                    segments,
                    span: Span::new(start, end),
                }))
            }
            Some(other) => Err(self.error_current(format!(
                "expected integer or identifier in range bound, found {other:?}"
            ))),
            None => Err(self.error_current("unexpected end of input".to_string())),
        }
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
        self.parse_expr_inner(true)
    }

    /// Parse an expression where struct literals are not allowed.
    /// Used in if/while/for/match scrutinee positions where `{` starts a block, not a struct literal.
    fn parse_expr_no_struct(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_inner(false)
    }

    fn parse_expr_inner(&mut self, allow_struct_literal: bool) -> Result<Expr, ParseError> {
        self.parse_expr_bp(0, allow_struct_literal)
    }

    /// Look ahead to see if a `<...>` type-arg list is closed and followed by
    /// a call `(` or struct literal `{`.
    fn type_args_followed_by_call_or_struct(&self) -> bool {
        if self.peek_kind() != Some(TokenKind::Lt) {
            return false;
        }
        let mut depth = 0usize;
        let mut idx = self.index;
        while idx < self.tokens.len() {
            match self.tokens[idx].kind {
                TokenKind::Lt => depth += 1,
                TokenKind::Gt => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                    if depth == 0 {
                        return matches!(
                            self.tokens.get(idx + 1).map(|t| &t.kind),
                            Some(TokenKind::LParen) | Some(TokenKind::LBrace)
                        );
                    }
                }
                _ => {}
            }
            idx += 1;
        }
        false
    }

    fn parse_expr_bp(&mut self, min_bp: u8, allow_struct_literal: bool) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_prefix(allow_struct_literal)?;

        loop {
            // First, check for postfix operators
            if let Some(kind) = self.peek_kind() {
                if let Some(bp) = postfix_binding_power(&kind) {
                    if bp < min_bp {
                        break;
                    }

                    match kind {
                        TokenKind::Dot => {
                            let start = lhs.span().start;
                            self.bump(); // consume '.'
                            let field = self.expect_ident()?;
                            let type_args = if self.peek_kind() == Some(TokenKind::Lt) {
                                self.parse_type_args()?
                            } else {
                                Vec::new()
                            };

                            // Check if this is a struct literal (followed by '{')
                            if allow_struct_literal && self.peek_kind() == Some(TokenKind::LBrace) {
                                // Convert the lhs and field into a path for the struct literal
                                let mut path = match lhs {
                                    Expr::Path(p) => p,
                                    Expr::FieldAccess(ref fa) => {
                                        // Convert FieldAccess chain to Path
                                        self.field_access_to_path(fa)?
                                    }
                                    _ => return Err(self.error_current("expected path before struct literal".to_string())),
                                };
                                path.segments.push(field);
                                path.span = Span::new(path.span.start, path.segments.last().unwrap().span.end);
                                lhs = self.parse_struct_literal(path, type_args)?;
                                continue;
                            }

                            // Check if this is a method call (followed by '(')
                            if self.peek_kind() == Some(TokenKind::LParen) {
                                self.bump(); // consume '('
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
                                lhs = Expr::MethodCall(MethodCallExpr {
                                    receiver: Box::new(lhs),
                                    method: field,
                                    type_args,
                                    args,
                                    span: Span::new(start, end),
                                });
                                continue;
                            }

                            if !type_args.is_empty() {
                                return Err(self.error_current(
                                    "type arguments require a method call or struct literal"
                                        .to_string(),
                                ));
                            }
                            // Otherwise, it's a field access
                            let span = Span::new(start, field.span.end);
                            lhs = Expr::FieldAccess(FieldAccessExpr {
                                object: Box::new(lhs),
                                field,
                                span,
                            });
                            continue;
                        }
                        TokenKind::LParen => {
                            lhs = self.finish_call(lhs, Vec::new())?;
                            continue;
                        }
                        TokenKind::LBracket => {
                            // With <> for generics, [] is unambiguously for indexing
                            let start = lhs.span().start;
                            self.bump(); // consume '['
                            let index = self.parse_expr()?;
                            let end = self.expect(TokenKind::RBracket)?.span.end;
                            lhs = Expr::Index(IndexExpr {
                                object: Box::new(lhs),
                                index: Box::new(index),
                                span: Span::new(start, end),
                            });
                            continue;
                        }
                        TokenKind::Question => {
                            let start = lhs.span().start;
                            let end = self.bump().unwrap().span.end;
                            lhs = Expr::Try(TryExpr {
                                expr: Box::new(lhs),
                                span: Span::new(start, end),
                            });
                            continue;
                        }
                        _ => unreachable!(),
                    }
                }
            }

            // Special handling for '<' which can be type arguments or less-than
            if self.peek_kind() == Some(TokenKind::Lt) {
                // Check if this looks like type arguments: path<Type>(args) or path<Type>{ ... }
                if matches!(&lhs, Expr::Path(_) | Expr::FieldAccess(_))
                    && self.type_args_followed_by_call_or_struct()
                {
                    let type_args = self.parse_type_args()?;
                    if allow_struct_literal && self.peek_kind() == Some(TokenKind::LBrace) {
                        let path = match lhs {
                            Expr::Path(p) => p,
                            Expr::FieldAccess(ref fa) => self.field_access_to_path(fa)?,
                            _ => unreachable!(),
                        };
                        lhs = self.parse_struct_literal(path, type_args)?;
                        continue;
                    }
                    if self.peek_kind() == Some(TokenKind::LParen) {
                        lhs = self.finish_call(lhs, type_args)?;
                        continue;
                    }
                    if !allow_struct_literal && self.peek_kind() == Some(TokenKind::LBrace) {
                        return Err(self.error_current(
                            "generic expressions in this context require parentheses".to_string(),
                        ));
                    }
                    return Err(self.error_current(
                        "type arguments require a call or struct literal".to_string(),
                    ));
                }
                // Fall through to treat as less-than comparison
            }

            // Then, check for binary operators
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
                _ => break,
            };

            let (l_bp, r_bp) = infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            self.bump();
            // Propagate struct-literal allowance to avoid block ambiguity in no-struct contexts.
            let rhs = self.parse_expr_bp(r_bp, allow_struct_literal)?;
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

    fn parse_prefix(&mut self, allow_struct_literal: bool) -> Result<Expr, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Minus) => {
                let start = self.bump().unwrap().span.start;
                // Propagate struct-literal allowance to avoid block ambiguity in no-struct contexts.
                let expr = self.parse_expr_bp(7, allow_struct_literal)?;
                Ok(Expr::Unary(UnaryExpr {
                    op: UnaryOp::Neg,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                }))
            }
            Some(TokenKind::Bang) => {
                let start = self.bump().unwrap().span.start;
                // Propagate struct-literal allowance to avoid block ambiguity in no-struct contexts.
                let expr = self.parse_expr_bp(7, allow_struct_literal)?;
                Ok(Expr::Unary(UnaryExpr {
                    op: UnaryOp::Not,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                }))
            }
            Some(TokenKind::Match) => self.parse_match(),
            _ => self.parse_primary(allow_struct_literal),
        }
    }

    fn parse_primary(&mut self, allow_struct_literal: bool) -> Result<Expr, ParseError> {
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
            Some(TokenKind::Char) => {
                let token = self.bump().unwrap();
                let value = unescape_char(&token.text).map_err(|message| {
                    self.error_at(token.span, format!("invalid char literal: {message}"))
                })?;
                Ok(Expr::Literal(LiteralExpr {
                    value: Literal::U8(value),
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
                // Parse path segments separated by ::
                let first_ident = self.expect_ident()?;
                let start = first_ident.span.start;
                let mut segments = vec![first_ident];

                // Parse additional segments with ::
                while self.peek_kind() == Some(TokenKind::ColonColon) {
                    self.bump(); // consume ::
                    let segment = self.expect_ident()?;
                    segments.push(segment);
                }

                let end = segments.last().unwrap().span.end;
                let path = Path {
                    segments,
                    span: Span::new(start, end),
                };

                if allow_struct_literal && self.peek_kind() == Some(TokenKind::LBrace) {
                    self.parse_struct_literal(path, Vec::new())
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
        let match_token = self.expect(TokenKind::Match)?;
        let start = match_token.span.start;
        // Use parse_expr_no_struct because `{` after the scrutinee starts the match arms, not a struct literal
        let expr = self.parse_expr_no_struct()?;
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
            match_span: match_token.span,
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
            Some(TokenKind::Char) => {
                let token = self.bump().unwrap();
                let value = unescape_char(&token.text).map_err(|message| {
                    self.error_at(token.span, format!("invalid char literal: {message}"))
                })?;
                Ok(Pattern::Literal(Literal::U8(value)))
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
                    } else if self.peek_kind() == Some(TokenKind::Underscore) {
                        self.bump();
                        None
                    } else {
                        None
                    };
                    let end = self.expect(TokenKind::RParen)?.span.end;
                    Ok(Pattern::Call {
                        path,
                        binding,
                        span: Span::new(start, end),
                    })
                } else if path.segments.len() == 1 {
                    // Single segment - could be binding or enum variant
                    // If lowercase, it's a binding; if uppercase, it's an enum variant
                    let name = &path.segments[0].item;
                    if name.chars().next().map(|c| c.is_lowercase()).unwrap_or(false) {
                        Ok(Pattern::Binding(path.segments.into_iter().next().unwrap()))
                    } else {
                        Ok(Pattern::Path(path))
                    }
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
        // Parse path segments separated by ::
        while self.peek_kind() == Some(TokenKind::ColonColon) {
            self.bump();
            segments.push(self.expect_ident()?);
        }
        let end = segments.last().map(|s| s.span.end).unwrap_or(start);
        Ok(Path {
            segments,
            span: Span::new(start, end),
        })
    }

    fn field_access_to_path(&self, field_access: &FieldAccessExpr) -> Result<Path, ParseError> {
        let mut segments = Vec::new();

        // Recursively collect segments from the object
        fn collect_segments(expr: &Expr, segments: &mut Vec<Ident>) -> Option<()> {
            match expr {
                Expr::Path(path) => {
                    segments.extend(path.segments.clone());
                    Some(())
                }
                Expr::FieldAccess(fa) => {
                    collect_segments(&fa.object, segments)?;
                    segments.push(fa.field.clone());
                    Some(())
                }
                _ => None,
            }
        }

        collect_segments(&field_access.object, &mut segments)
            .ok_or_else(|| self.error_at(field_access.span, "expected path or field access".to_string()))?;
        segments.push(field_access.field.clone());

        let start = segments.first().map(|s| s.span.start).unwrap_or(field_access.span.start);
        let end = segments.last().map(|s| s.span.end).unwrap_or(field_access.span.end);

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
        if self.peek_kind() == Some(TokenKind::Ampersand) {
            let start = self.bump().unwrap().span.start;
            let target = self.parse_type()?;
            let span = Span::new(start, target.span().end);
            return Ok(Type::Ref {
                target: Box::new(target),
                span,
            });
        }

        let path = self.parse_path()?;
        let mut args = Vec::new();
        let mut end = path.span.end;
        if self.peek_kind() == Some(TokenKind::Lt) {
            self.bump();
            if self.peek_kind() != Some(TokenKind::Gt) {
                loop {
                    args.push(self.parse_type()?);
                    if self.maybe_consume(TokenKind::Comma).is_none() {
                        break;
                    }
                }
            }
            end = self.expect(TokenKind::Gt)?.span.end;
        }
        let span = Span::new(path.span.start, end);
        Ok(Type::Path { path, args, span })
    }

    fn parse_struct_literal(&mut self, path: Path, type_args: Vec<Type>) -> Result<Expr, ParseError> {
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
            type_args,
            fields,
            span: Span::new(start, end),
        }))
    }

    fn finish_call(&mut self, callee: Expr, type_args: Vec<Type>) -> Result<Expr, ParseError> {
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
            type_args,
            args,
            span: Span::new(start, end),
        }))
    }

    fn parse_type_params(&mut self) -> Result<Vec<Ident>, ParseError> {
        if self.peek_kind() != Some(TokenKind::Lt) {
            return Ok(Vec::new());
        }
        self.bump();
        let mut params = Vec::new();
        if self.peek_kind() != Some(TokenKind::Gt) {
            loop {
                let ident = self.expect_ident()?;
                params.push(ident);
                if self.maybe_consume(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        self.expect(TokenKind::Gt)?;
        Ok(params)
    }

    fn parse_type_args(&mut self) -> Result<Vec<Type>, ParseError> {
        if self.peek_kind() != Some(TokenKind::Lt) {
            return Ok(Vec::new());
        }
        self.bump();
        let mut args = Vec::new();
        if self.peek_kind() != Some(TokenKind::Gt) {
            loop {
                args.push(self.parse_type()?);
                if self.maybe_consume(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        self.expect(TokenKind::Gt)?;
        Ok(args)
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

fn postfix_binding_power(kind: &TokenKind) -> Option<u8> {
    match kind {
        TokenKind::Dot | TokenKind::LParen | TokenKind::LBracket | TokenKind::Question => Some(13),
        _ => None,
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

fn unescape_char(text: &str) -> Result<u8, String> {
    let mut chars = text.chars();
    if chars.next() != Some('\'') || text.len() < 2 {
        return Err("missing quotes".to_string());
    }
    let Some(ch) = chars.next() else {
        return Err("empty char literal".to_string());
    };
    let value = if ch == '\\' {
        let Some(esc) = chars.next() else {
            return Err("invalid escape".to_string());
        };
        match esc {
            'n' => b'\n',
            'r' => b'\r',
            't' => b'\t',
            '\\' => b'\\',
            '\'' => b'\'',
            'x' => {
                let hi = chars.next().ok_or_else(|| "invalid hex escape".to_string())?;
                let lo = chars.next().ok_or_else(|| "invalid hex escape".to_string())?;
                let hex = format!("{hi}{lo}");
                u8::from_str_radix(&hex, 16).map_err(|_| "invalid hex escape".to_string())?
            }
            other => return Err(format!("unsupported escape \\{other}")),
        }
    } else {
        let code = ch as u32;
        if code > 255 {
            return Err("char literal out of range".to_string());
        }
        code as u8
    };
    Ok(value)
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
            Expr::MethodCall(method_call) => method_call.span,
            Expr::FieldAccess(field) => field.span,
            Expr::Index(index) => index.span,
            Expr::StructLiteral(lit) => lit.span,
            Expr::Unary(unary) => unary.span,
            Expr::Binary(binary) => binary.span,
            Expr::Match(m) => m.span,
            Expr::Try(try_expr) => try_expr.span,
            Expr::Grouping(g) => g.span,
        }
    }
}

fn unit_type_at(span: Span) -> Type {
    let ident = Spanned::new("unit".to_string(), span);
    let path = Path {
        segments: vec![ident],
        span,
    };
    Type::Path {
        path,
        args: Vec::new(),
        span,
    }
}
