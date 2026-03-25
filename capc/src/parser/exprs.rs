use super::*;

impl Parser {
    pub(super) fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_inner(true)
    }

    /// Parse an expression where struct literals are not allowed.
    /// Used in if/while/for/match scrutinee positions where `{` starts a block, not a struct literal.
    pub(super) fn parse_expr_no_struct(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_inner(false)
    }

    pub(super) fn parse_expr_inner(
        &mut self,
        allow_struct_literal: bool,
    ) -> Result<Expr, ParseError> {
        self.parse_expr_bp(0, allow_struct_literal)
    }

    /// Look ahead to see if a `<...>` type-arg list is closed and followed by
    /// a call `(` or struct literal `{`.
    pub(super) fn type_args_followed_by_call_or_struct(&self) -> bool {
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

    pub(super) fn parse_expr_bp(
        &mut self,
        min_bp: u8,
        allow_struct_literal: bool,
    ) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_prefix(allow_struct_literal)?;

        loop {
            if let Some(kind) = self.peek_kind() {
                if let Some(bp) = postfix_binding_power(&kind) {
                    if bp < min_bp {
                        break;
                    }

                    match kind {
                        TokenKind::Dot => {
                            let start = lhs.span().start;
                            self.bump();
                            let field = self.expect_ident()?;
                            let type_args = if self.peek_kind() == Some(TokenKind::Lt) {
                                self.parse_type_args()?
                            } else {
                                Vec::new()
                            };

                            if allow_struct_literal && self.peek_kind() == Some(TokenKind::LBrace) {
                                let mut path = match lhs {
                                    Expr::Path(p) => p,
                                    Expr::FieldAccess(ref fa) => self.field_access_to_path(fa)?,
                                    _ => {
                                        return Err(self.error_current(
                                            "expected path before struct literal".to_string(),
                                        ))
                                    }
                                };
                                path.segments.push(field);
                                path.span = Span::new(
                                    path.span.start,
                                    path.segments.last().unwrap().span.end,
                                );
                                lhs = self.parse_struct_literal(path, type_args)?;
                                continue;
                            }

                            if self.peek_kind() == Some(TokenKind::LParen) {
                                self.bump();
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
                                    id: self.fresh_expr_id(),
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
                            let span = Span::new(start, field.span.end);
                            lhs = Expr::FieldAccess(FieldAccessExpr {
                                id: self.fresh_expr_id(),
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
                            let start = lhs.span().start;
                            self.bump();
                            let index = self.parse_expr()?;
                            let end = self.expect(TokenKind::RBracket)?.span.end;
                            lhs = Expr::Index(IndexExpr {
                                id: self.fresh_expr_id(),
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
                                id: self.fresh_expr_id(),
                                expr: Box::new(lhs),
                                span: Span::new(start, end),
                            });
                            continue;
                        }
                        _ => {
                            return Err(self.error_current(
                                "unexpected postfix operator".to_string(),
                            ))
                        }
                    }
                }
            }

            if self.peek_kind() == Some(TokenKind::Lt) {
                if matches!(&lhs, Expr::Path(_) | Expr::FieldAccess(_))
                    && self.type_args_followed_by_call_or_struct()
                {
                    let type_args = self.parse_type_args()?;
                    if allow_struct_literal && self.peek_kind() == Some(TokenKind::LBrace) {
                        let path = match lhs {
                            Expr::Path(p) => p,
                            Expr::FieldAccess(ref fa) => self.field_access_to_path(fa)?,
                            _ => {
                                return Err(self.error_current(
                                    "type arguments require a path receiver".to_string(),
                                ))
                            }
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
            }

            let op = match self.peek_kind() {
                Some(TokenKind::OrOr) => BinaryOp::Or,
                Some(TokenKind::AndAnd) => BinaryOp::And,
                Some(TokenKind::Pipe) => BinaryOp::BitOr,
                Some(TokenKind::Caret) => BinaryOp::BitXor,
                Some(TokenKind::Ampersand) => BinaryOp::BitAnd,
                Some(TokenKind::EqEq) => BinaryOp::Eq,
                Some(TokenKind::NotEq) => BinaryOp::Neq,
                Some(TokenKind::Lt) => BinaryOp::Lt,
                Some(TokenKind::Lte) => BinaryOp::Lte,
                Some(TokenKind::Gt) => BinaryOp::Gt,
                Some(TokenKind::Gte) => BinaryOp::Gte,
                Some(TokenKind::Shl) => BinaryOp::Shl,
                Some(TokenKind::Shr) => BinaryOp::Shr,
                Some(TokenKind::Plus) => BinaryOp::Add,
                Some(TokenKind::Minus) => BinaryOp::Sub,
                Some(TokenKind::Star) => BinaryOp::Mul,
                Some(TokenKind::Slash) => BinaryOp::Div,
                Some(TokenKind::Percent) => BinaryOp::Mod,
                _ => break,
            };

            let (l_bp, r_bp) = infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            self.bump();
            let rhs = self.parse_expr_bp(r_bp, allow_struct_literal)?;
            let span = Span::new(lhs.span().start, rhs.span().end);
            lhs = Expr::Binary(BinaryExpr {
                id: self.fresh_expr_id(),
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                span,
            });
        }

        Ok(lhs)
    }

    pub(super) fn parse_prefix(
        &mut self,
        allow_struct_literal: bool,
    ) -> Result<Expr, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Minus) => {
                let start = self.bump().unwrap().span.start;
                let expr = self.parse_expr_bp(7, allow_struct_literal)?;
                Ok(Expr::Unary(UnaryExpr {
                    id: self.fresh_expr_id(),
                    op: UnaryOp::Neg,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                }))
            }
            Some(TokenKind::Tilde) => {
                let start = self.bump().unwrap().span.start;
                let expr = self.parse_expr_bp(7, allow_struct_literal)?;
                Ok(Expr::Unary(UnaryExpr {
                    id: self.fresh_expr_id(),
                    op: UnaryOp::BitNot,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                }))
            }
            Some(TokenKind::Bang) => {
                let start = self.bump().unwrap().span.start;
                let expr = self.parse_expr_bp(7, allow_struct_literal)?;
                Ok(Expr::Unary(UnaryExpr {
                    id: self.fresh_expr_id(),
                    op: UnaryOp::Not,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                }))
            }
            Some(TokenKind::Match) => self.parse_match(),
            _ => self.parse_primary(allow_struct_literal),
        }
    }

    pub(super) fn parse_primary(
        &mut self,
        allow_struct_literal: bool,
    ) -> Result<Expr, ParseError> {
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
                            id: self.fresh_expr_id(),
                            value: Literal::U8(value as u8),
                            span: Span::new(token.span.start, suffix.span.end),
                        }));
                    }
                }
                Ok(Expr::Literal(LiteralExpr {
                    id: self.fresh_expr_id(),
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
                    id: self.fresh_expr_id(),
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
                    id: self.fresh_expr_id(),
                    value: Literal::U8(value),
                    span: token.span,
                }))
            }
            Some(TokenKind::True) => {
                let token = self.bump().unwrap();
                Ok(Expr::Literal(LiteralExpr {
                    id: self.fresh_expr_id(),
                    value: Literal::Bool(true),
                    span: token.span,
                }))
            }
            Some(TokenKind::False) => {
                let token = self.bump().unwrap();
                Ok(Expr::Literal(LiteralExpr {
                    id: self.fresh_expr_id(),
                    value: Literal::Bool(false),
                    span: token.span,
                }))
            }
            Some(TokenKind::LParen) => {
                let start = self.bump().unwrap().span.start;
                if self.peek_kind() == Some(TokenKind::RParen) {
                    let end = self.bump().unwrap().span.end;
                    Ok(Expr::Literal(LiteralExpr {
                        id: self.fresh_expr_id(),
                        value: Literal::Unit,
                        span: Span::new(start, end),
                    }))
                } else {
                    let expr = self.parse_expr()?;
                    let end = self.expect(TokenKind::RParen)?.span.end;
                    Ok(Expr::Grouping(GroupingExpr {
                        id: self.fresh_expr_id(),
                        expr: Box::new(expr),
                        span: Span::new(start, end),
                    }))
                }
            }
            Some(TokenKind::Ident) => {
                let first_ident = self.expect_ident()?;
                let start = first_ident.span.start;
                let mut segments = vec![first_ident];

                while self.peek_kind() == Some(TokenKind::ColonColon) {
                    self.bump();
                    let segment = self.expect_ident()?;
                    segments.push(segment);
                }

                let end = segments.last().unwrap().span.end;
                let path = Path {
                    id: self.fresh_expr_id(),
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

    pub(super) fn parse_match(&mut self) -> Result<Expr, ParseError> {
        let match_token = self.expect(TokenKind::Match)?;
        let start = match_token.span.start;
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
            id: self.fresh_expr_id(),
            expr: Box::new(expr),
            arms,
            span: Span::new(start, end),
            match_span: match_token.span,
        }))
    }

    pub(super) fn parse_struct_literal(
        &mut self,
        path: Path,
        type_args: Vec<Type>,
    ) -> Result<Expr, ParseError> {
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
            id: self.fresh_expr_id(),
            path,
            type_args,
            fields,
            span: Span::new(start, end),
        }))
    }

    pub(super) fn finish_call(
        &mut self,
        callee: Expr,
        type_args: Vec<Type>,
    ) -> Result<Expr, ParseError> {
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
            id: self.fresh_expr_id(),
            callee: Box::new(callee),
            type_args,
            args,
            span: Span::new(start, end),
        }))
    }
}
