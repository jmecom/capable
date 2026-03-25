use super::*;

impl Parser {
    pub(super) fn parse_block(&mut self) -> Result<Block, ParseError> {
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

    pub(super) fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Let) => self.parse_let(),
            Some(TokenKind::Return) => Ok(Stmt::Return(self.parse_return()?)),
            Some(TokenKind::Break) => Ok(Stmt::Break(self.parse_break()?)),
            Some(TokenKind::Continue) => Ok(Stmt::Continue(self.parse_continue()?)),
            Some(TokenKind::Defer) => Ok(Stmt::Defer(self.parse_defer()?)),
            Some(TokenKind::Try) => self.parse_try_stmt(),
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

    pub(super) fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        let let_token = self.expect(TokenKind::Let)?;
        let start = let_token.span.start;
        if self.peek_kind() == Some(TokenKind::Ident)
            && self
                .peek_token(1)
                .is_some_and(|t| matches!(t.kind, TokenKind::Colon | TokenKind::Eq))
        {
            let name = self.expect_ident()?;
            let ty = if self.maybe_consume(TokenKind::Colon).is_some() {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(TokenKind::Eq)?;
            let expr = self.parse_expr()?;
            let end = self
                .maybe_consume(TokenKind::Semi)
                .map_or(expr.span().end, |t| t.span.end);
            return Ok(Stmt::Let(LetStmt {
                name,
                ty,
                expr,
                span: Span::new(start, end),
            }));
        }

        let pattern = self.parse_pattern()?;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Else)?;
        let else_block = self.parse_block()?;
        if self.pattern_binding_ident(&pattern).is_none() {
            return Err(self.error_at(
                let_token.span,
                "`let ... else` requires a binding pattern".to_string(),
            ));
        }
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(else_block.span.end, |t| t.span.end);
        Ok(Stmt::LetElse(LetElseStmt {
            pattern,
            expr,
            else_block,
            span: Span::new(start, end),
        }))
    }

    pub(super) fn parse_assign(&mut self) -> Result<AssignStmt, ParseError> {
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

    pub(super) fn parse_return(&mut self) -> Result<ReturnStmt, ParseError> {
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

    pub(super) fn parse_try_stmt(&mut self) -> Result<Stmt, ParseError> {
        let try_token = self.expect(TokenKind::Try)?;
        let start = try_token.span.start;

        if self.peek_kind() == Some(TokenKind::Let) {
            self.bump();
            if !(self.peek_kind() == Some(TokenKind::Ident)
                && self
                    .peek_token(1)
                    .is_some_and(|t| matches!(t.kind, TokenKind::Colon | TokenKind::Eq)))
            {
                return Err(self.error_at(
                    try_token.span,
                    "`try let` requires a plain binding name".to_string(),
                ));
            }

            let name = self.expect_ident()?;
            let ty = if self.maybe_consume(TokenKind::Colon).is_some() {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(TokenKind::Eq)?;
            let expr = self.parse_expr()?;
            self.expect(TokenKind::Else)?;
            let err_binding = if self.peek_kind() == Some(TokenKind::Ident)
                && self
                    .peek_token(1)
                    .is_some_and(|t| t.kind == TokenKind::LBrace)
            {
                Some(self.expect_ident()?)
            } else {
                None
            };
            let else_block = self.parse_block()?;
            let end = self
                .maybe_consume(TokenKind::Semi)
                .map_or(else_block.span.end, |t| t.span.end);
            return Ok(Stmt::TryLet(TryLetStmt {
                name,
                ty,
                expr,
                err_binding,
                else_block,
                span: Span::new(start, end),
            }));
        }

        let expr = self.parse_expr()?;
        self.expect(TokenKind::Else)?;
        let err_binding = if self.peek_kind() == Some(TokenKind::Ident)
            && self
                .peek_token(1)
                .is_some_and(|t| t.kind == TokenKind::LBrace)
        {
            Some(self.expect_ident()?)
        } else {
            None
        };
        let else_block = self.parse_block()?;
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(else_block.span.end, |t| t.span.end);
        Ok(Stmt::TryElse(TryElseStmt {
            expr,
            err_binding,
            else_block,
            span: Span::new(start, end),
        }))
    }

    pub(super) fn parse_break(&mut self) -> Result<BreakStmt, ParseError> {
        let token = self.expect(TokenKind::Break)?;
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(token.span.end, |t| t.span.end);
        Ok(BreakStmt {
            span: Span::new(token.span.start, end),
        })
    }

    pub(super) fn parse_continue(&mut self) -> Result<ContinueStmt, ParseError> {
        let token = self.expect(TokenKind::Continue)?;
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(token.span.end, |t| t.span.end);
        Ok(ContinueStmt {
            span: Span::new(token.span.start, end),
        })
    }

    pub(super) fn parse_defer(&mut self) -> Result<DeferStmt, ParseError> {
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

    pub(super) fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let if_token = self.expect(TokenKind::If)?;
        let start = if_token.span.start;
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

    pub(super) fn parse_while(&mut self) -> Result<WhileStmt, ParseError> {
        let start = self.expect(TokenKind::While)?.span.start;
        let cond = self.parse_expr_no_struct()?;
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(WhileStmt {
            cond,
            body,
            span: Span::new(start, end),
        })
    }

    pub(super) fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
        let for_token = self.expect(TokenKind::For)?;
        let start = for_token.span.start;
        if self.peek_kind() == Some(TokenKind::LBrace) {
            let body = self.parse_block()?;
            let end = body.span.end;
            let cond = Expr::Literal(LiteralExpr {
                id: self.fresh_expr_id(),
                value: Literal::Bool(true),
                span: for_token.span,
            });
            return Ok(Stmt::While(WhileStmt {
                cond,
                body,
                span: Span::new(start, end),
            }));
        }
        let first = self.expect_ident()?;
        let second = if self.maybe_consume(TokenKind::Comma).is_some() {
            Some(self.expect_ident()?)
        } else {
            None
        };
        self.expect(TokenKind::In)?;
        let range_or_source = self.parse_expr_no_struct()?;
        if self.maybe_consume(TokenKind::DotDot).is_some() {
            if second.is_some() {
                return Err(self.error_at(
                    first.span,
                    "range for loops accept only one binding".to_string(),
                ));
            }
            let range_end = self.parse_range_bound()?;
            let body = self.parse_block()?;
            let end = body.span.end;
            return Ok(Stmt::For(ForStmt {
                var: first,
                start: range_or_source,
                end: range_end,
                body,
                span: Span::new(start, end),
            }));
        }

        let item = second.clone().unwrap_or_else(|| first.clone());
        let index = second.map(|_| first);
        let body = self.parse_block()?;
        Ok(Stmt::ForEach(ForEachStmt {
            index,
            item,
            source: range_or_source,
            span: Span::new(start, body.span.end),
            body,
        }))
    }

    pub(super) fn parse_range_bound(&mut self) -> Result<Expr, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Int) => {
                let token = self.bump().unwrap();
                let value = token.text.parse::<i64>().map_err(|_| {
                    self.error_at(token.span, "invalid integer literal".to_string())
                })?;
                Ok(Expr::Literal(LiteralExpr {
                    id: self.fresh_expr_id(),
                    value: Literal::Int(value),
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
                Ok(Expr::Path(Path {
                    id: self.fresh_expr_id(),
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

    pub(super) fn parse_expr_stmt(&mut self) -> Result<ExprStmt, ParseError> {
        let expr = self.parse_expr()?;
        if self.peek_kind() == Some(TokenKind::Else) {
            return Err(self.error_current("`expr else` now requires a leading `try`".to_string()));
        }
        let expr_span = expr.span();
        let end = self
            .maybe_consume(TokenKind::Semi)
            .map_or(expr_span.end, |t| t.span.end);
        Ok(ExprStmt {
            expr,
            span: Span::new(expr_span.start, end),
        })
    }
}
