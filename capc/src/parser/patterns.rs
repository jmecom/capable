use super::*;

impl Parser {
    pub(super) fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
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
                    let name = &path.segments[0].item;
                    if name
                        .chars()
                        .next()
                        .map(|c| c.is_lowercase())
                        .unwrap_or(false)
                    {
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

    pub(super) fn pattern_binding_ident(&self, pattern: &Pattern) -> Option<Ident> {
        match pattern {
            Pattern::Binding(ident) => Some(ident.clone()),
            Pattern::Call {
                binding: Some(ident),
                ..
            } => Some(ident.clone()),
            _ => None,
        }
    }

    pub(super) fn parse_path(&mut self) -> Result<Path, ParseError> {
        let first = self.expect_ident()?;
        let start = first.span.start;
        let mut segments = vec![first];
        while self.peek_kind() == Some(TokenKind::ColonColon) {
            self.bump();
            segments.push(self.expect_ident()?);
        }
        let end = segments.last().map(|s| s.span.end).unwrap_or(start);
        Ok(Path {
            id: self.fresh_expr_id(),
            segments,
            span: Span::new(start, end),
        })
    }

    pub(super) fn field_access_to_path(
        &self,
        field_access: &FieldAccessExpr,
    ) -> Result<Path, ParseError> {
        let mut segments = Vec::new();

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

        collect_segments(&field_access.object, &mut segments).ok_or_else(|| {
            self.error_at(
                field_access.span,
                "expected path or field access".to_string(),
            )
        })?;
        segments.push(field_access.field.clone());

        let start = segments
            .first()
            .map(|s| s.span.start)
            .unwrap_or(field_access.span.start);
        let end = segments
            .last()
            .map(|s| s.span.end)
            .unwrap_or(field_access.span.end);

        Ok(Path {
            id: field_access.id,
            segments,
            span: Span::new(start, end),
        })
    }
}
