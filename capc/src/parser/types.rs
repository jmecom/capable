use super::*;

impl Parser {
    pub(super) fn parse_type(&mut self) -> Result<Type, ParseError> {
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

    pub(super) fn parse_type_params(&mut self) -> Result<Vec<TypeParam>, ParseError> {
        if self.peek_kind() != Some(TokenKind::Lt) {
            return Ok(Vec::new());
        }
        self.bump();
        let mut params = Vec::new();
        if self.peek_kind() != Some(TokenKind::Gt) {
            loop {
                let name = self.expect_ident()?;
                let mut bounds = Vec::new();
                if self.maybe_consume(TokenKind::Colon).is_some() {
                    loop {
                        let bound = self.parse_path()?;
                        bounds.push(bound);
                        if self.maybe_consume(TokenKind::Plus).is_none() {
                            break;
                        }
                    }
                }
                params.push(TypeParam { name, bounds });
                if self.maybe_consume(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        self.expect(TokenKind::Gt)?;
        Ok(params)
    }

    pub(super) fn parse_type_args(&mut self) -> Result<Vec<Type>, ParseError> {
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
}
