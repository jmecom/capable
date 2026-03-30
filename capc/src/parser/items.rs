use super::*;

impl Parser {
    pub(super) fn parse_use(&mut self) -> Result<UseDecl, ParseError> {
        let start = self.expect(TokenKind::Use)?.span.start;
        let path = self.parse_path()?;
        let span = Span::new(start, path.span.end);
        self.maybe_consume(TokenKind::Semi);
        Ok(UseDecl { path, span })
    }

    pub(super) fn parse_item(&mut self, doc: Option<String>) -> Result<Item, ParseError> {
        let mut is_pub = false;
        let mut is_linear = false;
        let mut is_copy = false;
        let mut is_opaque = false;
        let mut is_capability = false;
        loop {
            match self.peek_kind() {
                Some(TokenKind::Pub) => {
                    if is_pub {
                        return Err(self.error_current("duplicate `pub` modifier".to_string()));
                    }
                    self.bump();
                    is_pub = true;
                }
                Some(TokenKind::Linear) => {
                    if is_linear {
                        return Err(self.error_current("duplicate `linear` modifier".to_string()));
                    }
                    self.bump();
                    is_linear = true;
                }
                Some(TokenKind::Copy) => {
                    if is_copy {
                        return Err(self.error_current("duplicate `copy` modifier".to_string()));
                    }
                    self.bump();
                    is_copy = true;
                }
                Some(TokenKind::Opaque) => {
                    if is_opaque {
                        return Err(self.error_current("duplicate `opaque` modifier".to_string()));
                    }
                    self.bump();
                    is_opaque = true;
                }
                Some(TokenKind::Capability) => {
                    if is_capability {
                        return Err(
                            self.error_current("duplicate `capability` modifier".to_string())
                        );
                    }
                    self.bump();
                    is_capability = true;
                }
                _ => break,
            }
        }
        if is_linear && is_copy {
            return Err(
                self.error_current("cannot combine `linear` and `copy` modifiers".to_string())
            );
        }
        if self.peek_kind() == Some(TokenKind::Extern) {
            if is_opaque || is_linear || is_copy || is_capability {
                return Err(self.error_current(
                    "linear/copy/opaque/capability applies only to struct declarations".to_string(),
                ));
            }
            return Ok(Item::ExternFunction(
                self.parse_extern_function(is_pub, doc)?,
            ));
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
            Some(TokenKind::Trait) => {
                if is_opaque || is_linear || is_copy || is_capability {
                    return Err(self.error_current(
                        "linear/copy/opaque/capability applies only to struct declarations"
                            .to_string(),
                    ));
                }
                Ok(Item::Trait(self.parse_trait(is_pub, doc)?))
            }
            Some(TokenKind::Impl) => {
                if is_pub {
                    return Err(self.error_current("impl blocks cannot be marked pub".to_string()));
                }
                if is_opaque || is_linear || is_copy || is_capability {
                    return Err(self.error_current(
                        "linear/copy/opaque/capability applies only to struct declarations"
                            .to_string(),
                    ));
                }
                Ok(Item::Impl(self.parse_impl_block(doc)?))
            }
            Some(other) => Err(self.error_current(format!("expected item, found {other:?}"))),
            None => Err(self.error_current("unexpected end of input".to_string())),
        }
    }

    pub(super) fn parse_impl_block(
        &mut self,
        impl_doc: Option<String>,
    ) -> Result<ImplBlock, ParseError> {
        let start = self.expect(TokenKind::Impl)?.span.start;
        let type_params = self.parse_type_params()?;
        let first_type = self.parse_type()?;
        let (trait_path, target) = if self.maybe_consume(TokenKind::For).is_some() {
            let trait_path = match first_type {
                Type::Path { path, args, .. } => {
                    if !args.is_empty() {
                        return Err(self.error_current(
                            "trait impls do not support type arguments yet".to_string(),
                        ));
                    }
                    path
                }
                _ => return Err(self.error_current("trait impls require a trait name".to_string())),
            };
            let target = self.parse_type()?;
            (Some(trait_path), target)
        } else {
            (None, first_type)
        };
        self.expect(TokenKind::LBrace)?;
        let mut methods = Vec::new();
        while self.peek_kind() != Some(TokenKind::RBrace) {
            let doc = self.take_doc_comments();
            let is_pub = self.maybe_consume(TokenKind::Pub).is_some();
            if self.peek_kind() != Some(TokenKind::Fn) {
                return Err(
                    self.error_current("expected method declaration in impl block".to_string())
                );
            }
            methods.push(self.parse_function(is_pub, doc)?);
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(ImplBlock {
            target,
            methods,
            type_params,
            trait_path,
            doc: impl_doc,
            span: Span::new(start, end),
        })
    }

    pub(super) fn parse_extern_function(
        &mut self,
        is_pub: bool,
        doc: Option<String>,
    ) -> Result<ExternFunction, ParseError> {
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

    pub(super) fn parse_function(
        &mut self,
        is_pub: bool,
        doc: Option<String>,
    ) -> Result<Function, ParseError> {
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
                    return Err(self.error_current("expected ':' after parameter name".to_string()));
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

    pub(super) fn parse_trait(
        &mut self,
        is_pub: bool,
        doc: Option<String>,
    ) -> Result<TraitDecl, ParseError> {
        let start = self.expect(TokenKind::Trait)?.span.start;
        let name = self.expect_ident()?;
        let type_params = self.parse_type_params()?;
        self.expect(TokenKind::LBrace)?;
        let mut methods = Vec::new();
        while self.peek_kind() != Some(TokenKind::RBrace) {
            let doc = self.take_doc_comments();
            if self.maybe_consume(TokenKind::Pub).is_some() {
                return Err(self.error_current("trait methods cannot be marked pub".to_string()));
            }
            methods.push(self.parse_trait_method(doc)?);
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(TraitDecl {
            name,
            type_params,
            methods,
            is_pub,
            doc,
            span: Span::new(start, end),
        })
    }

    pub(super) fn parse_trait_method(
        &mut self,
        doc: Option<String>,
    ) -> Result<TraitMethod, ParseError> {
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
                    return Err(self.error_current("expected ':' after parameter name".to_string()));
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
        let end = self.expect(TokenKind::Semi)?.span.end;
        Ok(TraitMethod {
            name,
            type_params,
            params,
            ret,
            doc,
            span: Span::new(start, end),
        })
    }

    pub(super) fn parse_struct(
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

    pub(super) fn parse_enum(
        &mut self,
        is_pub: bool,
        doc: Option<String>,
    ) -> Result<EnumDecl, ParseError> {
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
}
