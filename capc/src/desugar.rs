use crate::ast::*;

pub fn desugar_module(module: &Module) -> Module {
    let mut ctx = DesugarCtx::new(module);
    ctx.desugar_module(module)
}

struct DesugarCtx {
    next_expr_id: u32,
}

impl DesugarCtx {
    fn new(module: &Module) -> Self {
        Self {
            next_expr_id: max_module_expr_id(module).saturating_add(1),
        }
    }

    fn fresh_expr_id(&mut self) -> ExprId {
        let id = ExprId(self.next_expr_id);
        self.next_expr_id += 1;
        id
    }

    fn desugar_module(&mut self, module: &Module) -> Module {
        Module {
            package: module.package,
            name: module.name.clone(),
            uses: module.uses.clone(),
            items: module
                .items
                .iter()
                .map(|item| self.desugar_item(item))
                .collect(),
            span: module.span,
        }
    }

    fn desugar_item(&mut self, item: &Item) -> Item {
        match item {
            Item::Function(func) => Item::Function(self.desugar_function(func)),
            Item::ExternFunction(func) => Item::ExternFunction(func.clone()),
            Item::Struct(decl) => Item::Struct(decl.clone()),
            Item::Enum(decl) => Item::Enum(decl.clone()),
            Item::Trait(decl) => Item::Trait(decl.clone()),
            Item::Impl(impl_block) => Item::Impl(self.desugar_impl_block(impl_block)),
        }
    }

    fn desugar_function(&mut self, func: &Function) -> Function {
        Function {
            name: func.name.clone(),
            type_params: func.type_params.clone(),
            params: func.params.clone(),
            ret: func.ret.clone(),
            body: self.desugar_block(&func.body),
            is_pub: func.is_pub,
            doc: func.doc.clone(),
            span: func.span,
        }
    }

    fn desugar_impl_block(&mut self, impl_block: &ImplBlock) -> ImplBlock {
        ImplBlock {
            type_params: impl_block.type_params.clone(),
            trait_path: impl_block.trait_path.clone(),
            target: impl_block.target.clone(),
            methods: impl_block
                .methods
                .iter()
                .map(|method| self.desugar_function(method))
                .collect(),
            doc: impl_block.doc.clone(),
            span: impl_block.span,
        }
    }

    fn desugar_block(&mut self, block: &Block) -> Block {
        let mut stmts = Vec::new();
        for stmt in &block.stmts {
            stmts.extend(self.desugar_stmt(stmt));
        }
        Block {
            stmts,
            span: block.span,
        }
    }

    fn desugar_stmt(&mut self, stmt: &Stmt) -> Vec<Stmt> {
        match stmt {
            Stmt::Let(let_stmt) => vec![Stmt::Let(LetStmt {
                name: let_stmt.name.clone(),
                ty: let_stmt.ty.clone(),
                expr: self.desugar_expr(&let_stmt.expr),
                span: let_stmt.span,
            })],
            Stmt::LetElse(let_else) => vec![Stmt::Let(self.lower_let_else(let_else))],
            Stmt::TryLet(try_let) => vec![Stmt::Let(self.lower_try_let(try_let))],
            Stmt::TryElse(try_else) => vec![Stmt::Expr(ExprStmt {
                expr: self.lower_try_else(try_else),
                span: try_else.span,
            })],
            Stmt::Assign(assign) => vec![Stmt::Assign(AssignStmt {
                name: assign.name.clone(),
                expr: self.desugar_expr(&assign.expr),
                span: assign.span,
            })],
            Stmt::Defer(defer_stmt) => vec![Stmt::Defer(DeferStmt {
                expr: self.desugar_expr(&defer_stmt.expr),
                span: defer_stmt.span,
            })],
            Stmt::Return(ret) => vec![Stmt::Return(ReturnStmt {
                expr: ret.expr.as_ref().map(|expr| self.desugar_expr(expr)),
                span: ret.span,
            })],
            Stmt::Break(break_stmt) => vec![Stmt::Break(break_stmt.clone())],
            Stmt::Continue(continue_stmt) => vec![Stmt::Continue(continue_stmt.clone())],
            Stmt::If(if_stmt) => vec![Stmt::If(IfStmt {
                cond: self.desugar_expr(&if_stmt.cond),
                then_block: self.desugar_block(&if_stmt.then_block),
                else_block: if_stmt.else_block.as_ref().map(|b| self.desugar_block(b)),
                span: if_stmt.span,
            })],
            Stmt::While(while_stmt) => vec![Stmt::While(WhileStmt {
                cond: self.desugar_expr(&while_stmt.cond),
                body: self.desugar_block(&while_stmt.body),
                span: while_stmt.span,
            })],
            Stmt::For(for_stmt) => vec![Stmt::For(ForStmt {
                var: for_stmt.var.clone(),
                start: self.desugar_expr(&for_stmt.start),
                end: self.desugar_expr(&for_stmt.end),
                body: self.desugar_block(&for_stmt.body),
                span: for_stmt.span,
            })],
            Stmt::ForEach(for_each) => vec![self.lower_for_each(for_each)],
            Stmt::Expr(expr_stmt) => vec![Stmt::Expr(ExprStmt {
                expr: self.desugar_expr(&expr_stmt.expr),
                span: expr_stmt.span,
            })],
        }
    }

    fn desugar_expr(&mut self, expr: &Expr) -> Expr {
        match expr {
            Expr::Literal(lit) => Expr::Literal(lit.clone()),
            Expr::Path(path) => Expr::Path(path.clone()),
            Expr::Call(call) => Expr::Call(CallExpr {
                id: call.id,
                callee: Box::new(self.desugar_expr(&call.callee)),
                type_args: call.type_args.clone(),
                args: call.args.iter().map(|arg| self.desugar_expr(arg)).collect(),
                span: call.span,
            }),
            Expr::MethodCall(method_call) => Expr::MethodCall(MethodCallExpr {
                id: method_call.id,
                receiver: Box::new(self.desugar_expr(&method_call.receiver)),
                method: method_call.method.clone(),
                type_args: method_call.type_args.clone(),
                args: method_call
                    .args
                    .iter()
                    .map(|arg| self.desugar_expr(arg))
                    .collect(),
                span: method_call.span,
            }),
            Expr::FieldAccess(field_access) => Expr::FieldAccess(FieldAccessExpr {
                id: field_access.id,
                object: Box::new(self.desugar_expr(&field_access.object)),
                field: field_access.field.clone(),
                span: field_access.span,
            }),
            Expr::Index(index_expr) => Expr::Index(IndexExpr {
                id: index_expr.id,
                object: Box::new(self.desugar_expr(&index_expr.object)),
                index: Box::new(self.desugar_expr(&index_expr.index)),
                span: index_expr.span,
            }),
            Expr::StructLiteral(lit) => Expr::StructLiteral(StructLiteralExpr {
                id: lit.id,
                path: lit.path.clone(),
                type_args: lit.type_args.clone(),
                fields: lit
                    .fields
                    .iter()
                    .map(|field| StructLiteralField {
                        name: field.name.clone(),
                        expr: self.desugar_expr(&field.expr),
                        span: field.span,
                    })
                    .collect(),
                span: lit.span,
            }),
            Expr::Unary(unary) => Expr::Unary(UnaryExpr {
                id: unary.id,
                op: unary.op.clone(),
                expr: Box::new(self.desugar_expr(&unary.expr)),
                span: unary.span,
            }),
            Expr::Binary(binary) => Expr::Binary(BinaryExpr {
                id: binary.id,
                op: binary.op.clone(),
                left: Box::new(self.desugar_expr(&binary.left)),
                right: Box::new(self.desugar_expr(&binary.right)),
                span: binary.span,
            }),
            Expr::Match(match_expr) => Expr::Match(MatchExpr {
                id: match_expr.id,
                expr: Box::new(self.desugar_expr(&match_expr.expr)),
                arms: match_expr
                    .arms
                    .iter()
                    .map(|arm| MatchArm {
                        pattern: arm.pattern.clone(),
                        body: self.desugar_block(&arm.body),
                        span: arm.span,
                    })
                    .collect(),
                span: match_expr.span,
                match_span: match_expr.match_span,
            }),
            Expr::Try(try_expr) => Expr::Try(TryExpr {
                id: try_expr.id,
                expr: Box::new(self.desugar_expr(&try_expr.expr)),
                span: try_expr.span,
            }),
            Expr::Grouping(group) => Expr::Grouping(GroupingExpr {
                id: group.id,
                expr: Box::new(self.desugar_expr(&group.expr)),
                span: group.span,
            }),
        }
    }

    fn lower_let_else(&mut self, stmt: &LetElseStmt) -> LetStmt {
        let binding = self
            .pattern_binding_ident(&stmt.pattern)
            .expect("parser validated let-else binding");
        let expr = self.desugar_expr(&stmt.expr);
        let else_block = self.desugar_block(&stmt.else_block);

        let binding_expr = self.ident_expr(&binding);
        let ok_body = Block {
            stmts: vec![Stmt::Expr(ExprStmt {
                expr: binding_expr,
                span: binding.span,
            })],
            span: binding.span,
        };

        let mut else_stmts = else_block.stmts;
        else_stmts.push(Stmt::Expr(ExprStmt {
            expr: self.panic_expr(stmt.else_block.span),
            span: stmt.else_block.span,
        }));
        let else_body = Block {
            stmts: else_stmts,
            span: stmt.else_block.span,
        };

        let match_span = Span::new(stmt.span.start, stmt.else_block.span.end);
        let match_expr = Expr::Match(MatchExpr {
            id: self.fresh_expr_id(),
            expr: Box::new(expr),
            arms: vec![
                MatchArm {
                    pattern: stmt.pattern.clone(),
                    body: ok_body,
                    span: match_span,
                },
                MatchArm {
                    pattern: Pattern::Wildcard(stmt.else_block.span),
                    body: else_body,
                    span: match_span,
                },
            ],
            span: match_span,
            match_span: stmt.span,
        });

        LetStmt {
            name: binding,
            ty: None,
            expr: match_expr,
            span: stmt.span,
        }
    }

    fn lower_try_let(&mut self, stmt: &TryLetStmt) -> LetStmt {
        let expr = self.desugar_expr(&stmt.expr);
        let else_block = self.desugar_block(&stmt.else_block);

        let binding_expr = self.ident_expr(&stmt.name);
        let ok_body = Block {
            stmts: vec![Stmt::Expr(ExprStmt {
                expr: binding_expr,
                span: stmt.name.span,
            })],
            span: stmt.name.span,
        };

        let mut else_stmts = else_block.stmts;
        else_stmts.push(Stmt::Expr(ExprStmt {
            expr: self.panic_expr(stmt.else_block.span),
            span: stmt.else_block.span,
        }));
        let else_body = Block {
            stmts: else_stmts,
            span: stmt.else_block.span,
        };

        let expr_span = expr.span();
        let ok_ident = Spanned::new("Ok".to_string(), expr_span);
        let err_ident = Spanned::new("Err".to_string(), stmt.else_block.span);
        let match_span = Span::new(stmt.span.start, stmt.else_block.span.end);
        let match_expr = Expr::Match(MatchExpr {
            id: self.fresh_expr_id(),
            expr: Box::new(expr),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Call {
                        path: Path {
                            id: self.fresh_expr_id(),
                            segments: vec![ok_ident],
                            span: expr_span,
                        },
                        binding: Some(stmt.name.clone()),
                        span: expr_span,
                    },
                    body: ok_body,
                    span: match_span,
                },
                MatchArm {
                    pattern: Pattern::Call {
                        path: Path {
                            id: self.fresh_expr_id(),
                            segments: vec![err_ident],
                            span: stmt.else_block.span,
                        },
                        binding: stmt.err_binding.clone(),
                        span: stmt.else_block.span,
                    },
                    body: else_body,
                    span: match_span,
                },
            ],
            span: match_span,
            match_span: stmt.span,
        });

        LetStmt {
            name: stmt.name.clone(),
            ty: stmt.ty.clone(),
            expr: match_expr,
            span: stmt.span,
        }
    }

    fn lower_try_else(&mut self, stmt: &TryElseStmt) -> Expr {
        let expr = self.desugar_expr(&stmt.expr);
        let else_block = self.desugar_block(&stmt.else_block);
        let expr_span = expr.span();
        let ok_ident = Spanned::new("Ok".to_string(), expr_span);
        let err_ident = Spanned::new("Err".to_string(), stmt.else_block.span);

        Expr::Match(MatchExpr {
            id: self.fresh_expr_id(),
            expr: Box::new(expr),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Call {
                        path: Path {
                            id: self.fresh_expr_id(),
                            segments: vec![ok_ident],
                            span: expr_span,
                        },
                        binding: None,
                        span: expr_span,
                    },
                    body: Block {
                        stmts: Vec::new(),
                        span: expr_span,
                    },
                    span: stmt.span,
                },
                MatchArm {
                    pattern: Pattern::Call {
                        path: Path {
                            id: self.fresh_expr_id(),
                            segments: vec![err_ident],
                            span: stmt.else_block.span,
                        },
                        binding: stmt.err_binding.clone(),
                        span: stmt.else_block.span,
                    },
                    body: else_block,
                    span: stmt.span,
                },
            ],
            span: stmt.span,
            match_span: expr_span,
        })
    }

    fn lower_for_each(&mut self, stmt: &ForEachStmt) -> Stmt {
        let body = self.desugar_block(&stmt.body);
        let source = self.desugar_expr(&stmt.source);

        let hidden_source_span = self.synthetic_span(stmt.span, 1);
        let hidden_len_span = self.synthetic_span(stmt.span, 2);
        let hidden_idx_span = self.synthetic_span(stmt.span, 3);
        let source_free_span = self.synthetic_span(stmt.span, 4);
        let len_call_span = self.synthetic_span(stmt.span, 5);
        let get_call_span = self.synthetic_span(stmt.span, 6);
        let try_span = self.synthetic_span(stmt.span, 7);
        let else_span = self.synthetic_span(stmt.span, 8);
        let zero_span = self.synthetic_span(stmt.span, 9);

        let hidden_source = self.synthetic_ident("__for_source", hidden_source_span);
        let hidden_idx = self.synthetic_ident("__for_idx", hidden_idx_span);
        let hidden_len = self.synthetic_ident("__for_len", hidden_len_span);
        let hidden_idx_expr = self.ident_expr(&hidden_idx);
        let hidden_len_expr = self.ident_expr(&hidden_len);

        let (source_expr, mut setup_stmts) = if source.to_path().is_some() {
            (source, Vec::new())
        } else {
            let hidden_source_expr = self.ident_expr(&hidden_source);
            let source_stmt = Stmt::Let(LetStmt {
                name: hidden_source.clone(),
                ty: None,
                expr: source,
                span: hidden_source_span,
            });
            let free_stmt = Stmt::Defer(DeferStmt {
                expr: self.method_call_expr(
                    hidden_source_expr.clone(),
                    "free",
                    Vec::new(),
                    source_free_span,
                ),
                span: source_free_span,
            });
            (hidden_source_expr, vec![source_stmt, free_stmt])
        };

        let len_stmt = Stmt::Let(LetStmt {
            name: hidden_len.clone(),
            ty: None,
            expr: self.method_call_expr(source_expr.clone(), "len", Vec::new(), len_call_span),
            span: hidden_len_span,
        });
        let get_expr = self.method_call_expr(
            source_expr,
            "get",
            vec![hidden_idx_expr.clone()],
            get_call_span,
        );
        let get_stmt = Stmt::Let(self.lower_try_let(&TryLetStmt {
            name: stmt.item.clone(),
            ty: None,
            expr: get_expr,
            err_binding: None,
            else_block: Block {
                stmts: Vec::new(),
                span: else_span,
            },
            span: try_span,
        }));

        let mut loop_stmts = Vec::new();
        if let Some(index_ident) = &stmt.index {
            loop_stmts.push(Stmt::Let(LetStmt {
                name: index_ident.clone(),
                ty: None,
                expr: hidden_idx_expr.clone(),
                span: index_ident.span,
            }));
        }
        loop_stmts.push(get_stmt);
        loop_stmts.extend(body.stmts);

        setup_stmts.push(len_stmt);
        setup_stmts.push(Stmt::For(ForStmt {
            var: hidden_idx,
            start: Expr::Literal(LiteralExpr {
                id: self.fresh_expr_id(),
                value: Literal::Int(0),
                span: zero_span,
            }),
            end: hidden_len_expr,
            body: Block {
                stmts: loop_stmts,
                span: stmt.body.span,
            },
            span: stmt.span,
        }));

        Stmt::If(IfStmt {
            cond: Expr::Literal(LiteralExpr {
                id: self.fresh_expr_id(),
                value: Literal::Bool(true),
                span: stmt.span,
            }),
            then_block: Block {
                stmts: setup_stmts,
                span: stmt.span,
            },
            else_block: None,
            span: stmt.span,
        })
    }

    fn pattern_binding_ident(&self, pattern: &Pattern) -> Option<Ident> {
        match pattern {
            Pattern::Binding(ident) => Some(ident.clone()),
            Pattern::Call {
                binding: Some(ident),
                ..
            } => Some(ident.clone()),
            _ => None,
        }
    }

    fn synthetic_ident(&self, prefix: &str, span: Span) -> Ident {
        Spanned::new(format!("{prefix}_{}", span.start), span)
    }

    fn synthetic_span(&self, base: Span, offset: usize) -> Span {
        let point = base.start.saturating_add(offset);
        Span::new(point, point)
    }

    fn ident_expr(&mut self, ident: &Ident) -> Expr {
        Expr::Path(Path {
            id: self.fresh_expr_id(),
            segments: vec![ident.clone()],
            span: ident.span,
        })
    }

    fn method_call_expr(
        &mut self,
        receiver: Expr,
        method: &str,
        args: Vec<Expr>,
        span: Span,
    ) -> Expr {
        Expr::MethodCall(MethodCallExpr {
            id: self.fresh_expr_id(),
            receiver: Box::new(receiver),
            method: Spanned::new(method.to_string(), span),
            type_args: Vec::new(),
            args,
            span,
        })
    }

    fn panic_expr(&mut self, span: Span) -> Expr {
        let panic_ident = Spanned::new("panic".to_string(), span);
        Expr::Call(CallExpr {
            id: self.fresh_expr_id(),
            callee: Box::new(Expr::Path(Path {
                id: self.fresh_expr_id(),
                segments: vec![panic_ident],
                span,
            })),
            type_args: Vec::new(),
            args: Vec::new(),
            span,
        })
    }
}

fn max_module_expr_id(module: &Module) -> u32 {
    let mut max_id = module.name.id.0;
    for use_decl in &module.uses {
        max_id = max_id.max(use_decl.path.id.0);
    }
    for item in &module.items {
        max_id = max_id.max(max_item_expr_id(item));
    }
    max_id
}

fn max_item_expr_id(item: &Item) -> u32 {
    match item {
        Item::Function(func) => max_block_expr_id(&func.body),
        Item::ExternFunction(_) => 0,
        Item::Struct(decl) => max_struct_expr_id(decl),
        Item::Enum(decl) => max_enum_expr_id(decl),
        Item::Trait(decl) => max_trait_expr_id(decl),
        Item::Impl(impl_block) => {
            let mut max_id = max_type_expr_id(&impl_block.target);
            if let Some(path) = &impl_block.trait_path {
                max_id = max_id.max(path.id.0);
            }
            for method in &impl_block.methods {
                max_id = max_id.max(max_block_expr_id(&method.body));
                max_id = max_id.max(max_type_expr_id(&method.ret));
                for param in &method.params {
                    if let Some(ty) = &param.ty {
                        max_id = max_id.max(max_type_expr_id(ty));
                    }
                }
            }
            max_id
        }
    }
}

fn max_struct_expr_id(decl: &StructDecl) -> u32 {
    let mut max_id = 0;
    for param in &decl.type_params {
        for bound in &param.bounds {
            max_id = max_id.max(bound.id.0);
        }
    }
    for field in &decl.fields {
        max_id = max_id.max(max_type_expr_id(&field.ty));
    }
    max_id
}

fn max_enum_expr_id(decl: &EnumDecl) -> u32 {
    let mut max_id = 0;
    for param in &decl.type_params {
        for bound in &param.bounds {
            max_id = max_id.max(bound.id.0);
        }
    }
    for variant in &decl.variants {
        if let Some(payload) = &variant.payload {
            max_id = max_id.max(max_type_expr_id(payload));
        }
    }
    max_id
}

fn max_trait_expr_id(decl: &TraitDecl) -> u32 {
    let mut max_id = 0;
    for param in &decl.type_params {
        for bound in &param.bounds {
            max_id = max_id.max(bound.id.0);
        }
    }
    for method in &decl.methods {
        max_id = max_id.max(max_type_expr_id(&method.ret));
        for param in &method.params {
            if let Some(ty) = &param.ty {
                max_id = max_id.max(max_type_expr_id(ty));
            }
        }
    }
    max_id
}

fn max_block_expr_id(block: &Block) -> u32 {
    let mut max_id = 0;
    for stmt in &block.stmts {
        max_id = max_id.max(max_stmt_expr_id(stmt));
    }
    max_id
}

fn max_stmt_expr_id(stmt: &Stmt) -> u32 {
    match stmt {
        Stmt::Let(let_stmt) => max_expr_id(&let_stmt.expr),
        Stmt::LetElse(stmt) => max_expr_id(&stmt.expr).max(max_block_expr_id(&stmt.else_block)),
        Stmt::TryLet(stmt) => max_expr_id(&stmt.expr).max(max_block_expr_id(&stmt.else_block)),
        Stmt::TryElse(stmt) => max_expr_id(&stmt.expr).max(max_block_expr_id(&stmt.else_block)),
        Stmt::Assign(assign) => max_expr_id(&assign.expr),
        Stmt::Defer(defer_stmt) => max_expr_id(&defer_stmt.expr),
        Stmt::Return(ret) => ret.expr.as_ref().map(max_expr_id).unwrap_or(0),
        Stmt::Break(_) | Stmt::Continue(_) => 0,
        Stmt::If(if_stmt) => {
            let mut max_id = max_expr_id(&if_stmt.cond).max(max_block_expr_id(&if_stmt.then_block));
            if let Some(else_block) = &if_stmt.else_block {
                max_id = max_id.max(max_block_expr_id(else_block));
            }
            max_id
        }
        Stmt::While(while_stmt) => {
            max_expr_id(&while_stmt.cond).max(max_block_expr_id(&while_stmt.body))
        }
        Stmt::For(for_stmt) => max_expr_id(&for_stmt.start)
            .max(max_expr_id(&for_stmt.end))
            .max(max_block_expr_id(&for_stmt.body)),
        Stmt::ForEach(stmt) => max_expr_id(&stmt.source).max(max_block_expr_id(&stmt.body)),
        Stmt::Expr(expr_stmt) => max_expr_id(&expr_stmt.expr),
    }
}

fn max_expr_id(expr: &Expr) -> u32 {
    match expr {
        Expr::Literal(lit) => lit.id.0,
        Expr::Path(path) => path.id.0,
        Expr::Call(call) => {
            let mut max_id = call.id.0.max(max_expr_id(&call.callee));
            for arg in &call.args {
                max_id = max_id.max(max_expr_id(arg));
            }
            for arg in &call.type_args {
                max_id = max_id.max(max_type_expr_id(arg));
            }
            max_id
        }
        Expr::MethodCall(method_call) => {
            let mut max_id = method_call.id.0.max(max_expr_id(&method_call.receiver));
            for arg in &method_call.args {
                max_id = max_id.max(max_expr_id(arg));
            }
            for arg in &method_call.type_args {
                max_id = max_id.max(max_type_expr_id(arg));
            }
            max_id
        }
        Expr::FieldAccess(field_access) => field_access.id.0.max(max_expr_id(&field_access.object)),
        Expr::Index(index_expr) => index_expr
            .id
            .0
            .max(max_expr_id(&index_expr.object))
            .max(max_expr_id(&index_expr.index)),
        Expr::StructLiteral(lit) => {
            let mut max_id = lit.id.0.max(lit.path.id.0);
            for arg in &lit.type_args {
                max_id = max_id.max(max_type_expr_id(arg));
            }
            for field in &lit.fields {
                max_id = max_id.max(max_expr_id(&field.expr));
            }
            max_id
        }
        Expr::Unary(unary) => unary.id.0.max(max_expr_id(&unary.expr)),
        Expr::Binary(binary) => binary
            .id
            .0
            .max(max_expr_id(&binary.left))
            .max(max_expr_id(&binary.right)),
        Expr::Match(match_expr) => {
            let mut max_id = match_expr.id.0.max(max_expr_id(&match_expr.expr));
            for arm in &match_expr.arms {
                max_id = max_id.max(max_pattern_expr_id(&arm.pattern));
                max_id = max_id.max(max_block_expr_id(&arm.body));
            }
            max_id
        }
        Expr::Try(try_expr) => try_expr.id.0.max(max_expr_id(&try_expr.expr)),
        Expr::Grouping(group) => group.id.0.max(max_expr_id(&group.expr)),
    }
}

fn max_pattern_expr_id(pattern: &Pattern) -> u32 {
    match pattern {
        Pattern::Wildcard(_) | Pattern::Binding(_) | Pattern::Literal(_) => 0,
        Pattern::Path(path) => path.id.0,
        Pattern::Call { path, .. } => path.id.0,
    }
}

fn max_type_expr_id(ty: &Type) -> u32 {
    match ty {
        Type::Path { path, args, .. } => {
            let mut max_id = path.id.0;
            for arg in args {
                max_id = max_id.max(max_type_expr_id(arg));
            }
            max_id
        }
        Type::Ptr { target, .. } | Type::Ref { target, .. } => max_type_expr_id(target),
    }
}
