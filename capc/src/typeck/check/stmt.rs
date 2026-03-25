use super::*;

/// Type-check a statement and update move state in the current scope.
pub(super) fn check_stmt(
    stmt: &Stmt,
    ret_ty: &Ty,
    functions: &HashMap<String, FunctionSig>,
    trait_map: &HashMap<String, TraitInfo>,
    trait_impls: &[TraitImplInfo],
    scopes: &mut Scopes,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
    type_params: &HashSet<String>,
    type_param_bounds: &HashMap<String, Vec<String>>,
    in_loop: bool,
) -> Result<(), TypeError> {
    let trusted_stdlib = module_name.starts_with("sys.");
    match stmt {
        Stmt::LetElse(_)
        | Stmt::TryLet(_)
        | Stmt::TryElse(_)
        | Stmt::ForEach(_) => {
            return Err(TypeError::new(
                "internal error: desugaring did not lower high-level statement".to_string(),
                stmt.span(),
            ));
        }
        Stmt::Let(let_stmt) => {
            if scopes.contains(&let_stmt.name.item) {
                return Err(TypeError::new(
                    format!(
                        "variable shadowing is not allowed: `{}`",
                        let_stmt.name.item
                    ),
                    let_stmt.name.span,
                ));
            }
            let annot_ref = let_stmt
                .ty
                .as_ref()
                .is_some_and(|ty| matches!(ty, Type::Ref { .. }));
            let expr_use_mode = if annot_ref {
                UseMode::Read
            } else {
                UseMode::Move
            };
            let expr_ty = if let Expr::Match(match_expr) = &let_stmt.expr {
                let expr_ty = check_match_expr_value(
                    match_expr,
                    functions,
                    trait_map,
                    trait_impls,
                    scopes,
                    expr_use_mode,
                    recorder,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                    type_params,
                    type_param_bounds,
                    in_loop,
                )?;
                recorder.record(&let_stmt.expr, &expr_ty);
                expr_ty
            } else {
                check_expr(
                    &let_stmt.expr,
                    functions,
                    trait_map,
                    trait_impls,
                    scopes,
                    expr_use_mode,
                    recorder,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                    type_params,
                    type_param_bounds,
                )?
            };
            let final_ty = if let Some(annot) = &let_stmt.ty {
                if let Some(span) = type_contains_ref(annot) {
                    match annot {
                        Type::Ref { target, .. } => {
                            if type_contains_ref(target).is_some() {
                                return Err(TypeError::new(
                                    "nested reference types are not allowed".to_string(),
                                    span,
                                ));
                            }
                        }
                        _ => {
                            return Err(TypeError::new(
                                "reference types are only allowed as direct local types"
                                    .to_string(),
                                span,
                            ));
                        }
                    }
                }
                let annot_ty = lower_type(annot, use_map, stdlib, type_params)?;
                validate_type_args(&annot_ty, struct_map, enum_map, annot.span())?;
                let matches_ref = if let Ty::Ref(inner) = &annot_ty {
                    &expr_ty == inner.as_ref() || &expr_ty == &annot_ty
                } else {
                    false
                };
                if annot_ty != expr_ty
                    && !matches_ref
                    && !matches!(expr_ty, Ty::Builtin(BuiltinType::Never))
                {
                    return Err(TypeError::new(
                        format!("type mismatch: expected {annot_ty:?}, found {expr_ty:?}"),
                        let_stmt.span,
                    ));
                }
                if matches!(annot_ty, Ty::Ref(_)) {
                    let Some((name, _span)) = leftmost_local_in_chain(&let_stmt.expr) else {
                        return Err(TypeError::new(
                            "reference locals must be initialized from a local value".to_string(),
                            let_stmt.expr.span(),
                        ));
                    };
                    if !scopes.contains(name) {
                        return Err(TypeError::new(
                            "reference locals must be initialized from a local value".to_string(),
                            let_stmt.expr.span(),
                        ));
                    }
                }
                annot_ty
            } else {
                if matches!(expr_ty, Ty::Ref(_)) {
                    let Some((name, _span)) = leftmost_local_in_chain(&let_stmt.expr) else {
                        return Err(TypeError::new(
                            "reference locals must be initialized from a local value".to_string(),
                            let_stmt.expr.span(),
                        ));
                    };
                    if !scopes.contains(name) {
                        return Err(TypeError::new(
                            "reference locals must be initialized from a local value".to_string(),
                            let_stmt.expr.span(),
                        ));
                    }
                }
                expr_ty
            };
            scopes.insert_local(let_stmt.name.item.clone(), final_ty);
        }
        Stmt::Assign(assign) => {
            let Some(existing) = scopes.lookup(&assign.name.item) else {
                return Err(TypeError::new(
                    format!("unknown identifier `{}`", assign.name.item),
                    assign.name.span,
                ));
            };
            let existing = existing.ty.clone();
            if matches!(existing, Ty::Ref(_)) {
                return Err(TypeError::new(
                    "cannot assign to a reference local".to_string(),
                    assign.span,
                ));
            }
            let expr_ty = check_expr(
                &assign.expr,
                functions,
                trait_map,
                trait_impls,
                scopes,
                UseMode::Move,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
                type_params,
                type_param_bounds,
            )?;
            if expr_ty != existing && !matches!(expr_ty, Ty::Builtin(BuiltinType::Never)) {
                return Err(TypeError::new(
                    format!("assignment type mismatch: expected {existing:?}, found {expr_ty:?}"),
                    assign.span,
                ));
            }
            scopes.assign(&assign.name.item, expr_ty);
        }
        Stmt::Defer(defer_stmt) => {
            match &defer_stmt.expr {
                Expr::Call(_) | Expr::MethodCall(_) => {}
                _ => {
                    return Err(TypeError::new(
                        "defer expects a function or method call".to_string(),
                        defer_stmt.span,
                    ))
                }
            }
            let _ = check_expr(
                &defer_stmt.expr,
                functions,
                trait_map,
                trait_impls,
                scopes,
                UseMode::Move,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
                type_params,
                type_param_bounds,
            )?;
        }
        Stmt::Return(ret_stmt) => {
            let expr_ty = if let Some(expr) = &ret_stmt.expr {
                check_expr(
                    expr,
                    functions,
                    trait_map,
                    trait_impls,
                    scopes,
                    UseMode::Move,
                    recorder,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                    type_params,
                    type_param_bounds,
                )?
            } else {
                Ty::Builtin(BuiltinType::Unit)
            };
            if &expr_ty != ret_ty {
                if matches!(expr_ty, Ty::Builtin(BuiltinType::Never)) {
                    if !trusted_stdlib {
                        ensure_linear_all_consumed(scopes, struct_map, enum_map, ret_stmt.span)?;
                    }
                    return Ok(());
                }
                return Err(TypeError::new(
                    format!("return type mismatch: expected {ret_ty:?}, found {expr_ty:?}"),
                    ret_stmt.span,
                ));
            }
            if !trusted_stdlib {
                ensure_linear_all_consumed(scopes, struct_map, enum_map, ret_stmt.span)?;
            }
        }
        Stmt::Break(break_stmt) => {
            if !in_loop {
                return Err(TypeError::new(
                    "break statement outside of loop".to_string(),
                    break_stmt.span,
                ));
            }
            let depth = scopes.current_loop_depth().ok_or_else(|| {
                TypeError::new(
                    "break statement outside of loop".to_string(),
                    break_stmt.span,
                )
            })?;
            if !trusted_stdlib {
                ensure_linear_scopes_consumed_from(
                    scopes,
                    depth,
                    struct_map,
                    enum_map,
                    break_stmt.span,
                )?;
            }
        }
        Stmt::Continue(continue_stmt) => {
            if !in_loop {
                return Err(TypeError::new(
                    "continue statement outside of loop".to_string(),
                    continue_stmt.span,
                ));
            }
            let depth = scopes.current_loop_depth().ok_or_else(|| {
                TypeError::new(
                    "continue statement outside of loop".to_string(),
                    continue_stmt.span,
                )
            })?;
            if !trusted_stdlib {
                ensure_linear_scopes_consumed_from(
                    scopes,
                    depth,
                    struct_map,
                    enum_map,
                    continue_stmt.span,
                )?;
            }
        }
        Stmt::If(if_stmt) => {
            let cond_ty = check_expr(
                &if_stmt.cond,
                functions,
                trait_map,
                trait_impls,
                scopes,
                UseMode::Read,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
                type_params,
                type_param_bounds,
            )?;
            if cond_ty != Ty::Builtin(BuiltinType::Bool) {
                return Err(TypeError::new(
                    "if condition must be bool".to_string(),
                    if_stmt.cond.span(),
                ));
            }
            let mut then_scopes = scopes.clone();
            check_block(
                &if_stmt.then_block,
                ret_ty,
                functions,
                trait_map,
                trait_impls,
                &mut then_scopes,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
                type_params,
                type_param_bounds,
                in_loop,
            )?;
            let mut else_scopes = scopes.clone();
            if let Some(block) = &if_stmt.else_block {
                check_block(
                    block,
                    ret_ty,
                    functions,
                    trait_map,
                    trait_impls,
                    &mut else_scopes,
                    recorder,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    module_name,
                    type_params,
                    type_param_bounds,
                    in_loop,
                )?;
            }
            if !trusted_stdlib {
                merge_branch_states(
                    scopes,
                    &then_scopes,
                    &else_scopes,
                    struct_map,
                    enum_map,
                    if_stmt.span,
                )?;
            }
        }
        Stmt::While(while_stmt) => {
            let cond_ty = check_expr(
                &while_stmt.cond,
                functions,
                trait_map,
                trait_impls,
                scopes,
                UseMode::Read,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
                type_params,
                type_param_bounds,
            )?;
            if cond_ty != Ty::Builtin(BuiltinType::Bool) {
                return Err(TypeError::new(
                    "while condition must be bool".to_string(),
                    while_stmt.cond.span(),
                ));
            }
            let mut body_scopes = scopes.clone();
            body_scopes.push_loop();
            check_block(
                &while_stmt.body,
                ret_ty,
                functions,
                trait_map,
                trait_impls,
                &mut body_scopes,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
                type_params,
                type_param_bounds,
                true,
            )?;
            body_scopes.pop_loop();
            if !trusted_stdlib {
                ensure_affine_states_match(
                    scopes,
                    &body_scopes,
                    struct_map,
                    enum_map,
                    while_stmt.span,
                )?;
            }
        }
        Stmt::For(for_stmt) => {
            let start_ty = check_expr(
                &for_stmt.start,
                functions,
                trait_map,
                trait_impls,
                scopes,
                UseMode::Read,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
                type_params,
                type_param_bounds,
            )?;
            if start_ty != Ty::Builtin(BuiltinType::I32) {
                return Err(TypeError::new(
                    "for loop range start must be i32".to_string(),
                    for_stmt.start.span(),
                ));
            }

            let end_ty = check_expr(
                &for_stmt.end,
                functions,
                trait_map,
                trait_impls,
                scopes,
                UseMode::Read,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
                type_params,
                type_param_bounds,
            )?;
            if end_ty != Ty::Builtin(BuiltinType::I32) {
                return Err(TypeError::new(
                    "for loop range end must be i32".to_string(),
                    for_stmt.end.span(),
                ));
            }

            let mut body_scopes = scopes.clone();
            body_scopes.push_scope();
            body_scopes.insert_local(for_stmt.var.item.clone(), Ty::Builtin(BuiltinType::I32));

            body_scopes.push_loop();
            check_block(
                &for_stmt.body,
                ret_ty,
                functions,
                trait_map,
                trait_impls,
                &mut body_scopes,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
                type_params,
                type_param_bounds,
                true,
            )?;
            body_scopes.pop_loop();
            body_scopes.pop_scope();

            if !trusted_stdlib {
                ensure_affine_states_match(
                    scopes,
                    &body_scopes,
                    struct_map,
                    enum_map,
                    for_stmt.span,
                )?;
            }
        }
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                let _ = check_match_stmt(
                    match_expr,
                    functions,
                    trait_map,
                    trait_impls,
                    scopes,
                    UseMode::Move,
                    recorder,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                    type_params,
                    type_param_bounds,
                    in_loop,
                )?;
            } else {
                check_expr(
                    &expr_stmt.expr,
                    functions,
                    trait_map,
                    trait_impls,
                    scopes,
                    UseMode::Move,
                    recorder,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                    type_params,
                    type_param_bounds,
                )?;
            }
        }
    }

    Ok(())
}

/// Type-check a block with a fresh lexical scope.
pub(super) fn check_block(
    block: &Block,
    ret_ty: &Ty,
    functions: &HashMap<String, FunctionSig>,
    trait_map: &HashMap<String, TraitInfo>,
    trait_impls: &[TraitImplInfo],
    scopes: &mut Scopes,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
    type_params: &HashSet<String>,
    type_param_bounds: &HashMap<String, Vec<String>>,
    in_loop: bool,
) -> Result<(), TypeError> {
    scopes.push_scope();
    for stmt in &block.stmts {
        check_stmt(
            stmt,
            ret_ty,
            functions,
            trait_map,
            trait_impls,
            scopes,
            recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
            type_params,
            type_param_bounds,
            in_loop,
        )?;
    }
    if !module_name.starts_with("sys.") {
        ensure_linear_scope_consumed(scopes, struct_map, enum_map, block.span)?;
    }
    scopes.pop_scope();
    Ok(())
}
