use super::*;

struct CheckedMatchArm {
    ty: Ty,
    scope_after: Option<Scopes>,
}

fn check_match_arms(
    match_expr: &MatchExpr,
    match_ty: &Ty,
    functions: &HashMap<String, FunctionSig>,
    trait_map: &HashMap<String, TraitInfo>,
    trait_impls: &[TraitImplInfo],
    scopes: &Scopes,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
    type_params: &HashSet<String>,
    type_param_bounds: &HashMap<String, Vec<String>>,
    in_loop: bool,
    expression_mode: bool,
) -> Result<Vec<CheckedMatchArm>, TypeError> {
    let mut arms = Vec::with_capacity(match_expr.arms.len());
    for arm in &match_expr.arms {
        let mut arm_scope = scopes.clone();
        arm_scope.push_scope();
        bind_pattern(
            &arm.pattern,
            match_ty,
            &mut arm_scope,
            use_map,
            enum_map,
            module_name,
        )?;
        let arm_ty = if expression_mode {
            check_match_arm_value(
                &arm.body,
                functions,
                trait_map,
                trait_impls,
                &mut arm_scope,
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
            )?
        } else {
            check_block(
                &arm.body,
                ret_ty,
                functions,
                trait_map,
                trait_impls,
                &mut arm_scope,
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
            Ty::Builtin(BuiltinType::Unit)
        };
        arm_scope.pop_scope();
        let scope_after = if matches!(arm_ty, Ty::Builtin(BuiltinType::Never)) {
            None
        } else {
            Some(arm_scope)
        };
        arms.push(CheckedMatchArm {
            ty: arm_ty,
            scope_after,
        });
    }
    Ok(arms)
}

/// Check a statement-form match (arms may return, no value required).
pub(super) fn check_match_stmt(
    match_expr: &MatchExpr,
    functions: &HashMap<String, FunctionSig>,
    trait_map: &HashMap<String, TraitInfo>,
    trait_impls: &[TraitImplInfo],
    scopes: &mut Scopes,
    scrutinee_mode: UseMode,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
    type_params: &HashSet<String>,
    type_param_bounds: &HashMap<String, Vec<String>>,
    in_loop: bool,
) -> Result<Ty, TypeError> {
    let match_ty = check_expr(
        &match_expr.expr,
        functions,
        trait_map,
        trait_impls,
        scopes,
        scrutinee_mode,
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
    let arms = check_match_arms(
        match_expr,
        &match_ty,
        functions,
        trait_map,
        trait_impls,
        scopes,
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
        false,
    )?;
    let arm_scopes = arms
        .into_iter()
        .filter_map(|arm| arm.scope_after)
        .collect::<Vec<_>>();
    check_match_exhaustive(
        &match_ty,
        &match_expr.arms,
        use_map,
        enum_map,
        module_name,
        match_expr.match_span,
    )?;
    if !module_name.starts_with("sys.") && !arm_scopes.is_empty() {
        merge_match_states(scopes, &arm_scopes, struct_map, enum_map, match_expr.span)?;
    }
    Ok(Ty::Builtin(BuiltinType::Unit))
}

/// Check an expression-form match (all arms must evaluate to the same type).
pub(super) fn check_match_expr_value(
    match_expr: &MatchExpr,
    functions: &HashMap<String, FunctionSig>,
    trait_map: &HashMap<String, TraitInfo>,
    trait_impls: &[TraitImplInfo],
    scopes: &mut Scopes,
    scrutinee_mode: UseMode,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
    type_params: &HashSet<String>,
    type_param_bounds: &HashMap<String, Vec<String>>,
    in_loop: bool,
) -> Result<Ty, TypeError> {
    let match_ty = check_expr(
        &match_expr.expr,
        functions,
        trait_map,
        trait_impls,
        scopes,
        scrutinee_mode,
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
    let arms = check_match_arms(
        match_expr,
        &match_ty,
        functions,
        trait_map,
        trait_impls,
        scopes,
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
        true,
    )?;
    let mut result_ty: Option<Ty> = None;
    let mut arm_scopes = Vec::new();
    for arm in arms {
        if let Some(scope_after) = arm.scope_after {
            arm_scopes.push(scope_after);
        }
        if let Some(prev) = &result_ty {
            if matches!(prev, Ty::Builtin(BuiltinType::Never)) {
                result_ty = Some(arm.ty);
            } else if matches!(arm.ty, Ty::Builtin(BuiltinType::Never)) {
                // Keep the previous type; never can coerce to any type.
            } else if prev != &arm.ty {
                return Err(TypeError::new(
                    format!("match arm type mismatch: expected {prev:?}, found {:?}",
                        arm.ty),
                    match_expr.span,
                ));
            }
        } else {
            result_ty = Some(arm.ty);
        }
    }
    check_match_exhaustive(
        &match_ty,
        &match_expr.arms,
        use_map,
        enum_map,
        module_name,
        match_expr.match_span,
    )?;
    if !module_name.starts_with("sys.") && !arm_scopes.is_empty() {
        merge_match_states(scopes, &arm_scopes, struct_map, enum_map, match_expr.span)?;
    }
    Ok(result_ty.unwrap_or(Ty::Builtin(BuiltinType::Unit)))
}

/// Check a single match arm in expression context.
fn check_match_arm_value(
    block: &Block,
    functions: &HashMap<String, FunctionSig>,
    trait_map: &HashMap<String, TraitInfo>,
    trait_impls: &[TraitImplInfo],
    scopes: &mut Scopes,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
    type_params: &HashSet<String>,
    type_param_bounds: &HashMap<String, Vec<String>>,
    in_loop: bool,
) -> Result<Ty, TypeError> {
    let Some((last, prefix)) = block.stmts.split_last() else {
        return Err(TypeError::new(
            "match arm must end with expression".to_string(),
            block.span,
        ));
    };
    for stmt in prefix {
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
    match last {
        Stmt::Expr(expr_stmt) => check_expr(
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
        ),
        _ => Err(TypeError::new(
            "match arm must end with expression".to_string(),
            block.span,
        )),
    }
}

fn check_match_exhaustive(
    match_ty: &Ty,
    arms: &[MatchArm],
    use_map: &UseMap,
    enum_map: &HashMap<String, EnumInfo>,
    module_name: &str,
    span: Span,
) -> Result<(), TypeError> {
    if arms
        .iter()
        .any(|arm| matches!(arm.pattern, Pattern::Wildcard(_) | Pattern::Binding(_)))
    {
        return Ok(());
    }

    match match_ty {
        Ty::Builtin(BuiltinType::Bool) => {
            let mut seen_true = false;
            let mut seen_false = false;
            for arm in arms {
                if let Pattern::Literal(Literal::Bool(value)) = arm.pattern {
                    if value {
                        seen_true = true;
                    } else {
                        seen_false = true;
                    }
                }
            }
            if seen_true && seen_false {
                return Ok(());
            }
            let mut missing = Vec::new();
            if !seen_true {
                missing.push("true");
            }
            if !seen_false {
                missing.push("false");
            }
            return Err(TypeError::new(
                format!(
                    "non-exhaustive match on bool, missing: {}",
                    missing.join(", ")
                ),
                span,
            ));
        }
        Ty::Path(name, args) if name == "sys.result.Result" && args.len() == 2 => {
            let mut seen_ok = false;
            let mut seen_err = false;
            for arm in arms {
                if let Pattern::Call { path, .. } = &arm.pattern {
                    if path.segments.len() == 1 {
                        let variant = path.segments[0].item.as_str();
                        if variant == "Ok" {
                            seen_ok = true;
                        } else if variant == "Err" {
                            seen_err = true;
                        }
                    }
                }
            }
            if seen_ok && seen_err {
                return Ok(());
            }
            let mut missing = Vec::new();
            if !seen_ok {
                missing.push("Ok");
            }
            if !seen_err {
                missing.push("Err");
            }
            return Err(TypeError::new(
                format!(
                    "non-exhaustive match on Result, missing: {}",
                    missing.join(", ")
                ),
                span,
            ));
        }
        Ty::Path(name, _) => {
            let info = enum_map.get(name).or_else(|| {
                if name.contains('.') {
                    None
                } else {
                    enum_map.get(&format!("{module_name}.{name}"))
                }
            });
            let Some(info) = info else {
                return Ok(());
            };
            let mut seen = HashSet::new();
            for arm in arms {
                let path = match &arm.pattern {
                    Pattern::Path(path) => Some(path),
                    Pattern::Call { path, .. } => Some(path),
                    _ => None,
                };
                if let Some(path) = path {
                    if let Some(ty) = resolve_enum_variant(path, use_map, enum_map, module_name) {
                        if same_type_constructor(&ty, match_ty) {
                            if let Some(seg) = path.segments.last() {
                                seen.insert(seg.item.clone());
                            }
                        }
                    }
                }
            }
            if info.variants.iter().all(|v| seen.contains(v)) {
                return Ok(());
            }
            let missing: Vec<String> = info
                .variants
                .iter()
                .filter(|v| !seen.contains(*v))
                .cloned()
                .collect();
            return Err(TypeError::new(
                format!(
                    "non-exhaustive match, missing variants: {}",
                    missing.join(", ")
                ),
                span,
            ));
        }
        _ => {}
    }

    Ok(())
}

fn same_type_constructor(left: &Ty, right: &Ty) -> bool {
    match (left, right) {
        (Ty::Path(left_name, _), Ty::Path(right_name, _)) => left_name == right_name,
        _ => left == right,
    }
}
