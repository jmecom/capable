use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::error::TypeError;

use super::{
    build_type_params, is_affine_type, is_numeric_type, is_orderable_type, is_string_ty,
    lower_type, resolve_enum_variant, resolve_method_target, resolve_path, resolve_type_name,
    stdlib_string_ty, type_contains_ref, type_kind, validate_type_args, BuiltinType, EnumInfo,
    FunctionSig, MoveState, Scopes, SpanExt, StdlibIndex, StructInfo, Ty, TypeKind, TypeTable,
    UseMap, UseMode,
};

/// Optional recorder for expression types during checking.
pub(super) struct TypeRecorder<'a> {
    table: Option<&'a mut TypeTable>,
}

impl<'a> TypeRecorder<'a> {
    pub(super) fn new(table: Option<&'a mut TypeTable>) -> Self {
        Self { table }
    }

    pub(super) fn record(&mut self, expr: &Expr, ty: &Ty) {
        if let Some(table) = self.table.as_deref_mut() {
            table.record(expr.span(), ty.clone());
        }
    }
}

fn record_expr_type(
    recorder: &mut TypeRecorder,
    expr: &Expr,
    ty: Ty,
) -> Result<Ty, TypeError> {
    recorder.record(expr, &ty);
    Ok(ty)
}

fn infer_enum_args(template: &Ty, actual: &Ty, inferred: &mut HashMap<String, Ty>) -> bool {
    match template {
        Ty::Param(name) => match inferred.get(name) {
            Some(existing) => existing == actual,
            None => {
                inferred.insert(name.clone(), actual.clone());
                true
            }
        },
        Ty::Builtin(b) => matches!(actual, Ty::Builtin(other) if other == b),
        Ty::Ptr(inner) => matches!(actual, Ty::Ptr(other) if infer_enum_args(inner, other, inferred)),
        Ty::Ref(inner) => matches!(actual, Ty::Ref(other) if infer_enum_args(inner, other, inferred)),
        Ty::Path(name, args) => match actual {
            Ty::Path(other_name, other_args) if other_name == name && args.len() == other_args.len() => {
                args.iter()
                    .zip(other_args.iter())
                    .all(|(a, b)| infer_enum_args(a, b, inferred))
            }
            _ => false,
        },
    }
}

fn resolve_enum_type_args(
    enum_name: &str,
    type_params: &[String],
    inferred: &HashMap<String, Ty>,
    ret_ty: &Ty,
) -> Vec<Ty> {
    if type_params.is_empty() {
        return Vec::new();
    }
    let ret_args = match ret_ty {
        Ty::Path(ret_name, args) if ret_name == enum_name && args.len() == type_params.len() => {
            Some(args)
        }
        _ => None,
    };
    type_params
        .iter()
        .enumerate()
        .map(|(idx, param)| {
            if let Some(ty) = inferred.get(param) {
                return ty.clone();
            }
            if let Some(args) = ret_args {
                return args[idx].clone();
            }
            Ty::Builtin(BuiltinType::Unit)
        })
        .collect()
}

fn apply_enum_type_args(ty: &Ty, type_params: &[String], type_args: &[Ty]) -> Ty {
    match ty {
        Ty::Param(name) => {
            if let Some(idx) = type_params.iter().position(|p| p == name) {
                return type_args.get(idx).cloned().unwrap_or_else(|| ty.clone());
            }
            ty.clone()
        }
        Ty::Builtin(_) => ty.clone(),
        Ty::Ptr(inner) => Ty::Ptr(Box::new(apply_enum_type_args(inner, type_params, type_args))),
        Ty::Ref(inner) => Ty::Ref(Box::new(apply_enum_type_args(inner, type_params, type_args))),
        Ty::Path(name, args) => Ty::Path(
            name.clone(),
            args.iter()
                .map(|arg| apply_enum_type_args(arg, type_params, type_args))
                .collect(),
        ),
    }
}

fn enum_payload_matches(payload: &Ty, arg_ty: &Ty, type_params: &[String], type_args: &[Ty]) -> bool {
    let expected = apply_enum_type_args(payload, type_params, type_args);
    &expected == arg_ty
}

/// Safe packages cannot mention externs or raw pointer types anywhere.
pub(super) fn validate_package_safety(module: &Module) -> Result<(), TypeError> {
    if module.package != PackageSafety::Safe {
        return Ok(());
    }
    for item in &module.items {
        match item {
            Item::ExternFunction(func) => {
                return Err(TypeError::new(
                    "extern declarations require `package unsafe`".to_string(),
                    func.span,
                ));
            }
            Item::Function(func) => {
                if let Some(span) = type_contains_ptr_fn(func) {
                    return Err(TypeError::new(
                        "raw pointer types require `package unsafe`".to_string(),
                        span,
                    ));
                }
            }
            Item::Impl(impl_block) => {
                for method in &impl_block.methods {
                    if let Some(span) = type_contains_ptr_fn(method) {
                        return Err(TypeError::new(
                            "raw pointer types require `package unsafe`".to_string(),
                            span,
                        ));
                    }
                }
            }
            Item::Struct(decl) => {
                if let Some(span) = type_contains_ptr_struct(decl) {
                    return Err(TypeError::new(
                        "raw pointer types require `package unsafe`".to_string(),
                        span,
                    ));
                }
            }
            Item::Enum(decl) => {
                if let Some(span) = type_contains_ptr_enum(decl) {
                    return Err(TypeError::new(
                        "raw pointer types require `package unsafe`".to_string(),
                        span,
                    ));
                }
            }
        }
    }
    Ok(())
}

fn type_contains_ptr(ty: &Type) -> Option<Span> {
    match ty {
        Type::Ptr { span, .. } => Some(*span),
        Type::Ref { target, .. } => type_contains_ptr(target),
        Type::Path { args, .. } => {
            for arg in args {
                if let Some(span) = type_contains_ptr(arg) {
                    return Some(span);
                }
            }
            None
        }
    }
}

fn type_contains_ptr_fn(func: &Function) -> Option<Span> {
    for param in &func.params {
        if let Some(ty) = &param.ty {
            if let Some(span) = type_contains_ptr(ty) {
                return Some(span);
            }
        }
    }
    if let Some(span) = type_contains_ptr(&func.ret) {
        return Some(span);
    }
    block_contains_ptr(&func.body)
}

fn type_contains_ptr_struct(decl: &StructDecl) -> Option<Span> {
    for field in &decl.fields {
        if let Some(span) = type_contains_ptr(&field.ty) {
            return Some(span);
        }
    }
    None
}

fn type_contains_ptr_enum(decl: &EnumDecl) -> Option<Span> {
    for variant in &decl.variants {
        if let Some(payload) = &variant.payload {
            if let Some(span) = type_contains_ptr(payload) {
                return Some(span);
            }
        }
    }
    None
}

fn block_contains_ptr(block: &Block) -> Option<Span> {
    for stmt in &block.stmts {
        match stmt {
            Stmt::Let(let_stmt) => {
                if let Some(ty) = &let_stmt.ty {
                    if let Some(span) = type_contains_ptr(ty) {
                        return Some(span);
                    }
                }
            }
            Stmt::Assign(_) => {}
            Stmt::Defer(_) => {}
            Stmt::Break(_) => {}
            Stmt::Continue(_) => {}
            Stmt::If(if_stmt) => {
                if let Some(span) = block_contains_ptr(&if_stmt.then_block) {
                    return Some(span);
                }
                if let Some(span) = if_stmt
                    .else_block
                    .as_ref()
                    .and_then(block_contains_ptr)
                {
                    return Some(span);
                }
            }
            Stmt::While(while_stmt) => {
                if let Some(span) = block_contains_ptr(&while_stmt.body) {
                    return Some(span);
                }
            }
            Stmt::For(for_stmt) => {
                if let Some(span) = block_contains_ptr(&for_stmt.body) {
                    return Some(span);
                }
            }
            Stmt::Expr(expr_stmt) => {
                if let Expr::Match(match_expr) = &expr_stmt.expr {
                    for arm in &match_expr.arms {
                        if let Some(span) = block_contains_ptr(&arm.body) {
                            return Some(span);
                        }
                    }
                }
            }
            Stmt::Return(_) => {}
        }
    }
    None
}

/// Check if a statement is syntactically total (always returns).
/// This is a purely syntactic check, not real control-flow analysis.
fn stmt_is_total(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Return(ret_stmt) => ret_stmt.expr.is_some(),
        Stmt::Defer(_) => false,
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                match_is_total(match_expr)
            } else {
                false
            }
        }
        Stmt::If(if_stmt) => {
            if let Some(else_block) = &if_stmt.else_block {
                block_ends_with_return(&if_stmt.then_block) && block_ends_with_return(else_block)
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if a block ends with a syntactically total statement.
fn block_ends_with_return(block: &Block) -> bool {
    block.stmts.last().map_or(false, stmt_is_total)
}

/// Check if a match expression is syntactically total (all arms end with return).
fn match_is_total(match_expr: &MatchExpr) -> bool {
    !match_expr.arms.is_empty() && match_expr.arms.iter().all(|arm| block_ends_with_return(&arm.body))
}

/// Type-check a function body, including move/linear rules.
pub(super) fn check_function(
    func: &Function,
    functions: &HashMap<String, FunctionSig>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
    type_table: Option<&mut TypeTable>,
) -> Result<(), TypeError> {
    let type_params = build_type_params(&func.type_params)?;
    let mut params_map = HashMap::new();
    for param in &func.params {
        let Some(ty) = &param.ty else {
            return Err(TypeError::new(
                format!("parameter `{}` requires a type annotation", param.name.item),
                param.name.span,
            ));
        };
        if let Some(span) = type_contains_ref(ty) {
            match ty {
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
                        "reference types are only allowed as direct parameter types".to_string(),
                        span,
                    ));
                }
            }
        }
        let ty = lower_type(ty, use_map, stdlib, &type_params)?;
        validate_type_args(&ty, struct_map, enum_map, param.ty.as_ref().unwrap().span())?;
        params_map.insert(param.name.item.clone(), ty);
    }
    let mut scopes = Scopes::from_flat_map(params_map);
    let mut recorder = TypeRecorder::new(type_table);

    let ret_ty = lower_type(&func.ret, use_map, stdlib, &type_params)?;
    validate_type_args(&ret_ty, struct_map, enum_map, func.ret.span())?;
    if let Some(span) = type_contains_ref(&func.ret) {
        return Err(TypeError::new(
            "reference types cannot be returned".to_string(),
            span,
        ));
    }

    for stmt in &func.body.stmts {
        check_stmt(
            stmt,
            &ret_ty,
            functions,
            &mut scopes,
            &mut recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
            &type_params,
            false, // not inside a loop at function top level
        )?;
    }

    if ret_ty != Ty::Builtin(BuiltinType::Unit) {
        if let Some(last_stmt) = func.body.stmts.last() {
            if !stmt_is_total(last_stmt) {
                return Err(TypeError::new(
                    "expected return <expr>; as the final statement of this function".to_string(),
                    last_stmt.span(),
                ));
            }
        } else {
            return Err(TypeError::new(
                "expected return <expr>; as the final statement of this function".to_string(),
                func.body.span,
            ));
        }
    }

    ensure_linear_all_consumed(&scopes, struct_map, enum_map, func.body.span)?;

    Ok(())
}

/// Type-check a statement and update move state in the current scope.
fn check_stmt(
    stmt: &Stmt,
    ret_ty: &Ty,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
    type_params: &HashSet<String>,
    in_loop: bool,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Let(let_stmt) => {
            if scopes.contains(&let_stmt.name.item) {
                return Err(TypeError::new(
                    format!("variable shadowing is not allowed: `{}`", let_stmt.name.item),
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
            let expr_ty = check_expr(
                &let_stmt.expr,
                functions,
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
            )?;
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
                validate_type_args( &annot_ty, struct_map, enum_map, annot.span())?;
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
            )?;
            if expr_ty != existing && !matches!(expr_ty, Ty::Builtin(BuiltinType::Never)) {
                return Err(TypeError::new(
                    format!(
                        "assignment type mismatch: expected {existing:?}, found {expr_ty:?}"
                    ),
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
            )?;
        }
        Stmt::Return(ret_stmt) => {
            let expr_ty = if let Some(expr) = &ret_stmt.expr {
                check_expr(
                    expr,
                    functions,
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
                )?
            } else {
                Ty::Builtin(BuiltinType::Unit)
            };
            if &expr_ty != ret_ty {
                if matches!(expr_ty, Ty::Builtin(BuiltinType::Never)) {
                    ensure_linear_all_consumed(scopes, struct_map, enum_map, ret_stmt.span)?;
                    return Ok(());
                }
                return Err(TypeError::new(
                    format!("return type mismatch: expected {ret_ty:?}, found {expr_ty:?}"),
                    ret_stmt.span,
                ));
            }
            ensure_linear_all_consumed(scopes, struct_map, enum_map, ret_stmt.span)?;
        }
        Stmt::Break(break_stmt) => {
            if !in_loop {
                return Err(TypeError::new(
                    "break statement outside of loop".to_string(),
                    break_stmt.span,
                ));
            }
            let depth = scopes.current_loop_depth().ok_or_else(|| {
                TypeError::new("break statement outside of loop".to_string(), break_stmt.span)
            })?;
            ensure_linear_scopes_consumed_from(scopes, depth, struct_map, enum_map, break_stmt.span)?;
        }
        Stmt::Continue(continue_stmt) => {
            if !in_loop {
                return Err(TypeError::new(
                    "continue statement outside of loop".to_string(),
                    continue_stmt.span,
                ));
            }
            let depth = scopes.current_loop_depth().ok_or_else(|| {
                TypeError::new("continue statement outside of loop".to_string(), continue_stmt.span)
            })?;
            ensure_linear_scopes_consumed_from(
                scopes,
                depth,
                struct_map,
                enum_map,
                continue_stmt.span,
            )?;
        }
        Stmt::If(if_stmt) => {
            let cond_ty = check_expr(
                &if_stmt.cond,
                functions,
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
                &mut then_scopes,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
                type_params,
                in_loop,
            )?;
            let mut else_scopes = scopes.clone();
            if let Some(block) = &if_stmt.else_block {
                check_block(
                    block,
                    ret_ty,
                    functions,
                    &mut else_scopes,
                    recorder,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    module_name,
                    type_params,
                    in_loop,
                )?;
            }
            merge_branch_states(
                scopes,
                &then_scopes,
                &else_scopes,
                struct_map,
                enum_map,
                if_stmt.span,
            )?;
        }
        Stmt::While(while_stmt) => {
            let cond_ty = check_expr(
                &while_stmt.cond,
                functions,
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
                &mut body_scopes,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
                type_params,
                true, // inside loop, break/continue allowed
            )?;
            body_scopes.pop_loop();
            ensure_affine_states_match(
                scopes,
                &body_scopes,
                struct_map,
                enum_map,
                while_stmt.span,
            )?;
        }
        Stmt::For(for_stmt) => {
            // Check start expression - must be i32
            let start_ty = check_expr(
                &for_stmt.start,
                functions,
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
            )?;
            if start_ty != Ty::Builtin(BuiltinType::I32) {
                return Err(TypeError::new(
                    "for loop range start must be i32".to_string(),
                    for_stmt.start.span(),
                ));
            }

            // Check end expression - must be i32
            let end_ty = check_expr(
                &for_stmt.end,
                functions,
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
            )?;
            if end_ty != Ty::Builtin(BuiltinType::I32) {
                return Err(TypeError::new(
                    "for loop range end must be i32".to_string(),
                    for_stmt.end.span(),
                ));
            }

            // Create body scope with loop variable bound
            let mut body_scopes = scopes.clone();
            body_scopes.push_scope();
            body_scopes.insert_local(
                for_stmt.var.item.clone(),
                Ty::Builtin(BuiltinType::I32),
            );

            body_scopes.push_loop();
            check_block(
                &for_stmt.body,
                ret_ty,
                functions,
                &mut body_scopes,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
                type_params,
                true, // inside loop, break/continue allowed
            )?;
            body_scopes.pop_loop();

            // Pop the loop variable scope before checking affine states
            body_scopes.pop_scope();

            ensure_affine_states_match(
                scopes,
                &body_scopes,
                struct_map,
                enum_map,
                for_stmt.span,
            )?;
        }
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                let _ = check_match_stmt(
                    match_expr,
                    functions,
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
                    in_loop,
                )?;
            } else {
                check_expr(
                    &expr_stmt.expr,
                    functions,
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
                )?;
            }
        }
    }

    Ok(())
}

/// Type-check a block with a fresh lexical scope.
fn check_block(
    block: &Block,
    ret_ty: &Ty,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
    type_params: &HashSet<String>,
    in_loop: bool,
) -> Result<(), TypeError> {
    scopes.push_scope();
    for stmt in &block.stmts {
        check_stmt(
            stmt,
            ret_ty,
            functions,
            scopes,
            recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
            type_params,
            in_loop,
        )?;
    }
    ensure_linear_scope_consumed(scopes, struct_map, enum_map, block.span)?;
    scopes.pop_scope();
    Ok(())
}

/// Merge move states after if/else branches.
fn merge_branch_states(
    base: &mut Scopes,
    left: &Scopes,
    right: &Scopes,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    for (base_scope, (left_scope, right_scope)) in base
        .stack
        .iter_mut()
        .zip(left.stack.iter().zip(&right.stack))
    {
        for (name, info) in base_scope.iter_mut() {
            let left_info = left_scope.get(name).ok_or_else(|| {
                TypeError::new(format!("unknown identifier `{name}`"), span)
            })?;
            let right_info = right_scope.get(name).ok_or_else(|| {
                TypeError::new(format!("unknown identifier `{name}`"), span)
            })?;
            match type_kind(&info.ty, struct_map, enum_map) {
                TypeKind::Affine => {
                    info.state = if left_info.state == MoveState::Moved
                        || right_info.state == MoveState::Moved
                    {
                        MoveState::Moved
                    } else {
                        MoveState::Available
                    };
                }
                TypeKind::Linear => {
                    if left_info.state != right_info.state {
                        return Err(TypeError::new(
                            format!("linear value `{name}` must be consumed on all paths"),
                            span,
                        ));
                    }
                    info.state = left_info.state;
                }
                TypeKind::Unrestricted => {}
            }
        }
    }
    Ok(())
}

/// Ensure loop bodies do not change move-only locals' states.
fn ensure_affine_states_match(
    base: &Scopes,
    other: &Scopes,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    for (base_scope, other_scope) in base.stack.iter().zip(&other.stack) {
        for (name, info) in base_scope {
            let other_info = other_scope.get(name).ok_or_else(|| {
                TypeError::new(format!("unknown identifier `{name}`"), span)
            })?;
            if type_kind(&info.ty, struct_map, enum_map) != TypeKind::Unrestricted
                && info.state != other_info.state
            {
                return Err(TypeError::new(
                    format!("move-only value `{name}` moved inside loop"),
                    span,
                ));
            }
        }
    }
    Ok(())
}

/// Enforce that linear locals in the current scope are consumed.
fn ensure_linear_scope_consumed(
    scopes: &Scopes,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    if let Some(scope) = scopes.stack.last() {
        for (name, info) in scope {
            if type_kind(&info.ty, struct_map, enum_map) == TypeKind::Linear
                && info.state != MoveState::Moved
            {
                return Err(TypeError::new(
                    format!("linear value `{name}` not consumed"),
                    span,
                ));
            }
        }
    }
    Ok(())
}

/// Enforce that linear locals in scopes starting at a depth are consumed.
fn ensure_linear_scopes_consumed_from(
    scopes: &Scopes,
    depth: usize,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    for scope in scopes.stack.iter().skip(depth) {
        for (name, info) in scope {
            if type_kind(&info.ty, struct_map, enum_map) == TypeKind::Linear
                && info.state != MoveState::Moved
            {
                return Err(TypeError::new(
                    format!("linear value `{name}` not consumed"),
                    span,
                ));
            }
        }
    }
    Ok(())
}

/// Enforce that all linear locals across scopes are consumed.
fn ensure_linear_all_consumed(
    scopes: &Scopes,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    for scope in &scopes.stack {
        for (name, info) in scope {
            if type_kind(&info.ty, struct_map, enum_map) == TypeKind::Linear
                && info.state != MoveState::Moved
            {
                return Err(TypeError::new(
                    format!("linear value `{name}` not consumed"),
                    span,
                ));
            }
        }
    }
    Ok(())
}

/// Merge move states across match arms.
fn merge_match_states(
    base: &mut Scopes,
    arms: &[Scopes],
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    let Some((first, rest)) = arms.split_first() else {
        return Ok(());
    };
    for (depth, (base_scope, first_scope)) in base.stack.iter_mut().zip(&first.stack).enumerate()
    {
        for (name, info) in base_scope.iter_mut() {
            let first_info = first_scope.get(name).ok_or_else(|| {
                TypeError::new(format!("unknown identifier `{name}`"), span)
            })?;
            match type_kind(&info.ty, struct_map, enum_map) {
                TypeKind::Affine => {
                    let mut moved = first_info.state == MoveState::Moved;
                    for arm in rest {
                        let arm_scope = arm.stack.get(depth).ok_or_else(|| {
                            TypeError::new(format!("unknown identifier `{name}`"), span)
                        })?;
                        let arm_info = arm_scope.get(name).ok_or_else(|| {
                            TypeError::new(format!("unknown identifier `{name}`"), span)
                        })?;
                        if arm_info.state == MoveState::Moved {
                            moved = true;
                        }
                    }
                    info.state = if moved {
                        MoveState::Moved
                    } else {
                        MoveState::Available
                    };
                }
                TypeKind::Linear => {
                    let state = first_info.state;
                    for arm in rest {
                        let arm_scope = arm.stack.get(depth).ok_or_else(|| {
                            TypeError::new(format!("unknown identifier `{name}`"), span)
                        })?;
                        let arm_info = arm_scope.get(name).ok_or_else(|| {
                            TypeError::new(format!("unknown identifier `{name}`"), span)
                        })?;
                        if arm_info.state != state {
                            return Err(TypeError::new(
                                format!("linear value `{name}` must be consumed on all paths"),
                                span,
                            ));
                        }
                    }
                    info.state = state;
                }
                TypeKind::Unrestricted => {}
            }
        }
    }
    Ok(())
}

/// Type-check an expression, applying move rules based on `use_mode`.
pub(super) fn check_expr(
    expr: &Expr,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    use_mode: UseMode,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
    type_params: &HashSet<String>,
) -> Result<Ty, TypeError> {
    let ty = match expr {
        Expr::Literal(lit) => match &lit.value {
            Literal::Int(_) => Ok(Ty::Builtin(BuiltinType::I32)),
            Literal::U8(_) => Ok(Ty::Builtin(BuiltinType::U8)),
            Literal::String(_) => Ok(stdlib_string_ty(stdlib)),
            Literal::Bool(_) => Ok(Ty::Builtin(BuiltinType::Bool)),
            Literal::Unit => Ok(Ty::Builtin(BuiltinType::Unit)),
        },
        Expr::Path(path) => {
            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if let Some(info) = scopes.lookup(name) {
                    let ty = info.ty.clone();
                    if info.state == MoveState::Moved {
                        return Err(TypeError::new(
                            format!("use of moved value `{name}`"),
                            path.segments[0].span,
                        ));
                    }
                    if use_mode == UseMode::Move && is_affine_type(&ty, struct_map, enum_map) {
                        scopes.mark_moved(name, path.segments[0].span)?;
                    }
                    return record_expr_type(recorder, expr, ty);
                }
            }
            if let Some(Ty::Path(enum_name, _)) =
                resolve_enum_variant(path, use_map, enum_map, module_name)
            {
                if let Some(info) = enum_map.get(&enum_name) {
                    let ty = if info.type_params.is_empty() {
                        Ty::Path(enum_name, Vec::new())
                    } else if let Ty::Path(ret_name, ret_args) = ret_ty {
                        if ret_name == &enum_name && ret_args.len() == info.type_params.len() {
                            Ty::Path(enum_name, ret_args.clone())
                        } else {
                            Ty::Path(
                                enum_name,
                                vec![Ty::Builtin(BuiltinType::Unit); info.type_params.len()],
                            )
                        }
                    } else {
                        Ty::Path(
                            enum_name,
                            vec![Ty::Builtin(BuiltinType::Unit); info.type_params.len()],
                        )
                    };
                    return record_expr_type(recorder, expr, ty);
                }
            }
            Err(TypeError::new(
                format!("unknown value `{path}`"),
                path.span,
            ))
        }
        Expr::Call(call) => {
            let path = call.callee.to_path().ok_or_else(|| {
                TypeError::new(
                    "call target must be a function path".to_string(),
                    call.callee.span(),
                )
            })?;

            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if name == "drop" {
                    if call.args.len() != 1 {
                        return Err(TypeError::new(
                            "drop expects exactly one argument".to_string(),
                            call.span,
                        ));
                    }
                    let _ = check_expr(
                        &call.args[0],
                        functions,
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
                    )?;
                    return record_expr_type(recorder, expr, Ty::Builtin(BuiltinType::Unit));
                }
                if name == "panic" {
                    if !call.args.is_empty() {
                        return Err(TypeError::new(
                            "panic takes no arguments".to_string(),
                            call.span,
                        ));
                    }
                    // panic() is a diverging expression - it never returns.
                    return record_expr_type(recorder, expr, Ty::Builtin(BuiltinType::Never));
                }
                if name == "Ok" || name == "Err" {
                    if call.args.len() != 1 {
                        return Err(TypeError::new(
                            format!("{name} takes exactly one argument"),
                            call.span,
                        ));
                    }
                    let arg_ty = check_expr(
                        &call.args[0],
                        functions,
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
                    )?;
                    if let Ty::Path(ty_name, args) = ret_ty {
                        if ty_name == "sys.result.Result" && args.len() == 2 {
                            let expected = if name == "Ok" { &args[0] } else { &args[1] };
                            if &arg_ty != expected {
                                return Err(TypeError::new(
                                    format!("{name} argument type mismatch: expected {expected:?}, got {arg_ty:?}"),
                                    call.args[0].span(),
                                ));
                            }
                            return record_expr_type(recorder, expr, ret_ty.clone());
                        }
                    }
                    return record_expr_type(
                        recorder,
                        expr,
                        Ty::Path(
                            "sys.result.Result".to_string(),
                            if name == "Ok" {
                                vec![arg_ty, Ty::Builtin(BuiltinType::Unit)]
                            } else {
                                vec![Ty::Builtin(BuiltinType::Unit), arg_ty]
                            },
                        ),
                    );
                }
            }

            if let Some(Ty::Path(enum_name, _)) =
                resolve_enum_variant(&path, use_map, enum_map, module_name)
            {
                let Some(info) = enum_map.get(&enum_name) else {
                    return Err(TypeError::new(
                        "unknown enum variant".to_string(),
                        call.span,
                    ));
                };
                let variant = path
                    .segments
                    .last()
                    .map(|s| s.item.clone())
                    .unwrap_or_else(|| "unknown".to_string());
                let payload = info.payloads.get(&variant).cloned().unwrap_or(None);

                if payload.is_none() && !call.args.is_empty() {
                    return Err(TypeError::new(
                        format!("{variant} takes no arguments"),
                        call.span,
                    ));
                }
                if payload.is_some() && call.args.len() != 1 {
                    return Err(TypeError::new(
                        format!("{variant} takes exactly one argument"),
                        call.span,
                    ));
                }

                let mut inferred: HashMap<String, Ty> = HashMap::new();
                let arg_ty = if let Some(payload_ty) = payload.clone() {
                    let arg_ty = check_expr(
                        &call.args[0],
                        functions,
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
                    )?;
                    if !infer_enum_args(&payload_ty, &arg_ty, &mut inferred) {
                        return Err(TypeError::new(
                            format!(
                                "variant argument type mismatch: expected {payload_ty:?}, got {arg_ty:?}"
                            ),
                            call.args[0].span(),
                        ));
                    }
                    Some(arg_ty)
                } else {
                    None
                };

                let type_args = resolve_enum_type_args(
                    &enum_name,
                    &info.type_params,
                    &inferred,
                    ret_ty,
                );

                if let Some(payload_ty) = payload {
                    if let Some(arg_ty) = arg_ty {
                        if !enum_payload_matches(&payload_ty, &arg_ty, &info.type_params, &type_args) {
                            return Err(TypeError::new(
                                "variant argument type mismatch".to_string(),
                                call.args[0].span(),
                            ));
                        }
                    }
                }

                return record_expr_type(recorder, expr, Ty::Path(enum_name, type_args));
            }

            let resolved = resolve_path(&path, use_map);
            let key = resolved.join(".");

            let sig = if let Some(sig) = functions.get(&key) {
                sig
            } else if resolved.len() == 1 {
                let qualified = format!("{}.{}", module_name, key);
                functions.get(&qualified).ok_or_else(|| {
                    TypeError::new(format!("unknown function `{key}`"), path.span)
                })?
            } else {
                return Err(TypeError::new(format!("unknown function `{key}`"), path.span));
            };
            if sig.module != module_name && !sig.is_pub {
                return Err(TypeError::new(
                    format!("function `{}` is private", key),
                    call.span,
                ));
            }
            let explicit_type_args = lower_type_args(
                &call.type_args,
                use_map,
                stdlib,
                struct_map,
                enum_map,
                type_params,
            )?;
            let subs = build_call_substitution(sig, &explicit_type_args, HashMap::new(), call.span)?;
            let instantiated_params: Vec<Ty> = sig
                .params
                .iter()
                .map(|ty| substitute_type(ty, &subs))
                .collect();
            let instantiated_ret = substitute_type(&sig.ret, &subs);
            if instantiated_params.len() != call.args.len() {
                return Err(TypeError::new(
                    format!(
                        "argument count mismatch: expected {}, found {}",
                        instantiated_params.len(),
                        call.args.len()
                    ),
                    call.span,
                ));
            }
            for (arg, expected) in call.args.iter().zip(&instantiated_params) {
                let (expected_inner, use_mode) = if let Ty::Ref(inner) = expected {
                    (inner.as_ref(), UseMode::Read)
                } else {
                    (expected, UseMode::Move)
                };
                let arg_ty = check_expr(
                    arg,
                    functions,
                    scopes,
                    use_mode,
                    recorder,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                    type_params,
                )?;
                if !matches!(expected, Ty::Ref(_)) && matches!(arg_ty, Ty::Ref(_)) {
                    return Err(TypeError::new(
                        "cannot pass a reference to a value parameter".to_string(),
                        arg.span(),
                    ));
                }
                let matches_ref = if let Ty::Ref(inner) = expected {
                    &arg_ty == inner.as_ref() || &arg_ty == expected
                } else {
                    false
                };
                if &arg_ty != expected_inner
                    && !matches_ref
                    && !matches!(arg_ty, Ty::Builtin(BuiltinType::Never))
                {
                    return Err(TypeError::new(
                        format!("argument type mismatch: expected {expected:?}, found {arg_ty:?}"),
                        arg.span(),
                    ));
                }
            }
            Ok(instantiated_ret)
        }
        Expr::MethodCall(method_call) => {
            fn get_leftmost_segment(expr: &Expr) -> Option<&str> {
                match expr {
                    Expr::Path(path) if path.segments.len() == 1 => {
                        Some(&path.segments[0].item)
                    }
                    Expr::FieldAccess(fa) => get_leftmost_segment(&fa.object),
                    _ => None,
                }
            }

            let base_is_local = if let Some(base_name) = get_leftmost_segment(&method_call.receiver) {
                scopes.contains(base_name)
            } else {
                true
            };

            let path_call = method_call.receiver.to_path().map(|mut path| {
                path.segments.push(method_call.method.clone());
                path.span = Span::new(path.span.start, method_call.method.span.end);
                path
            });

            let is_function = if let Some(path) = &path_call {
                let resolved = resolve_path(path, use_map);
                let key = resolved.join(".");
                functions.contains_key(&key)
            } else {
                false
            };

            if !base_is_local && is_function {
                let path = path_call.expect("path exists for function call");
                let resolved = resolve_path(&path, use_map);
                let key = resolved.join(".");
                let sig = functions.get(&key).ok_or_else(|| {
                    TypeError::new(format!("unknown function `{key}`"), path.span)
                })?;
                if sig.module != module_name && !sig.is_pub {
                    return Err(TypeError::new(
                        format!("function `{key}` is private"),
                        method_call.span,
                    ));
                }
                let explicit_type_args = lower_type_args(
                    &method_call.type_args,
                    use_map,
                    stdlib,
                    struct_map,
                    enum_map,
                    type_params,
                )?;
                let subs =
                    build_call_substitution(sig, &explicit_type_args, HashMap::new(), method_call.span)?;
                let instantiated_params: Vec<Ty> = sig
                    .params
                    .iter()
                    .map(|ty| substitute_type(ty, &subs))
                    .collect();
                let instantiated_ret = substitute_type(&sig.ret, &subs);
                if instantiated_params.len() != method_call.args.len() {
                    return Err(TypeError::new(
                        format!(
                            "argument count mismatch: expected {}, found {}",
                            instantiated_params.len(),
                            method_call.args.len()
                        ),
                        method_call.span,
                    ));
                }
                for (arg, expected) in method_call.args.iter().zip(&instantiated_params) {
                    let (expected_inner, use_mode) = if let Ty::Ref(inner) = expected {
                        (inner.as_ref(), UseMode::Read)
                    } else {
                        (expected, UseMode::Move)
                    };
                    let arg_ty = check_expr(
                        arg,
                        functions,
                        scopes,
                        use_mode,
                        recorder,
                        use_map,
                        struct_map,
                        enum_map,
                        stdlib,
                        ret_ty,
                        module_name,
                        type_params,
                    )?;
                    if !matches!(expected, Ty::Ref(_)) && matches!(arg_ty, Ty::Ref(_)) {
                        return Err(TypeError::new(
                            "cannot pass a reference to a value parameter".to_string(),
                            arg.span(),
                        ));
                    }
                    let matches_ref = if let Ty::Ref(inner) = expected {
                        &arg_ty == inner.as_ref() || &arg_ty == expected
                    } else {
                        false
                    };
                    if &arg_ty != expected_inner
                        && !matches_ref
                        && !matches!(arg_ty, Ty::Builtin(BuiltinType::Never))
                    {
                        return Err(TypeError::new(
                            format!(
                                "argument type mismatch: expected {expected:?}, found {arg_ty:?}"
                            ),
                            arg.span(),
                        ));
                    }
                }
                return record_expr_type(recorder, expr, instantiated_ret);
            }

            let receiver_ty = check_expr(
                &method_call.receiver,
                functions,
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
            )?;
            let (method_module, type_name, receiver_args) = resolve_method_target(
                &receiver_ty,
                module_name,
                struct_map,
                enum_map,
                method_call.receiver.span(),
            )?;
            let method_fn = format!("{type_name}__{}", method_call.method.item);
            let qualified_key = format!("{method_module}.{method_fn}");
            let key = if functions.contains_key(&qualified_key) {
                qualified_key
            } else if method_module == module_name && functions.contains_key(&method_fn) {
                method_fn.clone()
            } else {
                return Err(TypeError::new(
                    format!("unknown method `{qualified_key}`"),
                    method_call.span,
                ));
            };
            let sig = functions
                .get(&key)
                .ok_or_else(|| TypeError::new(format!("unknown method `{key}`"), method_call.span))?;
            if sig.module != module_name && !sig.is_pub {
                return Err(TypeError::new(
                    format!("method `{key}` is private"),
                    method_call.span,
                ));
            }
            let mut inferred = HashMap::new();
            let expected_receiver = match &sig.params[0] {
                Ty::Ref(inner) | Ty::Ptr(inner) => inner.as_ref(),
                other => other,
            };
            let actual_receiver = match &receiver_ty {
                Ty::Ref(inner) | Ty::Ptr(inner) => inner.as_ref(),
                other => other,
            };
            let normalized_actual_receiver = match (expected_receiver, actual_receiver) {
                (Ty::Path(expected_name, _), Ty::Path(actual_name, args))
                    if !expected_name.contains('.')
                        && actual_name
                            .rsplit_once('.')
                            .map(|(_, t)| t == expected_name)
                            .unwrap_or(false) =>
                {
                    Ty::Path(expected_name.clone(), args.clone())
                }
                _ => actual_receiver.clone(),
            };
            match_type_params(
                expected_receiver,
                &normalized_actual_receiver,
                &mut inferred,
                method_call.receiver.span(),
            )?;
            let explicit_type_args = lower_type_args(
                &method_call.type_args,
                use_map,
                stdlib,
                struct_map,
                enum_map,
                type_params,
            )?;
            let subs = build_call_substitution(sig, &explicit_type_args, inferred, method_call.span)?;
            let instantiated_params: Vec<Ty> = sig
                .params
                .iter()
                .map(|ty| substitute_type(ty, &subs))
                .collect();
            let instantiated_ret = substitute_type(&sig.ret, &subs);
            if instantiated_params.len() != method_call.args.len() + 1 {
                return Err(TypeError::new(
                    format!(
                        "argument count mismatch: expected {}, found {}",
                        instantiated_params.len() - 1,
                        method_call.args.len()
                    ),
                    method_call.span,
                ));
            }
            let receiver_base = match &receiver_ty {
                Ty::Ref(inner) | Ty::Ptr(inner) => inner.as_ref().clone(),
                _ => receiver_ty.clone(),
            };
            let receiver_unqualified = Ty::Path(type_name.clone(), receiver_args);
            let receiver_ref = Ty::Ref(Box::new(receiver_base.clone()));
            let receiver_ref_unqualified = Ty::Ref(Box::new(receiver_unqualified.clone()));
            let receiver_ptr = Ty::Ptr(Box::new(receiver_base.clone()));
            let receiver_ptr_unqualified = Ty::Ptr(Box::new(receiver_unqualified.clone()));
            let expected_qualified = match &instantiated_params[0] {
                Ty::Path(name, args) if !name.contains('.') => {
                    Some(Ty::Path(format!("{method_module}.{name}"), args.clone()))
                }
                _ => None,
            };
            let expected_ref_qualified = expected_qualified
                .as_ref()
                .map(|ty| Ty::Ref(Box::new(ty.clone())));
            let expected_ptr_qualified = expected_qualified
                .as_ref()
                .map(|ty| Ty::Ptr(Box::new(ty.clone())));

            let expects_ref = matches!(instantiated_params[0], Ty::Ref(_));
            let expects_ptr = matches!(instantiated_params[0], Ty::Ptr(_));

            if matches!(receiver_ty, Ty::Ref(_)) && !expects_ref {
                return Err(TypeError::new(
                    "cannot use a reference receiver where a value is expected".to_string(),
                    method_call.receiver.span(),
                ));
            }
            if matches!(receiver_ty, Ty::Ptr(_)) && !expects_ptr {
                return Err(TypeError::new(
                    "cannot use a pointer receiver where a value is expected".to_string(),
                    method_call.receiver.span(),
                ));
            }

            if instantiated_params[0] != receiver_ty
                && expected_qualified.as_ref() != Some(&receiver_ty)
                && instantiated_params[0] != receiver_unqualified
                && instantiated_params[0] != receiver_ref
                && expected_ref_qualified.as_ref() != Some(&receiver_ref)
                && instantiated_params[0] != receiver_ref_unqualified
                && instantiated_params[0] != receiver_ptr
                && expected_ptr_qualified.as_ref() != Some(&receiver_ptr)
                && instantiated_params[0] != receiver_ptr_unqualified
            {
                return Err(TypeError::new(
                    format!(
                        "method receiver type mismatch: expected {expected:?}, found {receiver_ty:?}",
                        expected = instantiated_params[0]
                    ),
                    method_call.receiver.span(),
                ));
            }
            if instantiated_params[0] != receiver_ref && instantiated_params[0] != receiver_ref_unqualified {
                let _ = check_expr(
                    &method_call.receiver,
                    functions,
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
                )?;
            }
            for (arg, expected) in method_call.args.iter().zip(&instantiated_params[1..]) {
                let (expected_inner, use_mode) = if let Ty::Ref(inner) = expected {
                    (inner.as_ref(), UseMode::Read)
                } else {
                    (expected, UseMode::Move)
                };
                let arg_ty = check_expr(
                    arg,
                    functions,
                    scopes,
                    use_mode,
                    recorder,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                    type_params,
                )?;
                if !matches!(expected, Ty::Ref(_)) && matches!(arg_ty, Ty::Ref(_)) {
                    return Err(TypeError::new(
                        "cannot pass a reference to a value parameter".to_string(),
                        arg.span(),
                    ));
                }
                let matches_ref = if let Ty::Ref(inner) = expected {
                    &arg_ty == inner.as_ref() || &arg_ty == expected
                } else {
                    false
                };
                if &arg_ty != expected_inner
                    && !matches_ref
                    && !matches!(arg_ty, Ty::Builtin(BuiltinType::Never))
                {
                    return Err(TypeError::new(
                        format!("argument type mismatch: expected {expected:?}, found {arg_ty:?}"),
                        arg.span(),
                    ));
                }
            }
            Ok(instantiated_ret)
        }
        Expr::StructLiteral(lit) => check_struct_literal(
            lit,
            functions,
            scopes,
            recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
            type_params,
        ),
        Expr::Unary(unary) => {
            let expr_ty = check_expr(
                &unary.expr,
                functions,
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
            )?;
            match unary.op {
                UnaryOp::Neg => {
                    if expr_ty == Ty::Builtin(BuiltinType::I32)
                        || expr_ty == Ty::Builtin(BuiltinType::I64)
                    {
                        Ok(expr_ty)
                    } else {
                        Err(TypeError::new(
                            "unary - expects integer".to_string(),
                            unary.span,
                        ))
                    }
                }
                UnaryOp::Not => {
                    if expr_ty == Ty::Builtin(BuiltinType::Bool) {
                        Ok(expr_ty)
                    } else {
                        Err(TypeError::new(
                            "unary ! expects bool".to_string(),
                            unary.span,
                        ))
                    }
                }
            }
        }
        Expr::Binary(binary) => {
            let left = check_expr(
                &binary.left,
                functions,
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
            )?;
            let right = check_expr(
                &binary.right,
                functions,
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
            )?;
            match binary.op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div => {
                    if left == right && (left == Ty::Builtin(BuiltinType::I32)
                        || left == Ty::Builtin(BuiltinType::I64))
                    {
                        Ok(left)
                    } else if matches!(left, Ty::Builtin(BuiltinType::Never))
                        || matches!(right, Ty::Builtin(BuiltinType::Never))
                    {
                        Ok(Ty::Builtin(BuiltinType::Never))
                    } else if left != right && is_numeric_type(&left) && is_numeric_type(&right) {
                        Err(TypeError::new(
                            "implicit numeric conversions are not allowed".to_string(),
                            binary.span,
                        ))
                    } else {
                        Err(TypeError::new(
                            "binary arithmetic expects matching integer types".to_string(),
                            binary.span,
                        ))
                    }
                }
                BinaryOp::Eq | BinaryOp::Neq => {
                    if left == right {
                        Ok(Ty::Builtin(BuiltinType::Bool))
                    } else if matches!(left, Ty::Builtin(BuiltinType::Never))
                        || matches!(right, Ty::Builtin(BuiltinType::Never))
                    {
                        Ok(Ty::Builtin(BuiltinType::Bool))
                    } else if left != right && is_numeric_type(&left) && is_numeric_type(&right) {
                        Err(TypeError::new(
                            "implicit numeric conversions are not allowed".to_string(),
                            binary.span,
                        ))
                    } else {
                        Err(TypeError::new(
                            "comparison expects matching operand types".to_string(),
                            binary.span,
                        ))
                    }
                }
                BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
                    if left == right && is_orderable_type(&left) {
                        Ok(Ty::Builtin(BuiltinType::Bool))
                    } else if matches!(left, Ty::Builtin(BuiltinType::Never))
                        || matches!(right, Ty::Builtin(BuiltinType::Never))
                    {
                        Ok(Ty::Builtin(BuiltinType::Bool))
                    } else if left != right && is_numeric_type(&left) && is_numeric_type(&right) {
                        Err(TypeError::new(
                            "implicit numeric conversions are not allowed".to_string(),
                            binary.span,
                        ))
                    } else {
                        Err(TypeError::new(
                            "ordering expects matching integer types".to_string(),
                            binary.span,
                        ))
                    }
                }
                BinaryOp::And | BinaryOp::Or => {
                    if left == Ty::Builtin(BuiltinType::Bool)
                        && right == Ty::Builtin(BuiltinType::Bool)
                    {
                        Ok(Ty::Builtin(BuiltinType::Bool))
                    } else {
                        Err(TypeError::new(
                            "logical operators expect bool".to_string(),
                            binary.span,
                        ))
                    }
                }
            }
        }
        Expr::Match(match_expr) => check_match_expr_value(
            match_expr,
            functions,
            scopes,
            use_mode,
            recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
            type_params,
            false, // break/continue not allowed in value-producing match
        ),
        Expr::Try(try_expr) => {
            let inner_ty = check_expr(
                &try_expr.expr,
                functions,
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
            )?;
            let (ok_ty, err_ty) = match inner_ty {
                Ty::Path(name, args) if name == "sys.result.Result" && args.len() == 2 => {
                    (args[0].clone(), args[1].clone())
                }
                _ => {
                    return Err(TypeError::new(
                        "the `?` operator expects a Result value".to_string(),
                        try_expr.span,
                    ))
                }
            };

            let ret_err = match ret_ty {
                Ty::Path(name, args) if name == "sys.result.Result" && args.len() == 2 => &args[1],
                _ => {
                    return Err(TypeError::new(
                        "the `?` operator can only be used in functions returning Result".to_string(),
                        try_expr.span,
                    ))
                }
            };

            if &err_ty != ret_err {
                return Err(TypeError::new(
                    format!(
                        "mismatched error type for `?`: expected {ret_err:?}, found {err_ty:?}"
                    ),
                    try_expr.span,
                ));
            }

            Ok(ok_ty)
        }
        Expr::Grouping(group) => check_expr(
            &group.expr,
            functions,
            scopes,
            use_mode,
            recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
            type_params,
        ),
        Expr::FieldAccess(field_access) => {
            fn get_leftmost_path_segment(expr: &Expr) -> Option<&str> {
                match expr {
                    Expr::Path(path) if path.segments.len() == 1 => {
                        Some(&path.segments[0].item)
                    }
                    Expr::FieldAccess(fa) => get_leftmost_path_segment(&fa.object),
                    _ => None,
                }
            }

            let base_is_local = if let Some(base_name) = get_leftmost_path_segment(&field_access.object) {
                scopes.contains(base_name)
            } else {
                true
            };

            if !base_is_local {
                if let Some(path) = Expr::FieldAccess(field_access.clone()).to_path() {
                    if let Some(ty) = resolve_enum_variant(&path, use_map, enum_map, module_name) {
                        return record_expr_type(recorder, expr, ty);
                    }
                }
            }

            let object_ty = check_expr(
                &field_access.object,
                functions,
                scopes,
                UseMode::Project,
                recorder,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
                type_params,
            )?;
            let Ty::Path(struct_name, struct_args) = object_ty else {
                return Err(TypeError::new(
                    "field access requires a struct value".to_string(),
                    field_access.span,
                ));
            };
            let info = struct_map
                .get(&struct_name)
                .or_else(|| struct_map.get(&format!("{module_name}.{struct_name}")))
                .ok_or_else(|| {
                    TypeError::new(
                        format!("field access on non-struct `{struct_name}`"),
                        field_access.span,
                    )
                })?;
            if info.is_opaque && info.module != module_name {
                return Err(TypeError::new(
                    format!(
                        "cannot access fields of opaque/capability type `{struct_name}` outside module `{}`",
                        info.module
                    ),
                    field_access.span,
                ));
            }
            let field_ty = info.fields.get(&field_access.field.item).ok_or_else(|| {
                TypeError::new(
                    format!("unknown field `{}`", field_access.field.item),
                    field_access.field.span,
                )
            })?;
            let substitutions = build_type_substitution(&info.type_params, &struct_args, field_access.span)?;
            let field_ty = substitute_type(field_ty, &substitutions);
            if is_affine_type(&field_ty, struct_map, enum_map) {
                match use_mode {
                    UseMode::Read => {
                        return Err(TypeError::new(
                            "cannot read move-only field; moving it consumes the whole struct"
                                .to_string(),
                            field_access.span,
                        ));
                    }
                    UseMode::Project => {}
                    UseMode::Move => {
                        let (root, root_span) =
                            leftmost_local_in_chain(&field_access.object).ok_or_else(|| {
                                TypeError::new(
                                    "cannot move affine field from non-local expression; bind to a local first".to_string(),
                                    field_access.object.span(),
                                )
                            })?;
                        if !scopes.contains(root) {
                            return Err(TypeError::new(
                                "cannot move affine field from non-local expression; bind to a local first".to_string(),
                                field_access.object.span(),
                            ));
                        }
                        scopes.mark_moved(root, root_span)?;
                    }
                }
            }
            Ok(field_ty)
        }
        Expr::Index(index_expr) => {
            // Type check the object (must be string, Slice[T], or MutSlice[T])
            let object_ty = check_expr(
                &index_expr.object,
                functions,
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
            )?;

            // Type check the index (must be i32)
            let index_ty = check_expr(
                &index_expr.index,
                functions,
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
            )?;

            if index_ty != Ty::Builtin(BuiltinType::I32) {
                return Err(TypeError::new(
                    format!("index must be i32, found {:?}", index_ty),
                    index_expr.index.span(),
                ));
            }

            // Determine element type based on object type
            match &object_ty {
                ty if is_string_ty(ty) => Ok(Ty::Builtin(BuiltinType::U8)),
                Ty::Path(name, args) if name == "Slice" || name == "sys.buffer.Slice" => {
                    if args.len() != 1 {
                        return Err(TypeError::new(
                            "Slice requires exactly one type argument".to_string(),
                            index_expr.span,
                        ));
                    }
                    if args[0] != Ty::Builtin(BuiltinType::U8) {
                        return Err(TypeError::new(
                            "Slice indexing is only supported for Slice<u8>".to_string(),
                            index_expr.span,
                        ));
                    }
                    Ok(Ty::Builtin(BuiltinType::U8))
                }
                Ty::Path(name, args) if name == "MutSlice" || name == "sys.buffer.MutSlice" => {
                    if args.len() != 1 {
                        return Err(TypeError::new(
                            "MutSlice requires exactly one type argument".to_string(),
                            index_expr.span,
                        ));
                    }
                    if args[0] != Ty::Builtin(BuiltinType::U8) {
                        return Err(TypeError::new(
                            "MutSlice indexing is only supported for MutSlice<u8>".to_string(),
                            index_expr.span,
                        ));
                    }
                    Ok(Ty::Builtin(BuiltinType::U8))
                }
                // Vec types return Result<T, VecErr>
                Ty::Path(name, _) if name == "VecString" || name == "sys.vec.VecString" => {
                    Ok(Ty::Path(
                        "sys.result.Result".to_string(),
                        vec![
                            stdlib_string_ty(stdlib),
                            Ty::Path("sys.vec.VecErr".to_string(), vec![]),
                        ],
                    ))
                }
                Ty::Path(name, _) if name == "VecI32" || name == "sys.vec.VecI32" => {
                    Ok(Ty::Path(
                        "sys.result.Result".to_string(),
                        vec![
                            Ty::Builtin(BuiltinType::I32),
                            Ty::Path("sys.vec.VecErr".to_string(), vec![]),
                        ],
                    ))
                }
                Ty::Path(name, _) if name == "VecU8" || name == "sys.vec.VecU8" => {
                    Ok(Ty::Path(
                        "sys.result.Result".to_string(),
                        vec![
                            Ty::Builtin(BuiltinType::U8),
                            Ty::Path("sys.vec.VecErr".to_string(), vec![]),
                        ],
                    ))
                }
                _ => Err(TypeError::new(
                    format!("cannot index into type {:?}; only string, Slice[T], MutSlice[T], and Vec types are indexable", object_ty),
                    index_expr.span,
                )),
            }
        }
    }?;
    recorder.record(expr, &ty);
    Ok(ty)
}

/// Check a statement-form match (arms may return, no value required).
fn check_match_stmt(
    match_expr: &MatchExpr,
    functions: &HashMap<String, FunctionSig>,
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
    in_loop: bool,
) -> Result<Ty, TypeError> {
    let match_ty = check_expr(
        &match_expr.expr,
        functions,
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
    )?;
    let mut arm_scopes = Vec::with_capacity(match_expr.arms.len());
    for arm in &match_expr.arms {
        let mut arm_scope = scopes.clone();
        arm_scope.push_scope();
        bind_pattern(&arm.pattern, &match_ty, &mut arm_scope, use_map, enum_map, module_name)?;
        check_block(
            &arm.body,
            ret_ty,
            functions,
            &mut arm_scope,
            recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
            type_params,
            in_loop,
        )?;
        arm_scope.pop_scope();
        arm_scopes.push(arm_scope);
    }
    check_match_exhaustive(
        &match_ty,
        &match_expr.arms,
        use_map,
        enum_map,
        module_name,
        match_expr.match_span,
    )?;
    merge_match_states(scopes, &arm_scopes, struct_map, enum_map, match_expr.span)?;
    Ok(Ty::Builtin(BuiltinType::Unit))
}

/// Check an expression-form match (all arms must evaluate to the same type).
fn check_match_expr_value(
    match_expr: &MatchExpr,
    functions: &HashMap<String, FunctionSig>,
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
    in_loop: bool,
) -> Result<Ty, TypeError> {
    let match_ty = check_expr(
        &match_expr.expr,
        functions,
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
    )?;
    let mut result_ty: Option<Ty> = None;
    let mut arm_scopes = Vec::with_capacity(match_expr.arms.len());
    for arm in &match_expr.arms {
        let mut arm_scope = scopes.clone();
        arm_scope.push_scope();
        bind_pattern(&arm.pattern, &match_ty, &mut arm_scope, use_map, enum_map, module_name)?;
        let arm_ty = check_match_arm_value(
            &arm.body,
            functions,
            &mut arm_scope,
            recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
            type_params,
            in_loop,
        )?;
        arm_scope.pop_scope();
        arm_scopes.push(arm_scope);
        if let Some(prev) = &result_ty {
            if matches!(prev, Ty::Builtin(BuiltinType::Never)) {
                result_ty = Some(arm_ty);
            } else if matches!(arm_ty, Ty::Builtin(BuiltinType::Never)) {
                // Keep the previous type; never can coerce to any type.
            } else if prev != &arm_ty {
                return Err(TypeError::new(
                    format!("match arm type mismatch: expected {prev:?}, found {arm_ty:?}"),
                    arm.body.span,
                ));
            }
        } else {
            result_ty = Some(arm_ty);
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
    merge_match_states(scopes, &arm_scopes, struct_map, enum_map, match_expr.span)?;
    Ok(result_ty.unwrap_or(Ty::Builtin(BuiltinType::Unit)))
}

/// Check a single match arm in expression context.
fn check_match_arm_value(
    block: &Block,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
    type_params: &HashSet<String>,
    in_loop: bool,
) -> Result<Ty, TypeError> {
    let Some((last, prefix)) = block.stmts.split_last() else {
        return Err(TypeError::new(
            "match arm must end with expression".to_string(),
            block.span,
        ));
    };
    for stmt in prefix {
        if matches!(stmt, Stmt::Return(_)) {
            return Err(TypeError::new(
                "match arm cannot return in expression context".to_string(),
                block.span,
            ));
        }
        check_stmt(
            stmt,
            ret_ty,
            functions,
            scopes,
            recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
            type_params,
            in_loop,
        )?;
    }
    match last {
        Stmt::Expr(expr_stmt) => check_expr(
            &expr_stmt.expr,
            functions,
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
                format!("non-exhaustive match on bool, missing: {}", missing.join(", ")),
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
                format!("non-exhaustive match on Result, missing: {}", missing.join(", ")),
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
                if let Pattern::Path(path) = &arm.pattern {
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
                format!("non-exhaustive match, missing variants: {}", missing.join(", ")),
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

/// Check a struct literal and ensure all fields are present and typed.
fn check_struct_literal(
    lit: &StructLiteralExpr,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    recorder: &mut TypeRecorder,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
    type_params: &HashSet<String>,
) -> Result<Ty, TypeError> {
    let type_args = lower_type_args(
        &lit.type_args,
        use_map,
        stdlib,
        struct_map,
        enum_map,
        type_params,
    )?;
    let type_name = resolve_type_name(&lit.path, use_map, stdlib);
    let key = if lit.path.segments.len() == 1 {
        if stdlib.types.contains_key(&lit.path.segments[0].item) {
            type_name.clone()
        } else {
            lit.path.segments[0].item.clone()
        }
    } else {
        type_name.clone()
    };
    let info = struct_map.get(&key).ok_or_else(|| {
        TypeError::new(format!("unknown struct `{}`", key), lit.span)
    })?;
    if info.type_params.is_empty() {
        if !type_args.is_empty() {
            return Err(TypeError::new(
                format!("type `{}` does not accept type arguments", key),
                lit.span,
            ));
        }
    } else if type_args.len() != info.type_params.len() {
        return Err(TypeError::new(
            format!(
                "type `{}` expects {} type argument(s), found {}",
                key,
                info.type_params.len(),
                type_args.len()
            ),
            lit.span,
        ));
    }
    let substitutions = build_type_substitution(&info.type_params, &type_args, lit.span)?;
    if info.is_opaque && info.module != module_name {
        return Err(TypeError::new(
            format!(
                "cannot construct opaque/capability type `{}` outside module `{}`",
                key, info.module
            ),
            lit.span,
        ));
    }

    let mut remaining = info.fields.clone();
    for field in &lit.fields {
        let expected = remaining.remove(&field.name.item).ok_or_else(|| {
            TypeError::new(
                format!("unknown field `{}`", field.name.item),
                field.span,
            )
        })?;
        let expected = substitute_type(&expected, &substitutions);
        let actual = check_expr(
            &field.expr,
            functions,
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
        )?;
        if actual != expected {
            return Err(TypeError::new(
                format!("field `{}` expects {expected:?}, found {actual:?}", field.name.item),
                field.span,
            ));
        }
    }
    if let Some((missing, _)) = remaining.into_iter().next() {
        return Err(TypeError::new(
            format!("missing field `{missing}`"),
            lit.span,
        ));
    }

    Ok(Ty::Path(type_name, type_args))
}

fn lower_type_args(
    args: &[Type],
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    type_params: &HashSet<String>,
) -> Result<Vec<Ty>, TypeError> {
    let mut out = Vec::with_capacity(args.len());
    for arg in args {
        let ty = lower_type(arg, use_map, stdlib, type_params)?;
        validate_type_args(&ty, struct_map, enum_map, arg.span())?;
        out.push(ty);
    }
    Ok(out)
}

fn build_type_substitution(
    params: &[String],
    args: &[Ty],
    span: Span,
) -> Result<HashMap<String, Ty>, TypeError> {
    if params.len() != args.len() {
        return Err(TypeError::new(
            format!(
                "expected {} type argument(s), found {}",
                params.len(),
                args.len()
            ),
            span,
        ));
    }
    let mut map = HashMap::new();
    for (param, arg) in params.iter().zip(args.iter()) {
        map.insert(param.clone(), arg.clone());
    }
    Ok(map)
}

fn substitute_type(ty: &Ty, subs: &HashMap<String, Ty>) -> Ty {
    match ty {
        Ty::Param(name) => subs.get(name).cloned().unwrap_or_else(|| ty.clone()),
        Ty::Builtin(_) => ty.clone(),
        Ty::Ptr(inner) => Ty::Ptr(Box::new(substitute_type(inner, subs))),
        Ty::Ref(inner) => Ty::Ref(Box::new(substitute_type(inner, subs))),
        Ty::Path(name, args) => Ty::Path(
            name.clone(),
            args.iter().map(|arg| substitute_type(arg, subs)).collect(),
        ),
    }
}

fn match_type_params(
    expected: &Ty,
    actual: &Ty,
    subs: &mut HashMap<String, Ty>,
    span: Span,
) -> Result<(), TypeError> {
    match expected {
        Ty::Param(name) => {
            if let Some(existing) = subs.get(name) {
                if existing != actual {
                    return Err(TypeError::new(
                        format!(
                            "conflicting type arguments for `{}`: {existing:?} vs {actual:?}",
                            name
                        ),
                        span,
                    ));
                }
            } else {
                subs.insert(name.clone(), actual.clone());
            }
            Ok(())
        }
        Ty::Builtin(_) => {
            if expected != actual {
                return Err(TypeError::new(
                    format!("type mismatch: expected {expected:?}, found {actual:?}"),
                    span,
                ));
            }
            Ok(())
        }
        Ty::Ptr(inner) => match actual {
            Ty::Ptr(actual_inner) => match_type_params(inner, actual_inner, subs, span),
            _ => Err(TypeError::new(
                format!("type mismatch: expected {expected:?}, found {actual:?}"),
                span,
            )),
        },
        Ty::Ref(inner) => match actual {
            Ty::Ref(actual_inner) => match_type_params(inner, actual_inner, subs, span),
            _ => Err(TypeError::new(
                format!("type mismatch: expected {expected:?}, found {actual:?}"),
                span,
            )),
        },
        Ty::Path(name, args) => match actual {
            Ty::Path(actual_name, actual_args) => {
                if name != actual_name || args.len() != actual_args.len() {
                    return Err(TypeError::new(
                        format!("type mismatch: expected {expected:?}, found {actual:?}"),
                        span,
                    ));
                }
                for (arg, actual_arg) in args.iter().zip(actual_args.iter()) {
                    match_type_params(arg, actual_arg, subs, span)?;
                }
                Ok(())
            }
            _ => Err(TypeError::new(
                format!("type mismatch: expected {expected:?}, found {actual:?}"),
                span,
            )),
        },
    }
}

fn build_call_substitution(
    sig: &FunctionSig,
    explicit_args: &[Ty],
    inferred: HashMap<String, Ty>,
    span: Span,
) -> Result<HashMap<String, Ty>, TypeError> {
    if sig.type_params.is_empty() {
        if !explicit_args.is_empty() {
            return Err(TypeError::new(
                format!(
                    "function does not accept type arguments (found {})",
                    explicit_args.len()
                ),
                span,
            ));
        }
        return Ok(inferred);
    }

    let mut subs = inferred;
    let mut remaining = Vec::new();
    for name in &sig.type_params {
        if !subs.contains_key(name) {
            remaining.push(name.clone());
        }
    }
    if explicit_args.len() != remaining.len() {
        return Err(TypeError::new(
            format!(
                "expected {} type argument(s), found {}",
                remaining.len(),
                explicit_args.len()
            ),
            span,
        ));
    }
    for (name, arg) in remaining.into_iter().zip(explicit_args.iter()) {
        subs.insert(name, arg.clone());
    }
    Ok(subs)
}

/// Bind locals introduced by a match pattern.
fn bind_pattern(
    pattern: &Pattern,
    match_ty: &Ty,
    scopes: &mut Scopes,
    use_map: &UseMap,
    enum_map: &HashMap<String, EnumInfo>,
    module_name: &str,
) -> Result<(), TypeError> {
    match pattern {
        Pattern::Call { path, binding, .. } => {
            let Some(binding) = binding else {
                return Ok(());
            };
            let name = path
                .segments
                .iter()
                .map(|seg| seg.item.as_str())
                .collect::<Vec<_>>()
                .join(".");
            if let Ty::Path(ty_name, args) = match_ty {
                if ty_name == "sys.result.Result" && args.len() == 2 {
                    let ty = if name == "Ok" {
                        args[0].clone()
                    } else if name == "Err" {
                        args[1].clone()
                    } else {
                        return Ok(());
                    };
                    scopes.insert_local(binding.item.clone(), ty);
                    return Ok(());
                }
            }
            Err(TypeError::new(
                "pattern binding requires a Result match".to_string(),
                path.span,
            ))
        }
        Pattern::Binding(ident) => {
            scopes.insert_local(ident.item.clone(), match_ty.clone());
            Ok(())
        }
        Pattern::Path(path) => {
            if let Some(ty) = resolve_enum_variant(path, use_map, enum_map, module_name) {
                if !same_type_constructor(&ty, match_ty) {
                    return Err(TypeError::new(
                        format!("pattern type mismatch: expected {match_ty:?}, found {ty:?}"),
                        path.span,
                    ));
                }
            }
            Ok(())
        }
        Pattern::Literal(_) | Pattern::Wildcard(_) => Ok(()),
    }
}

fn leftmost_local_in_chain(expr: &Expr) -> Option<(&str, Span)> {
    match expr {
        Expr::Path(path) if path.segments.len() == 1 => {
            let seg = &path.segments[0];
            Some((seg.item.as_str(), seg.span))
        }
        Expr::FieldAccess(field_access) => leftmost_local_in_chain(&field_access.object),
        Expr::Grouping(group) => leftmost_local_in_chain(&group.expr),
        Expr::Try(try_expr) => leftmost_local_in_chain(&try_expr.expr),
        _ => None,
    }
}
