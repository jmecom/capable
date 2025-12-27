use std::collections::HashMap;

use crate::ast::*;
use crate::error::TypeError;

use super::{
    is_affine_type, lower_type, resolve_enum_variant, resolve_method_target, resolve_path,
    resolve_type_name, type_contains_ref, type_kind, BuiltinType, EnumInfo, FunctionSig, MoveState,
    Scopes, SpanExt, StdlibIndex, StructInfo, Ty, TypeKind, UseMap, UseMode,
};

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
) -> Result<(), TypeError> {
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
        let ty = lower_type(ty, use_map, stdlib)?;
        params_map.insert(param.name.item.clone(), ty);
    }
    let mut scopes = Scopes::from_flat_map(params_map);

    let ret_ty = lower_type(&func.ret, use_map, stdlib)?;
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
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
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
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Let(let_stmt) => {
            let expr_ty = check_expr(
                &let_stmt.expr,
                functions,
                scopes,
                UseMode::Move,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            let final_ty = if let Some(annot) = &let_stmt.ty {
                if let Some(span) = type_contains_ref(annot) {
                    return Err(TypeError::new(
                        "reference types cannot be stored in locals".to_string(),
                        span,
                    ));
                }
                let annot_ty = lower_type(annot, use_map, stdlib)?;
                if annot_ty != expr_ty {
                    return Err(TypeError::new(
                        format!("type mismatch: expected {annot_ty:?}, found {expr_ty:?}"),
                        let_stmt.span,
                    ));
                }
                annot_ty
            } else {
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
            let expr_ty = check_expr(
                &assign.expr,
                functions,
                scopes,
                UseMode::Move,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            if expr_ty != existing {
                return Err(TypeError::new(
                    format!(
                        "assignment type mismatch: expected {existing:?}, found {expr_ty:?}"
                    ),
                    assign.span,
                ));
            }
            scopes.assign(&assign.name.item, expr_ty);
        }
        Stmt::Return(ret_stmt) => {
            let expr_ty = if let Some(expr) = &ret_stmt.expr {
                check_expr(
                    expr,
                    functions,
                    scopes,
                    UseMode::Move,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?
            } else {
                Ty::Builtin(BuiltinType::Unit)
            };
            if &expr_ty != ret_ty {
                return Err(TypeError::new(
                    format!("return type mismatch: expected {ret_ty:?}, found {expr_ty:?}"),
                    ret_stmt.span,
                ));
            }
            ensure_linear_all_consumed(scopes, struct_map, enum_map, ret_stmt.span)?;
        }
        Stmt::If(if_stmt) => {
            let cond_ty = check_expr(
                &if_stmt.cond,
                functions,
                scopes,
                UseMode::Read,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
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
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
            )?;
            let mut else_scopes = scopes.clone();
            if let Some(block) = &if_stmt.else_block {
                check_block(
                    block,
                    ret_ty,
                    functions,
                    &mut else_scopes,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    module_name,
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
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            if cond_ty != Ty::Builtin(BuiltinType::Bool) {
                return Err(TypeError::new(
                    "while condition must be bool".to_string(),
                    while_stmt.cond.span(),
                ));
            }
            let mut body_scopes = scopes.clone();
            check_block(
                &while_stmt.body,
                ret_ty,
                functions,
                &mut body_scopes,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
            )?;
            ensure_affine_states_match(
                scopes,
                &body_scopes,
                struct_map,
                enum_map,
                while_stmt.span,
            )?;
        }
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                let _ = check_match_stmt(
                    match_expr,
                    functions,
                    scopes,
                    UseMode::Move,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?;
            } else {
                check_expr(
                    &expr_stmt.expr,
                    functions,
                    scopes,
                    UseMode::Move,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
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
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
) -> Result<(), TypeError> {
    scopes.push_scope();
    for stmt in &block.stmts {
        check_stmt(
            stmt,
            ret_ty,
            functions,
            scopes,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
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
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
    match expr {
        Expr::Literal(lit) => match &lit.value {
            Literal::Int(_) => Ok(Ty::Builtin(BuiltinType::I32)),
            Literal::U8(_) => Ok(Ty::Builtin(BuiltinType::U8)),
            Literal::String(_) => Ok(Ty::Builtin(BuiltinType::String)),
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
                    return Ok(ty);
                }
            }
            if let Some(ty) = resolve_enum_variant(path, use_map, enum_map, module_name) {
                return Ok(ty);
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
                        use_map,
                        struct_map,
                        enum_map,
                        stdlib,
                        ret_ty,
                        module_name,
                    )?;
                    return Ok(Ty::Builtin(BuiltinType::Unit));
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
                        use_map,
                        struct_map,
                        enum_map,
                        stdlib,
                        ret_ty,
                        module_name,
                    )?;
                    if let Ty::Path(ty_name, args) = ret_ty {
                        if ty_name == "Result" && args.len() == 2 {
                            let expected = if name == "Ok" { &args[0] } else { &args[1] };
                            if &arg_ty != expected {
                                return Err(TypeError::new(
                                    format!("{name} argument type mismatch: expected {expected:?}, got {arg_ty:?}"),
                                    call.args[0].span(),
                                ));
                            }
                            return Ok(ret_ty.clone());
                        }
                    }
                    return Ok(Ty::Path(
                        "Result".to_string(),
                        if name == "Ok" {
                            vec![arg_ty, Ty::Builtin(BuiltinType::Unit)]
                        } else {
                            vec![Ty::Builtin(BuiltinType::Unit), arg_ty]
                        },
                    ));
                }
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
            if sig.params.len() != call.args.len() {
                return Err(TypeError::new(
                    format!(
                        "argument count mismatch: expected {}, found {}",
                        sig.params.len(),
                        call.args.len()
                    ),
                    call.span,
                ));
            }
            for (arg, expected) in call.args.iter().zip(&sig.params) {
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
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?;
                let matches_ref = if let Ty::Ref(inner) = expected {
                    &arg_ty == inner.as_ref()
                } else {
                    false
                };
                if &arg_ty != expected_inner && !matches_ref {
                    return Err(TypeError::new(
                        format!("argument type mismatch: expected {expected:?}, found {arg_ty:?}"),
                        arg.span(),
                    ));
                }
            }
            Ok(sig.ret.clone())
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
                if sig.params.len() != method_call.args.len() {
                    return Err(TypeError::new(
                        format!(
                            "argument count mismatch: expected {}, found {}",
                            sig.params.len(),
                            method_call.args.len()
                        ),
                        method_call.span,
                    ));
                }
                for (arg, expected) in method_call.args.iter().zip(&sig.params) {
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
                        use_map,
                        struct_map,
                        enum_map,
                        stdlib,
                        ret_ty,
                        module_name,
                    )?;
                    let matches_ref = if let Ty::Ref(inner) = expected {
                        &arg_ty == inner.as_ref()
                    } else {
                        false
                    };
                    if &arg_ty != expected_inner && !matches_ref {
                        return Err(TypeError::new(
                            format!(
                                "argument type mismatch: expected {expected:?}, found {arg_ty:?}"
                            ),
                            arg.span(),
                        ));
                    }
                }
                return Ok(sig.ret.clone());
            }

            let receiver_ty = check_expr(
                &method_call.receiver,
                functions,
                scopes,
                UseMode::Read,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            let (method_module, type_name, receiver_args) = resolve_method_target(
                &receiver_ty,
                module_name,
                struct_map,
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
            if sig.params.len() != method_call.args.len() + 1 {
                return Err(TypeError::new(
                    format!(
                        "argument count mismatch: expected {}, found {}",
                        sig.params.len() - 1,
                        method_call.args.len()
                    ),
                    method_call.span,
                ));
            }
            let receiver_ptr = Ty::Ptr(Box::new(receiver_ty.clone()));
            let receiver_unqualified = Ty::Path(type_name.clone(), receiver_args);
            let receiver_ptr_unqualified = Ty::Ptr(Box::new(receiver_unqualified.clone()));
            let receiver_ref = Ty::Ref(Box::new(receiver_ty.clone()));
            let receiver_ref_unqualified = Ty::Ref(Box::new(receiver_unqualified.clone()));
            if sig.params[0] != receiver_ty
                && sig.params[0] != receiver_ptr
                && sig.params[0] != receiver_unqualified
                && sig.params[0] != receiver_ptr_unqualified
                && sig.params[0] != receiver_ref
                && sig.params[0] != receiver_ref_unqualified
            {
                return Err(TypeError::new(
                    format!(
                        "method receiver type mismatch: expected {expected:?}, found {receiver_ty:?}",
                        expected = sig.params[0]
                    ),
                    method_call.receiver.span(),
                ));
            }
            if sig.params[0] != receiver_ref && sig.params[0] != receiver_ref_unqualified {
                let _ = check_expr(
                    &method_call.receiver,
                    functions,
                    scopes,
                    UseMode::Move,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?;
            }
            for (arg, expected) in method_call.args.iter().zip(&sig.params[1..]) {
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
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?;
                let matches_ref = if let Ty::Ref(inner) = expected {
                    &arg_ty == inner.as_ref()
                } else {
                    false
                };
                if &arg_ty != expected_inner && !matches_ref {
                    return Err(TypeError::new(
                        format!("argument type mismatch: expected {expected:?}, found {arg_ty:?}"),
                        arg.span(),
                    ));
                }
            }
            Ok(sig.ret.clone())
        }
        Expr::StructLiteral(lit) => check_struct_literal(
            lit,
            functions,
            scopes,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        ),
        Expr::Unary(unary) => {
            let expr_ty = check_expr(
                &unary.expr,
                functions,
                scopes,
                UseMode::Read,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
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
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            let right = check_expr(
                &binary.right,
                functions,
                scopes,
                UseMode::Read,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
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
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        ),
        Expr::Try(try_expr) => {
            let inner_ty = check_expr(
                &try_expr.expr,
                functions,
                scopes,
                UseMode::Move,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            let (ok_ty, err_ty) = match inner_ty {
                Ty::Path(name, args) if name == "Result" && args.len() == 2 => {
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
                Ty::Path(name, args) if name == "Result" && args.len() == 2 => &args[1],
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
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
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
                        return Ok(ty);
                    }
                }
            }

            let object_ty = check_expr(
                &field_access.object,
                functions,
                scopes,
                UseMode::Project,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            let Ty::Path(struct_name, _) = object_ty else {
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
                        "cannot access fields of opaque type `{struct_name}` outside module `{}`",
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
            let field_ty = field_ty.clone();
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
    }
}

/// Check a statement-form match (arms may return, no value required).
fn check_match_stmt(
    match_expr: &MatchExpr,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    scrutinee_mode: UseMode,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
    let match_ty = check_expr(
        &match_expr.expr,
        functions,
        scopes,
        scrutinee_mode,
        use_map,
        struct_map,
        enum_map,
        stdlib,
        ret_ty,
        module_name,
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
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
        )?;
        arm_scope.pop_scope();
        arm_scopes.push(arm_scope);
    }
    merge_match_states(scopes, &arm_scopes, struct_map, enum_map, match_expr.span)?;
    Ok(Ty::Builtin(BuiltinType::Unit))
}

/// Check an expression-form match (all arms must evaluate to the same type).
fn check_match_expr_value(
    match_expr: &MatchExpr,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    scrutinee_mode: UseMode,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
    let match_ty = check_expr(
        &match_expr.expr,
        functions,
        scopes,
        scrutinee_mode,
        use_map,
        struct_map,
        enum_map,
        stdlib,
        ret_ty,
        module_name,
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
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        )?;
        arm_scope.pop_scope();
        arm_scopes.push(arm_scope);
        if let Some(prev) = &result_ty {
            if prev != &arm_ty {
                return Err(TypeError::new(
                    format!("match arm type mismatch: expected {prev:?}, found {arm_ty:?}"),
                    arm.body.span,
                ));
            }
        } else {
            result_ty = Some(arm_ty);
        }
    }
    merge_match_states(scopes, &arm_scopes, struct_map, enum_map, match_expr.span)?;
    Ok(result_ty.unwrap_or(Ty::Builtin(BuiltinType::Unit)))
}

/// Check a single match arm in expression context.
fn check_match_arm_value(
    block: &Block,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
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
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
        )?;
    }
    match last {
        Stmt::Expr(expr_stmt) => check_expr(
            &expr_stmt.expr,
            functions,
            scopes,
            UseMode::Move,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        ),
        _ => Err(TypeError::new(
            "match arm must end with expression".to_string(),
            block.span,
        )),
    }
}

/// Check a struct literal and ensure all fields are present and typed.
fn check_struct_literal(
    lit: &StructLiteralExpr,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
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
    if info.is_opaque && info.module != module_name {
        return Err(TypeError::new(
            format!(
                "cannot construct opaque type `{}` outside module `{}`",
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
        let actual = check_expr(
            &field.expr,
            functions,
            scopes,
            UseMode::Move,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
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

    Ok(Ty::Path(type_name, Vec::new()))
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
                if ty_name == "Result" && args.len() == 2 {
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
                if &ty != match_ty {
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

fn is_orderable_type(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Builtin(BuiltinType::I32)
            | Ty::Builtin(BuiltinType::I64)
            | Ty::Builtin(BuiltinType::U32)
            | Ty::Builtin(BuiltinType::U8)
    )
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
