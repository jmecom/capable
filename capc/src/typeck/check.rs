mod calls;
mod match_check;
mod stmt;
mod type_params;

use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::error::TypeError;

use super::{
    bind_pattern, build_type_param_bounds, build_type_params, enum_payload_matches,
    infer_enum_args, is_affine_type, is_numeric_type, is_orderable_type, is_string_ty,
    leftmost_local_in_chain, lower_type, resolve_enum_type_args, resolve_enum_variant,
    resolve_method_target, resolve_path, resolve_type_name, stdlib_string_ty,
    type_contains_ref, ty_equivalent_for_set,
    validate_type_args, BuiltinType, EnumInfo, FunctionSig, MoveState, Scopes, StdlibIndex,
    StructInfo, TraitImplInfo, TraitInfo, Ty, TypeTable, UseMap, UseMode, ensure_affine_states_match,
    ensure_linear_all_consumed, ensure_linear_scope_consumed,
    ensure_linear_scopes_consumed_from, merge_branch_states, merge_match_states, stmt_is_total,
};
use calls::{check_call_expr, check_method_call_expr};
use match_check::{check_match_expr_value, check_match_stmt};
use stmt::{check_block, check_stmt};
use type_params::{
    build_call_substitution, build_type_substitution, enforce_type_param_bounds,
    lower_type_args, match_type_params, substitute_type,
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
            table.record(expr.id(), ty.clone());
        }
    }
}

fn record_expr_type(recorder: &mut TypeRecorder, expr: &Expr, ty: Ty) -> Result<Ty, TypeError> {
    recorder.record(expr, &ty);
    Ok(ty)
}

/// Type-check a function body, including move/linear rules.
pub(super) fn check_function(
    func: &Function,
    functions: &HashMap<String, FunctionSig>,
    trait_map: &HashMap<String, TraitInfo>,
    trait_impls: &[TraitImplInfo],
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
    type_table: Option<&mut TypeTable>,
) -> Result<(), TypeError> {
    let trusted_stdlib = module_name.starts_with("sys.");
    let type_params = build_type_params(&func.type_params)?;
    let type_param_bounds = build_type_param_bounds(&func.type_params, use_map, module_name);
    for (param, bounds) in &type_param_bounds {
        for bound in bounds {
            if !trait_map.contains_key(bound) {
                return Err(TypeError::new(
                    format!("unknown trait `{bound}` for type parameter `{param}`"),
                    func.span,
                ));
            }
        }
    }
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
            trait_map,
            trait_impls,
            &mut scopes,
            &mut recorder,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
            &type_params,
            &type_param_bounds,
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

    if !trusted_stdlib {
        ensure_linear_all_consumed(&scopes, struct_map, enum_map, func.body.span)?;
    }

    Ok(())
}

/// Type-check an expression, applying move rules based on `use_mode`.
pub(super) fn check_expr(
    expr: &Expr,
    functions: &HashMap<String, FunctionSig>,
    trait_map: &HashMap<String, TraitInfo>,
    trait_impls: &[TraitImplInfo],
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
    type_param_bounds: &HashMap<String, Vec<String>>,
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
                    let trusted_stdlib = module_name.starts_with("sys.");
                    if info.state == MoveState::Moved && !trusted_stdlib {
                        return Err(TypeError::new(
                            format!("use of moved value `{name}`"),
                            path.segments[0].span,
                        ));
                    }
                    if !trusted_stdlib
                        && use_mode == UseMode::Move
                        && is_affine_type(&ty, struct_map, enum_map)
                    {
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
            Err(TypeError::new(format!("unknown value `{path}`"), path.span))
        }
        Expr::Call(call) => check_call_expr(
            expr,
            call,
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
        ),
        Expr::MethodCall(method_call) => check_method_call_expr(
            method_call,
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
        ),
        Expr::StructLiteral(lit) => check_struct_literal(
            lit,
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
        ),
        Expr::Unary(unary) => {
            let expr_ty = check_expr(
                &unary.expr,
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
                UnaryOp::BitNot => {
                    if is_numeric_type(&expr_ty) {
                        Ok(expr_ty)
                    } else {
                        Err(TypeError::new(
                            "unary ~ expects integer".to_string(),
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
            let right = check_expr(
                &binary.right,
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
            match binary.op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                    if left == right
                        && (left == Ty::Builtin(BuiltinType::I32)
                            || left == Ty::Builtin(BuiltinType::I64))
                    {
                        Ok(left)
                    } else if left == right
                        && matches!(left, Ty::Param(_))
                        && module_name == "sys.vec"
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
                BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                    if left == right && is_numeric_type(&left) {
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
                            "bitwise operators expect matching integer types".to_string(),
                            binary.span,
                        ))
                    }
                }
                BinaryOp::Shl | BinaryOp::Shr => {
                    if left == right && is_numeric_type(&left) {
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
                            "shift operators expect matching integer types".to_string(),
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
            trait_map,
            trait_impls,
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
            type_param_bounds,
            false, // nested expression matches still cannot break/continue
        ),
        Expr::Try(try_expr) => {
            let inner_ty = check_expr(
                &try_expr.expr,
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
                        "the `?` operator can only be used in functions returning Result"
                            .to_string(),
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
            trait_map,
            trait_impls,
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
            type_param_bounds,
        ),
        Expr::FieldAccess(field_access) => {
            fn get_leftmost_path_segment(expr: &Expr) -> Option<&str> {
                match expr {
                    Expr::Path(path) if path.segments.len() == 1 => Some(&path.segments[0].item),
                    Expr::FieldAccess(fa) => get_leftmost_path_segment(&fa.object),
                    _ => None,
                }
            }

            let base_is_local =
                if let Some(base_name) = get_leftmost_path_segment(&field_access.object) {
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
                trait_map,
                trait_impls,
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
                type_param_bounds,
            )?;
            // Unwrap reference if present (for borrow-lite field access)
            let object_ty = match &object_ty {
                Ty::Ref(inner) => inner.as_ref().clone(),
                other => other.clone(),
            };
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
            let substitutions =
                build_type_substitution(&info.type_params, &struct_args, field_access.span)?;
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

            // Type check the index (must be i32)
            let index_ty = check_expr(
                &index_expr.index,
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
                Ty::Path(name, args) if name == "Vec" || name == "sys.vec.Vec" => {
                    if args.len() != 1 {
                        return Err(TypeError::new(
                            "Vec expects exactly one type argument".to_string(),
                            index_expr.span,
                        ));
                    }
                    Ok(Ty::Path(
                        "sys.result.Result".to_string(),
                        vec![
                            args[0].clone(),
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

/// Check a struct literal and ensure all fields are present and typed.
fn check_struct_literal(
    lit: &StructLiteralExpr,
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
    let (key, info) = match struct_map.get(&key) {
        Some(info) => (key, info),
        None => {
            let qualified = if lit.path.segments.len() == 1 {
                format!("{}.{}", module_name, key)
            } else {
                key.clone()
            };
            let info = struct_map
                .get(&qualified)
                .ok_or_else(|| TypeError::new(format!("unknown struct `{}`", key), lit.span))?;
            (qualified, info)
        }
    };
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
            TypeError::new(format!("unknown field `{}`", field.name.item), field.span)
        })?;
        let expected = substitute_type(&expected, &substitutions);
        let actual = check_expr(
            &field.expr,
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
        if actual != expected {
            return Err(TypeError::new(
                format!(
                    "field `{}` expects {expected:?}, found {actual:?}",
                    field.name.item
                ),
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
