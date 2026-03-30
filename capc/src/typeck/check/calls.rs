use super::*;

fn enforce_vec_method_constraints(
    receiver_ty: &Ty,
    method: &str,
    span: Span,
) -> Result<(), TypeError> {
    let base = match receiver_ty {
        Ty::Ref(inner) | Ty::Ptr(inner) => inner.as_ref(),
        _ => receiver_ty,
    };
    let Ty::Path(name, args) = base else {
        return Ok(());
    };
    if name != "Vec" && name != "sys.vec.Vec" {
        return Ok(());
    }
    if args.len() != 1 {
        return Err(TypeError::new(
            "Vec expects exactly one type argument".to_string(),
            span,
        ));
    }
    let elem = &args[0];
    let is_u8 = matches!(elem, Ty::Builtin(BuiltinType::U8));
    let is_i32 = matches!(elem, Ty::Builtin(BuiltinType::I32));
    let is_string = is_string_ty(elem);
    let is_param = matches!(elem, Ty::Param(_));
    match method {
        "as_slice" | "slice" | "extend_slice" | "copy_string" => {
            if !is_u8 {
                return Err(TypeError::new(
                    format!("Vec<{elem:?}> does not support `{method}`"),
                    span,
                ));
            }
        }
        "capacity" | "reserve" | "shrink_to_fit" => {
            if !is_u8 && !is_i32 && !is_string && !is_param {
                return Err(TypeError::new(
                    format!("Vec<{elem:?}> does not support `{method}`"),
                    span,
                ));
            }
        }
        "filter" | "map_add" => {
            if !is_u8 && !is_i32 {
                return Err(TypeError::new(
                    format!("Vec<{elem:?}> does not support `{method}`"),
                    span,
                ));
            }
        }
        "set" => {}
        "join" => {
            if !is_string {
                return Err(TypeError::new(
                    format!("Vec<{elem:?}> does not support `{method}`"),
                    span,
                ));
            }
        }
        _ => {}
    }
    Ok(())
}

fn check_argument_types(
    args: &[Expr],
    expected_types: &[Ty],
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
) -> Result<(), TypeError> {
    if expected_types.len() != args.len() {
        return Err(TypeError::new(
            format!(
                "argument count mismatch: expected {}, found {}",
                expected_types.len(),
                args.len()
            ),
            args.last().map_or(Span::new(0, 0), Expr::span),
        ));
    }

    for (arg, expected) in args.iter().zip(expected_types) {
        let (expected_inner, use_mode) = if let Ty::Ref(inner) = expected {
            (inner.as_ref(), UseMode::Read)
        } else {
            (expected, UseMode::Move)
        };
        let arg_ty = check_expr(
            arg,
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

    Ok(())
}

pub(super) fn check_call_expr(
    expr: &Expr,
    call: &CallExpr,
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
            return Ok(Ty::Builtin(BuiltinType::Unit));
        }
        if name == "panic" {
            if !call.args.is_empty() {
                return Err(TypeError::new(
                    "panic takes no arguments".to_string(),
                    call.span,
                ));
            }
            return Ok(Ty::Builtin(BuiltinType::Never));
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
            if let Ty::Path(ty_name, args) = ret_ty {
                if ty_name == "sys.result.Result" && args.len() == 2 {
                    let expected = if name == "Ok" { &args[0] } else { &args[1] };
                    if !ty_equivalent_for_set(&arg_ty, expected, type_params) {
                        return Err(TypeError::new(
                            format!(
                                "{name} argument type mismatch: expected {expected:?}, got {arg_ty:?}"
                            ),
                            call.args[0].span(),
                        ));
                    }
                    return Ok(ret_ty.clone());
                }
            }
            return Ok(Ty::Path(
                "sys.result.Result".to_string(),
                if name == "Ok" {
                    vec![arg_ty, Ty::Builtin(BuiltinType::Unit)]
                } else {
                    vec![Ty::Builtin(BuiltinType::Unit), arg_ty]
                },
            ));
        }
    }

    if let Some(Ty::Path(enum_name, _)) = resolve_enum_variant(&path, use_map, enum_map, module_name)
    {
        let Some(info) = enum_map.get(&enum_name) else {
            return Err(TypeError::new("unknown enum variant".to_string(), call.span));
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

        let type_args = resolve_enum_type_args(&enum_name, &info.type_params, &inferred, ret_ty);

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

        return Ok(Ty::Path(enum_name, type_args));
    }

    let resolved = resolve_path(&path, use_map);
    let key = resolved.join(".");

    let sig = if let Some(sig) = functions.get(&key) {
        sig
    } else if resolved.len() == 1 {
        let qualified = format!("{}.{}", module_name, key);
        functions
            .get(&qualified)
            .ok_or_else(|| TypeError::new(format!("unknown function `{key}`"), path.span))?
    } else {
        return Err(TypeError::new(
            format!("unknown function `{key}`"),
            path.span,
        ));
    };
    if sig.module != module_name && !sig.is_pub {
        return Err(TypeError::new(
            format!("function `{}` is private", key),
            call.span,
        ));
    }
    let explicit_type_args =
        lower_type_args(&call.type_args, use_map, stdlib, struct_map, enum_map, type_params)?;
    let subs = build_call_substitution(sig, &explicit_type_args, HashMap::new(), call.span)?;
    enforce_type_param_bounds(sig, &subs, trait_impls, call.span)?;
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
    check_argument_types(
        &call.args,
        &instantiated_params,
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
    )?;
    let _ = expr;
    Ok(instantiated_ret)
}

pub(super) fn check_method_call_expr(
    method_call: &MethodCallExpr,
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
    fn get_leftmost_segment(expr: &Expr) -> Option<&str> {
        match expr {
            Expr::Path(path) if path.segments.len() == 1 => Some(&path.segments[0].item),
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
        let Some(path) = path_call else {
            return Err(TypeError::new(
                "method receiver path could not be resolved".to_string(),
                method_call.span,
            ));
        };
        let resolved = resolve_path(&path, use_map);
        let key = resolved.join(".");
        let sig = functions
            .get(&key)
            .ok_or_else(|| TypeError::new(format!("unknown function `{key}`"), path.span))?;
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
        enforce_type_param_bounds(sig, &subs, trait_impls, method_call.span)?;
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
        check_argument_types(
            &method_call.args,
            &instantiated_params,
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
        )?;
        return Ok(instantiated_ret);
    }

    let receiver_ty = check_expr(
        &method_call.receiver,
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
    enforce_vec_method_constraints(&receiver_ty, &method_call.method.item, method_call.method.span)?;
    let receiver_base = match &receiver_ty {
        Ty::Ref(inner) | Ty::Ptr(inner) => inner.as_ref(),
        _ => &receiver_ty,
    };
    if let Ty::Param(param_name) = receiver_base {
        let bounds = type_param_bounds
            .get(param_name)
            .cloned()
            .unwrap_or_default();
        let mut candidates = Vec::new();
        for bound in bounds {
            if let Some(info) = trait_map.get(&bound) {
                if let Some(sig) = info.methods.get(&method_call.method.item) {
                    candidates.push((bound, sig));
                }
            }
        }
        if candidates.is_empty() {
            return Err(TypeError::new(
                format!(
                    "no trait bound provides method `{}` for `{}`",
                    method_call.method.item, param_name
                ),
                method_call.span,
            ));
        }
        if candidates.len() > 1 {
            return Err(TypeError::new(
                format!(
                    "ambiguous method `{}` for `{}`; multiple trait bounds apply",
                    method_call.method.item, param_name
                ),
                method_call.span,
            ));
        }
        let (_trait_name, sig) = candidates.remove(0);
        let mut inferred = HashMap::new();
        let expected_receiver = match &sig.params[0] {
            Ty::Ref(inner) | Ty::Ptr(inner) => inner.as_ref(),
            other => other,
        };
        let actual_receiver = match &receiver_ty {
            Ty::Ref(inner) | Ty::Ptr(inner) => inner.as_ref(),
            other => other,
        };
        match_type_params(
            expected_receiver,
            actual_receiver,
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
        let subs =
            build_call_substitution(sig, &explicit_type_args, inferred, method_call.span)?;
        enforce_type_param_bounds(sig, &subs, trait_impls, method_call.span)?;
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
        if instantiated_params[0] != receiver_ty {
            return Err(TypeError::new(
                format!(
                    "method receiver type mismatch: expected {expected:?}, found {receiver_ty:?}",
                    expected = instantiated_params[0]
                ),
                method_call.receiver.span(),
            ));
        }
        if !matches!(instantiated_params[0], Ty::Ref(_)) {
            let _ = check_expr(
                &method_call.receiver,
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
        check_argument_types(
            &method_call.args,
            &instantiated_params[1..],
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
        )?;
        return Ok(instantiated_ret);
    }

    let (method_module, type_name, receiver_args) = resolve_method_target(
        &receiver_ty,
        module_name,
        struct_map,
        enum_map,
        method_call.receiver.span(),
    )?;

    let type_arg_suffix = super::super::build_type_arg_suffix(&receiver_args);
    let base_method_fn = format!("{type_name}__{}", method_call.method.item);
    let specific_method_fn = format!("{type_name}{type_arg_suffix}__{}", method_call.method.item);

    let qualified_specific = format!("{method_module}.{specific_method_fn}");
    let qualified_base = format!("{method_module}.{base_method_fn}");

    let key = if !type_arg_suffix.is_empty() && functions.contains_key(&qualified_specific) {
        qualified_specific
    } else if functions.contains_key(&qualified_base) {
        qualified_base
    } else if method_module == module_name
        && !type_arg_suffix.is_empty()
        && functions.contains_key(&specific_method_fn)
    {
        specific_method_fn.clone()
    } else if method_module == module_name && functions.contains_key(&base_method_fn) {
        base_method_fn.clone()
    } else {
        return Err(TypeError::new(
            format!("unknown method `{qualified_base}`"),
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
    enforce_type_param_bounds(sig, &subs, trait_impls, method_call.span)?;
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
    if instantiated_params[0] != receiver_ref && instantiated_params[0] != receiver_ref_unqualified
    {
        let _ = check_expr(
            &method_call.receiver,
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
    check_argument_types(
        &method_call.args,
        &instantiated_params[1..],
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
    )?;
    Ok(instantiated_ret)
}
