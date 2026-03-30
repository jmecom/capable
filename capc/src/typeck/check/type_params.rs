use super::*;

pub(super) fn lower_type_args(
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

pub(super) fn build_type_substitution(
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

pub(super) fn substitute_type(ty: &Ty, subs: &HashMap<String, Ty>) -> Ty {
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

pub(super) fn match_type_params(
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

pub(super) fn build_call_substitution(
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

pub(super) fn enforce_type_param_bounds(
    sig: &FunctionSig,
    subs: &HashMap<String, Ty>,
    trait_impls: &[TraitImplInfo],
    span: Span,
) -> Result<(), TypeError> {
    for (param, bounds) in &sig.type_param_bounds {
        let Some(actual) = subs.get(param) else {
            continue;
        };
        for bound in bounds {
            if type_satisfies_trait(actual, bound, trait_impls, span).is_err() {
                return Err(TypeError::new(
                    format!("type parameter `{param}` does not implement `{bound}`"),
                    span,
                ));
            }
        }
    }
    Ok(())
}

pub(super) fn type_satisfies_trait(
    actual: &Ty,
    trait_name: &str,
    trait_impls: &[TraitImplInfo],
    span: Span,
) -> Result<(), TypeError> {
    for impl_info in trait_impls {
        if impl_info.trait_name != trait_name {
            continue;
        }
        let mut subs = HashMap::new();
        if match_type_params(&impl_info.target_ty, actual, &mut subs, span).is_ok() {
            return Ok(());
        }
    }
    Err(TypeError::new(
        format!("type `{actual:?}` does not implement `{trait_name}`"),
        span,
    ))
}
