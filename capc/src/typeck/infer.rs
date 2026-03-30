use std::collections::{HashMap, HashSet};

use super::{BuiltinType, Ty};

pub(super) fn infer_enum_args(
    template: &Ty,
    actual: &Ty,
    inferred: &mut HashMap<String, Ty>,
) -> bool {
    match template {
        Ty::Param(name) => match inferred.get(name) {
            Some(existing) => {
                existing == actual
                    || matches!(actual, Ty::Path(actual_name, args) if actual_name == name && args.is_empty())
            }
            None => {
                inferred.insert(name.clone(), actual.clone());
                true
            }
        },
        Ty::Builtin(b) => matches!(actual, Ty::Builtin(other) if other == b),
        Ty::Ptr(inner) => {
            matches!(actual, Ty::Ptr(other) if infer_enum_args(inner, other, inferred))
        }
        Ty::Ref(inner) => {
            matches!(actual, Ty::Ref(other) if infer_enum_args(inner, other, inferred))
        }
        Ty::Path(name, args) => match actual {
            Ty::Path(other_name, other_args)
                if other_name == name && args.len() == other_args.len() =>
            {
                args.iter()
                    .zip(other_args.iter())
                    .all(|(a, b)| infer_enum_args(a, b, inferred))
            }
            _ => false,
        },
    }
}

pub(super) fn resolve_enum_type_args(
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

pub(super) fn apply_enum_type_args(ty: &Ty, type_params: &[String], type_args: &[Ty]) -> Ty {
    match ty {
        Ty::Param(name) => {
            if let Some(idx) = type_params.iter().position(|p| p == name) {
                return type_args.get(idx).cloned().unwrap_or_else(|| ty.clone());
            }
            ty.clone()
        }
        Ty::Builtin(_) => ty.clone(),
        Ty::Ptr(inner) => Ty::Ptr(Box::new(apply_enum_type_args(
            inner,
            type_params,
            type_args,
        ))),
        Ty::Ref(inner) => Ty::Ref(Box::new(apply_enum_type_args(
            inner,
            type_params,
            type_args,
        ))),
        Ty::Path(name, args) => Ty::Path(
            name.clone(),
            args.iter()
                .map(|arg| apply_enum_type_args(arg, type_params, type_args))
                .collect(),
        ),
    }
}

pub(super) fn enum_payload_matches(
    payload: &Ty,
    arg_ty: &Ty,
    type_params: &[String],
    type_args: &[Ty],
) -> bool {
    let expected = apply_enum_type_args(payload, type_params, type_args);
    ty_equivalent_for_params(&expected, arg_ty, type_params)
}

pub(super) fn ty_equivalent_for_params(left: &Ty, right: &Ty, type_params: &[String]) -> bool {
    match (left, right) {
        (Ty::Param(name), Ty::Path(other, args))
            if args.is_empty() && name == other && type_params.contains(name) =>
        {
            true
        }
        (Ty::Path(name, args), Ty::Param(other))
            if args.is_empty() && name == other && type_params.contains(other) =>
        {
            true
        }
        (Ty::Ptr(l), Ty::Ptr(r)) | (Ty::Ref(l), Ty::Ref(r)) => {
            ty_equivalent_for_params(l, r, type_params)
        }
        (Ty::Path(name, args), Ty::Path(other, other_args))
            if name == other && args.len() == other_args.len() =>
        {
            args.iter()
                .zip(other_args.iter())
                .all(|(a, b)| ty_equivalent_for_params(a, b, type_params))
        }
        _ => left == right,
    }
}

pub(super) fn ty_equivalent_for_set(left: &Ty, right: &Ty, type_params: &HashSet<String>) -> bool {
    match (left, right) {
        (Ty::Param(name), Ty::Path(other, args))
            if args.is_empty() && name == other && type_params.contains(name) =>
        {
            true
        }
        (Ty::Path(name, args), Ty::Param(other))
            if args.is_empty() && name == other && type_params.contains(other) =>
        {
            true
        }
        (Ty::Ptr(l), Ty::Ptr(r)) | (Ty::Ref(l), Ty::Ref(r)) => {
            ty_equivalent_for_set(l, r, type_params)
        }
        (Ty::Path(name, args), Ty::Path(other, other_args))
            if name == other && args.len() == other_args.len() =>
        {
            args.iter()
                .zip(other_args.iter())
                .all(|(a, b)| ty_equivalent_for_set(a, b, type_params))
        }
        _ => left == right,
    }
}
