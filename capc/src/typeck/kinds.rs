use std::collections::{HashMap, HashSet};

use crate::ast::{Span, Type};
use crate::error::TypeError;

use super::{EnumInfo, StdlibIndex, StructInfo, Ty, TypeKind};

pub(super) fn is_string_ty(ty: &Ty) -> bool {
    matches!(ty, Ty::Path(name, _) if name == "sys.string.string" || name == "string")
}

pub(super) fn stdlib_string_ty(stdlib: &StdlibIndex) -> Ty {
    let name = stdlib
        .types
        .get("string")
        .cloned()
        .unwrap_or_else(|| "sys.string.string".to_string());
    Ty::Path(name, Vec::new())
}

pub(super) fn type_contains_ref(ty: &Type) -> Option<Span> {
    match ty {
        Type::Ref { span, .. } => Some(*span),
        Type::Ptr { target, .. } => type_contains_ref(target),
        Type::Path { args, .. } => {
            for arg in args {
                if let Some(span) = type_contains_ref(arg) {
                    return Some(span);
                }
            }
            None
        }
    }
}

pub(super) fn type_contains_capability(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
) -> bool {
    let mut visiting = HashSet::new();
    type_contains_capability_inner(ty, struct_map, enum_map, &mut visiting)
}

fn type_contains_capability_inner(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    visiting: &mut HashSet<String>,
) -> bool {
    match ty {
        Ty::Builtin(_) | Ty::Ptr(_) | Ty::Ref(_) => false,
        Ty::Param(_) => true,
        Ty::Path(name, args) => {
            if name == "sys.result.Result" {
                return args
                    .iter()
                    .any(|arg| type_contains_capability_inner(arg, struct_map, enum_map, visiting));
            }
            if args
                .iter()
                .any(|arg| type_contains_capability_inner(arg, struct_map, enum_map, visiting))
            {
                return true;
            }
            if let Some(info) = struct_map.get(name) {
                if info.is_capability {
                    return true;
                }
                if !visiting.insert(name.clone()) {
                    return false;
                }
                let contains = info.fields.values().any(|field| {
                    type_contains_capability_inner(field, struct_map, enum_map, visiting)
                });
                visiting.remove(name);
                return contains;
            }
            if let Some(info) = enum_map.get(name) {
                if !visiting.insert(name.clone()) {
                    return false;
                }
                let contains = info.payloads.values().any(|payload| {
                    if let Some(payload_ty) = payload {
                        type_contains_capability_inner(payload_ty, struct_map, enum_map, visiting)
                    } else {
                        false
                    }
                });
                visiting.remove(name);
                return contains;
            }
            false
        }
    }
}

pub(super) fn type_contains_non_linear_capability(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
) -> bool {
    let mut visiting = HashSet::new();
    type_contains_non_linear_capability_inner(ty, struct_map, enum_map, &mut visiting)
}

fn type_contains_non_linear_capability_inner(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    visiting: &mut HashSet<String>,
) -> bool {
    match ty {
        Ty::Builtin(_) | Ty::Ptr(_) | Ty::Ref(_) => false,
        Ty::Param(_) => true,
        Ty::Path(name, args) => {
            if name == "sys.result.Result" {
                return args.iter().any(|arg| {
                    type_contains_non_linear_capability_inner(arg, struct_map, enum_map, visiting)
                });
            }
            if args.iter().any(|arg| {
                type_contains_non_linear_capability_inner(arg, struct_map, enum_map, visiting)
            }) {
                return true;
            }
            if let Some(info) = struct_map.get(name) {
                if info.is_capability {
                    return info.kind != TypeKind::Linear;
                }
                if !visiting.insert(name.clone()) {
                    return false;
                }
                let contains = info.fields.values().any(|field| {
                    type_contains_non_linear_capability_inner(field, struct_map, enum_map, visiting)
                });
                visiting.remove(name);
                return contains;
            }
            if let Some(info) = enum_map.get(name) {
                if !visiting.insert(name.clone()) {
                    return false;
                }
                let contains = info.payloads.values().any(|payload| {
                    if let Some(payload_ty) = payload {
                        type_contains_non_linear_capability_inner(
                            payload_ty, struct_map, enum_map, visiting,
                        )
                    } else {
                        false
                    }
                });
                visiting.remove(name);
                return contains;
            }
            false
        }
    }
}

pub(super) fn is_affine_type(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
) -> bool {
    type_kind(ty, struct_map, enum_map) != TypeKind::Unrestricted
}

pub(super) fn type_kind(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
) -> TypeKind {
    let mut visiting = HashSet::new();
    type_kind_inner(ty, struct_map, enum_map, &mut visiting)
}

fn combine_kind(left: TypeKind, right: TypeKind) -> TypeKind {
    match (left, right) {
        (TypeKind::Linear, _) | (_, TypeKind::Linear) => TypeKind::Linear,
        (TypeKind::Affine, _) | (_, TypeKind::Affine) => TypeKind::Affine,
        _ => TypeKind::Unrestricted,
    }
}

fn type_kind_inner(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    visiting: &mut HashSet<String>,
) -> TypeKind {
    match ty {
        Ty::Builtin(_) | Ty::Ptr(_) | Ty::Ref(_) => TypeKind::Unrestricted,
        Ty::Param(_) => TypeKind::Affine,
        Ty::Path(name, args) => {
            if name == "sys.result.Result" {
                return args.iter().fold(TypeKind::Unrestricted, |acc, arg| {
                    combine_kind(acc, type_kind_inner(arg, struct_map, enum_map, visiting))
                });
            }
            if visiting.contains(name) {
                return TypeKind::Unrestricted;
            }
            if let Some(info) = struct_map.get(name) {
                visiting.insert(name.clone());
                let fields_kind =
                    info.fields
                        .values()
                        .fold(TypeKind::Unrestricted, |acc, field| {
                            combine_kind(
                                acc,
                                type_kind_inner(field, struct_map, enum_map, visiting),
                            )
                        });
                visiting.remove(name);
                return combine_kind(info.kind, fields_kind);
            }
            if let Some(info) = enum_map.get(name) {
                visiting.insert(name.clone());
                let payload_kind =
                    info.payloads
                        .values()
                        .fold(TypeKind::Unrestricted, |acc, payload| {
                            if let Some(payload_ty) = payload {
                                combine_kind(
                                    acc,
                                    type_kind_inner(payload_ty, struct_map, enum_map, visiting),
                                )
                            } else {
                                acc
                            }
                        });
                visiting.remove(name);
                return payload_kind;
            }
            TypeKind::Unrestricted
        }
    }
}

pub(super) fn validate_type_args(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    match ty {
        Ty::Builtin(_) | Ty::Param(_) => Ok(()),
        Ty::Ptr(inner) | Ty::Ref(inner) => validate_type_args(inner, struct_map, enum_map, span),
        Ty::Path(name, args) => {
            if let Some(info) = struct_map.get(name) {
                if args.len() != info.type_params.len() {
                    return Err(TypeError::new(
                        format!(
                            "type `{}` expects {} type argument(s), found {}",
                            name,
                            info.type_params.len(),
                            args.len()
                        ),
                        span,
                    ));
                }
            } else if let Some(info) = enum_map.get(name) {
                if args.len() != info.type_params.len() {
                    return Err(TypeError::new(
                        format!(
                            "type `{}` expects {} type argument(s), found {}",
                            name,
                            info.type_params.len(),
                            args.len()
                        ),
                        span,
                    ));
                }
            }
            for arg in args {
                validate_type_args(arg, struct_map, enum_map, span)?;
            }
            Ok(())
        }
    }
}
