use std::collections::HashMap;

use crate::ast::*;
use crate::error::TypeError;

use super::{apply_enum_type_args, resolve_enum_variant, EnumInfo, Scopes, Ty, UseMap};

/// Bind locals introduced by a match pattern.
pub(super) fn bind_pattern(
    pattern: &Pattern,
    match_ty: &Ty,
    scopes: &mut Scopes,
    use_map: &UseMap,
    enum_map: &HashMap<String, EnumInfo>,
    module_name: &str,
) -> Result<(), TypeError> {
    match pattern {
        Pattern::Call { path, binding, .. } => {
            let name = path
                .segments
                .iter()
                .map(|seg| seg.item.as_str())
                .collect::<Vec<_>>()
                .join(".");
            if let Ty::Path(ty_name, args) = match_ty {
                if ty_name == "sys.result.Result" && args.len() == 2 {
                    if let Some(binding) = binding {
                        let ty = if name == "Ok" {
                            args[0].clone()
                        } else if name == "Err" {
                            args[1].clone()
                        } else {
                            return Ok(());
                        };
                        scopes.insert_local(binding.item.clone(), ty);
                    }
                    return Ok(());
                }
            }
            if let Some(Ty::Path(enum_name, _)) =
                resolve_enum_variant(path, use_map, enum_map, module_name)
            {
                let Ty::Path(match_name, match_args) = match_ty else {
                    return Err(TypeError::new(
                        format!(
                            "pattern type mismatch: expected {match_ty:?}, found {enum_name:?}"
                        ),
                        path.span,
                    ));
                };
                if match_name != &enum_name {
                    return Err(TypeError::new(
                        format!(
                            "pattern type mismatch: expected {match_ty:?}, found {enum_name:?}"
                        ),
                        path.span,
                    ));
                }
                if let Some(binding) = binding {
                    let Some(info) = enum_map.get(&enum_name) else {
                        return Err(TypeError::new("unknown enum variant".to_string(), path.span));
                    };
                    let variant = path
                        .segments
                        .last()
                        .map(|s| s.item.clone())
                        .unwrap_or_else(|| "unknown".to_string());
                    let payload = info.payloads.get(&variant).cloned().unwrap_or(None);
                    let Some(payload_ty) = payload else {
                        return Err(TypeError::new(
                            format!("variant `{name}` has no payload"),
                            path.span,
                        ));
                    };
                    if info.type_params.len() != match_args.len() {
                        return Err(TypeError::new("pattern type mismatch".to_string(), path.span));
                    }
                    let payload_ty =
                        apply_enum_type_args(&payload_ty, &info.type_params, match_args);
                    scopes.insert_local(binding.item.clone(), payload_ty);
                }
                return Ok(());
            }
            Err(TypeError::new(
                "pattern binding requires an enum match".to_string(),
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

fn same_type_constructor(left: &Ty, right: &Ty) -> bool {
    match (left, right) {
        (Ty::Builtin(l), Ty::Builtin(r)) => l == r,
        (Ty::Path(l, _), Ty::Path(r, _)) => l == r,
        (Ty::Ptr(_), Ty::Ptr(_)) | (Ty::Ref(_), Ty::Ref(_)) => true,
        _ => false,
    }
}

pub(super) fn leftmost_local_in_chain(expr: &Expr) -> Option<(&str, Span)> {
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
