use std::collections::{HashMap, HashSet};

use crate::ast::TypeParam;
use crate::error::TypeError;

use super::{resolve_trait_name, Ty, UseMap, RESERVED_TYPE_PARAMS};

/// Build type argument suffix for method names (e.g., "__u8" for Vec<u8>).
/// This is used to distinguish type-specific impl methods from generic ones.
pub(crate) fn build_type_arg_suffix(type_args: &[Ty]) -> String {
    if type_args.is_empty() {
        return String::new();
    }
    let args: Vec<String> = type_args
        .iter()
        .filter_map(|arg| match arg {
            Ty::Builtin(b) => Some(format!("{:?}", b).to_lowercase()),
            Ty::Path(name, _) => name.rsplit_once('.').map(|(_, t)| t.to_string()),
            Ty::Param(_) => None,
            _ => None,
        })
        .collect();
    if args.is_empty() {
        String::new()
    } else {
        format!("__{}", args.join("_"))
    }
}

pub(super) fn build_type_params(params: &[TypeParam]) -> Result<HashSet<String>, TypeError> {
    let mut set = HashSet::new();
    for param in params {
        let name = param.name.item.as_str();
        if RESERVED_TYPE_PARAMS.contains(&name) {
            return Err(TypeError::new(
                format!("type parameter `{}` is reserved", param.name.item),
                param.name.span,
            ));
        }
        if !set.insert(param.name.item.clone()) {
            return Err(TypeError::new(
                format!("duplicate type parameter `{}`", param.name.item),
                param.name.span,
            ));
        }
    }
    Ok(set)
}

pub(super) fn build_type_param_bounds(
    params: &[TypeParam],
    use_map: &UseMap,
    module_name: &str,
) -> HashMap<String, Vec<String>> {
    let mut bounds = HashMap::new();
    for param in params {
        let mut resolved = Vec::new();
        for bound in &param.bounds {
            resolved.push(resolve_trait_name(bound, use_map, module_name));
        }
        bounds.insert(param.name.item.clone(), resolved);
    }
    bounds
}

pub(super) fn type_param_names(params: &[TypeParam]) -> Vec<String> {
    params.iter().map(|param| param.name.item.clone()).collect()
}

pub(super) fn merge_type_params(
    base: &HashSet<String>,
    params: &[TypeParam],
) -> Result<HashSet<String>, TypeError> {
    let mut set = base.clone();
    for param in params {
        let name = param.name.item.as_str();
        if RESERVED_TYPE_PARAMS.contains(&name) {
            return Err(TypeError::new(
                format!("type parameter `{}` is reserved", param.name.item),
                param.name.span,
            ));
        }
        if set.contains(&param.name.item) {
            return Err(TypeError::new(
                format!("duplicate type parameter `{}`", param.name.item),
                param.name.span,
            ));
        }
        set.insert(param.name.item.clone());
    }
    Ok(set)
}
