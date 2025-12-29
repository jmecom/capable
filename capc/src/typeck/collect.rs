use std::collections::HashMap;

use crate::ast::*;
use crate::error::TypeError;

use super::{
    build_type_params, desugar_impl_methods, lower_type, type_contains_ref, EnumInfo, FunctionSig,
    StructInfo, TypeKind, UseMap, StdlibIndex, RESERVED_TYPE_PARAMS, validate_type_args,
};

/// Build the stdlib type index for name resolution.
pub(super) fn build_stdlib_index(stdlib: &[Module]) -> Result<StdlibIndex, TypeError> {
    let mut types = HashMap::new();
    for module in stdlib {
        let module_name = module.name.to_string();
        for item in &module.items {
            let name = match item {
                Item::Struct(decl) if decl.is_pub => decl.name.item.clone(),
                Item::Enum(decl) if decl.is_pub => decl.name.item.clone(),
                _ => continue,
            };
            let qualified = format!("{module_name}.{name}");
            if types.insert(name.clone(), qualified).is_some() {
                return Err(TypeError::new(
                    format!("duplicate stdlib type `{name}`"),
                    module.span,
                ));
            }
        }
    }
    Ok(StdlibIndex { types })
}

/// Collect and resolve function signatures across modules.
pub(super) fn collect_functions(
    modules: &[&Module],
    entry_name: &str,
    stdlib: &StdlibIndex,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
) -> Result<HashMap<String, FunctionSig>, TypeError> {
    let mut functions = HashMap::new();
    for module in modules {
        let module_name = module.name.to_string();
        let local_use = UseMap::new(module);
        let mut impl_methods = std::collections::HashSet::new();
        for item in &module.items {
            let mut add_function = |name: &Ident,
                                    type_params: &[Ident],
                                    params: &[Param],
                                    ret: &Type,
                                    span: Span,
                                    is_pub: bool|
             -> Result<(), TypeError> {
                let type_param_set = build_type_params(type_params)?;
                for param in params {
                    if param.ty.is_none() {
                        return Err(TypeError::new(
                            format!("parameter `{}` requires a type annotation", param.name.item),
                            param.name.span,
                        ));
                    }
                }
                let sig = FunctionSig {
                    type_params: type_params.iter().map(|p| p.item.clone()).collect(),
                    params: params
                        .iter()
                        .map(|p| {
                            lower_type(
                                p.ty.as_ref().expect("param type checked"),
                                &local_use,
                                stdlib,
                                &type_param_set,
                            )
                        })
                        .collect::<Result<_, _>>()?,
                    ret: lower_type(ret, &local_use, stdlib, &type_param_set)?,
                    module: module_name.clone(),
                    is_pub,
                };
                let qualified_key = format!("{module_name}.{}", name.item);
                if functions.insert(qualified_key.clone(), sig.clone()).is_some() {
                    return Err(TypeError::new(
                        format!("duplicate function `{qualified_key}`"),
                        span,
                    ));
                }
                if module_name == entry_name {
                    let key = name.item.clone();
                    if functions.insert(key.clone(), sig).is_some() {
                        return Err(TypeError::new(
                            format!("duplicate function `{key}`"),
                            span,
                        ));
                    }
                }
                Ok(())
            };

            match item {
                Item::Function(func) => {
                    if func.name.item.contains("__") {
                        return Err(TypeError::new(
                            "function names may not contain `__`".to_string(),
                            func.name.span,
                        ));
                    }
                    add_function(
                        &func.name,
                        &func.type_params,
                        &func.params,
                        &func.ret,
                        func.name.span,
                        func.is_pub,
                    )?;
                }
                Item::ExternFunction(func) => {
                    if func.name.item.contains("__") {
                        return Err(TypeError::new(
                            "function names may not contain `__`".to_string(),
                            func.name.span,
                        ));
                    }
                    add_function(
                        &func.name,
                        &func.type_params,
                        &func.params,
                        &func.ret,
                        func.name.span,
                        func.is_pub,
                    )?;
                }
                Item::Impl(impl_block) => {
                    let methods = desugar_impl_methods(
                        impl_block,
                        &module_name,
                        &local_use,
                        stdlib,
                        struct_map,
                        enum_map,
                    )?;
                    for method in methods {
                        if let Some((impl_ty, method_name)) = method.name.item.split_once("__") {
                            let key = format!("{impl_ty}::{method_name}");
                            if !impl_methods.insert(key.clone()) {
                                return Err(TypeError::new(
                                    format!(
                                        "duplicate method `{method_name}` for `{impl_ty}`"
                                    ),
                                    method.name.span,
                                ));
                            }
                        }
                        add_function(
                            &method.name,
                            &method.type_params,
                            &method.params,
                            &method.ret,
                            method.name.span,
                            method.is_pub,
                        )?;
                    }
                }
                _ => {}
            }
        }
    }
    Ok(functions)
}

/// Collect struct metadata needed by the checker.
pub(super) fn collect_structs(
    modules: &[&Module],
    entry_name: &str,
    stdlib: &StdlibIndex,
) -> Result<HashMap<String, StructInfo>, TypeError> {
    let mut structs = HashMap::new();
    for module in modules {
        let module_name = module.name.to_string();
        let local_use = UseMap::new(module);
        for item in &module.items {
            if let Item::Struct(decl) = item {
                if RESERVED_TYPE_PARAMS.contains(&decl.name.item.as_str()) {
                    return Err(TypeError::new(
                        format!("type name `{}` is reserved", decl.name.item),
                        decl.name.span,
                    ));
                }
                let type_param_set = build_type_params(&decl.type_params)?;
                let mut fields = HashMap::new();
                for field in &decl.fields {
                    if let Some(span) = type_contains_ref(&field.ty) {
                        return Err(TypeError::new(
                            "reference types cannot be stored in structs".to_string(),
                            span,
                        ));
                    }
                    let ty = lower_type(&field.ty, &local_use, stdlib, &type_param_set)?;
                    if fields.insert(field.name.item.clone(), ty).is_some() {
                        return Err(TypeError::new(
                            format!("duplicate field `{}`", field.name.item),
                            field.name.span,
                        ));
                    }
                }
                let qualified = format!("{module_name}.{}", decl.name.item);
                if structs.contains_key(&qualified) {
                    return Err(TypeError::new(
                        format!("duplicate struct `{qualified}`"),
                        decl.name.span,
                    ));
                }
                let declared_kind = if decl.is_linear {
                    TypeKind::Linear
                } else if decl.is_copy {
                    TypeKind::Unrestricted
                } else if decl.is_opaque || decl.is_capability {
                    TypeKind::Affine
                } else {
                    TypeKind::Unrestricted
                };
                let info = StructInfo {
                    type_params: decl.type_params.iter().map(|p| p.item.clone()).collect(),
                    fields,
                    is_opaque: decl.is_opaque || decl.is_capability,
                    is_capability: decl.is_capability,
                    kind: declared_kind,
                    module: module_name.clone(),
                };
                structs.insert(qualified, info.clone());
                if module_name == entry_name {
                    let name = decl.name.item.clone();
                    if structs.contains_key(&name) {
                        return Err(TypeError::new(
                            format!("duplicate struct `{name}`"),
                            decl.name.span,
                        ));
                    }
                    structs.insert(name, info);
                }
            }
        }
    }
    Ok(structs)
}

/// Collect enum metadata needed by the checker.
pub(super) fn collect_enums(
    modules: &[&Module],
    entry_name: &str,
    stdlib: &StdlibIndex,
) -> Result<HashMap<String, EnumInfo>, TypeError> {
    let mut enums = HashMap::new();
    for module in modules {
        let module_name = module.name.to_string();
        let local_use = UseMap::new(module);
        for item in &module.items {
            if let Item::Enum(decl) = item {
                if RESERVED_TYPE_PARAMS.contains(&decl.name.item.as_str()) {
                    return Err(TypeError::new(
                        format!("type name `{}` is reserved", decl.name.item),
                        decl.name.span,
                    ));
                }
                let type_param_set = build_type_params(&decl.type_params)?;
                let mut variants = Vec::new();
                let mut payloads = HashMap::new();
                for variant in &decl.variants {
                    if variants.contains(&variant.name.item) {
                        return Err(TypeError::new(
                            format!(
                                "duplicate variant `{}` in enum `{}`",
                                variant.name.item, decl.name.item
                            ),
                            variant.name.span,
                        ));
                    }
                    variants.push(variant.name.item.clone());
                    let payload_ty = if let Some(payload) = &variant.payload {
                        if let Some(span) = type_contains_ref(payload) {
                            return Err(TypeError::new(
                                "reference types cannot be stored in enums".to_string(),
                                span,
                            ));
                        }
                        Some(lower_type(payload, &local_use, stdlib, &type_param_set)?)
                    } else {
                        None
                    };
                    payloads.insert(variant.name.item.clone(), payload_ty);
                }
                let qualified = format!("{module_name}.{}", decl.name.item);
                if enums.contains_key(&qualified) {
                    return Err(TypeError::new(
                        format!("duplicate enum `{qualified}`"),
                        decl.name.span,
                    ));
                }
                let info = EnumInfo {
                    type_params: decl.type_params.iter().map(|p| p.item.clone()).collect(),
                    variants: variants.clone(),
                    payloads: payloads.clone(),
                };
                enums.insert(qualified, info.clone());
                if module_name == entry_name {
                    let name = decl.name.item.clone();
                    if enums.contains_key(&name) {
                        return Err(TypeError::new(
                            format!("duplicate enum `{name}`"),
                            decl.name.span,
                        ));
                    }
                    enums.insert(name, info);
                }
            }
        }
    }
    Ok(enums)
}

/// Validate type argument arity within struct fields and enum payloads.
pub(super) fn validate_type_defs(
    modules: &[&Module],
    stdlib: &StdlibIndex,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
) -> Result<(), TypeError> {
    for module in modules {
        let local_use = UseMap::new(module);
        for item in &module.items {
            match item {
                Item::Struct(decl) => {
                    let type_param_set = build_type_params(&decl.type_params)?;
                    for field in &decl.fields {
                        let ty = lower_type(&field.ty, &local_use, stdlib, &type_param_set)?;
                        validate_type_args(&ty, struct_map, enum_map, field.ty.span())?;
                    }
                }
                Item::Enum(decl) => {
                    let type_param_set = build_type_params(&decl.type_params)?;
                    for variant in &decl.variants {
                        if let Some(payload) = &variant.payload {
                            let ty = lower_type(payload, &local_use, stdlib, &type_param_set)?;
                            validate_type_args(&ty, struct_map, enum_map, payload.span())?;
                        }
                    }
                }
                _ => {}
            }
        }
    }
    Ok(())
}

/// Enforce that `copy struct` declarations contain only unrestricted fields.
pub(super) fn validate_copy_structs(
    modules: &[&Module],
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
) -> Result<(), TypeError> {
    for module in modules {
        let local_use = UseMap::new(module);
        for item in &module.items {
            if let Item::Struct(decl) = item {
                if !decl.is_copy {
                    continue;
                }
                let type_param_set = build_type_params(&decl.type_params)?;
                for field in &decl.fields {
                    let ty = lower_type(&field.ty, &local_use, stdlib, &type_param_set)?;
                    if super::type_kind(&ty, struct_map, enum_map) != TypeKind::Unrestricted {
                        return Err(TypeError::new(
                            "copy struct cannot contain move-only fields".to_string(),
                            field.ty.span(),
                        ));
                    }
                }
            }
        }
    }
    Ok(())
}
