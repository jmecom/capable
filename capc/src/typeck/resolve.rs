use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::error::TypeError;

use super::{
    build_type_arg_suffix, merge_type_params, BuiltinType, EnumInfo, StdlibIndex, StructInfo,
    TraitInfo, Ty, TypeKind, UseMap,
};

pub(super) fn resolve_path(path: &Path, use_map: &UseMap) -> Vec<String> {
    if path.segments.len() > 1 {
        let first = &path.segments[0].item;
        if let Some(prefix) = use_map.aliases.get(first) {
            let mut resolved = prefix.clone();
            for seg in path.segments.iter().skip(1) {
                resolved.push(seg.item.clone());
            }
            return resolved;
        }
    }
    path.segments.iter().map(|seg| seg.item.clone()).collect()
}

pub(super) fn path_to_string(path: &Path) -> String {
    let mut out = String::new();
    for (i, seg) in path.segments.iter().enumerate() {
        if i > 0 {
            out.push('.');
        }
        out.push_str(&seg.item);
    }
    out
}

/// Resolve a method receiver type to (module, type name, type args).
/// Builtins with methods are mapped to their stdlib modules.
pub(super) fn resolve_method_target(
    receiver_ty: &Ty,
    module_name: &str,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(String, String, Vec<Ty>), TypeError> {
    let base_ty = match receiver_ty {
        Ty::Ref(inner) | Ty::Ptr(inner) => inner.as_ref(),
        _ => receiver_ty,
    };
    let (receiver_name, receiver_args) = match base_ty {
        Ty::Path(name, args) => (name.as_str(), args),
        Ty::Builtin(BuiltinType::U8) => {
            return Ok(("sys.bytes".to_string(), "u8".to_string(), Vec::new()));
        }
        Ty::Builtin(BuiltinType::I64) => {
            return Ok(("sys.ints".to_string(), "i64".to_string(), Vec::new()));
        }
        Ty::Builtin(BuiltinType::U64) => {
            return Ok(("sys.ints".to_string(), "u64".to_string(), Vec::new()));
        }
        _ => {
            return Err(TypeError::new(
                "method receiver must be a struct or enum value".to_string(),
                span,
            ));
        }
    };

    if let Some(info) = struct_map.get(receiver_name) {
        let type_name = receiver_name
            .rsplit_once('.')
            .map(|(_, t)| t)
            .unwrap_or(receiver_name)
            .to_string();
        return Ok((info.module.clone(), type_name, receiver_args.clone()));
    }

    if enum_map.contains_key(receiver_name) {
        let type_name = receiver_name
            .rsplit_once('.')
            .map(|(_, t)| t)
            .unwrap_or(receiver_name)
            .to_string();
        let mod_part = receiver_name
            .rsplit_once('.')
            .map(|(m, _)| m)
            .unwrap_or(module_name);
        return Ok((mod_part.to_string(), type_name, receiver_args.clone()));
    }

    if receiver_name.contains('.') {
        let (mod_part, type_part) = receiver_name
            .rsplit_once('.')
            .ok_or_else(|| TypeError::new("invalid type path".to_string(), span))?;
        return Ok((
            mod_part.to_string(),
            type_part.to_string(),
            receiver_args.clone(),
        ));
    }

    if let Some(info) = struct_map.get(&format!("{module_name}.{receiver_name}")) {
        return Ok((
            info.module.clone(),
            receiver_name.to_string(),
            receiver_args.clone(),
        ));
    }
    if enum_map.contains_key(&format!("{module_name}.{receiver_name}")) {
        return Ok((
            module_name.to_string(),
            receiver_name.to_string(),
            receiver_args.clone(),
        ));
    }

    Err(TypeError::new(
        format!("unknown struct or enum `{receiver_name}`"),
        span,
    ))
}

pub(super) fn resolve_impl_target(
    target: &Type,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    type_params: &HashSet<String>,
    module_name: &str,
    span: Span,
) -> Result<(String, String, Ty), TypeError> {
    let target_ty = lower_type(target, use_map, stdlib, type_params)?;
    let (impl_module, type_name) = match &target_ty {
        Ty::Path(target_name, target_args) => {
            let type_arg_suffix = build_type_arg_suffix(target_args);
            if let Some(info) = struct_map.get(target_name) {
                let base_name = target_name
                    .rsplit_once('.')
                    .map(|(_, t)| t)
                    .unwrap_or(target_name);
                let type_name = format!("{}{}", base_name, type_arg_suffix);
                (info.module.clone(), type_name)
            } else if enum_map.contains_key(target_name) {
                let base_name = target_name
                    .rsplit_once('.')
                    .map(|(_, t)| t)
                    .unwrap_or(target_name);
                let type_name = format!("{}{}", base_name, type_arg_suffix);
                let mod_part = target_name
                    .rsplit_once('.')
                    .map(|(m, _)| m)
                    .unwrap_or(module_name);
                (mod_part.to_string(), type_name)
            } else if target_name.contains('.') {
                let (mod_part, type_part) = target_name
                    .rsplit_once('.')
                    .ok_or_else(|| TypeError::new("invalid type path".to_string(), span))?;
                let type_name = format!("{}{}", type_part, type_arg_suffix);
                (mod_part.to_string(), type_name)
            } else if let Some(info) = struct_map.get(&format!("{module_name}.{target_name}")) {
                let type_name = format!("{}{}", target_name, type_arg_suffix);
                (info.module.clone(), type_name)
            } else if enum_map.contains_key(&format!("{module_name}.{target_name}")) {
                let type_name = format!("{}{}", target_name, type_arg_suffix);
                (module_name.to_string(), type_name)
            } else {
                return Err(TypeError::new(
                    "impl target must be a struct or enum type name".to_string(),
                    span,
                ));
            }
        }
        Ty::Builtin(BuiltinType::I32) => (module_name.to_string(), "i32".to_string()),
        Ty::Builtin(BuiltinType::U32) => (module_name.to_string(), "u32".to_string()),
        Ty::Builtin(BuiltinType::I64) => (module_name.to_string(), "i64".to_string()),
        Ty::Builtin(BuiltinType::U64) => (module_name.to_string(), "u64".to_string()),
        Ty::Builtin(BuiltinType::U8) => (module_name.to_string(), "u8".to_string()),
        Ty::Builtin(BuiltinType::Bool) => (module_name.to_string(), "bool".to_string()),
        _ => {
            return Err(TypeError::new(
                "impl target must be a struct or enum type name".to_string(),
                span,
            ));
        }
    };
    if impl_module != module_name {
        return Err(TypeError::new(
            "impl blocks must be declared in the defining module".to_string(),
            span,
        ));
    }
    Ok((impl_module, type_name, target_ty))
}

pub(super) fn validate_impl_method(
    type_name: &str,
    target_ty: &Ty,
    target_ast: &Type,
    _module_name: &str,
    method: &Function,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    type_params: &HashSet<String>,
    _span: Span,
) -> Result<Vec<Param>, TypeError> {
    if method.name.item.contains("__") {
        return Err(TypeError::new(
            "method name in impl should be unqualified (write sum, not Pair__sum)".to_string(),
            method.name.span,
        ));
    }

    let Some(first_param) = method.params.first() else {
        return Err(TypeError::new(
            format!("first parameter must be self: {type_name}"),
            method.name.span,
        ));
    };
    if first_param.name.item != "self" {
        return Err(TypeError::new(
            format!("first parameter must be self: {type_name}"),
            first_param.name.span,
        ));
    }

    let mut params = method.params.clone();
    let expected = target_ty.clone();
    let expected_ptr = Ty::Ptr(Box::new(target_ty.clone()));
    let expected_ref = Ty::Ref(Box::new(target_ty.clone()));
    let mut receiver_is_ref = false;

    if let Some(ty) = &first_param.ty {
        let lowered = lower_type(ty, use_map, stdlib, type_params)?;
        if lowered != expected && lowered != expected_ptr && lowered != expected_ref {
            return Err(TypeError::new(
                format!("first parameter must be self: {type_name} (found {lowered:?})"),
                ty.span(),
            ));
        }
        receiver_is_ref = lowered == expected_ref;
    } else {
        params[0].ty = Some(target_ast.clone());
    }

    for param in params.iter().skip(1) {
        if param.ty.is_none() {
            return Err(TypeError::new(
                format!("parameter `{}` requires a type annotation", param.name.item),
                param.name.span,
            ));
        }
    }

    let ret_ty = lower_type(&method.ret, use_map, stdlib, type_params)?;
    if receiver_is_ref && super::type_contains_capability(&ret_ty, struct_map, enum_map) {
        let receiver_kind = super::type_kind(target_ty, struct_map, enum_map);
        let receiver_is_capability = match target_ty {
            Ty::Path(name, _) => struct_map
                .get(name)
                .map(|info| info.is_capability)
                .unwrap_or(false),
            _ => false,
        };
        if receiver_kind != TypeKind::Unrestricted
            && (!receiver_is_capability
                || super::type_contains_non_linear_capability(&ret_ty, struct_map, enum_map))
        {
            return Err(TypeError::new(
                "borrowed capability receivers may only return linear child capabilities"
                    .to_string(),
                method.ret.span(),
            ));
        }
    }

    Ok(params)
}

pub(super) fn desugar_impl_method(
    type_name: &str,
    method: &Function,
    params: Vec<Param>,
    type_params: Vec<TypeParam>,
) -> Function {
    let name = Spanned::new(
        format!("{type_name}__{}", method.name.item),
        method.name.span,
    );
    Function {
        name,
        type_params,
        params,
        ret: method.ret.clone(),
        body: method.body.clone(),
        is_pub: method.is_pub,
        doc: method.doc.clone(),
        span: method.span,
    }
}

pub(super) fn desugar_impl_methods(
    impl_block: &ImplBlock,
    module_name: &str,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    trait_map: &HashMap<String, TraitInfo>,
) -> Result<Vec<Function>, TypeError> {
    let impl_type_params = super::build_type_params(&impl_block.type_params)?;
    let (_impl_module, type_name, target_ty) = resolve_impl_target(
        &impl_block.target,
        use_map,
        stdlib,
        struct_map,
        enum_map,
        &impl_type_params,
        module_name,
        impl_block.span,
    )?;
    let trait_name = impl_block
        .trait_path
        .as_ref()
        .map(|path| resolve_trait_name(path, use_map, module_name));
    if let Some(trait_name) = &trait_name {
        let Some(trait_info) = trait_map.get(trait_name) else {
            return Err(TypeError::new(
                format!("unknown trait `{trait_name}`"),
                impl_block.span,
            ));
        };
        if trait_info.module != module_name && !trait_info.is_pub {
            return Err(TypeError::new(
                format!("trait `{trait_name}` is private"),
                impl_block.span,
            ));
        }
    }
    let mut method_names = std::collections::HashSet::new();
    let mut methods = Vec::with_capacity(impl_block.methods.len());
    for method in &impl_block.methods {
        if !method_names.insert(method.name.item.clone()) {
            return Err(TypeError::new(
                format!("duplicate method `{}` in impl block", method.name.item),
                method.name.span,
            ));
        }
        if trait_name.is_some() && !method.type_params.is_empty() {
            return Err(TypeError::new(
                "trait impl methods cannot declare type parameters".to_string(),
                method.name.span,
            ));
        }
        let method_type_params = merge_type_params(&impl_type_params, &method.type_params)?;
        let mut combined_type_params = impl_block.type_params.clone();
        combined_type_params.extend(method.type_params.clone());
        let params = validate_impl_method(
            &type_name,
            &target_ty,
            &impl_block.target,
            module_name,
            method,
            use_map,
            stdlib,
            struct_map,
            enum_map,
            &method_type_params,
            method.span,
        )?;
        if let Some(trait_name) = &trait_name {
            let trait_info = trait_map.get(trait_name).expect("trait already validated");
            let trait_method = trait_info.methods.get(&method.name.item).ok_or_else(|| {
                TypeError::new(
                    format!(
                        "method `{}` is not declared in trait `{trait_name}`",
                        method.name.item
                    ),
                    method.name.span,
                )
            })?;
            let mut lowered_params = Vec::new();
            for param in &params {
                let Some(ty) = &param.ty else {
                    return Err(TypeError::new(
                        format!("parameter `{}` requires a type annotation", param.name.item),
                        param.name.span,
                    ));
                };
                lowered_params.push(lower_type(ty, use_map, stdlib, &method_type_params)?);
            }
            let lowered_ret = lower_type(&method.ret, use_map, stdlib, &method_type_params)?;
            let mut expected_params = Vec::new();
            for ty in &trait_method.params {
                expected_params.push(super::substitute_self(ty, &target_ty));
            }
            let expected_ret = super::substitute_self(&trait_method.ret, &target_ty);
            if lowered_params.len() != expected_params.len() {
                return Err(TypeError::new(
                    format!(
                        "method `{}` has wrong arity for trait `{trait_name}`",
                        method.name.item
                    ),
                    method.name.span,
                ));
            }
            for (actual, expected) in lowered_params.iter().zip(expected_params.iter()) {
                if actual != expected {
                    return Err(TypeError::new(
                        format!(
                            "method `{}` has wrong parameter type for trait `{trait_name}`",
                            method.name.item
                        ),
                        method.name.span,
                    ));
                }
            }
            if lowered_ret != expected_ret {
                return Err(TypeError::new(
                    format!(
                        "method `{}` has wrong return type for trait `{trait_name}`",
                        method.name.item
                    ),
                    method.name.span,
                ));
            }
            let name = Spanned::new(
                super::trait_method_name(trait_name, &type_name, &method.name.item),
                method.name.span,
            );
            methods.push(Function {
                name,
                type_params: combined_type_params,
                params,
                ret: method.ret.clone(),
                body: method.body.clone(),
                is_pub: method.is_pub,
                doc: method.doc.clone(),
                span: method.span,
            });
        } else {
            methods.push(desugar_impl_method(
                &type_name,
                method,
                params,
                combined_type_params,
            ));
        }
    }
    if let Some(trait_name) = &trait_name {
        let trait_info = trait_map.get(trait_name).expect("trait already validated");
        for name in trait_info.methods.keys() {
            if !method_names.contains(name) {
                return Err(TypeError::new(
                    format!("missing method `{name}` for trait `{trait_name}`"),
                    impl_block.span,
                ));
            }
        }
    }
    Ok(methods)
}

/// Convert AST types into resolved Ty (builtins + fully qualified paths).
pub(super) fn lower_type(
    ty: &Type,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    type_params: &HashSet<String>,
) -> Result<Ty, TypeError> {
    match ty {
        Type::Ptr { target, .. } => Ok(Ty::Ptr(Box::new(lower_type(
            target,
            use_map,
            stdlib,
            type_params,
        )?))),
        Type::Ref { target, .. } => Ok(Ty::Ref(Box::new(lower_type(
            target,
            use_map,
            stdlib,
            type_params,
        )?))),
        Type::Path { path, args, .. } => {
            let resolved = resolve_path(path, use_map);
            let path_segments = resolved.iter().map(|seg| seg.as_str()).collect::<Vec<_>>();
            let args: Vec<Ty> = args
                .iter()
                .map(|arg| lower_type(arg, use_map, stdlib, type_params))
                .collect::<Result<_, _>>()?;
            if path_segments.len() == 1 {
                if type_params.contains(path_segments[0]) {
                    if !args.is_empty() {
                        return Err(TypeError::new(
                            format!(
                                "type parameter `{}` cannot take arguments",
                                path_segments[0]
                            ),
                            path.span,
                        ));
                    }
                    return Ok(Ty::Param(path_segments[0].to_string()));
                }
                let builtin = match path_segments[0] {
                    "i32" => Some(BuiltinType::I32),
                    "i64" => Some(BuiltinType::I64),
                    "u32" => Some(BuiltinType::U32),
                    "u64" => Some(BuiltinType::U64),
                    "u8" => Some(BuiltinType::U8),
                    "bool" => Some(BuiltinType::Bool),
                    "unit" => Some(BuiltinType::Unit),
                    "never" => Some(BuiltinType::Never),
                    _ => None,
                };
                if let Some(builtin) = builtin {
                    return Ok(Ty::Builtin(builtin));
                }
                let resolved_joined = resolved.join(".");
                let alias = resolve_type_name(path, use_map, stdlib);
                let joined = if alias != resolved_joined {
                    alias
                } else {
                    resolved_joined
                };
                if joined == "Vec" || joined == "sys.vec.Vec" {
                    if args.len() != 1 {
                        return Err(TypeError::new(
                            format!("Vec expects 1 type argument, found {}", args.len()),
                            path.span,
                        ));
                    }
                    return Ok(Ty::Path("sys.vec.Vec".to_string(), args));
                }
                return Ok(Ty::Path(joined, args));
            }
            let joined = path_segments.join(".");
            if joined == "Vec" || joined == "sys.vec.Vec" {
                if args.len() != 1 {
                    return Err(TypeError::new(
                        format!("Vec expects 1 type argument, found {}", args.len()),
                        path.span,
                    ));
                }
                return Ok(Ty::Path("sys.vec.Vec".to_string(), args));
            }
            Ok(Ty::Path(joined, args))
        }
    }
}

/// Resolve a path to an enum type if the last segment is a variant.
pub(super) fn resolve_enum_variant(
    path: &Path,
    use_map: &UseMap,
    enum_map: &HashMap<String, EnumInfo>,
    module_name: &str,
) -> Option<Ty> {
    let resolved = resolve_path(path, use_map);
    if resolved.len() < 2 {
        return None;
    }
    let (enum_path, variant) = resolved.split_at(resolved.len() - 1);
    let enum_name = enum_path.join(".");

    if let Some(info) = enum_map.get(&enum_name) {
        if info.variants.iter().any(|name| name == &variant[0]) {
            return Some(Ty::Path(enum_name, Vec::new()));
        }
    }

    if enum_path.len() == 1 {
        let qualified = format!("{}.{}", module_name, enum_name);
        if let Some(info) = enum_map.get(&qualified) {
            if info.variants.iter().any(|name| name == &variant[0]) {
                return Some(Ty::Path(qualified, Vec::new()));
            }
        }
    }

    None
}

pub(super) fn resolve_type_name(path: &Path, use_map: &UseMap, stdlib: &StdlibIndex) -> String {
    let resolved = resolve_path(path, use_map);
    if resolved.len() == 1 {
        if let Some(full) = stdlib.types.get(&resolved[0]) {
            return full.clone();
        }
    }
    resolved.join(".")
}

pub(super) fn resolve_trait_name(path: &Path, use_map: &UseMap, module_name: &str) -> String {
    let resolved = resolve_path(path, use_map);
    if resolved.len() == 1 {
        return format!("{module_name}.{}", resolved[0]);
    }
    resolved.join(".")
}
