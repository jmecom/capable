use std::collections::HashMap;

use crate::ast::Span;
use crate::error::TypeError;
use crate::hir::*;
use crate::typeck::Ty;

#[derive(Clone)]
pub(super) struct ModuleOut {
    pub(super) name: String,
    pub(super) functions: Vec<HirFunction>,
    pub(super) extern_functions: Vec<HirExternFunction>,
    pub(super) structs: Vec<HirStruct>,
    pub(super) enums: Vec<HirEnum>,
}

impl ModuleOut {
    pub(super) fn new(name: String) -> Self {
        Self {
            name,
            functions: Vec::new(),
            extern_functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub(super) struct FunctionInstance {
    pub(super) module: String,
    pub(super) base_name: String,
    pub(super) type_args: Vec<Ty>,
}

impl From<ModuleOut> for HirModule {
    fn from(module: ModuleOut) -> Self {
        Self {
            name: module.name,
            functions: module.functions,
            extern_functions: module.extern_functions,
            structs: module.structs,
            enums: module.enums,
        }
    }
}

pub(super) trait GenericSig {
    fn name(&self) -> &str;
    fn type_params(&self) -> &Vec<String>;
    fn params(&self) -> &Vec<HirParam>;
}

impl GenericSig for HirFunction {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_params(&self) -> &Vec<String> {
        &self.type_params
    }

    fn params(&self) -> &Vec<HirParam> {
        &self.params
    }
}

impl GenericSig for HirExternFunction {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_params(&self) -> &Vec<String> {
        &self.type_params
    }

    fn params(&self) -> &Vec<HirParam> {
        &self.params
    }
}

pub(super) fn split_name(module: &str, name: &str) -> (String, String, bool) {
    if let Some((mod_part, type_part)) = name.rsplit_once('.') {
        (mod_part.to_string(), type_part.to_string(), true)
    } else {
        (module.to_string(), name.to_string(), false)
    }
}

pub(super) fn find_type_in_all_modules(
    name: &str,
    current_module: &str,
    structs: &HashMap<String, HirStruct>,
    enums: &HashMap<String, HirEnum>,
) -> Option<(String, String)> {
    let (type_module, base_name, qualified) = split_name(current_module, name);
    let qualified_key = qualify(&type_module, &base_name);
    if structs.contains_key(&qualified_key) || enums.contains_key(&qualified_key) {
        return Some((type_module, qualified_key));
    }

    if qualified {
        return None;
    }

    for key in structs.keys() {
        if key.ends_with(&format!(".{}", base_name)) {
            let mod_part = key.rsplit_once('.').map(|(m, _)| m).unwrap_or("");
            return Some((mod_part.to_string(), key.clone()));
        }
    }
    for key in enums.keys() {
        if key.ends_with(&format!(".{}", base_name)) {
            let mod_part = key.rsplit_once('.').map(|(m, _)| m).unwrap_or("");
            return Some((mod_part.to_string(), key.clone()));
        }
    }

    None
}

pub(super) fn qualify(module: &str, name: &str) -> String {
    format!("{module}.{name}")
}

pub(super) fn function_symbol(module: &str, name: &str) -> String {
    format!("capable_{}", qualify(module, name).replace('.', "_"))
}

pub(super) fn build_substitution(
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

pub(super) fn substitute_ty(ty: &Ty, subs: &HashMap<String, Ty>) -> Ty {
    match ty {
        Ty::Param(name) => subs.get(name).cloned().unwrap_or_else(|| ty.clone()),
        Ty::Builtin(_) => ty.clone(),
        Ty::Ptr(inner) => Ty::Ptr(Box::new(substitute_ty(inner, subs))),
        Ty::Ref(inner) => Ty::Ref(Box::new(substitute_ty(inner, subs))),
        Ty::Path(name, args) => Ty::Path(
            name.clone(),
            args.iter().map(|arg| substitute_ty(arg, subs)).collect(),
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

pub(super) fn mangle_name(base: &str, args: &[Ty]) -> String {
    if args.is_empty() {
        return base.to_string();
    }
    let suffix = args.iter().map(mangle_type).collect::<Vec<_>>().join("__");
    format!("{base}__{suffix}")
}

pub(super) fn mangle_type(ty: &Ty) -> String {
    match ty {
        Ty::Builtin(b) => match b {
            crate::typeck::BuiltinType::I32 => "i32".to_string(),
            crate::typeck::BuiltinType::I64 => "i64".to_string(),
            crate::typeck::BuiltinType::U32 => "u32".to_string(),
            crate::typeck::BuiltinType::U8 => "u8".to_string(),
            crate::typeck::BuiltinType::Bool => "bool".to_string(),
            crate::typeck::BuiltinType::Unit => "unit".to_string(),
            crate::typeck::BuiltinType::Never => "never".to_string(),
        },
        Ty::Ptr(inner) => format!("ptr_{}", mangle_type(inner)),
        Ty::Ref(inner) => format!("ref_{}", mangle_type(inner)),
        Ty::Param(name) => format!("param_{name}"),
        Ty::Path(name, args) => {
            if name == "sys.string.string" || name == "string" {
                return "string".to_string();
            }
            let mut base = name.replace('.', "_");
            if !args.is_empty() {
                let suffix = args.iter().map(mangle_type).collect::<Vec<_>>().join("__");
                base = format!("{base}__{suffix}");
            }
            base
        }
    }
}
