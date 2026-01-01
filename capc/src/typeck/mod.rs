//! Type checker overview (high level)
//!
//! 1) Build indices: stdlib name aliases, structs/enums metadata, function sigs.
//! 2) Validate safety rules (safe packages cannot mention raw pointers, externs).
//! 3) Type-check functions with move tracking (affine/linear discipline).
//! 4) Lower the checked AST into HIR with resolved symbols and types.
//!
//! This module contains both the type checker and HIR lowering because the
//! lowering phase relies on the type checker as the single source of truth.

mod check;
mod collect;
mod lower;
mod monomorphize;

use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::error::TypeError;
use crate::hir::HirModule;

pub(super) const RESERVED_TYPE_PARAMS: [&str; 8] = [
    "i32", "i64", "u32", "u8", "bool", "string", "unit", "never",
];

/// Resolved type used after lowering. No spans, fully qualified paths.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Builtin(BuiltinType),
    Path(String, Vec<Ty>),
    Ptr(Box<Ty>),
    /// Borrow-lite reference: only valid as a direct parameter type.
    /// It is treated as a non-consuming read (no lifetime tracking).
    Ref(Box<Ty>),
    /// Generic type parameter.
    Param(String),
}

pub(super) fn build_type_params(params: &[Ident]) -> Result<HashSet<String>, TypeError> {
    let mut set = HashSet::new();
    for param in params {
        let name = param.item.as_str();
        if RESERVED_TYPE_PARAMS.contains(&name) {
            return Err(TypeError::new(
                format!("type parameter `{}` is reserved", param.item),
                param.span,
            ));
        }
        if !set.insert(param.item.clone()) {
            return Err(TypeError::new(
                format!("duplicate type parameter `{}`", param.item),
                param.span,
            ));
        }
    }
    Ok(set)
}

fn merge_type_params(
    base: &HashSet<String>,
    params: &[Ident],
) -> Result<HashSet<String>, TypeError> {
    let mut set = base.clone();
    for param in params {
        let name = param.item.as_str();
        if RESERVED_TYPE_PARAMS.contains(&name) {
            return Err(TypeError::new(
                format!("type parameter `{}` is reserved", param.item),
                param.span,
            ));
        }
        if set.contains(&param.item) {
            return Err(TypeError::new(
                format!("duplicate type parameter `{}`", param.item),
                param.span,
            ));
        }
        set.insert(param.item.clone());
    }
    Ok(set)
}

/// Built-in primitive types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    I32,
    I64,
    U32,
    U8,
    Bool,
    String,
    Unit,
    Never,
}

pub(super) fn function_key(module_name: &str, func_name: &str) -> String {
    format!("{module_name}::{func_name}")
}

/// Return true if the type is a built-in numeric type.
pub fn is_numeric_type(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Builtin(BuiltinType::I32)
            | Ty::Builtin(BuiltinType::I64)
            | Ty::Builtin(BuiltinType::U32)
            | Ty::Builtin(BuiltinType::U8)
    )
}

/// Return true if the type can be ordered with <, <=, >, >=.
pub fn is_orderable_type(ty: &Ty) -> bool {
    is_numeric_type(ty)
}

/// Return true if the type is an unsigned integer.
pub fn is_unsigned_type(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Builtin(BuiltinType::U32) | Ty::Builtin(BuiltinType::U8)
    )
}

/// Collected type information for expressions within a single function.
#[derive(Debug, Default, Clone)]
struct TypeTable {
    expr_types: HashMap<Span, Ty>,
}

impl TypeTable {
    fn record(&mut self, span: Span, ty: Ty) {
        self.expr_types.insert(span, ty);
    }

    fn get(&self, span: Span) -> Option<&Ty> {
        self.expr_types.get(&span)
    }
}

/// Type tables for all functions in a module, keyed by function name.
type FunctionTypeTables = HashMap<String, TypeTable>;

/// Resolved signature for a function.
#[derive(Debug, Clone)]
struct FunctionSig {
    type_params: Vec<String>,
    params: Vec<Ty>,
    ret: Ty,
    module: String,
    is_pub: bool,
}

/// Metadata about a struct needed by the type checker.
#[derive(Debug, Clone)]
struct StructInfo {
    type_params: Vec<String>,
    fields: HashMap<String, Ty>,
    is_opaque: bool,
    is_capability: bool,
    kind: TypeKind,
    module: String,
}

/// Metadata about an enum needed by the type checker.
#[derive(Debug, Clone)]
struct EnumInfo {
    type_params: Vec<String>,
    variants: Vec<String>,
    payloads: HashMap<String, Option<Ty>>,
}

/// Move tracking for locals (linear/affine discipline).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MoveState {
    Available,
    Moved,
}

/// Type "kind" controls move behavior and linear obligations.
/// - Unrestricted: copyable/duplicable values
/// - Affine: move-only, drop allowed
/// - Linear: move-only, must be consumed on all paths
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeKind {
    Unrestricted,
    Affine,
    Linear,
}

/// How an expression is used by its parent (drives move tracking).
/// - Move: consuming use (may move an affine/linear value)
/// - Read: non-consuming use
/// - Project: intermediate for a.b.c chains before deciding to move
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UseMode {
    Move,
    Read,
    Project,
}

/// Local variable information tracked by the checker.
#[derive(Debug, Clone)]
struct LocalInfo {
    ty: Ty,
    state: MoveState,
}

/// Stack-based scope manager for lexical scoping and move state.
/// Lets block-scoped `let`s shadow outer bindings while assignments
/// update the closest enclosing binding.
#[derive(Debug, Clone)]
struct Scopes {
    /// Stack of scopes, where each scope is a map from name to local info.
    /// The last element is the innermost (current) scope.
    stack: Vec<HashMap<String, LocalInfo>>,
    /// Stack of scope depths that mark loop bodies for break/continue checks.
    loop_stack: Vec<usize>,
}

impl Scopes {
    /// Push a new scope (entering a block)
    fn push_scope(&mut self) {
        self.stack.push(HashMap::new());
    }

    /// Pop the current scope (exiting a block)
    fn pop_scope(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        }
    }

    /// Insert a new local variable in the current scope
    fn insert_local(&mut self, name: String, ty: Ty) {
        if let Some(scope) = self.stack.last_mut() {
            scope.insert(
                name,
                LocalInfo {
                    ty,
                    state: MoveState::Available,
                },
            );
        }
    }

    /// Look up a variable, searching from innermost to outermost scope
    fn lookup(&self, name: &str) -> Option<&LocalInfo> {
        for scope in self.stack.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    /// Assign to an existing variable, searching from innermost to outermost scope.
    /// Returns true if the variable was found and updated, false otherwise.
    fn assign(&mut self, name: &str, ty: Ty) -> bool {
        for scope in self.stack.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(
                    name.to_string(),
                    LocalInfo {
                        ty,
                        state: MoveState::Available,
                    },
                );
                return true;
            }
        }
        false
    }

    /// Create from a flat HashMap (for function parameters initialization)
    fn from_flat_map(map: HashMap<String, Ty>) -> Self {
        let mut scope = HashMap::new();
        for (name, ty) in map {
            scope.insert(
                name,
                LocalInfo {
                    ty,
                    state: MoveState::Available,
                },
            );
        }
        Scopes {
            stack: vec![scope],
            loop_stack: Vec::new(),
        }
    }

    fn push_loop(&mut self) {
        self.loop_stack.push(self.stack.len());
    }

    fn pop_loop(&mut self) {
        self.loop_stack.pop();
    }

    fn current_loop_depth(&self) -> Option<usize> {
        self.loop_stack.last().copied()
    }

    fn contains(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }

    fn mark_moved(&mut self, name: &str, span: Span) -> Result<(), TypeError> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) {
                if info.state == MoveState::Moved {
                    return Err(TypeError::new(
                        format!("use of moved value `{name}`"),
                        span,
                    ));
                }
                info.state = MoveState::Moved;
                return Ok(());
            }
        }
        Err(TypeError::new(
            format!("unknown identifier `{name}`"),
            span,
        ))
    }
}

/// Maps `use`-aliases to fully qualified module paths.
struct UseMap {
    aliases: HashMap<String, Vec<String>>,
}

/// Index of public stdlib types for name resolution.
struct StdlibIndex {
    types: HashMap<String, String>,
}

impl UseMap {
    fn new(module: &Module) -> Self {
        let mut aliases = HashMap::new();
        for use_decl in &module.uses {
            let segments = use_decl
                .path
                .segments
                .iter()
                .map(|seg| seg.item.clone())
                .collect::<Vec<_>>();
            if let Some(alias) = segments.last() {
                aliases.insert(alias.clone(), segments);
            }
        }
        Self { aliases }
    }
}

fn resolve_path(path: &Path, use_map: &UseMap) -> Vec<String> {
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

/// Resolve a method receiver type to (module, type name, type args).
/// Builtins with methods (string/u8) are mapped to their stdlib modules.
fn resolve_method_target(
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
        Ty::Builtin(BuiltinType::String) => {
            return Ok((
                "sys.string".to_string(),
                "string".to_string(),
                Vec::new(),
            ));
        }
        Ty::Builtin(BuiltinType::U8) => {
            return Ok((
                "sys.bytes".to_string(),
                "u8".to_string(),
                Vec::new(),
            ));
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
        let (mod_part, type_part) = receiver_name.rsplit_once('.').ok_or_else(|| {
            TypeError::new("invalid type path".to_string(), span)
        })?;
        return Ok((
            mod_part.to_string(),
            type_part.to_string(),
            receiver_args.clone(),
        ));
    }

    if let Some(info) = struct_map.get(&format!("{module_name}.{receiver_name}")) {
        return Ok((info.module.clone(), receiver_name.to_string(), receiver_args.clone()));
    }
    if enum_map.contains_key(&format!("{module_name}.{receiver_name}")) {
        return Ok((module_name.to_string(), receiver_name.to_string(), receiver_args.clone()));
    }

    Err(TypeError::new(
        format!("unknown struct or enum `{receiver_name}`"),
        span,
    ))
}

fn resolve_impl_target(
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
        Ty::Path(target_name, _target_args) => {
            // Check struct_map first
            if let Some(info) = struct_map.get(target_name) {
                let type_name = target_name
                    .rsplit_once('.')
                    .map(|(_, t)| t)
                    .unwrap_or(target_name)
                    .to_string();
                (info.module.clone(), type_name)
            // Check enum_map
            } else if enum_map.contains_key(target_name) {
                let type_name = target_name
                    .rsplit_once('.')
                    .map(|(_, t)| t)
                    .unwrap_or(target_name)
                    .to_string();
                let mod_part = target_name
                    .rsplit_once('.')
                    .map(|(m, _)| m)
                    .unwrap_or(module_name);
                (mod_part.to_string(), type_name)
            } else if target_name.contains('.') {
                let (mod_part, type_part) = target_name.rsplit_once('.').ok_or_else(|| {
                    TypeError::new("invalid type path".to_string(), span)
                })?;
                (mod_part.to_string(), type_part.to_string())
            } else if let Some(info) = struct_map.get(&format!("{module_name}.{target_name}")) {
                (info.module.clone(), target_name.clone())
            } else if enum_map.contains_key(&format!("{module_name}.{target_name}")) {
                (module_name.to_string(), target_name.clone())
            } else {
                return Err(TypeError::new(
                    "impl target must be a struct or enum type name".to_string(),
                    span,
                ));
            }
        }
        Ty::Builtin(BuiltinType::String) => ("sys.string".to_string(), "string".to_string()),
        Ty::Builtin(BuiltinType::U8) => ("sys.bytes".to_string(), "u8".to_string()),
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

fn validate_impl_method(
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
                format!(
                    "first parameter must be self: {type_name} (found {lowered:?})"
                ),
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
    if receiver_is_ref && type_contains_capability(&ret_ty, struct_map, enum_map) {
        let receiver_kind = type_kind(target_ty, struct_map, enum_map);
        if receiver_kind != TypeKind::Unrestricted {
            return Err(TypeError::new(
                "methods returning capabilities must take `self` by value".to_string(),
                method.ret.span(),
            ));
        }
    }

    Ok(params)
}

fn desugar_impl_method(
    type_name: &str,
    method: &Function,
    params: Vec<Param>,
    type_params: Vec<Ident>,
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

fn desugar_impl_methods(
    impl_block: &ImplBlock,
    module_name: &str,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
) -> Result<Vec<Function>, TypeError> {
    let impl_type_params = build_type_params(&impl_block.type_params)?;
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
    let mut method_names = std::collections::HashSet::new();
    let mut methods = Vec::with_capacity(impl_block.methods.len());
    for method in &impl_block.methods {
        if !method_names.insert(method.name.item.clone()) {
            return Err(TypeError::new(
                format!("duplicate method `{}` in impl block", method.name.item),
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
        methods.push(desugar_impl_method(
            &type_name,
            method,
            params,
            combined_type_params,
        ));
    }
    Ok(methods)
}

/// Convert AST types into resolved Ty (builtins + fully qualified paths).
fn lower_type(
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
                            format!("type parameter `{}` cannot take arguments", path_segments[0]),
                            path.span,
                        ));
                    }
                    return Ok(Ty::Param(path_segments[0].to_string()));
                }
                let builtin = match path_segments[0] {
                    "i32" => Some(BuiltinType::I32),
                    "i64" => Some(BuiltinType::I64),
                    "u32" => Some(BuiltinType::U32),
                    "u8" => Some(BuiltinType::U8),
                    "bool" => Some(BuiltinType::Bool),
                    "string" => Some(BuiltinType::String),
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
                    let elem = &args[0];
                    let vec_name = match elem {
                        Ty::Builtin(BuiltinType::U8) => "sys.vec.VecU8",
                        Ty::Builtin(BuiltinType::I32) => "sys.vec.VecI32",
                        Ty::Builtin(BuiltinType::String) => "sys.vec.VecString",
                        _ => {
                            return Err(TypeError::new(
                                "Vec only supports u8, i32, and string element types".to_string(),
                                path.span,
                            ))
                        }
                    };
                    return Ok(Ty::Path(vec_name.to_string(), Vec::new()));
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
                let elem = &args[0];
                let vec_name = match elem {
                    Ty::Builtin(BuiltinType::U8) => "sys.vec.VecU8",
                    Ty::Builtin(BuiltinType::I32) => "sys.vec.VecI32",
                    Ty::Builtin(BuiltinType::String) => "sys.vec.VecString",
                    _ => {
                        return Err(TypeError::new(
                            "Vec only supports u8, i32, and string element types".to_string(),
                            path.span,
                        ))
                    }
                };
                return Ok(Ty::Path(vec_name.to_string(), Vec::new()));
            }
            Ok(Ty::Path(joined, args))
        }
    }
}

/// Resolve a path to an enum type if the last segment is a variant.
fn resolve_enum_variant(
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

    // First try the resolved path as-is
    if let Some(info) = enum_map.get(&enum_name) {
        if info.variants.iter().any(|name| name == &variant[0]) {
            return Some(Ty::Path(enum_name, Vec::new()));
        }
    }

    // If not found, try prepending current module (for local enums)
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

fn resolve_type_name(path: &Path, use_map: &UseMap, stdlib: &StdlibIndex) -> String {
    let resolved = resolve_path(path, use_map);
    if resolved.len() == 1 {
        if let Some(full) = stdlib.types.get(&resolved[0]) {
            return full.clone();
        }
    }
    resolved.join(".")
}

fn type_contains_ref(ty: &Type) -> Option<Span> {
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

fn type_contains_capability(
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

/// Move-only types are anything not unrestricted.
fn is_affine_type(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
) -> bool {
    type_kind(ty, struct_map, enum_map) != TypeKind::Unrestricted
}

/// Compute kind with cycle protection for recursive types.
fn type_kind(
    ty: &Ty,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
) -> TypeKind {
    let mut visiting = HashSet::new();
    type_kind_inner(ty, struct_map, enum_map, &mut visiting)
}

/// Linear dominates affine, which dominates unrestricted.
fn combine_kind(left: TypeKind, right: TypeKind) -> TypeKind {
    match (left, right) {
        (TypeKind::Linear, _) | (_, TypeKind::Linear) => TypeKind::Linear,
        (TypeKind::Affine, _) | (_, TypeKind::Affine) => TypeKind::Affine,
        _ => TypeKind::Unrestricted,
    }
}

/// Recursively compute type kind with recursion cycle protection.
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
                let fields_kind = info.fields.values().fold(TypeKind::Unrestricted, |acc, field| {
                    combine_kind(acc, type_kind_inner(field, struct_map, enum_map, visiting))
                });
                visiting.remove(name);
                return combine_kind(info.kind, fields_kind);
            }
            if let Some(info) = enum_map.get(name) {
                visiting.insert(name.clone());
                let payload_kind = info.payloads.values().fold(TypeKind::Unrestricted, |acc, payload| {
                    if let Some(payload_ty) = payload {
                        combine_kind(acc, type_kind_inner(payload_ty, struct_map, enum_map, visiting))
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

fn validate_type_args(
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

pub fn type_check(module: &Module) -> Result<crate::hir::HirProgram, TypeError> {
    type_check_program(module, &[], &[])
}

/// Type-check a program (entry module + dependencies) and lower to HIR.
pub fn type_check_program(
    module: &Module,
    stdlib: &[Module],
    user_modules: &[Module],
) -> Result<crate::hir::HirProgram, TypeError> {
    let use_map = UseMap::new(module);
    let stdlib_index = collect::build_stdlib_index(stdlib)?;
    let modules = stdlib
        .iter()
        .chain(user_modules.iter())
        .chain(std::iter::once(module))
        .collect::<Vec<_>>();
    let module_name = module.name.to_string();
    check::validate_package_safety(module)
        .map_err(|err| err.with_context(format!("in module `{}`", module.name)))?;
    for user_module in user_modules {
        check::validate_package_safety(user_module)
            .map_err(|err| err.with_context(format!("in module `{}`", user_module.name)))?;
    }
    let struct_map = collect::collect_structs(&modules, &module_name, &stdlib_index)
        .map_err(|err| err.with_context("while collecting structs"))?;
    let enum_map = collect::collect_enums(&modules, &module_name, &stdlib_index)
        .map_err(|err| err.with_context("while collecting enums"))?;
    collect::validate_type_defs(&modules, &stdlib_index, &struct_map, &enum_map)
        .map_err(|err| err.with_context("while validating type arguments"))?;
    collect::validate_copy_structs(&modules, &struct_map, &enum_map, &stdlib_index)
        .map_err(|err| err.with_context("while validating copy structs"))?;
    let functions = collect::collect_functions(
        &modules,
        &module_name,
        &stdlib_index,
        &struct_map,
        &enum_map,
    )
        .map_err(|err| err.with_context("while collecting functions"))?;

    let mut type_tables: FunctionTypeTables = HashMap::new();

    let mut check_module = |module: &Module| -> Result<(), TypeError> {
        let module_name = module.name.to_string();
        let module_use = UseMap::new(module);
        for item in &module.items {
            match item {
                Item::Function(func) => {
                    let mut table = TypeTable::default();
                    check::check_function(
                        func,
                        &functions,
                        &module_use,
                        &struct_map,
                        &enum_map,
                        &stdlib_index,
                        &module_name,
                        Some(&mut table),
                    )
                    .map_err(|err| err.with_context(format!("in module `{}`", module_name)))?;
                    type_tables.insert(function_key(&module_name, &func.name.item), table);
                }
                Item::Impl(impl_block) => {
                    let methods = desugar_impl_methods(
                        impl_block,
                        &module_name,
                        &module_use,
                        &stdlib_index,
                        &struct_map,
                        &enum_map,
                    )?;
                    for method in methods {
                        let mut table = TypeTable::default();
                        check::check_function(
                            &method,
                            &functions,
                            &module_use,
                            &struct_map,
                            &enum_map,
                            &stdlib_index,
                            &module_name,
                            Some(&mut table),
                        )
                        .map_err(|err| err.with_context(format!("in module `{}`", module_name)))?;
                        type_tables.insert(function_key(&module_name, &method.name.item), table);
                    }
                }
                _ => {}
            }
        }
        Ok(())
    };

    for module in user_modules {
        check_module(module)?;
    }
    check_module(module)?;

    let hir_stdlib: Result<Vec<HirModule>, TypeError> = stdlib
        .iter()
        .map(|m| {
            let use_map = UseMap::new(m);
            lower::lower_module(
                m,
                &functions,
                &struct_map,
                &enum_map,
                &use_map,
                &stdlib_index,
                Some(&type_tables),
                true,
            )
            .map_err(|err| err.with_context(format!("in module `{}`", m.name)))
        })
        .collect();

    let hir_user_modules: Result<Vec<HirModule>, TypeError> = user_modules
        .iter()
        .map(|m| {
            let use_map = UseMap::new(m);
            lower::lower_module(
                m,
                &functions,
                &struct_map,
                &enum_map,
                &use_map,
                &stdlib_index,
                Some(&type_tables),
                false,
            )
            .map_err(|err| err.with_context(format!("in module `{}`", m.name)))
        })
        .collect();

    let hir_entry = lower::lower_module(
        module,
        &functions,
        &struct_map,
        &enum_map,
        &use_map,
        &stdlib_index,
        Some(&type_tables),
        false,
    )
    .map_err(|err| err.with_context(format!("in module `{}`", module.name)))?;

    let hir_program = crate::hir::HirProgram {
        entry: hir_entry,
        user_modules: hir_user_modules?,
        stdlib: hir_stdlib?,
    };
    monomorphize::monomorphize_program(hir_program)
}

pub(super) trait SpanExt {
    fn span(&self) -> Span;
}

impl SpanExt for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Literal(lit) => lit.span,
            Expr::Path(path) => path.span,
            Expr::Call(call) => call.span,
            Expr::MethodCall(method_call) => method_call.span,
            Expr::FieldAccess(field) => field.span,
            Expr::Index(index) => index.span,
            Expr::StructLiteral(lit) => lit.span,
            Expr::Unary(unary) => unary.span,
            Expr::Binary(binary) => binary.span,
            Expr::Match(m) => m.span,
            Expr::Try(try_expr) => try_expr.span,
            Expr::Grouping(group) => group.span,
        }
    }
}
