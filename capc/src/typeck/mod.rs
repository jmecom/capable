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
mod infer;
mod kinds;
mod lower;
mod moveck;
mod monomorphize;
mod patterns;
mod resolve;
mod safety;
mod type_params;

use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::error::TypeError;
use crate::hir::{HirModule, HirTraitImpl};

use infer::{
    apply_enum_type_args, enum_payload_matches, infer_enum_args, resolve_enum_type_args,
    ty_equivalent_for_set,
};
use kinds::{
    is_affine_type, is_string_ty, stdlib_string_ty, type_contains_capability,
    type_contains_non_linear_capability, type_contains_ref, type_kind, validate_type_args,
};
use moveck::{
    ensure_affine_states_match, ensure_linear_all_consumed, ensure_linear_scope_consumed,
    ensure_linear_scopes_consumed_from, merge_branch_states, merge_match_states, stmt_is_total,
};
use patterns::{bind_pattern, leftmost_local_in_chain};
use resolve::{
    desugar_impl_methods, lower_type, path_to_string, resolve_enum_variant, resolve_impl_target,
    resolve_method_target, resolve_path, resolve_trait_name, resolve_type_name,
};
use safety::{validate_import_safety, validate_package_safety};
use type_params::{
    build_type_arg_suffix,
    build_type_param_bounds, build_type_params, merge_type_params, type_param_names,
};

pub(super) const RESERVED_TYPE_PARAMS: [&str; 8] =
    ["i32", "i64", "u32", "u8", "bool", "unit", "never", "Self"];

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

/// Built-in primitive types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    I32,
    I64,
    U32,
    U8,
    Bool,
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
    expr_types: HashMap<ExprId, Ty>,
}

impl TypeTable {
    fn record(&mut self, id: ExprId, ty: Ty) {
        self.expr_types.insert(id, ty);
    }

    fn get(&self, id: ExprId) -> Option<&Ty> {
        self.expr_types.get(&id)
    }
}

/// Type tables for all functions in a module, keyed by function name.
type FunctionTypeTables = HashMap<String, TypeTable>;

/// Resolved signature for a function.
#[derive(Debug, Clone)]
struct FunctionSig {
    type_params: Vec<String>,
    type_param_bounds: HashMap<String, Vec<String>>,
    params: Vec<Ty>,
    ret: Ty,
    module: String,
    is_pub: bool,
}

/// Metadata about a trait declaration.
#[derive(Debug, Clone)]
pub(super) struct TraitInfo {
    #[allow(dead_code)]
    type_params: Vec<String>,
    methods: HashMap<String, FunctionSig>,
    module: String,
    is_pub: bool,
}

#[derive(Debug, Clone)]
pub(super) struct TraitImplInfo {
    trait_name: String,
    type_name: String,
    type_params: Vec<String>,
    target_ty: Ty,
    module: String,
}

fn trait_method_name(trait_name: &str, type_name: &str, method: &str) -> String {
    let trait_part = trait_name.replace('.', "_");
    format!("trait__{trait_part}__{type_name}__{method}")
}

fn substitute_self(ty: &Ty, target: &Ty) -> Ty {
    match ty {
        Ty::Param(name) if name == "Self" => target.clone(),
        Ty::Ptr(inner) => Ty::Ptr(Box::new(substitute_self(inner, target))),
        Ty::Ref(inner) => Ty::Ref(Box::new(substitute_self(inner, target))),
        Ty::Path(name, args) => Ty::Path(
            name.clone(),
            args.iter()
                .map(|arg| substitute_self(arg, target))
                .collect(),
        ),
        Ty::Builtin(_) | Ty::Param(_) => ty.clone(),
    }
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
                    return Err(TypeError::new(format!("use of moved value `{name}`"), span));
                }
                info.state = MoveState::Moved;
                return Ok(());
            }
        }
        Err(TypeError::new(format!("unknown identifier `{name}`"), span))
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

pub fn type_check(module: &Module) -> Result<crate::hir::HirProgram, TypeError> {
    type_check_program(module, &[], &[])
}

/// Type-check a program (entry module + dependencies) and lower to HIR.
pub fn type_check_program(
    module: &Module,
    stdlib: &[Module],
    user_modules: &[Module],
) -> Result<crate::hir::HirProgram, TypeError> {
    let module = crate::desugar::desugar_module(module);
    let stdlib = stdlib
        .iter()
        .map(crate::desugar::desugar_module)
        .collect::<Vec<_>>();
    let user_modules = user_modules
        .iter()
        .map(crate::desugar::desugar_module)
        .collect::<Vec<_>>();

    let use_map = UseMap::new(&module);
    let stdlib_names: HashSet<String> = stdlib.iter().map(|m| path_to_string(&m.name)).collect();
    let mut package_map: HashMap<String, PackageSafety> = HashMap::new();
    for m in &stdlib {
        package_map.insert(path_to_string(&m.name), m.package);
    }
    for m in &user_modules {
        package_map.insert(path_to_string(&m.name), m.package);
    }
    package_map.insert(path_to_string(&module.name), module.package);
    let stdlib_index = collect::build_stdlib_index(&stdlib)?;
    let modules = stdlib
        .iter()
        .chain(user_modules.iter())
        .chain(std::iter::once(&module))
        .collect::<Vec<_>>();
    let module_name = module.name.to_string();
    validate_package_safety(&module, false)
        .map_err(|err| err.with_context(format!("in module `{}`", module.name)))?;
    validate_import_safety(&module, &package_map, &stdlib_names)
        .map_err(|err| err.with_context(format!("in module `{}`", module.name)))?;
    for user_module in &user_modules {
        validate_package_safety(user_module, false)
            .map_err(|err| err.with_context(format!("in module `{}`", user_module.name)))?;
        validate_import_safety(user_module, &package_map, &stdlib_names)
            .map_err(|err| err.with_context(format!("in module `{}`", user_module.name)))?;
    }
    for stdlib_module in &stdlib {
        validate_package_safety(stdlib_module, true)
            .map_err(|err| err.with_context(format!("in module `{}`", stdlib_module.name)))?;
        if stdlib_module.package == PackageSafety::Safe {
            validate_import_safety(stdlib_module, &package_map, &stdlib_names)
                .map_err(|err| err.with_context(format!("in module `{}`", stdlib_module.name)))?;
        }
    }
    let struct_map = collect::collect_structs(&modules, &module_name, &stdlib_index)
        .map_err(|err| err.with_context("while collecting structs"))?;
    let enum_map = collect::collect_enums(&modules, &module_name, &stdlib_index)
        .map_err(|err| err.with_context("while collecting enums"))?;
    let trait_map = collect::collect_traits(&modules, &stdlib_index)
        .map_err(|err| err.with_context("while collecting traits"))?;
    let trait_impls =
        collect::collect_trait_impls(&modules, &stdlib_index, &struct_map, &enum_map, &trait_map)
            .map_err(|err| err.with_context("while collecting trait impls"))?;
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
        &trait_map,
    )
    .map_err(|err| err.with_context("while collecting functions"))?;

    let mut type_tables: FunctionTypeTables = HashMap::new();

    let mut check_module = |module: &Module| -> Result<(), TypeError> {
        let module_name = module.name.to_string();
        let module_use = UseMap::new(module);
        for item in &module.items {
            match item {
                Item::Function(func) => {
                    if crate::runtime_intrinsics::is_runtime_intrinsic(
                        &module_name,
                        &func.name.item,
                    ) {
                        continue;
                    }
                    let mut table = TypeTable::default();
                    check::check_function(
                        func,
                        &functions,
                        &trait_map,
                        &trait_impls,
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
                        &trait_map,
                    )?;
                    for method in methods {
                        if crate::runtime_intrinsics::is_runtime_intrinsic(
                            &module_name,
                            &method.name.item,
                        ) {
                            continue;
                        }
                        let mut table = TypeTable::default();
                        check::check_function(
                            &method,
                            &functions,
                            &trait_map,
                            &trait_impls,
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

    for module in &stdlib {
        check_module(module)?;
    }
    for module in &user_modules {
        check_module(module)?;
    }
    check_module(&module)?;

    let hir_stdlib: Result<Vec<HirModule>, TypeError> = stdlib
        .iter()
        .map(|m| {
            let use_map = UseMap::new(m);
            lower::lower_module(
                m,
                &functions,
                &struct_map,
                &enum_map,
                &trait_map,
                &trait_impls,
                &use_map,
                &stdlib_index,
                Some(&type_tables),
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
                &trait_map,
                &trait_impls,
                &use_map,
                &stdlib_index,
                Some(&type_tables),
            )
            .map_err(|err| err.with_context(format!("in module `{}`", m.name)))
        })
        .collect();

    let hir_entry = lower::lower_module(
        &module,
        &functions,
        &struct_map,
        &enum_map,
        &trait_map,
        &trait_impls,
        &use_map,
        &stdlib_index,
        Some(&type_tables),
    )
    .map_err(|err| err.with_context(format!("in module `{}`", module.name)))?;

    let hir_trait_impls: Vec<HirTraitImpl> = trait_impls
        .iter()
        .map(|info| HirTraitImpl {
            trait_name: info.trait_name.clone(),
            type_name: info.type_name.clone(),
            type_params: info.type_params.clone(),
            target_ty: info.target_ty.clone(),
            module: info.module.clone(),
        })
        .collect();

    let hir_program = crate::hir::HirProgram {
        entry: hir_entry,
        user_modules: hir_user_modules?,
        stdlib: hir_stdlib?,
        trait_impls: hir_trait_impls,
    };
    monomorphize::monomorphize_program(hir_program)
}
