use std::collections::HashMap;

use crate::ast::*;
use crate::error::TypeError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Builtin(BuiltinType),
    Path(String, Vec<Ty>),
    Ptr(Box<Ty>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    I32,
    I64,
    U32,
    U8,
    Bool,
    String,
    Unit,
}

#[derive(Debug, Clone)]
struct FunctionSig {
    params: Vec<Ty>,
    ret: Ty,
    module: String,
    is_pub: bool,
}

#[derive(Debug, Clone)]
struct StructInfo {
    fields: HashMap<String, Ty>,
    is_opaque: bool,
    module: String,
}

#[derive(Debug, Clone)]
struct EnumInfo {
    variants: Vec<String>,
}

/// Stack-based scope manager for proper lexical scoping.
/// Allows block-scoped `let` declarations while enabling assignments
/// to mutate variables in outer scopes.
struct Scopes {
    /// Stack of scopes, where each scope is a map from name to type.
    /// The last element is the innermost (current) scope.
    stack: Vec<HashMap<String, Ty>>,
}

impl Scopes {
    fn new() -> Self {
        Scopes {
            stack: vec![HashMap::new()],
        }
    }

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
            scope.insert(name, ty);
        }
    }

    /// Look up a variable, searching from innermost to outermost scope
    fn lookup(&self, name: &str) -> Option<&Ty> {
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
                scope.insert(name.to_string(), ty);
                return true;
            }
        }
        false
    }

    /// Convert to a flat HashMap for compatibility with existing code.
    /// This flattens all scopes, with inner scopes shadowing outer ones.
    fn to_flat_map(&self) -> HashMap<String, Ty> {
        let mut result = HashMap::new();
        for scope in &self.stack {
            result.extend(scope.clone());
        }
        result
    }

    /// Create from a flat HashMap (for function parameters initialization)
    fn from_flat_map(map: HashMap<String, Ty>) -> Self {
        Scopes { stack: vec![map] }
    }
}

struct UseMap {
    aliases: HashMap<String, Vec<String>>,
}

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

fn resolve_method_target(
    receiver_ty: &Ty,
    module_name: &str,
    struct_map: &HashMap<String, StructInfo>,
    span: Span,
) -> Result<(String, String, Vec<Ty>), TypeError> {
    let Ty::Path(receiver_name, receiver_args) = receiver_ty else {
        return Err(TypeError::new(
            "method receiver must be a struct value".to_string(),
            span,
        ));
    };

    if let Some(info) = struct_map.get(receiver_name) {
        let type_name = receiver_name
            .rsplit_once('.')
            .map(|(_, t)| t)
            .unwrap_or(receiver_name)
            .to_string();
        return Ok((info.module.clone(), type_name, receiver_args.clone()));
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
        return Ok((
            info.module.clone(),
            receiver_name.clone(),
            receiver_args.clone(),
        ));
    }

    Err(TypeError::new(
        format!("unknown struct `{receiver_name}`"),
        span,
    ))
}

fn resolve_impl_target(
    target: &Type,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    struct_map: &HashMap<String, StructInfo>,
    module_name: &str,
    span: Span,
) -> Result<(String, String, Vec<Ty>), TypeError> {
    let target_ty = lower_type(target, use_map, stdlib)?;
    let Ty::Path(target_name, target_args) = &target_ty else {
        return Err(TypeError::new(
            "impl target must be a struct type name".to_string(),
            span,
        ));
    };
    if !target_args.is_empty() {
        return Err(TypeError::new(
            "generic impl targets not supported yet".to_string(),
            span,
        ));
    }
    let (impl_module, type_name, args) = if let Some(info) = struct_map.get(target_name) {
        let type_name = target_name
            .rsplit_once('.')
            .map(|(_, t)| t)
            .unwrap_or(target_name)
            .to_string();
        (info.module.clone(), type_name, target_args.clone())
    } else if target_name.contains('.') {
        let (mod_part, type_part) = target_name.rsplit_once('.').ok_or_else(|| {
            TypeError::new("invalid type path".to_string(), span)
        })?;
        (mod_part.to_string(), type_part.to_string(), target_args.clone())
    } else if let Some(info) = struct_map.get(&format!("{module_name}.{target_name}")) {
        (info.module.clone(), target_name.clone(), target_args.clone())
    } else {
        return Err(TypeError::new(
            "impl target must be a struct type name".to_string(),
            span,
        ));
    };
    if impl_module != module_name {
        return Err(TypeError::new(
            "impl blocks must be declared in the defining module".to_string(),
            span,
        ));
    }
    Ok((impl_module, type_name, args))
}

fn validate_impl_method(
    type_name: &str,
    module_name: &str,
    method: &Function,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    span: Span,
) -> Result<Vec<Param>, TypeError> {
    if method.name.item.contains("__") || method.name.item.starts_with(&format!("{type_name}__")) {
        return Err(TypeError::new(
            "method name in impl should be unqualified (write sum, not Pair__sum)".to_string(),
            method.name.span,
        ));
    }

    let Some(first_param) = method.params.first() else {
        return Err(TypeError::new(
            format!("first parameter must be self: {type_name}"),
            span,
        ));
    };
    if first_param.name.item != "self" {
        return Err(TypeError::new(
            format!("first parameter must be self: {type_name}"),
            span,
        ));
    }

    let mut params = method.params.clone();
    let expected_unqualified = Ty::Path(type_name.to_string(), Vec::new());
    let expected_qualified = Ty::Path(format!("{module_name}.{type_name}"), Vec::new());
    let expected_ptr_unqualified = Ty::Ptr(Box::new(expected_unqualified.clone()));
    let expected_ptr_qualified = Ty::Ptr(Box::new(expected_qualified.clone()));

    if let Some(ty) = &first_param.ty {
        let lowered = lower_type(ty, use_map, stdlib)?;
        if lowered != expected_unqualified
            && lowered != expected_qualified
            && lowered != expected_ptr_unqualified
            && lowered != expected_ptr_qualified
        {
            return Err(TypeError::new(
                format!("first parameter must be self: {type_name}"),
                span,
            ));
        }
    } else {
        let path = Path {
            segments: vec![Spanned::new(type_name.to_string(), span)],
            span,
        };
        params[0].ty = Some(Type::Path {
            path,
            args: Vec::new(),
            span,
        });
    }

    for param in params.iter().skip(1) {
        if param.ty.is_none() {
            return Err(TypeError::new(
                format!("parameter `{}` requires a type annotation", param.name.item),
                param.name.span,
            ));
        }
    }

    Ok(params)
}

fn desugar_impl_method(type_name: &str, method: &Function, params: Vec<Param>) -> Function {
    let name = Spanned::new(
        format!("{type_name}__{}", method.name.item),
        method.name.span,
    );
    Function {
        name,
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
) -> Result<Vec<Function>, TypeError> {
    let (_impl_module, type_name, _args) = resolve_impl_target(
        &impl_block.target,
        use_map,
        stdlib,
        struct_map,
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
        let params = validate_impl_method(
            &type_name,
            module_name,
            method,
            use_map,
            stdlib,
            method.span,
        )?;
        methods.push(desugar_impl_method(&type_name, method, params));
    }
    Ok(methods)
}

fn build_stdlib_index(stdlib: &[Module]) -> Result<StdlibIndex, TypeError> {
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

fn collect_functions(
    modules: &[&Module],
    entry_name: &str,
    stdlib: &StdlibIndex,
    struct_map: &HashMap<String, StructInfo>,
) -> Result<HashMap<String, FunctionSig>, TypeError> {
    let mut functions = HashMap::new();
    for module in modules {
        let module_name = module.name.to_string();
        let local_use = UseMap::new(module);
        for item in &module.items {
            let mut add_function = |name: &Ident,
                                    params: &[Param],
                                    ret: &Type,
                                    span: Span,
                                    is_pub: bool|
             -> Result<(), TypeError> {
                for param in params {
                    if param.ty.is_none() {
                        return Err(TypeError::new(
                            format!("parameter `{}` requires a type annotation", param.name.item),
                            param.name.span,
                        ));
                    }
                }
                let sig = FunctionSig {
                    params: params
                        .iter()
                        .map(|p| {
                            lower_type(
                                p.ty.as_ref().expect("param type checked"),
                                &local_use,
                                stdlib,
                            )
                        })
                        .collect::<Result<_, _>>()?,
                    ret: lower_type(ret, &local_use, stdlib)?,
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
                    add_function(&func.name, &func.params, &func.ret, func.name.span, func.is_pub)?;
                }
                Item::ExternFunction(func) => {
                    if func.name.item.contains("__") {
                        return Err(TypeError::new(
                            "function names may not contain `__`".to_string(),
                            func.name.span,
                        ));
                    }
                    add_function(&func.name, &func.params, &func.ret, func.name.span, func.is_pub)?;
                }
                Item::Impl(impl_block) => {
                    let methods =
                        desugar_impl_methods(impl_block, &module_name, &local_use, stdlib, struct_map)?;
                    for method in methods {
                        add_function(
                            &method.name,
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

fn collect_structs(
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
                let mut fields = HashMap::new();
                for field in &decl.fields {
                    let ty = lower_type(&field.ty, &local_use, stdlib)?;
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
                let info = StructInfo {
                    fields,
                    is_opaque: decl.is_opaque,
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

fn collect_enums(
    modules: &[&Module],
    entry_name: &str,
) -> Result<HashMap<String, EnumInfo>, TypeError> {
    let mut enums = HashMap::new();
    for module in modules {
        let module_name = module.name.to_string();
        for item in &module.items {
            if let Item::Enum(decl) = item {
                let mut variants = Vec::new();
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
                }
                let qualified = format!("{module_name}.{}", decl.name.item);
                if enums.contains_key(&qualified) {
                    return Err(TypeError::new(
                        format!("duplicate enum `{qualified}`"),
                        decl.name.span,
                    ));
                }
                let info = EnumInfo {
                    variants: variants.clone(),
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

use crate::hir::HirModule;

pub fn type_check(module: &Module) -> Result<crate::hir::HirProgram, TypeError> {
    type_check_program(module, &[], &[])
}

pub fn type_check_program(
    module: &Module,
    stdlib: &[Module],
    user_modules: &[Module],
) -> Result<crate::hir::HirProgram, TypeError> {
    let use_map = UseMap::new(module);
    let stdlib_index = build_stdlib_index(stdlib)?;
    let modules = stdlib
        .iter()
        .chain(user_modules.iter())
        .chain(std::iter::once(module))
        .collect::<Vec<_>>();
    let module_name = module.name.to_string();
    validate_package_safety(module)?;
    for user_module in user_modules {
        validate_package_safety(user_module)?;
    }
    let struct_map = collect_structs(&modules, &module_name, &stdlib_index)?;
    let enum_map = collect_enums(&modules, &module_name)?;
    let functions = collect_functions(&modules, &module_name, &stdlib_index, &struct_map)?;

    for item in &module.items {
        match item {
            Item::Function(func) => {
                check_function(
                    func,
                    &functions,
                    &use_map,
                    &struct_map,
                    &enum_map,
                    &stdlib_index,
                    &module_name,
                )?;
            }
            Item::Impl(impl_block) => {
                let methods = desugar_impl_methods(
                    impl_block,
                    &module_name,
                    &use_map,
                    &stdlib_index,
                    &struct_map,
                )?;
                for method in methods {
                    check_function(
                        &method,
                        &functions,
                        &use_map,
                        &struct_map,
                        &enum_map,
                        &stdlib_index,
                        &module_name,
                    )?;
                }
            }
            _ => {}
        }
    }

    // Lower all modules to HIR (Step 5)
    // Lower stdlib modules
    let hir_stdlib: Result<Vec<HirModule>, TypeError> = stdlib
        .iter()
        .map(|m| {
            let use_map = UseMap::new(m);
            lower_module(m, &functions, &struct_map, &enum_map, &use_map, &stdlib_index)
        })
        .collect();

    // Lower user modules
    let hir_user_modules: Result<Vec<HirModule>, TypeError> = user_modules
        .iter()
        .map(|m| {
            let use_map = UseMap::new(m);
            lower_module(m, &functions, &struct_map, &enum_map, &use_map, &stdlib_index)
        })
        .collect();

    // Lower entry module
    let hir_entry = lower_module(
        module,
        &functions,
        &struct_map,
        &enum_map,
        &use_map,
        &stdlib_index,
    )?;

    Ok(crate::hir::HirProgram {
        entry: hir_entry,
        user_modules: hir_user_modules?,
        stdlib: hir_stdlib?,
    })
}

fn validate_package_safety(module: &Module) -> Result<(), TypeError> {
    if module.package != PackageSafety::Safe {
        return Ok(());
    }
    for item in &module.items {
        match item {
            Item::ExternFunction(func) => {
                return Err(TypeError::new(
                    "extern declarations require `package unsafe`".to_string(),
                    func.span,
                ));
            }
            Item::Function(func) => {
                if let Some(span) = type_contains_ptr_fn(func) {
                    return Err(TypeError::new(
                        "raw pointer types require `package unsafe`".to_string(),
                        span,
                    ));
                }
            }
            Item::Impl(impl_block) => {
                for method in &impl_block.methods {
                    if let Some(span) = type_contains_ptr_fn(method) {
                        return Err(TypeError::new(
                            "raw pointer types require `package unsafe`".to_string(),
                            span,
                        ));
                    }
                }
            }
            Item::Struct(decl) => {
                if let Some(span) = type_contains_ptr_struct(decl) {
                    return Err(TypeError::new(
                        "raw pointer types require `package unsafe`".to_string(),
                        span,
                    ));
                }
            }
            Item::Enum(decl) => {
                if let Some(span) = type_contains_ptr_enum(decl) {
                    return Err(TypeError::new(
                        "raw pointer types require `package unsafe`".to_string(),
                        span,
                    ));
                }
            }
        }
    }
    Ok(())
}

fn type_contains_ptr(ty: &Type) -> Option<Span> {
    match ty {
        Type::Ptr { span, .. } => Some(*span),
        Type::Path { args, .. } => {
            for arg in args {
                if let Some(span) = type_contains_ptr(arg) {
                    return Some(span);
                }
            }
            None
        }
    }
}

fn type_contains_ptr_fn(func: &Function) -> Option<Span> {
    for param in &func.params {
        if let Some(ty) = &param.ty {
            if let Some(span) = type_contains_ptr(ty) {
                return Some(span);
            }
        }
    }
    if let Some(span) = type_contains_ptr(&func.ret) {
        return Some(span);
    }
    block_contains_ptr(&func.body)
}

fn type_contains_ptr_struct(decl: &StructDecl) -> Option<Span> {
    for field in &decl.fields {
        if let Some(span) = type_contains_ptr(&field.ty) {
            return Some(span);
        }
    }
    None
}

fn type_contains_ptr_enum(decl: &EnumDecl) -> Option<Span> {
    for variant in &decl.variants {
        if let Some(payload) = &variant.payload {
            if let Some(span) = type_contains_ptr(payload) {
                return Some(span);
            }
        }
    }
    None
}

fn block_contains_ptr(block: &Block) -> Option<Span> {
    for stmt in &block.stmts {
        match stmt {
            Stmt::Let(let_stmt) => {
                if let Some(ty) = &let_stmt.ty {
                    if let Some(span) = type_contains_ptr(ty) {
                        return Some(span);
                    }
                }
            }
            Stmt::Assign(_) => {}
            Stmt::If(if_stmt) => {
                if let Some(span) = block_contains_ptr(&if_stmt.then_block) {
                    return Some(span);
                }
                if let Some(else_block) = &if_stmt.else_block {
                    if let Some(span) = block_contains_ptr(else_block) {
                        return Some(span);
                    }
                }
            }
            Stmt::While(while_stmt) => {
                if let Some(span) = block_contains_ptr(&while_stmt.body) {
                    return Some(span);
                }
            }
            Stmt::Expr(expr_stmt) => {
                if let Expr::Match(match_expr) = &expr_stmt.expr {
                    for arm in &match_expr.arms {
                        if let Some(span) = block_contains_ptr(&arm.body) {
                            return Some(span);
                        }
                    }
                }
            }
            Stmt::Return(_) => {}
        }
    }
    None
}

/// Check if a statement is syntactically total (always returns).
/// This is a purely syntactic check, not real control-flow analysis.
fn stmt_is_total(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Return(ret_stmt) => ret_stmt.expr.is_some(),
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                // Match is total if all arms end with return
                match_is_total(match_expr)
            } else {
                false
            }
        }
        Stmt::If(if_stmt) => {
            // If is total if it has else and both branches end with return
            if let Some(else_block) = &if_stmt.else_block {
                block_ends_with_return(&if_stmt.then_block) && block_ends_with_return(else_block)
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if a block ends with a syntactically total statement.
fn block_ends_with_return(block: &Block) -> bool {
    block.stmts.last().map_or(false, stmt_is_total)
}

/// Check if a match expression is syntactically total (all arms end with return).
fn match_is_total(match_expr: &MatchExpr) -> bool {
    !match_expr.arms.is_empty() && match_expr.arms.iter().all(|arm| block_ends_with_return(&arm.body))
}

fn check_function(
    func: &Function,
    functions: &HashMap<String, FunctionSig>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
) -> Result<(), TypeError> {
    let mut params_map = HashMap::new();
    for param in &func.params {
        let Some(ty) = &param.ty else {
            return Err(TypeError::new(
                format!("parameter `{}` requires a type annotation", param.name.item),
                param.name.span,
            ));
        };
        let ty = lower_type(ty, use_map, stdlib)?;
        params_map.insert(param.name.item.clone(), ty);
    }
    let mut scopes = Scopes::from_flat_map(params_map);

    let ret_ty = lower_type(&func.ret, use_map, stdlib)?;

    for stmt in &func.body.stmts {
        check_stmt(
            stmt,
            &ret_ty,
            functions,
            &mut scopes,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
        )?;
    }

    // For non-unit functions, require that the last statement is syntactically total
    if ret_ty != Ty::Builtin(BuiltinType::Unit) {
        if let Some(last_stmt) = func.body.stmts.last() {
            if !stmt_is_total(last_stmt) {
                return Err(TypeError::new(
                    "expected return <expr>; as the final statement of this function".to_string(),
                    last_stmt.span(),
                ));
            }
        } else {
            return Err(TypeError::new(
                "expected return <expr>; as the final statement of this function".to_string(),
                func.body.span,
            ));
        }
    }

    Ok(())
}

fn check_stmt(
    stmt: &Stmt,
    ret_ty: &Ty,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Let(let_stmt) => {
            let locals_flat = scopes.to_flat_map();
            let expr_ty = check_expr(
                &let_stmt.expr,
                functions,
                &locals_flat,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            let final_ty = if let Some(annot) = &let_stmt.ty {
                let annot_ty = lower_type(annot, use_map, stdlib)?;
                if annot_ty != expr_ty {
                    return Err(TypeError::new(
                        format!("type mismatch: expected {annot_ty:?}, found {expr_ty:?}"),
                        let_stmt.span,
                    ));
                }
                annot_ty
            } else {
                expr_ty
            };
            scopes.insert_local(let_stmt.name.item.clone(), final_ty);
        }
        Stmt::Assign(assign) => {
            let Some(existing) = scopes.lookup(&assign.name.item) else {
                return Err(TypeError::new(
                    format!("unknown identifier `{}`", assign.name.item),
                    assign.name.span,
                ));
            };
            let existing = existing.clone();
            let locals_flat = scopes.to_flat_map();
            let expr_ty = check_expr(
                &assign.expr,
                functions,
                &locals_flat,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            if expr_ty != existing {
                return Err(TypeError::new(
                    format!(
                        "assignment type mismatch: expected {existing:?}, found {expr_ty:?}"
                    ),
                    assign.span,
                ));
            }
            scopes.assign(&assign.name.item, expr_ty);
        }
        Stmt::Return(ret_stmt) => {
            let locals_flat = scopes.to_flat_map();
            let expr_ty = if let Some(expr) = &ret_stmt.expr {
                check_expr(
                    expr,
                    functions,
                    &locals_flat,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?
            } else {
                Ty::Builtin(BuiltinType::Unit)
            };
            if &expr_ty != ret_ty {
                return Err(TypeError::new(
                    format!("return type mismatch: expected {ret_ty:?}, found {expr_ty:?}"),
                    ret_stmt.span,
                ));
            }
        }
        Stmt::If(if_stmt) => {
            let locals_flat = scopes.to_flat_map();
            let cond_ty = check_expr(
                &if_stmt.cond,
                functions,
                &locals_flat,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            if cond_ty != Ty::Builtin(BuiltinType::Bool) {
                return Err(TypeError::new(
                    "if condition must be bool".to_string(),
                    if_stmt.cond.span(),
                ));
            }
            check_block(
                &if_stmt.then_block,
                ret_ty,
                functions,
                scopes,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
            )?;
            if let Some(block) = &if_stmt.else_block {
                check_block(
                    block,
                    ret_ty,
                    functions,
                    scopes,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    module_name,
                )?;
            }
        }
        Stmt::While(while_stmt) => {
            let locals_flat = scopes.to_flat_map();
            let cond_ty = check_expr(
                &while_stmt.cond,
                functions,
                &locals_flat,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            if cond_ty != Ty::Builtin(BuiltinType::Bool) {
                return Err(TypeError::new(
                    "while condition must be bool".to_string(),
                    while_stmt.cond.span(),
                ));
            }
            check_block(
                &while_stmt.body,
                ret_ty,
                functions,
                scopes,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                module_name,
            )?;
        }
        Stmt::Expr(expr_stmt) => {
            let locals_flat = scopes.to_flat_map();
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                let _ = check_match_stmt(
                    match_expr,
                    functions,
                    &locals_flat,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?;
            } else {
                check_expr(
                    &expr_stmt.expr,
                    functions,
                    &locals_flat,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?;
            }
        }
    }

    Ok(())
}

fn check_block(
    block: &Block,
    ret_ty: &Ty,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
) -> Result<(), TypeError> {
    scopes.push_scope();
    for stmt in &block.stmts {
        check_stmt(
            stmt,
            ret_ty,
            functions,
            scopes,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
        )?;
    }
    scopes.pop_scope();
    Ok(())
}

fn check_expr(
    expr: &Expr,
    functions: &HashMap<String, FunctionSig>,
    locals: &HashMap<String, Ty>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
    match expr {
        Expr::Literal(lit) => match &lit.value {
            Literal::Int(_) => Ok(Ty::Builtin(BuiltinType::I32)),
            Literal::U8(_) => Ok(Ty::Builtin(BuiltinType::U8)),
            Literal::String(_) => Ok(Ty::Builtin(BuiltinType::String)),
            Literal::Bool(_) => Ok(Ty::Builtin(BuiltinType::Bool)),
            Literal::Unit => Ok(Ty::Builtin(BuiltinType::Unit)),
        },
        Expr::Path(path) => {
            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if let Some(ty) = locals.get(name) {
                    return Ok(ty.clone());
                }
            }
            if let Some(ty) = resolve_enum_variant(path, use_map, enum_map, module_name) {
                return Ok(ty);
            }
            Err(TypeError::new(
                format!("unknown value `{path}`"),
                path.span,
            ))
        }
        Expr::Call(call) => {
            // Convert the callee (Path or FieldAccess chain) to a Path for function lookup
            let path = call.callee.to_path().ok_or_else(|| {
                TypeError::new(
                    "call target must be a function path".to_string(),
                    call.callee.span(),
                )
            })?;

            // Special handling for Ok(...) and Err(...) Result constructors
            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if name == "Ok" || name == "Err" {
                    // Check that we have exactly one argument
                    if call.args.len() != 1 {
                        return Err(TypeError::new(
                            format!("{name} takes exactly one argument"),
                            call.span,
                        ));
                    }
                    // Check the argument type
                    let arg_ty = check_expr(
                        &call.args[0],
                        functions,
                        locals,
                        use_map,
                        struct_map,
                        enum_map,
                        stdlib,
                        ret_ty,
                        module_name,
                    )?;
                    // Infer Result type from context (ret_ty) if available
                    if let Ty::Path(ty_name, args) = ret_ty {
                        if ty_name == "Result" && args.len() == 2 {
                            // Verify the argument matches the expected type
                            let expected = if name == "Ok" { &args[0] } else { &args[1] };
                            if &arg_ty != expected {
                                return Err(TypeError::new(
                                    format!("{name} argument type mismatch: expected {expected:?}, got {arg_ty:?}"),
                                    call.args[0].span(),
                                ));
                            }
                            return Ok(ret_ty.clone());
                        }
                    }
                    // If no Result context, construct Result from arg type
                    return Ok(Ty::Path(
                        "Result".to_string(),
                        if name == "Ok" {
                            vec![arg_ty, Ty::Builtin(BuiltinType::Unit)]
                        } else {
                            vec![Ty::Builtin(BuiltinType::Unit), arg_ty]
                        },
                    ));
                }
            }

            let resolved = resolve_path(&path, use_map);
            let key = resolved.join(".");

            // First try the resolved path as-is
            let sig = if let Some(sig) = functions.get(&key) {
                sig
            } else if resolved.len() == 1 {
                // Try prepending current module for unqualified function names
                let qualified = format!("{}.{}", module_name, key);
                functions.get(&qualified).ok_or_else(|| {
                    TypeError::new(format!("unknown function `{key}`"), path.span)
                })?
            } else {
                return Err(TypeError::new(format!("unknown function `{key}`"), path.span));
            };
            if sig.module != module_name && !sig.is_pub {
                return Err(TypeError::new(
                    format!("function `{}` is private", key),
                    call.span,
                ));
            }
            if sig.params.len() != call.args.len() {
                return Err(TypeError::new(
                    format!(
                        "argument count mismatch: expected {}, found {}",
                        sig.params.len(),
                        call.args.len()
                    ),
                    call.span,
                ));
            }
            for (arg, expected) in call.args.iter().zip(&sig.params) {
                let arg_ty = check_expr(
                    arg,
                    functions,
                    locals,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?;
                if &arg_ty != expected {
                    return Err(TypeError::new(
                        format!("argument type mismatch: expected {expected:?}, found {arg_ty:?}"),
                        arg.span(),
                    ));
                }
            }
            Ok(sig.ret.clone())
        }
        Expr::MethodCall(method_call) => {
            // Disambiguation: is this a method on a value or a module-qualified function call?
            fn get_leftmost_segment(expr: &Expr) -> Option<&str> {
                match expr {
                    Expr::Path(path) if path.segments.len() == 1 => {
                        Some(&path.segments[0].item)
                    }
                    Expr::FieldAccess(fa) => get_leftmost_segment(&fa.object),
                    _ => None,
                }
            }

            let base_is_local = if let Some(base_name) = get_leftmost_segment(&method_call.receiver) {
                locals.get(base_name).is_some()
            } else {
                // Not a pure a.b.c chain (e.g., call result) - treat as method on value
                true
            };

            let path_call = method_call.receiver.to_path().map(|mut path| {
                path.segments.push(method_call.method.clone());
                path.span = Span::new(path.span.start, method_call.method.span.end);
                path
            });

            let is_function = if let Some(path) = &path_call {
                let resolved = resolve_path(path, use_map);
                let key = resolved.join(".");
                functions.contains_key(&key)
            } else {
                false
            };

            if !base_is_local && is_function {
                let path = path_call.expect("path exists for function call");
                let resolved = resolve_path(&path, use_map);
                let key = resolved.join(".");
                let sig = functions.get(&key).ok_or_else(|| {
                    TypeError::new(format!("unknown function `{key}`"), path.span)
                })?;
                if sig.module != module_name && !sig.is_pub {
                    return Err(TypeError::new(
                        format!("function `{key}` is private"),
                        method_call.span,
                    ));
                }
                if sig.params.len() != method_call.args.len() {
                    return Err(TypeError::new(
                        format!(
                            "argument count mismatch: expected {}, found {}",
                            sig.params.len(),
                            method_call.args.len()
                        ),
                        method_call.span,
                    ));
                }
                for (arg, expected) in method_call.args.iter().zip(&sig.params) {
                    let arg_ty = check_expr(
                        arg,
                        functions,
                        locals,
                        use_map,
                        struct_map,
                        enum_map,
                        stdlib,
                        ret_ty,
                        module_name,
                    )?;
                    if &arg_ty != expected {
                        return Err(TypeError::new(
                            format!(
                                "argument type mismatch: expected {expected:?}, found {arg_ty:?}"
                            ),
                            arg.span(),
                        ));
                    }
                }
                return Ok(sig.ret.clone());
            }

            // Method on a value: resolve to module.Type__method
            let receiver_ty = check_expr(
                &method_call.receiver,
                functions,
                locals,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            let (method_module, type_name, receiver_args) = resolve_method_target(
                &receiver_ty,
                module_name,
                struct_map,
                method_call.receiver.span(),
            )?;
            let method_fn = format!("{type_name}__{}", method_call.method.item);
            let qualified_key = format!("{method_module}.{method_fn}");
            let key = if functions.contains_key(&qualified_key) {
                qualified_key
            } else if method_module == module_name && functions.contains_key(&method_fn) {
                method_fn.clone()
            } else {
                return Err(TypeError::new(
                    format!("unknown method `{qualified_key}`"),
                    method_call.span,
                ));
            };
            let sig = functions
                .get(&key)
                .ok_or_else(|| TypeError::new(format!("unknown method `{key}`"), method_call.span))?;
            if sig.module != module_name && !sig.is_pub {
                return Err(TypeError::new(
                    format!("method `{key}` is private"),
                    method_call.span,
                ));
            }
            if sig.params.len() != method_call.args.len() + 1 {
                return Err(TypeError::new(
                    format!(
                        "argument count mismatch: expected {}, found {}",
                        sig.params.len() - 1,
                        method_call.args.len()
                    ),
                    method_call.span,
                ));
            }
            let receiver_ptr = Ty::Ptr(Box::new(receiver_ty.clone()));
            let receiver_unqualified = Ty::Path(type_name.clone(), receiver_args);
            let receiver_ptr_unqualified = Ty::Ptr(Box::new(receiver_unqualified.clone()));
            if sig.params[0] != receiver_ty
                && sig.params[0] != receiver_ptr
                && sig.params[0] != receiver_unqualified
                && sig.params[0] != receiver_ptr_unqualified
            {
                return Err(TypeError::new(
                    format!(
                        "method receiver type mismatch: expected {expected:?}, found {receiver_ty:?}",
                        expected = sig.params[0]
                    ),
                    method_call.receiver.span(),
                ));
            }
            for (arg, expected) in method_call.args.iter().zip(&sig.params[1..]) {
                let arg_ty = check_expr(
                    arg,
                    functions,
                    locals,
                    use_map,
                    struct_map,
                    enum_map,
                    stdlib,
                    ret_ty,
                    module_name,
                )?;
                if &arg_ty != expected {
                    return Err(TypeError::new(
                        format!("argument type mismatch: expected {expected:?}, found {arg_ty:?}"),
                        arg.span(),
                    ));
                }
            }
            Ok(sig.ret.clone())
        }
        Expr::StructLiteral(lit) => check_struct_literal(
            lit,
            functions,
            locals,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        ),
        Expr::Unary(unary) => {
            let expr_ty = check_expr(
                &unary.expr,
                functions,
                locals,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            match unary.op {
                UnaryOp::Neg => {
                    if expr_ty == Ty::Builtin(BuiltinType::I32)
                        || expr_ty == Ty::Builtin(BuiltinType::I64)
                    {
                        Ok(expr_ty)
                    } else {
                        Err(TypeError::new(
                            "unary - expects integer".to_string(),
                            unary.span,
                        ))
                    }
                }
                UnaryOp::Not => {
                    if expr_ty == Ty::Builtin(BuiltinType::Bool) {
                        Ok(expr_ty)
                    } else {
                        Err(TypeError::new(
                            "unary ! expects bool".to_string(),
                            unary.span,
                        ))
                    }
                }
            }
        }
        Expr::Binary(binary) => {
            let left = check_expr(
                &binary.left,
                functions,
                locals,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            let right = check_expr(
                &binary.right,
                functions,
                locals,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            match binary.op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div => {
                    if left == right && (left == Ty::Builtin(BuiltinType::I32)
                        || left == Ty::Builtin(BuiltinType::I64))
                    {
                        Ok(left)
                    } else {
                        Err(TypeError::new(
                            "binary arithmetic expects matching integer types".to_string(),
                            binary.span,
                        ))
                    }
                }
                BinaryOp::Eq | BinaryOp::Neq => {
                    if left == right {
                        Ok(Ty::Builtin(BuiltinType::Bool))
                    } else {
                        Err(TypeError::new(
                            "comparison expects matching operand types".to_string(),
                            binary.span,
                        ))
                    }
                }
                BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
                    if left == right && is_orderable_type(&left) {
                        Ok(Ty::Builtin(BuiltinType::Bool))
                    } else {
                        Err(TypeError::new(
                            "ordering expects matching integer types".to_string(),
                            binary.span,
                        ))
                    }
                }
                BinaryOp::And | BinaryOp::Or => {
                    if left == Ty::Builtin(BuiltinType::Bool)
                        && right == Ty::Builtin(BuiltinType::Bool)
                    {
                        Ok(Ty::Builtin(BuiltinType::Bool))
                    } else {
                        Err(TypeError::new(
                            "logical operators expect bool".to_string(),
                            binary.span,
                        ))
                    }
                }
            }
        }
        Expr::Match(match_expr) => check_match_expr_value(
            match_expr,
            functions,
            locals,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        ),
        Expr::Grouping(group) => check_expr(
            &group.expr,
            functions,
            locals,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        ),
        Expr::FieldAccess(field_access) => {
            // Disambiguation rule: enum/module paths vs value field access
            // If the leftmost segment is a local variable, treat as field access.
            // If it's not a pure identifier.identifier... chain, treat as field access.
            fn get_leftmost_path_segment(expr: &Expr) -> Option<&str> {
                match expr {
                    // Note: After P0.5, parse_primary creates single-segment Paths only.
                    // Multi-segment paths don't exist in expressions anymore (dots become FieldAccess).
                    Expr::Path(path) if path.segments.len() == 1 => {
                        Some(&path.segments[0].item)
                    }
                    Expr::FieldAccess(fa) => get_leftmost_path_segment(&fa.object),
                    _ => None,  // Not a simple chain (e.g., call result, literal, etc.)
                }
            }

            let base_is_local = if let Some(base_name) = get_leftmost_path_segment(&field_access.object) {
                locals.contains_key(base_name)
            } else {
                // Language rule: if it's not a pure a.b.c chain (e.g., f().x or 123.x),
                // treat as value field access, not a module/enum path
                true
            };

            if !base_is_local {
                // Try to convert the FieldAccess chain to a Path and resolve as an enum variant
                // This handles cases like fs.FsErr.InvalidPath where fs is a module, not a local
                if let Some(path) = Expr::FieldAccess(field_access.clone()).to_path() {
                    if let Some(ty) = resolve_enum_variant(&path, use_map, enum_map, module_name) {
                        return Ok(ty);
                    }
                }
            }

            let object_ty = check_expr(
                &field_access.object,
                functions,
                locals,
                use_map,
                struct_map,
                enum_map,
                stdlib,
                ret_ty,
                module_name,
            )?;
            let Ty::Path(struct_name, _) = object_ty else {
                return Err(TypeError::new(
                    "field access requires a struct value".to_string(),
                    field_access.span,
                ));
            };
            let info = struct_map
                .get(&struct_name)
                .or_else(|| struct_map.get(&format!("{module_name}.{struct_name}")))
                .ok_or_else(|| {
                    TypeError::new(
                        format!("field access on non-struct `{struct_name}`"),
                        field_access.span,
                    )
                })?;
            if info.is_opaque && info.module != module_name {
                return Err(TypeError::new(
                    format!(
                        "cannot access fields of opaque type `{struct_name}` outside module `{}`",
                        info.module
                    ),
                    field_access.span,
                ));
            }
            let field_ty = info.fields.get(&field_access.field.item).ok_or_else(|| {
                TypeError::new(
                    format!("unknown field `{}`", field_access.field.item),
                    field_access.field.span,
                )
            })?;
            Ok(field_ty.clone())
        }
    }
}

fn check_match_stmt(
    match_expr: &MatchExpr,
    functions: &HashMap<String, FunctionSig>,
    locals: &HashMap<String, Ty>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
    let match_ty = check_expr(
        &match_expr.expr,
        functions,
        locals,
        use_map,
        struct_map,
        enum_map,
        stdlib,
        ret_ty,
        module_name,
    )?;
    let mut parent_scopes = Scopes::from_flat_map(locals.clone());
    for arm in &match_expr.arms {
        parent_scopes.push_scope();
        bind_pattern(&arm.pattern, &match_ty, &mut parent_scopes, use_map, enum_map, module_name)?;
        check_block(
            &arm.body,
            ret_ty,
            functions,
            &mut parent_scopes,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
        )?;
        parent_scopes.pop_scope();
    }
    Ok(Ty::Builtin(BuiltinType::Unit))
}

fn check_match_expr_value(
    match_expr: &MatchExpr,
    functions: &HashMap<String, FunctionSig>,
    locals: &HashMap<String, Ty>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
    let match_ty = check_expr(
        &match_expr.expr,
        functions,
        locals,
        use_map,
        struct_map,
        enum_map,
        stdlib,
        ret_ty,
        module_name,
    )?;
    let mut result_ty: Option<Ty> = None;
    let mut parent_scopes = Scopes::from_flat_map(locals.clone());
    for arm in &match_expr.arms {
        parent_scopes.push_scope();
        bind_pattern(&arm.pattern, &match_ty, &mut parent_scopes, use_map, enum_map, module_name)?;
        let arm_ty = check_match_arm_value(
            &arm.body,
            functions,
            &mut parent_scopes,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        )?;
        parent_scopes.pop_scope();
        if let Some(prev) = &result_ty {
            if prev != &arm_ty {
                return Err(TypeError::new(
                    format!("match arm type mismatch: expected {prev:?}, found {arm_ty:?}"),
                    arm.body.span,
                ));
            }
        } else {
            result_ty = Some(arm_ty);
        }
    }
    Ok(result_ty.unwrap_or(Ty::Builtin(BuiltinType::Unit)))
}

fn check_match_arm_value(
    block: &Block,
    functions: &HashMap<String, FunctionSig>,
    scopes: &mut Scopes,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
    let Some((last, prefix)) = block.stmts.split_last() else {
        return Err(TypeError::new(
            "match arm must end with expression".to_string(),
            block.span,
        ));
    };
    for stmt in prefix {
        // Match expression arms cannot contain return statements
        if matches!(stmt, Stmt::Return(_)) {
            return Err(TypeError::new(
                "match arm cannot return in expression context".to_string(),
                block.span,
            ));
        }
        check_stmt(
            stmt,
            ret_ty,
            functions,
            scopes,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            module_name,
        )?;
    }
    let locals_flat = scopes.to_flat_map();
    match last {
        Stmt::Expr(expr_stmt) => check_expr(
            &expr_stmt.expr,
            functions,
            &locals_flat,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        ),
        _ => Err(TypeError::new(
            "match arm must end with expression".to_string(),
            block.span,
        )),
    }
}

fn check_struct_literal(
    lit: &StructLiteralExpr,
    functions: &HashMap<String, FunctionSig>,
    locals: &HashMap<String, Ty>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
    let type_name = resolve_type_name(&lit.path, use_map, stdlib);
    let key = if lit.path.segments.len() == 1 {
        if stdlib.types.contains_key(&lit.path.segments[0].item) {
            type_name.clone()
        } else {
            lit.path.segments[0].item.clone()
        }
    } else {
        type_name.clone()
    };
    let info = struct_map.get(&key).ok_or_else(|| {
        TypeError::new(format!("unknown struct `{}`", key), lit.span)
    })?;
    if info.is_opaque && info.module != module_name {
        return Err(TypeError::new(
            format!(
                "cannot construct opaque type `{}` outside module `{}`",
                key, info.module
            ),
            lit.span,
        ));
    }

    let mut remaining = info.fields.clone();
    for field in &lit.fields {
        let expected = remaining.remove(&field.name.item).ok_or_else(|| {
            TypeError::new(
                format!("unknown field `{}`", field.name.item),
                field.span,
            )
        })?;
        let actual = check_expr(
            &field.expr,
            functions,
            locals,
            use_map,
            struct_map,
            enum_map,
            stdlib,
            ret_ty,
            module_name,
        )?;
        if actual != expected {
            return Err(TypeError::new(
                format!("field `{}` expects {expected:?}, found {actual:?}", field.name.item),
                field.span,
            ));
        }
    }
    if let Some((missing, _)) = remaining.into_iter().next() {
        return Err(TypeError::new(
            format!("missing field `{missing}`"),
            lit.span,
        ));
    }

    Ok(Ty::Path(type_name, Vec::new()))
}

fn bind_pattern(
    pattern: &Pattern,
    match_ty: &Ty,
    scopes: &mut Scopes,
    use_map: &UseMap,
    enum_map: &HashMap<String, EnumInfo>,
    module_name: &str,
) -> Result<(), TypeError> {
    match pattern {
        Pattern::Call { path, binding, .. } => {
            let Some(binding) = binding else {
                return Ok(());
            };
            let name = path
                .segments
                .iter()
                .map(|seg| seg.item.as_str())
                .collect::<Vec<_>>()
                .join(".");
            if let Ty::Path(ty_name, args) = match_ty {
                if ty_name == "Result" && args.len() == 2 {
                    let ty = if name == "Ok" {
                        args[0].clone()
                    } else if name == "Err" {
                        args[1].clone()
                    } else {
                        return Ok(());
                    };
                    scopes.insert_local(binding.item.clone(), ty);
                    return Ok(());
                }
            }
            Err(TypeError::new(
                "pattern binding requires a Result match".to_string(),
                path.span,
            ))
        }
        Pattern::Binding(ident) => {
            scopes.insert_local(ident.item.clone(), match_ty.clone());
            Ok(())
        }
        Pattern::Path(path) => {
            if let Some(ty) = resolve_enum_variant(path, use_map, enum_map, module_name) {
                if &ty != match_ty {
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

fn lower_type(ty: &Type, use_map: &UseMap, stdlib: &StdlibIndex) -> Result<Ty, TypeError> {
    match ty {
        Type::Ptr { target, .. } => Ok(Ty::Ptr(Box::new(lower_type(target, use_map, stdlib)?))),
        Type::Path { path, args, .. } => {
            let resolved = resolve_path(path, use_map);
            let path_segments = resolved.iter().map(|seg| seg.as_str()).collect::<Vec<_>>();
            if path_segments.len() == 1 {
                let builtin = match path_segments[0] {
                    "i32" => Some(BuiltinType::I32),
                    "i64" => Some(BuiltinType::I64),
                    "u32" => Some(BuiltinType::U32),
                    "u8" => Some(BuiltinType::U8),
                    "bool" => Some(BuiltinType::Bool),
                    "string" => Some(BuiltinType::String),
                    "unit" => Some(BuiltinType::Unit),
                    _ => None,
                };
                if let Some(builtin) = builtin {
                    return Ok(Ty::Builtin(builtin));
                }
                let alias = resolve_type_name(path, use_map, stdlib);
                if alias != resolved.join(".") {
                    return Ok(Ty::Path(alias, Vec::new()));
                }
            }
            let joined = path_segments.join(".");
            let args = args
                .iter()
                .map(|arg| lower_type(arg, use_map, stdlib))
                .collect::<Result<_, _>>()?;
            Ok(Ty::Path(joined, args))
        }
    }
}

fn is_orderable_type(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Builtin(BuiltinType::I32)
            | Ty::Builtin(BuiltinType::I64)
            | Ty::Builtin(BuiltinType::U32)
            | Ty::Builtin(BuiltinType::U8)
    )
}

// ============================================================================
// HIR Lowering - Convert typechecked AST to HIR with full resolution
// ============================================================================

use crate::hir::{
    HirBinary, HirBlock, HirCall, HirEnum, HirEnumVariant, HirEnumVariantExpr, HirExpr, HirExprStmt, HirField,
    HirFieldAccess, HirFunction, HirIfStmt, HirLetStmt, HirLiteral, HirLocal, HirMatch,
    HirMatchArm, HirParam, HirPattern, HirReturnStmt, HirStmt, HirStruct, HirStructLiteral,
    HirStructLiteralField, HirUnary, HirWhileStmt, HirAssignStmt, LocalId, ResolvedCallee,
};

struct LoweringCtx<'a> {
    functions: &'a HashMap<String, FunctionSig>,
    structs: &'a HashMap<String, StructInfo>,
    enums: &'a HashMap<String, EnumInfo>,
    use_map: &'a UseMap,
    stdlib: &'a StdlibIndex,
    module_name: &'a str,
    /// Maps variable names to their LocalId
    local_map: HashMap<String, LocalId>,
    /// Maps variable names to their types (needed for type checking during lowering)
    local_types: HashMap<String, Ty>,
    local_counter: usize,
}

impl<'a> LoweringCtx<'a> {
    fn new(
        functions: &'a HashMap<String, FunctionSig>,
        structs: &'a HashMap<String, StructInfo>,
        enums: &'a HashMap<String, EnumInfo>,
        use_map: &'a UseMap,
        stdlib: &'a StdlibIndex,
        module_name: &'a str,
    ) -> Self {
        Self {
            functions,
            structs,
            enums,
            use_map,
            stdlib,
            module_name,
            local_map: HashMap::new(),
            local_types: HashMap::new(),
            local_counter: 0,
        }
    }

    fn fresh_local(&mut self, name: String, ty: Ty) -> LocalId {
        let id = LocalId(self.local_counter);
        self.local_counter += 1;
        self.local_map.insert(name.clone(), id);
        self.local_types.insert(name, ty);
        id
    }

    fn get_local(&self, name: &str) -> Option<LocalId> {
        self.local_map.get(name).copied()
    }

    fn push_scope(&mut self) {
        // For now, we'll use a simple flat map
        // Future: use a scope stack for proper shadowing
    }

    fn pop_scope(&mut self) {
        // Future: pop scope from stack
    }
}

fn lower_module(
    module: &Module,
    functions: &HashMap<String, FunctionSig>,
    structs: &HashMap<String, StructInfo>,
    enums: &HashMap<String, EnumInfo>,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
) -> Result<HirModule, TypeError> {
    let module_name = module.name.to_string();
    let mut ctx = LoweringCtx::new(functions, structs, enums, use_map, stdlib, &module_name);

    // Lower all functions
    let mut hir_functions = Vec::new();
    for item in &module.items {
        match item {
            Item::Function(func) => {
                let hir_func = lower_function(func, &mut ctx)?;
                hir_functions.push(hir_func);
            }
            Item::Impl(impl_block) => {
                let methods =
                    desugar_impl_methods(impl_block, &module_name, use_map, stdlib, structs)?;
                for method in methods {
                    let hir_func = lower_function(&method, &mut ctx)?;
                    hir_functions.push(hir_func);
                }
            }
            _ => {}
        }
    }

    // Lower structs (just collect metadata for now)
    let mut hir_structs = Vec::new();
    for item in &module.items {
        if let Item::Struct(decl) = item {
            let fields: Result<Vec<HirField>, TypeError> = decl
                .fields
                .iter()
                .map(|f| {
                    Ok(HirField {
                        name: f.name.item.clone(),
                        ty: lower_type(&f.ty, use_map, stdlib)?,
                    })
                })
                .collect();
            hir_structs.push(HirStruct {
                name: decl.name.item.clone(),
                fields: fields?,
                is_pub: decl.is_pub,
                is_opaque: decl.is_opaque,
                span: decl.span,
            });
        }
    }

    // Lower enums
    let mut hir_enums = Vec::new();
    for item in &module.items {
        if let Item::Enum(decl) = item {
            let variants: Result<Vec<HirEnumVariant>, TypeError> = decl
                .variants
                .iter()
                .map(|v| {
                    let payload = v.payload.as_ref()
                        .map(|ty| lower_type(ty, use_map, stdlib))
                        .transpose()?;
                    Ok(HirEnumVariant {
                        name: v.name.item.clone(),
                        payload,
                    })
                })
                .collect();
            hir_enums.push(HirEnum {
                name: decl.name.item.clone(),
                variants: variants?,
                is_pub: decl.is_pub,
                span: decl.span,
            });
        }
    }

    Ok(HirModule {
        name: module_name,
        functions: hir_functions,
        structs: hir_structs,
        enums: hir_enums,
    })
}

fn lower_function(func: &Function, ctx: &mut LoweringCtx) -> Result<HirFunction, TypeError> {
    // Reset local counter for this function
    ctx.local_counter = 0;
    ctx.local_map.clear();
    ctx.local_types.clear();

    // Create locals for parameters
    let params: Result<Vec<HirParam>, TypeError> = func
        .params
        .iter()
        .map(|p| {
            let Some(ty) = &p.ty else {
                return Err(TypeError::new(
                    format!("parameter `{}` requires a type annotation", p.name.item),
                    p.name.span,
                ));
            };
            let ty = lower_type(ty, ctx.use_map, ctx.stdlib)?;
            let local_id = ctx.fresh_local(p.name.item.clone(), ty.clone());
            Ok(HirParam {
                local_id,
                name: p.name.item.clone(),
                ty,
            })
        })
        .collect();
    let params = params?;

    let ret_ty = lower_type(&func.ret, ctx.use_map, ctx.stdlib)?;
    let body = lower_block(&func.body, ctx, &ret_ty)?;

    Ok(HirFunction {
        name: func.name.item.clone(),
        params,
        ret_ty,
        body,
        is_pub: func.is_pub,
        span: func.span,
    })
}

fn lower_block(block: &Block, ctx: &mut LoweringCtx, ret_ty: &Ty) -> Result<HirBlock, TypeError> {
    ctx.push_scope();
    let stmts: Result<Vec<HirStmt>, TypeError> = block
        .stmts
        .iter()
        .map(|stmt| lower_stmt(stmt, ctx, ret_ty))
        .collect();
    ctx.pop_scope();

    Ok(HirBlock {
        stmts: stmts?,
        span: block.span,
    })
}

fn lower_stmt(stmt: &Stmt, ctx: &mut LoweringCtx, ret_ty: &Ty) -> Result<HirStmt, TypeError> {
    match stmt {
        Stmt::Let(let_stmt) => {
            let expr = lower_expr(&let_stmt.expr, ctx, ret_ty)?;
            let ty = expr_type(&expr);
            let local_id = ctx.fresh_local(let_stmt.name.item.clone(), ty.clone());

            Ok(HirStmt::Let(HirLetStmt {
                local_id,
                name: let_stmt.name.item.clone(),
                ty,
                expr,
                span: let_stmt.span,
            }))
        }
        Stmt::Assign(assign) => {
            let expr = lower_expr(&assign.expr, ctx, ret_ty)?;
            let local_id = ctx
                .get_local(&assign.name.item)
                .ok_or_else(|| TypeError::new("unknown variable".to_string(), assign.name.span))?;

            Ok(HirStmt::Assign(HirAssignStmt {
                local_id,
                name: assign.name.item.clone(),
                expr,
                span: assign.span,
            }))
        }
        Stmt::Return(ret) => {
            let expr = ret.expr.as_ref().map(|e| lower_expr(e, ctx, ret_ty)).transpose()?;
            Ok(HirStmt::Return(HirReturnStmt {
                expr,
                span: ret.span,
            }))
        }
        Stmt::If(if_stmt) => {
            let cond = lower_expr(&if_stmt.cond, ctx, ret_ty)?;
            let then_block = lower_block(&if_stmt.then_block, ctx, ret_ty)?;
            let else_block = if_stmt
                .else_block
                .as_ref()
                .map(|b| lower_block(b, ctx, ret_ty))
                .transpose()?;

            Ok(HirStmt::If(HirIfStmt {
                cond,
                then_block,
                else_block,
                span: if_stmt.span,
            }))
        }
        Stmt::While(while_stmt) => {
            let cond = lower_expr(&while_stmt.cond, ctx, ret_ty)?;
            let body = lower_block(&while_stmt.body, ctx, ret_ty)?;

            Ok(HirStmt::While(HirWhileStmt {
                cond,
                body,
                span: while_stmt.span,
            }))
        }
        Stmt::Expr(expr_stmt) => {
            // For match expressions, try to lower as expression first
            // If that fails (arms have returns), fall back to statement lowering
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                // Try expression lowering first
                match lower_expr(&expr_stmt.expr, ctx, ret_ty) {
                    Ok(expr) => {
                        return Ok(HirStmt::Expr(HirExprStmt {
                            expr,
                            span: expr_stmt.span,
                        }));
                    }
                    Err(_) => {
                        // Expression lowering failed (arms have returns)
                        // Use statement lowering
                        let expr = lower_match_stmt(match_expr, ctx, ret_ty)?;
                        return Ok(HirStmt::Expr(HirExprStmt {
                            expr,
                            span: expr_stmt.span,
                        }));
                    }
                }
            }

            let expr = lower_expr(&expr_stmt.expr, ctx, ret_ty)?;
            Ok(HirStmt::Expr(HirExprStmt {
                expr,
                span: expr_stmt.span,
            }))
        }
    }
}

/// Helper to get the type of an AST expression using the existing typechecker
/// This ensures we have a single source of truth for types
fn type_of_ast_expr(
    expr: &Expr,
    ctx: &LoweringCtx,
    ret_ty: &Ty,
) -> Result<Ty, TypeError> {
    check_expr(
        expr,
        ctx.functions,
        &ctx.local_types,  // Use the types we've been tracking
        ctx.use_map,
        ctx.structs,
        ctx.enums,
        ctx.stdlib,
        ret_ty,
        ctx.module_name,
    )
}

fn lower_expr(expr: &Expr, ctx: &mut LoweringCtx, ret_ty: &Ty) -> Result<HirExpr, TypeError> {
    // Get the type from the canonical typechecker
    let ty = type_of_ast_expr(expr, ctx, ret_ty)?;

    match expr {
        Expr::Literal(lit) => {
            // Type already computed by check_expr
            Ok(HirExpr::Literal(HirLiteral {
                value: lit.value.clone(),
                ty,
                span: lit.span,
            }))
        }
        Expr::Grouping(grouping) => {
            // Grouping is transparent - just lower the inner expression
            lower_expr(&grouping.expr, ctx, ret_ty)
        }
        Expr::Path(path) => {
            // Check if it's a local variable
            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if ctx.local_types.contains_key(name) {
                    let local_id = ctx.get_local(name).unwrap();
                    return Ok(HirExpr::Local(HirLocal {
                        local_id,
                        name: name.clone(),
                        ty,  // Use the type from check_expr
                        span: path.span,
                    }));
                }
            }
            // Otherwise it's an enum variant (check_expr verified this)
            // Extract variant name from path
            let variant_name = path.segments.last()
                .map(|s| s.item.clone())
                .unwrap_or_else(|| String::from("unknown"));

            Ok(HirExpr::EnumVariant(HirEnumVariantExpr {
                enum_ty: ty,
                variant_name,
                payload: None,
                span: path.span,
            }))
        }
        Expr::Call(call) => {
            // Convert callee to path
            let path = call.callee.to_path().ok_or_else(|| {
                TypeError::new(
                    "call target must be a function path".to_string(),
                    call.callee.span(),
                )
            })?;

            // Special handling for Ok(...) and Err(...) Result constructors
            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if name == "Ok" || name == "Err" {
                    // Lower the argument
                    let arg = lower_expr(&call.args[0], ctx, ret_ty)?;
                    let variant_name = name.clone();

                    // The result type comes from check_expr (stored in ty)
                    return Ok(HirExpr::EnumVariant(HirEnumVariantExpr {
                        enum_ty: ty.clone(),
                        variant_name,
                        payload: Some(Box::new(arg)),
                        span: call.span,
                    }));
                }
            }

            // Resolve function path
            let mut resolved = resolve_path(&path, ctx.use_map);

            // For unqualified names, prepend current module
            if resolved.len() == 1 {
                resolved.insert(0, ctx.module_name.to_string());
            }

            let key = resolved.join(".");

            // Lower arguments
            let args: Result<Vec<HirExpr>, TypeError> = call
                .args
                .iter()
                .map(|arg| lower_expr(arg, ctx, ret_ty))
                .collect();

            // Build resolved callee
            let module = resolved[..resolved.len() - 1].join(".");
            let name = resolved.last().unwrap().clone();
            let symbol = format!("capable_{}", key.replace('.', "_"));

            Ok(HirExpr::Call(HirCall {
                callee: ResolvedCallee::Function {
                    module,
                    name,
                    symbol,
                },
                args: args?,
                ret_ty: ty,  // Use type from check_expr
                span: call.span,
            }))
        }
        Expr::MethodCall(method_call) => {
            // Apply the same disambiguation as in check_expr
            fn get_leftmost_segment(expr: &Expr) -> Option<&str> {
                match expr {
                    Expr::Path(path) if path.segments.len() == 1 => Some(&path.segments[0].item),
                    Expr::FieldAccess(fa) => get_leftmost_segment(&fa.object),
                    _ => None,
                }
            }

            let base_is_local = if let Some(base_name) = get_leftmost_segment(&method_call.receiver) {
                ctx.local_types.contains_key(base_name)
            } else {
                true
            };

            let path_call = method_call.receiver.to_path().map(|mut path| {
                path.segments.push(method_call.method.clone());
                path.span = Span::new(path.span.start, method_call.method.span.end);
                path
            });

            let is_function = if let Some(path) = &path_call {
                let resolved = resolve_path(path, ctx.use_map);
                let key = resolved.join(".");
                ctx.functions.contains_key(&key)
            } else {
                false
            };

            if !base_is_local && is_function {
                let path = path_call.expect("path exists for function call");
                let resolved = resolve_path(&path, ctx.use_map);
                let key = resolved.join(".");
                let args: Result<Vec<HirExpr>, TypeError> = method_call
                    .args
                    .iter()
                    .map(|arg| lower_expr(arg, ctx, ret_ty))
                    .collect();

                let module = resolved[..resolved.len() - 1].join(".");
                let name = resolved.last().unwrap().clone();
                let symbol = format!("capable_{}", key.replace('.', "_"));

                return Ok(HirExpr::Call(HirCall {
                    callee: ResolvedCallee::Function {
                        module,
                        name,
                        symbol,
                    },
                    args: args?,
                    ret_ty: ty,
                    span: method_call.span,
                }));
            }

            let receiver = lower_expr(&method_call.receiver, ctx, ret_ty)?;
            let receiver_ty = type_of_ast_expr(&method_call.receiver, ctx, ret_ty)?;
            let (method_module, type_name, _) = resolve_method_target(
                &receiver_ty,
                ctx.module_name,
                ctx.structs,
                method_call.receiver.span(),
            )?;
            let method_fn = format!("{type_name}__{}", method_call.method.item);
            let key = format!("{method_module}.{method_fn}");
            let symbol = format!("capable_{}", key.replace('.', "_"));

            let mut args = Vec::with_capacity(method_call.args.len() + 1);
            args.push(receiver);
            for arg in &method_call.args {
                args.push(lower_expr(arg, ctx, ret_ty)?);
            }

            Ok(HirExpr::Call(HirCall {
                callee: ResolvedCallee::Function {
                    module: method_module,
                    name: method_fn,
                    symbol,
                },
                args,
                ret_ty: ty,
                span: method_call.span,
            }))
        }
        Expr::Binary(bin) => {
            // Type already verified by check_expr - just lower children and build node
            let left = lower_expr(&bin.left, ctx, ret_ty)?;
            let right = lower_expr(&bin.right, ctx, ret_ty)?;

            Ok(HirExpr::Binary(HirBinary {
                left: Box::new(left),
                op: bin.op.clone(),
                right: Box::new(right),
                ty,  // Use type from check_expr
                span: bin.span,
            }))
        }
        Expr::Unary(un) => {
            // Type already verified by check_expr - just lower child and build node
            let operand = lower_expr(&un.expr, ctx, ret_ty)?;

            Ok(HirExpr::Unary(HirUnary {
                op: un.op.clone(),
                expr: Box::new(operand),
                ty,  // Use type from check_expr
                span: un.span,
            }))
        }
        Expr::FieldAccess(field_access) => {
            // Disambiguation: enum/module paths vs struct field access
            // Same logic as check_expr to keep them in agreement
            fn get_leftmost_path_segment(expr: &Expr) -> Option<&str> {
                match expr {
                    Expr::Path(path) if path.segments.len() == 1 => Some(&path.segments[0].item),
                    Expr::FieldAccess(fa) => get_leftmost_path_segment(&fa.object),
                    _ => None,
                }
            }

            let base_is_local = if let Some(base_name) = get_leftmost_path_segment(&field_access.object) {
                ctx.local_types.contains_key(base_name)
            } else {
                true  // Not a pure chain - treat as value field access
            };

            if !base_is_local {
                // Try to convert to path and resolve as enum variant
                if let Some(path) = Expr::FieldAccess(field_access.clone()).to_path() {
                    if let Some(enum_ty) = resolve_enum_variant(&path, ctx.use_map, ctx.enums, ctx.module_name) {
                        // It's an enum variant - extract variant name
                        let variant_name = path.segments.last()
                            .map(|s| s.item.clone())
                            .unwrap_or_else(|| String::from("unknown"));

                        return Ok(HirExpr::EnumVariant(HirEnumVariantExpr {
                            enum_ty,
                            variant_name,
                            payload: None,
                            span: field_access.span,
                        }));
                    }
                }
            }

            let object = lower_expr(&field_access.object, ctx, ret_ty)?;
            let object_ty = type_of_ast_expr(&field_access.object, ctx, ret_ty)?;

            Ok(HirExpr::FieldAccess(HirFieldAccess {
                object: Box::new(object),
                object_ty,
                field_name: field_access.field.item.clone(),
                field_ty: ty,
                span: field_access.span,
            }))
        }
        Expr::StructLiteral(lit) => {
            // Resolve the struct type name
            let type_name = resolve_type_name(&lit.path, ctx.use_map, ctx.stdlib);
            let key = if lit.path.segments.len() == 1 {
                if ctx.stdlib.types.contains_key(&lit.path.segments[0].item) {
                    type_name.clone()
                } else {
                    lit.path.segments[0].item.clone()
                }
            } else {
                type_name.clone()
            };

            let info = ctx.structs.get(&key).ok_or_else(|| {
                TypeError::new(format!("unknown struct `{}`", key), lit.span)
            })?;

            // Check opaque constraint
            if info.is_opaque && info.module != ctx.module_name {
                return Err(TypeError::new(
                    format!(
                        "cannot construct opaque type `{}` outside module `{}`",
                        key, info.module
                    ),
                    lit.span,
                ));
            }

            // Lower each field expression
            let mut hir_fields = Vec::new();
            for field in &lit.fields {
                let hir_expr = lower_expr(&field.expr, ctx, ret_ty)?;
                hir_fields.push(HirStructLiteralField {
                    name: field.name.item.clone(),
                    expr: hir_expr,
                });
            }

            // Construct the struct type
            let struct_ty = Ty::Path(type_name, vec![]);

            Ok(HirExpr::StructLiteral(HirStructLiteral {
                struct_ty,
                fields: hir_fields,
                span: lit.span,
            }))
        }
        Expr::Match(match_expr) => {
            // This path is for match-as-expression (needs value from arms)
            // Lower the scrutinee expression
            let scrutinee = lower_expr(&match_expr.expr, ctx, ret_ty)?;
            let scrutinee_ty = expr_type(&scrutinee);

            // Lower each arm
            let mut hir_arms = Vec::new();
            for arm in &match_expr.arms {
                ctx.push_scope();

                // Lower the pattern, adding any bindings to ctx
                let hir_pattern = lower_pattern(&arm.pattern, &scrutinee_ty, ctx)?;

                // Lower the arm body
                let hir_body = lower_block(&arm.body, ctx, ret_ty)?;

                ctx.pop_scope();

                hir_arms.push(HirMatchArm {
                    pattern: hir_pattern,
                    body: hir_body,
                });
            }

            Ok(HirExpr::Match(HirMatch {
                expr: Box::new(scrutinee),
                expr_ty: scrutinee_ty,
                arms: hir_arms,
                result_ty: ty,  // Type from check_expr
                span: match_expr.span,
            }))
        }
    }
}

/// Lower a match-as-statement (arms can contain returns, don't need to produce values)
fn lower_match_stmt(
    match_expr: &MatchExpr,
    ctx: &mut LoweringCtx,
    ret_ty: &Ty,
) -> Result<HirExpr, TypeError> {
    // Lower the scrutinee expression
    let scrutinee = lower_expr(&match_expr.expr, ctx, ret_ty)?;
    let scrutinee_ty = expr_type(&scrutinee);

    // Lower each arm - using check_block instead of check_match_expr_value
    // This allows arms to end with return statements
    let mut hir_arms = Vec::new();
    for arm in &match_expr.arms {
        ctx.push_scope();

        // Lower the pattern, adding any bindings to ctx
        let hir_pattern = lower_pattern(&arm.pattern, &scrutinee_ty, ctx)?;

        // Lower the arm body (this uses lower_block which handles returns)
        let hir_body = lower_block(&arm.body, ctx, ret_ty)?;

        ctx.pop_scope();

        hir_arms.push(HirMatchArm {
            pattern: hir_pattern,
            body: hir_body,
        });
    }

    Ok(HirExpr::Match(HirMatch {
        expr: Box::new(scrutinee),
        expr_ty: scrutinee_ty,
        arms: hir_arms,
        result_ty: Ty::Builtin(BuiltinType::Unit),  // Statement context, no value
        span: match_expr.span,
    }))
}

/// Lower an AST pattern to an HIR pattern, adding any bindings to the context
fn lower_pattern(
    pattern: &Pattern,
    match_ty: &Ty,
    ctx: &mut LoweringCtx,
) -> Result<HirPattern, TypeError> {
    match pattern {
        Pattern::Wildcard(_) => Ok(HirPattern::Wildcard),

        Pattern::Literal(lit) => Ok(HirPattern::Literal(lit.clone())),

        Pattern::Binding(ident) => {
            // Binding introduces a new local with the match type
            let local_id = ctx.fresh_local(ident.item.clone(), match_ty.clone());
            Ok(HirPattern::Binding(local_id, ident.item.clone()))
        }

        Pattern::Path(path) => {
            // Path pattern is an enum variant without binding
            if let Some(enum_ty) = resolve_enum_variant(path, ctx.use_map, ctx.enums, ctx.module_name) {
                let variant_name = path.segments.last()
                    .map(|s| s.item.clone())
                    .unwrap_or_else(|| "unknown".to_string());
                Ok(HirPattern::Variant {
                    enum_ty,
                    variant_name,
                    binding: None,
                })
            } else {
                Err(TypeError::new(
                    format!("unknown enum variant in pattern: {}", path),
                    path.span,
                ))
            }
        }

        Pattern::Call { path, binding, span } => {
            // Call pattern is an enum variant with optional binding (e.g., Ok(x), Err(e))

            // Special handling for Result's Ok/Err variants
            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if name == "Ok" || name == "Err" {
                    if let Ty::Path(ty_name, args) = match_ty {
                        if ty_name == "Result" && args.len() == 2 {
                            let variant_name = name.clone();

                            // Handle binding if present
                            let binding_info = if let Some(bind_ident) = binding {
                                let bind_ty = if variant_name == "Ok" {
                                    args[0].clone()
                                } else {
                                    args[1].clone()
                                };
                                let local_id = ctx.fresh_local(bind_ident.item.clone(), bind_ty);
                                Some((local_id, bind_ident.item.clone()))
                            } else {
                                None
                            };

                            return Ok(HirPattern::Variant {
                                enum_ty: match_ty.clone(),
                                variant_name,
                                binding: binding_info,
                            });
                        }
                    }
                }
            }

            // Resolve the enum variant for user-defined enums
            if let Some(enum_ty) = resolve_enum_variant(path, ctx.use_map, ctx.enums, ctx.module_name) {
                let variant_name = path.segments.last()
                    .map(|s| s.item.clone())
                    .unwrap_or_else(|| "unknown".to_string());

                // Handle binding if present
                let binding_info = if let Some(bind_ident) = binding {
                    // For user-defined enums, use the match type as binding type
                    // TODO: Extract payload type from enum variant definition
                    let bind_ty = match_ty.clone();
                    let local_id = ctx.fresh_local(bind_ident.item.clone(), bind_ty);
                    Some((local_id, bind_ident.item.clone()))
                } else {
                    None
                };

                Ok(HirPattern::Variant {
                    enum_ty,
                    variant_name,
                    binding: binding_info,
                })
            } else {
                Err(TypeError::new(
                    format!("unknown enum variant in pattern: {}", path),
                    *span,
                ))
            }
        }
    }
}

// Helper to get type from HIR expr
fn expr_type(expr: &HirExpr) -> Ty {
    match expr {
        HirExpr::Literal(lit) => lit.ty.clone(),
        HirExpr::Local(local) => local.ty.clone(),
        HirExpr::EnumVariant(variant) => variant.enum_ty.clone(),
        HirExpr::Call(call) => call.ret_ty.clone(),
        HirExpr::FieldAccess(fa) => fa.field_ty.clone(),
        HirExpr::StructLiteral(sl) => sl.struct_ty.clone(),
        HirExpr::Binary(bin) => bin.ty.clone(),
        HirExpr::Unary(un) => un.ty.clone(),
        HirExpr::Match(m) => m.result_ty.clone(),
    }
}

trait SpanExt {
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
            Expr::StructLiteral(lit) => lit.span,
            Expr::Unary(unary) => unary.span,
            Expr::Binary(binary) => binary.span,
            Expr::Match(m) => m.span,
            Expr::Grouping(group) => group.span,
        }
    }
}
