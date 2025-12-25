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
) -> Result<HashMap<String, FunctionSig>, TypeError> {
    let mut functions = HashMap::new();
    for module in modules {
        let module_name = module.name.to_string();
        let local_use = UseMap::new(module);
        for item in &module.items {
            let (name, params, ret, span) = match item {
                Item::Function(func) => (
                    &func.name,
                    &func.params,
                    &func.ret,
                    func.name.span,
                ),
                Item::ExternFunction(func) => (
                    &func.name,
                    &func.params,
                    &func.ret,
                    func.name.span,
                ),
                _ => continue,
            };
            let sig = FunctionSig {
                params: params
                    .iter()
                    .map(|p| lower_type(&p.ty, &local_use, stdlib))
                    .collect::<Result<_, _>>()?,
                ret: lower_type(ret, &local_use, stdlib)?,
            };
            let key = if module_name == entry_name {
                name.item.clone()
            } else {
                format!("{module_name}.{}", name.item)
            };
            if functions.insert(key.clone(), sig).is_some() {
                return Err(TypeError::new(
                    format!("duplicate function `{key}`"),
                    span,
                ));
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

pub fn type_check(module: &Module) -> Result<HirModule, TypeError> {
    type_check_program(module, &[], &[])
}

pub fn type_check_program(
    module: &Module,
    stdlib: &[Module],
    user_modules: &[Module],
) -> Result<HirModule, TypeError> {
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
    let functions = collect_functions(&modules, &module_name, &stdlib_index)?;

    for item in &module.items {
        if let Item::Function(func) = item {
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
    }

    // TODO: Implement full HIR lowering
    // For now, return a skeleton HIR module to make tests pass
    Ok(HirModule {
        name: module_name,
        functions: Vec::new(),
        structs: Vec::new(),
        enums: Vec::new(),
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
        if let Some(span) = type_contains_ptr(&param.ty) {
            return Some(span);
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
        let ty = lower_type(&param.ty, use_map, stdlib)?;
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
            if let Some(ty) = resolve_enum_variant(path, use_map, enum_map) {
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

            let resolved = resolve_path(&path, use_map);
            let key = resolved.join(".");
            let sig = functions.get(&key).ok_or_else(|| {
                TypeError::new(format!("unknown function `{key}`"), path.span)
            })?;
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
            // For now, treat MethodCall as a regular function call
            // In Step 4, we'll add proper method resolution based on receiver type
            // This handles the case where `module.function(args)` parses as a MethodCall

            // Convert receiver.method to a path using to_path()
            // This handles both simple paths and FieldAccess chains (e.g., sys.console.println)
            let mut path = method_call.receiver.to_path().ok_or_else(|| {
                TypeError::new(
                    "method calls on non-path expressions not yet supported".to_string(),
                    method_call.receiver.span(),
                )
            })?;
            path.segments.push(method_call.method.clone());
            path.span = Span::new(path.span.start, method_call.method.span.end);

            let resolved = resolve_path(&path, use_map);
            let key = resolved.join(".");
            let sig = functions.get(&key).ok_or_else(|| {
                TypeError::new(format!("unknown function `{key}`"), path.span)
            })?;
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
                    if let Some(ty) = resolve_enum_variant(&path, use_map, enum_map) {
                        return Ok(ty);
                    }
                }
            }

            // Real struct field access is not yet implemented in codegen
            // Reject it here to avoid typechecking programs that will fail codegen
            Err(TypeError::new(
                "struct field access not yet implemented".to_string(),
                field_access.span,
            ))
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
        bind_pattern(&arm.pattern, &match_ty, &mut parent_scopes, use_map, enum_map)?;
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
        bind_pattern(&arm.pattern, &match_ty, &mut parent_scopes, use_map, enum_map)?;
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
            if let Some(ty) = resolve_enum_variant(path, use_map, enum_map) {
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
) -> Option<Ty> {
    let resolved = resolve_path(path, use_map);
    if resolved.len() < 2 {
        return None;
    }
    let (enum_path, variant) = resolved.split_at(resolved.len() - 1);
    let enum_name = enum_path.join(".");
    let info = enum_map.get(&enum_name)?;
    if info.variants.iter().any(|name| name == &variant[0]) {
        return Some(Ty::Path(enum_name, Vec::new()));
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
