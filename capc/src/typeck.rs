use std::collections::HashMap;

use crate::ast::*;
use crate::error::TypeError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Builtin(BuiltinType),
    Path(String, Vec<Ty>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    I32,
    I64,
    U32,
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
            if let Item::Function(func) = item {
                let sig = FunctionSig {
                    params: func
                        .params
                        .iter()
                        .map(|p| lower_type(&p.ty, &local_use, stdlib))
                        .collect::<Result<_, _>>()?,
                    ret: lower_type(&func.ret, &local_use, stdlib)?,
                };
                let key = if module_name == entry_name {
                    func.name.item.clone()
                } else {
                    format!("{module_name}.{}", func.name.item)
                };
                if functions.insert(key.clone(), sig).is_some() {
                    return Err(TypeError::new(
                        format!("duplicate function `{key}`"),
                        func.name.span,
                    ));
                }
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

pub fn type_check(module: &Module) -> Result<(), TypeError> {
    type_check_program(module, &[])
}

pub fn type_check_program(module: &Module, stdlib: &[Module]) -> Result<(), TypeError> {
    let use_map = UseMap::new(module);
    let stdlib_index = build_stdlib_index(stdlib)?;
    let modules = stdlib
        .iter()
        .chain(std::iter::once(module))
        .collect::<Vec<_>>();
    let module_name = module.name.to_string();
    let struct_map = collect_structs(&modules, &module_name, &stdlib_index)?;
    let functions = collect_functions(&modules, &module_name, &stdlib_index)?;

    for item in &module.items {
        if let Item::Function(func) = item {
            check_function(
                func,
                &functions,
                &use_map,
                &struct_map,
                &stdlib_index,
                &module_name,
            )?;
        }
    }

    Ok(())
}

fn check_function(
    func: &Function,
    functions: &HashMap<String, FunctionSig>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
) -> Result<(), TypeError> {
    let mut locals = HashMap::new();
    for param in &func.params {
        let ty = lower_type(&param.ty, use_map, stdlib)?;
        locals.insert(param.name.item.clone(), ty);
    }

    let ret_ty = lower_type(&func.ret, use_map, stdlib)?;
    let mut has_return = false;

    for stmt in &func.body.stmts {
        check_stmt(
            stmt,
            &ret_ty,
            functions,
            &mut locals,
            use_map,
            struct_map,
            stdlib,
            module_name,
            &mut has_return,
        )?;
    }

    if ret_ty != Ty::Builtin(BuiltinType::Unit) && !has_return {
        return Err(TypeError::new(
            "missing return statement".to_string(),
            func.body.span,
        ));
    }

    Ok(())
}

fn check_stmt(
    stmt: &Stmt,
    ret_ty: &Ty,
    functions: &HashMap<String, FunctionSig>,
    locals: &mut HashMap<String, Ty>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
    has_return: &mut bool,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Let(let_stmt) => {
            let expr_ty = check_expr(
                &let_stmt.expr,
                functions,
                locals,
                use_map,
                struct_map,
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
            locals.insert(let_stmt.name.item.clone(), final_ty);
        }
        Stmt::Return(ret_stmt) => {
            let expr_ty = if let Some(expr) = &ret_stmt.expr {
                check_expr(
                    expr,
                    functions,
                    locals,
                    use_map,
                    struct_map,
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
            *has_return = true;
        }
        Stmt::If(if_stmt) => {
            let cond_ty = check_expr(
                &if_stmt.cond,
                functions,
                locals,
                use_map,
                struct_map,
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
                locals,
                use_map,
                struct_map,
                stdlib,
                module_name,
                has_return,
            )?;
            if let Some(block) = &if_stmt.else_block {
                check_block(
                    block,
                    ret_ty,
                    functions,
                    locals,
                    use_map,
                    struct_map,
                    stdlib,
                    module_name,
                    has_return,
                )?;
            }
        }
        Stmt::While(while_stmt) => {
            let cond_ty = check_expr(
                &while_stmt.cond,
                functions,
                locals,
                use_map,
                struct_map,
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
                locals,
                use_map,
                struct_map,
                stdlib,
                module_name,
                has_return,
            )?;
        }
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                let mut any_return = false;
                let _ = check_match_expr(
                    match_expr,
                    functions,
                    locals,
                    use_map,
                    struct_map,
                    stdlib,
                    ret_ty,
                    module_name,
                    Some(&mut any_return),
                )?;
                if any_return {
                    *has_return = true;
                }
            } else {
                check_expr(
                    &expr_stmt.expr,
                    functions,
                    locals,
                    use_map,
                    struct_map,
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
    locals: &mut HashMap<String, Ty>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    stdlib: &StdlibIndex,
    module_name: &str,
    has_return: &mut bool,
) -> Result<(), TypeError> {
    for stmt in &block.stmts {
        check_stmt(
            stmt,
            ret_ty,
            functions,
            locals,
            use_map,
            struct_map,
            stdlib,
            module_name,
            has_return,
        )?;
    }
    Ok(())
}

fn check_expr(
    expr: &Expr,
    functions: &HashMap<String, FunctionSig>,
    locals: &HashMap<String, Ty>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
) -> Result<Ty, TypeError> {
    match expr {
        Expr::Literal(lit) => match &lit.value {
            Literal::Int(_) => Ok(Ty::Builtin(BuiltinType::I32)),
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
            Err(TypeError::new(
                format!("unknown value `{path}`"),
                path.span,
            ))
        }
        Expr::Call(call) => match &*call.callee {
            Expr::Path(path) => {
                let resolved = resolve_path(path, use_map);
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
            _ => Err(TypeError::new(
                "call target must be a function path".to_string(),
                call.callee.span(),
            )),
        },
        Expr::StructLiteral(lit) => check_struct_literal(
            lit,
            functions,
            locals,
            use_map,
            struct_map,
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
                BinaryOp::Eq
                | BinaryOp::Neq
                | BinaryOp::Lt
                | BinaryOp::Lte
                | BinaryOp::Gt
                | BinaryOp::Gte => {
                    if left == right {
                        Ok(Ty::Builtin(BuiltinType::Bool))
                    } else {
                        Err(TypeError::new(
                            "comparison expects matching operand types".to_string(),
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
        Expr::Match(match_expr) => {
            check_match_expr(
                match_expr,
                functions,
                locals,
                use_map,
                struct_map,
                stdlib,
                ret_ty,
                module_name,
                None,
            )
        }
        Expr::Grouping(group) => check_expr(
            &group.expr,
            functions,
            locals,
            use_map,
            struct_map,
            stdlib,
            ret_ty,
            module_name,
        ),
    }
}

fn check_match_expr(
    match_expr: &MatchExpr,
    functions: &HashMap<String, FunctionSig>,
    locals: &HashMap<String, Ty>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
    stdlib: &StdlibIndex,
    ret_ty: &Ty,
    module_name: &str,
    any_return: Option<&mut bool>,
) -> Result<Ty, TypeError> {
    let match_ty = check_expr(
        &match_expr.expr,
        functions,
        locals,
        use_map,
        struct_map,
        stdlib,
        ret_ty,
        module_name,
    )?;
    let mut saw_return = false;
    for arm in &match_expr.arms {
        let mut arm_locals = locals.clone();
        bind_pattern(&arm.pattern, &match_ty, &mut arm_locals)?;
        let mut arm_return = false;
        check_block(
            &arm.body,
            ret_ty,
            functions,
            &mut arm_locals,
            use_map,
            struct_map,
            stdlib,
            module_name,
            &mut arm_return,
        )?;
        if arm_return {
            saw_return = true;
        }
    }
    if let Some(flag) = any_return {
        *flag = saw_return;
    }
    Ok(Ty::Builtin(BuiltinType::Unit))
}

fn check_struct_literal(
    lit: &StructLiteralExpr,
    functions: &HashMap<String, FunctionSig>,
    locals: &HashMap<String, Ty>,
    use_map: &UseMap,
    struct_map: &HashMap<String, StructInfo>,
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
    locals: &mut HashMap<String, Ty>,
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
                    locals.insert(binding.item.clone(), ty);
                    return Ok(());
                }
            }
            Err(TypeError::new(
                "pattern binding requires a Result match".to_string(),
                path.span,
            ))
        }
        Pattern::Binding(ident) => {
            locals.insert(ident.item.clone(), match_ty.clone());
            Ok(())
        }
        Pattern::Path(_) | Pattern::Wildcard(_) => Ok(()),
    }
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
    let resolved = resolve_path(&ty.path, use_map);
    let path = resolved.iter().map(|seg| seg.as_str()).collect::<Vec<_>>();
    if path.len() == 1 {
        let builtin = match path[0] {
            "i32" => Some(BuiltinType::I32),
            "i64" => Some(BuiltinType::I64),
            "u32" => Some(BuiltinType::U32),
            "bool" => Some(BuiltinType::Bool),
            "string" => Some(BuiltinType::String),
            "unit" => Some(BuiltinType::Unit),
            _ => None,
        };
        if let Some(builtin) = builtin {
            return Ok(Ty::Builtin(builtin));
        }
        let alias = resolve_type_name(&ty.path, use_map, stdlib);
        if alias != resolved.join(".") {
            return Ok(Ty::Path(alias, Vec::new()));
        }
    }
    let joined = path.join(".");
    let args = ty
        .args
        .iter()
        .map(|arg| lower_type(arg, use_map, stdlib))
        .collect::<Result<_, _>>()?;
    Ok(Ty::Path(joined, args))
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
            Expr::StructLiteral(lit) => lit.span,
            Expr::Unary(unary) => unary.span,
            Expr::Binary(binary) => binary.span,
            Expr::Match(m) => m.span,
            Expr::Grouping(group) => group.span,
        }
    }
}
