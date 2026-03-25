use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::error::TypeError;

use super::PackageSafety;

/// Safe packages cannot mention externs or raw pointer types anywhere.
pub(super) fn validate_package_safety(module: &Module, is_stdlib: bool) -> Result<(), TypeError> {
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
                if !is_stdlib {
                    if let Some(span) = type_contains_ptr_fn(func) {
                        return Err(TypeError::new(
                            "raw pointer types require `package unsafe`".to_string(),
                            span,
                        ));
                    }
                    if let Some(span) = type_contains_slice(&func.ret) {
                        return Err(TypeError::new(
                            "Slice types cannot be returned from safe modules".to_string(),
                            span,
                        ));
                    }
                }
            }
            Item::Impl(impl_block) => {
                if is_stdlib {
                    continue;
                }
                for method in &impl_block.methods {
                    if let Some(span) = type_contains_ptr_fn(method) {
                        return Err(TypeError::new(
                            "raw pointer types require `package unsafe`".to_string(),
                            span,
                        ));
                    }
                    if let Some(span) = type_contains_slice(&method.ret) {
                        return Err(TypeError::new(
                            "Slice types cannot be returned from safe modules".to_string(),
                            span,
                        ));
                    }
                }
            }
            Item::Struct(decl) => {
                if is_stdlib {
                    continue;
                }
                if let Some(span) = type_contains_ptr_struct(decl) {
                    return Err(TypeError::new(
                        "raw pointer types require `package unsafe`".to_string(),
                        span,
                    ));
                }
                if let Some(span) = type_contains_slice_struct(decl) {
                    return Err(TypeError::new(
                        "Slice types cannot appear in structs in safe modules".to_string(),
                        span,
                    ));
                }
            }
            Item::Enum(decl) => {
                if !is_stdlib {
                    if let Some(span) = type_contains_ptr_enum(decl) {
                        return Err(TypeError::new(
                            "raw pointer types require `package unsafe`".to_string(),
                            span,
                        ));
                    }
                    if let Some(span) = type_contains_slice_enum(decl) {
                        return Err(TypeError::new(
                            "Slice types cannot appear in enums in safe modules".to_string(),
                            span,
                        ));
                    }
                }
            }
            Item::Trait(_) => {}
        }
    }
    Ok(())
}

pub(super) fn validate_import_safety(
    module: &Module,
    package_map: &HashMap<String, PackageSafety>,
    stdlib_names: &HashSet<String>,
) -> Result<(), TypeError> {
    if module.package != PackageSafety::Safe {
        return Ok(());
    }
    for use_decl in &module.uses {
        let mut name = String::new();
        for (i, seg) in use_decl.path.segments.iter().enumerate() {
            if i > 0 {
                name.push('.');
            }
            name.push_str(&seg.item);
        }
        if let Some(pkg) = package_map.get(&name) {
            if *pkg == PackageSafety::Unsafe {
                if stdlib_names.contains(&name) {
                    continue;
                }
                return Err(TypeError::new(
                    format!("safe module cannot import unsafe module `{name}`"),
                    use_decl.span,
                ));
            }
        }
    }
    Ok(())
}

fn type_contains_ptr(ty: &Type) -> Option<Span> {
    match ty {
        Type::Ptr { span, .. } => Some(*span),
        Type::Ref { target, .. } => type_contains_ptr(target),
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

fn is_slice_type_path(path: &Path) -> bool {
    let Some(last) = path.segments.last() else {
        return false;
    };
    if last.item != "Slice" && last.item != "MutSlice" {
        return false;
    }
    if path.segments.len() == 1 {
        return true;
    }
    if path.segments.len() == 3 {
        return path.segments[0].item == "sys"
            && path.segments[1].item == "buffer"
            && (last.item == "Slice" || last.item == "MutSlice");
    }
    false
}

fn type_contains_slice(ty: &Type) -> Option<Span> {
    match ty {
        Type::Path { path, args, span } => {
            if is_slice_type_path(path) {
                return Some(*span);
            }
            for arg in args {
                if let Some(span) = type_contains_slice(arg) {
                    return Some(span);
                }
            }
            None
        }
        Type::Ptr { target, .. } | Type::Ref { target, .. } => type_contains_slice(target),
    }
}

fn type_contains_slice_struct(decl: &StructDecl) -> Option<Span> {
    for field in &decl.fields {
        if let Some(span) = type_contains_slice(&field.ty) {
            return Some(span);
        }
    }
    None
}

fn type_contains_slice_enum(decl: &EnumDecl) -> Option<Span> {
    for variant in &decl.variants {
        if let Some(payload) = &variant.payload {
            if let Some(span) = type_contains_slice(payload) {
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
            Stmt::LetElse(let_else) => {
                if let Some(span) = block_contains_ptr(&let_else.else_block) {
                    return Some(span);
                }
            }
            Stmt::TryLet(try_let) => {
                if let Some(ty) = &try_let.ty {
                    if let Some(span) = type_contains_ptr(ty) {
                        return Some(span);
                    }
                }
                if let Some(span) = block_contains_ptr(&try_let.else_block) {
                    return Some(span);
                }
            }
            Stmt::TryElse(try_else) => {
                if let Some(span) = block_contains_ptr(&try_else.else_block) {
                    return Some(span);
                }
            }
            Stmt::Assign(_) => {}
            Stmt::Defer(_) => {}
            Stmt::Break(_) => {}
            Stmt::Continue(_) => {}
            Stmt::If(if_stmt) => {
                if let Some(span) = block_contains_ptr(&if_stmt.then_block) {
                    return Some(span);
                }
                if let Some(span) = if_stmt.else_block.as_ref().and_then(block_contains_ptr) {
                    return Some(span);
                }
            }
            Stmt::While(while_stmt) => {
                if let Some(span) = block_contains_ptr(&while_stmt.body) {
                    return Some(span);
                }
            }
            Stmt::For(for_stmt) => {
                if let Some(span) = block_contains_ptr(&for_stmt.body) {
                    return Some(span);
                }
            }
            Stmt::ForEach(for_each) => {
                if let Some(span) = block_contains_ptr(&for_each.body) {
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
