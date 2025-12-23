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

pub fn type_check(module: &Module) -> Result<(), TypeError> {
    let mut functions = HashMap::new();
    for item in &module.items {
        if let Item::Function(func) = item {
            let sig = FunctionSig {
                params: func
                    .params
                    .iter()
                    .map(|p| lower_type(&p.ty))
                    .collect::<Result<_, _>>()?,
                ret: lower_type(&func.ret)?,
            };
            let name = func.name.item.clone();
            if functions.insert(name.clone(), sig).is_some() {
                return Err(TypeError::new(
                    format!("duplicate function `{name}`"),
                    func.name.span,
                ));
            }
        }
    }

    for item in &module.items {
        if let Item::Function(func) = item {
            check_function(func, &functions)?;
        }
    }

    Ok(())
}

fn check_function(func: &Function, functions: &HashMap<String, FunctionSig>) -> Result<(), TypeError> {
    let mut locals = HashMap::new();
    for param in &func.params {
        let ty = lower_type(&param.ty)?;
        locals.insert(param.name.item.clone(), ty);
    }

    let ret_ty = lower_type(&func.ret)?;
    let mut has_return = false;

    for stmt in &func.body.stmts {
        check_stmt(stmt, &ret_ty, functions, &mut locals, &mut has_return)?;
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
    has_return: &mut bool,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Let(let_stmt) => {
            let expr_ty = check_expr(&let_stmt.expr, functions, locals)?;
            let final_ty = if let Some(annot) = &let_stmt.ty {
                let annot_ty = lower_type(annot)?;
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
                check_expr(expr, functions, locals)?
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
            let cond_ty = check_expr(&if_stmt.cond, functions, locals)?;
            if cond_ty != Ty::Builtin(BuiltinType::Bool) {
                return Err(TypeError::new(
                    "if condition must be bool".to_string(),
                    if_stmt.cond.span(),
                ));
            }
            check_block(&if_stmt.then_block, ret_ty, functions, locals, has_return)?;
            if let Some(block) = &if_stmt.else_block {
                check_block(block, ret_ty, functions, locals, has_return)?;
            }
        }
        Stmt::While(while_stmt) => {
            let cond_ty = check_expr(&while_stmt.cond, functions, locals)?;
            if cond_ty != Ty::Builtin(BuiltinType::Bool) {
                return Err(TypeError::new(
                    "while condition must be bool".to_string(),
                    while_stmt.cond.span(),
                ));
            }
            check_block(&while_stmt.body, ret_ty, functions, locals, has_return)?;
        }
        Stmt::Expr(expr_stmt) => {
            check_expr(&expr_stmt.expr, functions, locals)?;
        }
    }

    Ok(())
}

fn check_block(
    block: &Block,
    ret_ty: &Ty,
    functions: &HashMap<String, FunctionSig>,
    locals: &mut HashMap<String, Ty>,
    has_return: &mut bool,
) -> Result<(), TypeError> {
    for stmt in &block.stmts {
        check_stmt(stmt, ret_ty, functions, locals, has_return)?;
    }
    Ok(())
}

fn check_expr(
    expr: &Expr,
    functions: &HashMap<String, FunctionSig>,
    locals: &HashMap<String, Ty>,
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
                if path.segments.len() != 1 {
                    return Err(TypeError::new(
                        "only local function calls are supported".to_string(),
                        path.span,
                    ));
                }
                let name = &path.segments[0].item;
                let sig = functions.get(name).ok_or_else(|| {
                    TypeError::new(format!("unknown function `{name}`"), path.span)
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
                    let arg_ty = check_expr(arg, functions, locals)?;
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
        Expr::Unary(unary) => {
            let expr_ty = check_expr(&unary.expr, functions, locals)?;
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
            let left = check_expr(&binary.left, functions, locals)?;
            let right = check_expr(&binary.right, functions, locals)?;
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
            let _ = check_expr(&match_expr.expr, functions, locals)?;
            for arm in &match_expr.arms {
                let mut arm_locals = locals.clone();
                let mut arm_return = false;
                check_block(
                    &arm.body,
                    ret_ty_placeholder(),
                    functions,
                    &mut arm_locals,
                    &mut arm_return,
                )?;
            }
            Ok(Ty::Builtin(BuiltinType::Unit))
        }
        Expr::Grouping(group) => check_expr(&group.expr, functions, locals),
    }
}

fn lower_type(ty: &Type) -> Result<Ty, TypeError> {
    let path = ty
        .path
        .segments
        .iter()
        .map(|seg| seg.item.as_str())
        .collect::<Vec<_>>();
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
    }
    let joined = path.join(".");
    let args = ty
        .args
        .iter()
        .map(lower_type)
        .collect::<Result<_, _>>()?;
    Ok(Ty::Path(joined, args))
}

fn ret_ty_placeholder() -> &'static Ty {
    static UNIT: Ty = Ty::Builtin(BuiltinType::Unit);
    &UNIT
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
            Expr::Unary(unary) => unary.span,
            Expr::Binary(binary) => binary.span,
            Expr::Match(m) => m.span,
            Expr::Grouping(group) => group.span,
        }
    }
}
