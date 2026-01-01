use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::error::TypeError;
use crate::abi::AbiType;
use crate::hir::{
    HirAssignStmt, HirBinary, HirBlock, HirBreakStmt, HirCall, HirContinueStmt, HirEnum,
    HirEnumVariant, HirEnumVariantExpr, HirExpr, HirExprStmt, HirExternFunction, HirField,
    HirFieldAccess, HirForStmt, HirFunction, HirIfStmt, HirLetStmt, HirLiteral, HirLocal, HirMatch,
    HirMatchArm, HirParam, HirPattern, HirReturnStmt, HirStmt, HirStruct, HirStructLiteral,
    HirStructLiteralField, HirTrap, HirType, HirUnary, HirWhileStmt, IntrinsicId, LocalId,
    ResolvedCallee,
};

use super::{
    build_type_params, check, function_key, lower_type, resolve_enum_variant, resolve_method_target,
    resolve_type_name, BuiltinType, EnumInfo, FunctionSig, FunctionTypeTables, SpanExt,
    StdlibIndex, StructInfo, Ty, TypeTable, UseMap,
};

/// Context for HIR lowering (uses the type checker as source of truth).
struct LoweringCtx<'a> {
    functions: &'a HashMap<String, FunctionSig>,
    structs: &'a HashMap<String, StructInfo>,
    enums: &'a HashMap<String, EnumInfo>,
    use_map: &'a UseMap,
    stdlib: &'a StdlibIndex,
    module_name: &'a str,
    type_tables: Option<&'a FunctionTypeTables>,
    type_table: Option<&'a TypeTable>,
    allow_type_fallback: bool,
    /// Maps variable names to their LocalId
    local_map: HashMap<String, LocalId>,
    /// Maps variable names to their types (needed for type checking during lowering)
    local_types: HashMap<String, Ty>,
    local_counter: usize,
    type_params: HashSet<String>,
}

impl<'a> LoweringCtx<'a> {
    fn new(
        functions: &'a HashMap<String, FunctionSig>,
        structs: &'a HashMap<String, StructInfo>,
        enums: &'a HashMap<String, EnumInfo>,
        use_map: &'a UseMap,
        stdlib: &'a StdlibIndex,
        module_name: &'a str,
        type_tables: Option<&'a FunctionTypeTables>,
        allow_type_fallback: bool,
    ) -> Self {
        Self {
            functions,
            structs,
            enums,
            use_map,
            stdlib,
            module_name,
            type_tables,
            type_table: None,
            allow_type_fallback,
            local_map: HashMap::new(),
            local_types: HashMap::new(),
            local_counter: 0,
            type_params: HashSet::new(),
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

    /// Push a new scope for name bindings (shadowing is not yet modeled here).
    fn push_scope(&mut self) {
    }

    /// Pop the most recent scope (placeholder for future scope stacks).
    fn pop_scope(&mut self) {
    }
}

/// Lower a fully type-checked module into HIR.
pub(super) fn lower_module(
    module: &Module,
    functions: &HashMap<String, FunctionSig>,
    structs: &HashMap<String, StructInfo>,
    enums: &HashMap<String, EnumInfo>,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    type_tables: Option<&FunctionTypeTables>,
    allow_type_fallback: bool,
) -> Result<crate::hir::HirModule, TypeError> {
    let module_name = module.name.to_string();
    let mut ctx = LoweringCtx::new(
        functions,
        structs,
        enums,
        use_map,
        stdlib,
        &module_name,
        type_tables,
        allow_type_fallback,
    );

    let mut hir_functions = Vec::new();
    let mut hir_extern_functions = Vec::new();
    for item in &module.items {
        match item {
            Item::Function(func) => {
                let hir_func = lower_function(func, &mut ctx)?;
                hir_functions.push(hir_func);
            }
            Item::Impl(impl_block) => {
                let methods = super::desugar_impl_methods(
                    impl_block,
                    &module_name,
                    use_map,
                    stdlib,
                    structs,
                    enums,
                )?;
                for method in methods {
                    let hir_func = lower_function(&method, &mut ctx)?;
                    hir_functions.push(hir_func);
                }
            }
            Item::ExternFunction(func) => {
                let type_params = build_type_params(&func.type_params)?;
                let params: Result<Vec<HirParam>, TypeError> = func
                    .params
                    .iter()
                    .map(|param| {
                        let Some(ty) = &param.ty else {
                            return Err(TypeError::new(
                                format!("parameter `{}` requires a type annotation", param.name.item),
                                param.name.span,
                            ));
                        };
                        let lowered = lower_type(ty, use_map, stdlib, &type_params)?;
                        Ok(HirParam {
                            local_id: LocalId(0),
                            ty: hir_type_for(lowered, &ctx, ty.span())?,
                        })
                    })
                    .collect();
                hir_extern_functions.push(HirExternFunction {
                    name: func.name.item.clone(),
                    type_params: func.type_params.iter().map(|p| p.item.clone()).collect(),
                    params: params?,
                    ret_ty: {
                        let lowered = lower_type(&func.ret, use_map, stdlib, &type_params)?;
                        hir_type_for(lowered, &ctx, func.ret.span())?
                    },
                });
            }
            _ => {}
        }
    }

    let mut hir_structs = Vec::new();
    for item in &module.items {
        if let Item::Struct(decl) = item {
            let type_params = build_type_params(&decl.type_params)?;
            let fields: Result<Vec<HirField>, TypeError> = decl
                .fields
                .iter()
                .map(|f| {
                    let lowered = lower_type(&f.ty, use_map, stdlib, &type_params)?;
                    Ok(HirField {
                        name: f.name.item.clone(),
                        ty: hir_type_for(lowered, &ctx, f.ty.span())?,
                    })
                })
                .collect();
            hir_structs.push(HirStruct {
                name: decl.name.item.clone(),
                type_params: decl.type_params.iter().map(|p| p.item.clone()).collect(),
                fields: fields?,
                is_opaque: decl.is_opaque || decl.is_capability,
            });
        }
    }

    let mut hir_enums = Vec::new();
    for item in &module.items {
        if let Item::Enum(decl) = item {
            let type_params = build_type_params(&decl.type_params)?;
            let variants: Result<Vec<HirEnumVariant>, TypeError> = decl
                .variants
                .iter()
                .map(|v| {
                    let payload = v
                        .payload
                        .as_ref()
                        .map(|ty| {
                            let lowered = lower_type(ty, use_map, stdlib, &type_params)?;
                            hir_type_for(lowered, &ctx, ty.span())
                        })
                        .transpose()?;
                    Ok(HirEnumVariant {
                        name: v.name.item.clone(),
                        payload,
                    })
                })
                .collect();
            hir_enums.push(HirEnum {
                name: decl.name.item.clone(),
                type_params: decl.type_params.iter().map(|p| p.item.clone()).collect(),
                variants: variants?,
            });
        }
    }

    Ok(crate::hir::HirModule {
        name: module_name,
        functions: hir_functions,
        extern_functions: hir_extern_functions,
        structs: hir_structs,
        enums: hir_enums,
    })
}

/// Lower a type-checked function into HIR, assigning LocalIds.
fn lower_function(func: &Function, ctx: &mut LoweringCtx) -> Result<HirFunction, TypeError> {
    ctx.local_counter = 0;
    ctx.local_map.clear();
    ctx.local_types.clear();
    ctx.type_table = ctx
        .type_tables
        .and_then(|tables| tables.get(&function_key(ctx.module_name, &func.name.item)));
    let type_params = build_type_params(&func.type_params)?;
    ctx.type_params = type_params.clone();

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
            let ty = lower_type(ty, ctx.use_map, ctx.stdlib, &type_params)?;
            let hir_ty = hir_type_for(ty.clone(), ctx, p.ty.as_ref().unwrap().span())?;
            let local_id = ctx.fresh_local(p.name.item.clone(), ty);
            Ok(HirParam {
                local_id,
                ty: hir_ty,
            })
        })
        .collect();
    let params = params?;

    let ret_ty = lower_type(&func.ret, ctx.use_map, ctx.stdlib, &type_params)?;
    let hir_ret_ty = hir_type_for(ret_ty.clone(), ctx, func.ret.span())?;
    let body = lower_block(&func.body, ctx, &ret_ty)?;

    Ok(HirFunction {
        name: func.name.item.clone(),
        type_params: func.type_params.iter().map(|p| p.item.clone()).collect(),
        params,
        ret_ty: hir_ret_ty,
        body,
    })
}

/// Lower a block into HIR.
fn lower_block(block: &Block, ctx: &mut LoweringCtx, ret_ty: &Ty) -> Result<HirBlock, TypeError> {
    ctx.push_scope();
    let stmts: Result<Vec<HirStmt>, TypeError> = block
        .stmts
        .iter()
        .map(|stmt| lower_stmt(stmt, ctx, ret_ty))
        .collect();
    ctx.pop_scope();

    Ok(HirBlock { stmts: stmts? })
}

/// Lower a statement into HIR.
fn lower_stmt(stmt: &Stmt, ctx: &mut LoweringCtx, ret_ty: &Ty) -> Result<HirStmt, TypeError> {
    match stmt {
        Stmt::Let(let_stmt) => {
            let expr = lower_expr(&let_stmt.expr, ctx, ret_ty)?;
            let ty = expr.ty().clone();
            let local_id = ctx.fresh_local(let_stmt.name.item.clone(), ty.ty.clone());

            Ok(HirStmt::Let(HirLetStmt {
                local_id,
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
        Stmt::Break(break_stmt) => {
            Ok(HirStmt::Break(HirBreakStmt {
                span: break_stmt.span,
            }))
        }
        Stmt::Continue(continue_stmt) => {
            Ok(HirStmt::Continue(HirContinueStmt {
                span: continue_stmt.span,
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
        Stmt::For(for_stmt) => {
            let start = lower_expr(&for_stmt.start, ctx, ret_ty)?;
            let end = lower_expr(&for_stmt.end, ctx, ret_ty)?;

            // Create a fresh local for the loop variable
            let var_id = ctx.fresh_local(
                for_stmt.var.item.clone(),
                crate::typeck::Ty::Builtin(crate::typeck::BuiltinType::I32),
            );

            let body = lower_block(&for_stmt.body, ctx, ret_ty)?;

            Ok(HirStmt::For(HirForStmt {
                var_id,
                start,
                end,
                body,
                span: for_stmt.span,
            }))
        }
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                match lower_expr(&expr_stmt.expr, ctx, ret_ty) {
                    Ok(expr) => {
                        return Ok(HirStmt::Expr(HirExprStmt {
                            expr,
                            span: expr_stmt.span,
                        }));
                    }
                    Err(_) => {
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

/// Helper to get the type of an AST expression using the existing typechecker.
/// This ensures we have a single source of truth for types.
fn type_of_ast_expr(
    expr: &Expr,
    ctx: &LoweringCtx,
    ret_ty: &Ty,
) -> Result<Ty, TypeError> {
    if let Some(table) = ctx.type_table {
        if let Some(ty) = table.get(expr.span()) {
            return Ok(ty.clone());
        }
        return Err(TypeError::new(
            "internal error: missing type information during lowering".to_string(),
            expr.span(),
        ));
    }
    if !ctx.allow_type_fallback {
        return Err(TypeError::new(
            "internal error: lowering requires typed expression data".to_string(),
            expr.span(),
        ));
    }
    let mut scopes = super::Scopes::from_flat_map(ctx.local_types.clone());
    let mut recorder = super::check::TypeRecorder::new(None);
    let type_params = HashSet::new();
    check::check_expr(
        expr,
        ctx.functions,
        &mut scopes,
        super::UseMode::Read,
        &mut recorder,
        ctx.use_map,
        ctx.structs,
        ctx.enums,
        ctx.stdlib,
        ret_ty,
        ctx.module_name,
        &type_params,
    )
}

fn lower_call_type_args(args: &[Type], ctx: &LoweringCtx) -> Result<Vec<Ty>, TypeError> {
    args.iter()
        .map(|arg| lower_type(arg, ctx.use_map, ctx.stdlib, &ctx.type_params))
        .collect::<Result<Vec<_>, _>>()
}

fn abi_type_for(ty: &Ty, ctx: &LoweringCtx, span: Span) -> Result<AbiType, TypeError> {
    use super::BuiltinType;
    match ty {
        Ty::Builtin(b) => match b {
            BuiltinType::I32 => Ok(AbiType::I32),
            BuiltinType::I64 => Err(TypeError::new(
                "i64 is not supported by the current codegen backend".to_string(),
                span,
            )),
            BuiltinType::U32 => Ok(AbiType::U32),
            BuiltinType::U8 => Ok(AbiType::U8),
            BuiltinType::Bool => Ok(AbiType::Bool),
            BuiltinType::String => Ok(AbiType::String),
            BuiltinType::Unit => Ok(AbiType::Unit),
        },
        Ty::Ptr(_) => Ok(AbiType::Ptr),
        Ty::Ref(inner) => abi_type_for(inner, ctx, span),
        Ty::Param(_) => Ok(AbiType::Ptr),
        Ty::Path(name, args) => {
            if name == "Result" && args.len() == 2 {
                let ok = abi_type_for(&args[0], ctx, span)?;
                let err = abi_type_for(&args[1], ctx, span)?;
                return Ok(AbiType::Result(Box::new(ok), Box::new(err)));
            }
            let qualified = if name.contains('.') {
                None
            } else {
                Some(format!("{}.{}", ctx.module_name, name))
            };
            if let Some(info) = ctx
                .structs
                .get(name)
                .or_else(|| qualified.as_ref().and_then(|q| ctx.structs.get(q)))
            {
                return Ok(if info.is_opaque {
                    AbiType::Handle
                } else {
                    AbiType::Ptr
                });
            }
            if ctx
                .enums
                .contains_key(name)
                || qualified
                    .as_ref()
                    .is_some_and(|q| ctx.enums.contains_key(q))
            {
                return Ok(AbiType::I32);
            }
            Err(TypeError::new(
                format!("unknown type `{name}` for ABI lowering"),
                span,
            ))
        }
    }
}

fn hir_type_for(ty: Ty, ctx: &LoweringCtx, span: Span) -> Result<HirType, TypeError> {
    let abi = abi_type_for(&ty, ctx, span)?;
    Ok(HirType { ty, abi })
}

/// Lower an expression into HIR with resolved callee information.
fn lower_expr(expr: &Expr, ctx: &mut LoweringCtx, ret_ty: &Ty) -> Result<HirExpr, TypeError> {
    let ty = type_of_ast_expr(expr, ctx, ret_ty)?;
    let hir_ty = hir_type_for(ty.clone(), ctx, expr.span())?;

    match expr {
        Expr::Literal(lit) => Ok(HirExpr::Literal(HirLiteral {
            value: lit.value.clone(),
            ty: hir_ty,
            span: lit.span,
        })),
        Expr::Grouping(grouping) => lower_expr(&grouping.expr, ctx, ret_ty),
        Expr::Path(path) => {
            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if ctx.local_types.contains_key(name) {
                    let local_id = ctx.get_local(name).unwrap();
                    return Ok(HirExpr::Local(HirLocal {
                        local_id,
                        ty: hir_ty,
                        span: path.span,
                    }));
                }
            }

            let variant_name = path.segments.last()
                .map(|s| s.item.clone())
                .unwrap_or_else(|| String::from("unknown"));

            Ok(HirExpr::EnumVariant(HirEnumVariantExpr {
                enum_ty: hir_ty,
                variant_name,
                payload: None,
                span: path.span,
            }))
        }
        Expr::Call(call) => {
            let path = call.callee.to_path().ok_or_else(|| {
                TypeError::new(
                    "call target must be a function path".to_string(),
                    call.callee.span(),
                )
            })?;

            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if name == "drop" {
                    let args: Result<Vec<HirExpr>, TypeError> = call
                        .args
                        .iter()
                        .map(|arg| lower_expr(arg, ctx, ret_ty))
                        .collect();
                    return Ok(HirExpr::Call(HirCall {
                        callee: ResolvedCallee::Intrinsic(IntrinsicId::Drop),
                        type_args: Vec::new(),
                        args: args?,
                        ret_ty: hir_ty,
                        span: call.span,
                    }));
                }
                if name == "Ok" || name == "Err" {
                    let arg = lower_expr(&call.args[0], ctx, ret_ty)?;
                    let variant_name = name.clone();

                    return Ok(HirExpr::EnumVariant(HirEnumVariantExpr {
                        enum_ty: hir_ty.clone(),
                        variant_name,
                        payload: Some(Box::new(arg)),
                        span: call.span,
                    }));
                }
            }

            let mut resolved = super::resolve_path(&path, ctx.use_map);

            if resolved.len() == 1 {
                resolved.insert(0, ctx.module_name.to_string());
            }

            let key = resolved.join(".");

            let args: Result<Vec<HirExpr>, TypeError> = call
                .args
                .iter()
                .map(|arg| lower_expr(arg, ctx, ret_ty))
                .collect();

            let module = resolved[..resolved.len() - 1].join(".");
            let name = resolved.last().unwrap().clone();
            let symbol = format!("capable_{}", key.replace('.', "_"));

            Ok(HirExpr::Call(HirCall {
                callee: ResolvedCallee::Function {
                    module,
                    name,
                    symbol,
                },
                type_args: lower_call_type_args(&call.type_args, ctx)?,
                args: args?,
                ret_ty: hir_ty,
                span: call.span,
            }))
        }
        Expr::MethodCall(method_call) => {
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
                let resolved = super::resolve_path(path, ctx.use_map);
                let key = resolved.join(".");
                ctx.functions.contains_key(&key)
            } else {
                false
            };

            if !base_is_local && is_function {
                let path = path_call.expect("path exists for function call");
                let resolved = super::resolve_path(&path, ctx.use_map);
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
                    type_args: lower_call_type_args(&method_call.type_args, ctx)?,
                    args: args?,
                    ret_ty: hir_ty,
                    span: method_call.span,
                }));
            }

            let receiver = lower_expr(&method_call.receiver, ctx, ret_ty)?;
            let receiver_ty = type_of_ast_expr(&method_call.receiver, ctx, ret_ty)?;
            if let Ty::Path(name, args) = &receiver_ty {
                if name == "Result" && args.len() == 2 {
                    match method_call.method.item.as_str() {
                        "unwrap_or" => {
                            let default_expr = lower_expr(&method_call.args[0], ctx, ret_ty)?;
                            let ok_name = format!("__unwrap_ok_{}", ctx.local_counter);
                            let ok_local_id = ctx.fresh_local(ok_name.clone(), args[0].clone());
                            let ok_pattern = HirPattern::Variant {
                                variant_name: "Ok".to_string(),
                                binding: Some(ok_local_id),
                            };
                            let err_pattern = HirPattern::Variant {
                                variant_name: "Err".to_string(),
                                binding: None,
                            };
                            let ok_expr = HirExpr::Local(HirLocal {
                                local_id: ok_local_id,
                                ty: hir_type_for(args[0].clone(), ctx, method_call.span)?,
                                span: method_call.span,
                            });
                            let ok_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: ok_expr,
                                    span: method_call.span,
                                })],
                            };
                            let err_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: default_expr,
                                    span: method_call.span,
                                })],
                            };
                            return Ok(HirExpr::Match(HirMatch {
                                expr: Box::new(receiver),
                                arms: vec![
                                    HirMatchArm {
                                        pattern: ok_pattern,
                                        body: ok_block,
                                    },
                                    HirMatchArm {
                                        pattern: err_pattern,
                                        body: err_block,
                                    },
                                ],
                                result_ty: hir_ty,
                                span: method_call.span,
                            }));
                        }
                        "unwrap_err_or" => {
                            let default_expr = lower_expr(&method_call.args[0], ctx, ret_ty)?;
                            let err_name = format!("__unwrap_err_{}", ctx.local_counter);
                            let err_local_id = ctx.fresh_local(err_name.clone(), args[1].clone());
                            let ok_pattern = HirPattern::Variant {
                                variant_name: "Ok".to_string(),
                                binding: None,
                            };
                            let err_pattern = HirPattern::Variant {
                                variant_name: "Err".to_string(),
                                binding: Some(err_local_id),
                            };
                            let err_expr = HirExpr::Local(HirLocal {
                                local_id: err_local_id,
                                ty: hir_type_for(args[1].clone(), ctx, method_call.span)?,
                                span: method_call.span,
                            });
                            let ok_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: default_expr,
                                    span: method_call.span,
                                })],
                            };
                            let err_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: err_expr,
                                    span: method_call.span,
                                })],
                            };
                            return Ok(HirExpr::Match(HirMatch {
                                expr: Box::new(receiver),
                                arms: vec![
                                    HirMatchArm {
                                        pattern: ok_pattern,
                                        body: ok_block,
                                    },
                                    HirMatchArm {
                                        pattern: err_pattern,
                                        body: err_block,
                                    },
                                ],
                                result_ty: hir_ty,
                                span: method_call.span,
                            }));
                        }
                        "is_ok" => {
                            let bool_ty = HirType {
                                ty: Ty::Builtin(BuiltinType::Bool),
                                abi: AbiType::Bool,
                            };
                            let true_lit = HirExpr::Literal(HirLiteral {
                                value: Literal::Bool(true),
                                ty: bool_ty.clone(),
                                span: method_call.span,
                            });
                            let false_lit = HirExpr::Literal(HirLiteral {
                                value: Literal::Bool(false),
                                ty: bool_ty.clone(),
                                span: method_call.span,
                            });
                            let ok_pattern = HirPattern::Variant {
                                variant_name: "Ok".to_string(),
                                binding: None,
                            };
                            let err_pattern = HirPattern::Variant {
                                variant_name: "Err".to_string(),
                                binding: None,
                            };
                            let ok_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: true_lit,
                                    span: method_call.span,
                                })],
                            };
                            let err_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: false_lit,
                                    span: method_call.span,
                                })],
                            };
                            return Ok(HirExpr::Match(HirMatch {
                                expr: Box::new(receiver),
                                arms: vec![
                                    HirMatchArm {
                                        pattern: ok_pattern,
                                        body: ok_block,
                                    },
                                    HirMatchArm {
                                        pattern: err_pattern,
                                        body: err_block,
                                    },
                                ],
                                result_ty: bool_ty,
                                span: method_call.span,
                            }));
                        }
                        "is_err" => {
                            let bool_ty = HirType {
                                ty: Ty::Builtin(BuiltinType::Bool),
                                abi: AbiType::Bool,
                            };
                            let true_lit = HirExpr::Literal(HirLiteral {
                                value: Literal::Bool(true),
                                ty: bool_ty.clone(),
                                span: method_call.span,
                            });
                            let false_lit = HirExpr::Literal(HirLiteral {
                                value: Literal::Bool(false),
                                ty: bool_ty.clone(),
                                span: method_call.span,
                            });
                            let ok_pattern = HirPattern::Variant {
                                variant_name: "Ok".to_string(),
                                binding: None,
                            };
                            let err_pattern = HirPattern::Variant {
                                variant_name: "Err".to_string(),
                                binding: None,
                            };
                            let ok_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: false_lit,
                                    span: method_call.span,
                                })],
                            };
                            let err_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: true_lit,
                                    span: method_call.span,
                                })],
                            };
                            return Ok(HirExpr::Match(HirMatch {
                                expr: Box::new(receiver),
                                arms: vec![
                                    HirMatchArm {
                                        pattern: ok_pattern,
                                        body: ok_block,
                                    },
                                    HirMatchArm {
                                        pattern: err_pattern,
                                        body: err_block,
                                    },
                                ],
                                result_ty: bool_ty,
                                span: method_call.span,
                            }));
                        }
                        "ok" => {
                            let ok_name = format!("__ok_{}", ctx.local_counter);
                            let ok_local_id = ctx.fresh_local(ok_name.clone(), args[0].clone());
                            let ok_pattern = HirPattern::Variant {
                                variant_name: "Ok".to_string(),
                                binding: Some(ok_local_id),
                            };
                            let err_pattern = HirPattern::Variant {
                                variant_name: "Err".to_string(),
                                binding: None,
                            };
                            let ok_expr = HirExpr::Local(HirLocal {
                                local_id: ok_local_id,
                                ty: hir_type_for(args[0].clone(), ctx, method_call.span)?,
                                span: method_call.span,
                            });
                            let trap_expr = HirExpr::Trap(HirTrap {
                                ty: hir_type_for(args[0].clone(), ctx, method_call.span)?,
                                span: method_call.span,
                            });
                            let ok_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: ok_expr,
                                    span: method_call.span,
                                })],
                            };
                            let err_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: trap_expr,
                                    span: method_call.span,
                                })],
                            };
                            return Ok(HirExpr::Match(HirMatch {
                                expr: Box::new(receiver),
                                arms: vec![
                                    HirMatchArm {
                                        pattern: ok_pattern,
                                        body: ok_block,
                                    },
                                    HirMatchArm {
                                        pattern: err_pattern,
                                        body: err_block,
                                    },
                                ],
                                result_ty: hir_ty,
                                span: method_call.span,
                            }));
                        }
                        "err" => {
                            let err_name = format!("__err_{}", ctx.local_counter);
                            let err_local_id = ctx.fresh_local(err_name.clone(), args[1].clone());
                            let ok_pattern = HirPattern::Variant {
                                variant_name: "Ok".to_string(),
                                binding: None,
                            };
                            let err_pattern = HirPattern::Variant {
                                variant_name: "Err".to_string(),
                                binding: Some(err_local_id),
                            };
                            let err_expr = HirExpr::Local(HirLocal {
                                local_id: err_local_id,
                                ty: hir_type_for(args[1].clone(), ctx, method_call.span)?,
                                span: method_call.span,
                            });
                            let trap_expr = HirExpr::Trap(HirTrap {
                                ty: hir_type_for(args[1].clone(), ctx, method_call.span)?,
                                span: method_call.span,
                            });
                            let ok_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: trap_expr,
                                    span: method_call.span,
                                })],
                            };
                            let err_block = HirBlock {
                                stmts: vec![HirStmt::Expr(HirExprStmt {
                                    expr: err_expr,
                                    span: method_call.span,
                                })],
                            };
                            return Ok(HirExpr::Match(HirMatch {
                                expr: Box::new(receiver),
                                arms: vec![
                                    HirMatchArm {
                                        pattern: ok_pattern,
                                        body: ok_block,
                                    },
                                    HirMatchArm {
                                        pattern: err_pattern,
                                        body: err_block,
                                    },
                                ],
                                result_ty: hir_ty,
                                span: method_call.span,
                            }));
                        }
                        _ => {}
                    }
                }
            }
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
                type_args: lower_call_type_args(&method_call.type_args, ctx)?,
                args,
                ret_ty: hir_ty,
                span: method_call.span,
            }))
        }
        Expr::Binary(bin) => {
            let left = lower_expr(&bin.left, ctx, ret_ty)?;
            let right = lower_expr(&bin.right, ctx, ret_ty)?;

            Ok(HirExpr::Binary(HirBinary {
                left: Box::new(left),
                op: bin.op.clone(),
                right: Box::new(right),
                ty: hir_ty,
                span: bin.span,
            }))
        }
        Expr::Unary(un) => {
            let operand = lower_expr(&un.expr, ctx, ret_ty)?;

            Ok(HirExpr::Unary(HirUnary {
                op: un.op.clone(),
                expr: Box::new(operand),
                ty: hir_ty,
                span: un.span,
            }))
        }
        Expr::FieldAccess(field_access) => {
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
                true
            };

            if !base_is_local {
                if let Some(path) = Expr::FieldAccess(field_access.clone()).to_path() {
                    if let Some(enum_ty) =
                        resolve_enum_variant(&path, ctx.use_map, ctx.enums, ctx.module_name)
                    {
                        let variant_name = path.segments.last()
                            .map(|s| s.item.clone())
                            .unwrap_or_else(|| String::from("unknown"));

                        return Ok(HirExpr::EnumVariant(HirEnumVariantExpr {
                            enum_ty: hir_type_for(enum_ty, ctx, path.span)?,
                            variant_name,
                            payload: None,
                            span: field_access.span,
                        }));
                    }
                }
            }

            let object = lower_expr(&field_access.object, ctx, ret_ty)?;

            Ok(HirExpr::FieldAccess(HirFieldAccess {
                object: Box::new(object),
                field_name: field_access.field.item.clone(),
                field_ty: hir_ty,
                span: field_access.span,
            }))
        }
        Expr::StructLiteral(lit) => {
            let struct_ty = type_of_ast_expr(expr, ctx, ret_ty)?;
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

            if info.is_opaque && info.module != ctx.module_name {
                return Err(TypeError::new(
                    format!(
                        "cannot construct opaque/capability type `{}` outside module `{}`",
                        key, info.module
                    ),
                    lit.span,
                ));
            }

            let mut hir_fields = Vec::new();
            for field in &lit.fields {
                let hir_expr = lower_expr(&field.expr, ctx, ret_ty)?;
                hir_fields.push(HirStructLiteralField {
                    name: field.name.item.clone(),
                    expr: hir_expr,
                });
            }

            let hir_struct_ty = hir_type_for(struct_ty, ctx, lit.span)?;

            Ok(HirExpr::StructLiteral(HirStructLiteral {
                struct_ty: hir_struct_ty,
                fields: hir_fields,
                span: lit.span,
            }))
        }
        Expr::Match(match_expr) => {
            let scrutinee = lower_expr(&match_expr.expr, ctx, ret_ty)?;
            let scrutinee_ty = scrutinee.ty().clone();

            let mut hir_arms = Vec::new();
            for arm in &match_expr.arms {
                ctx.push_scope();

                let hir_pattern = lower_pattern(&arm.pattern, &scrutinee_ty, ctx)?;

                let hir_body = lower_block(&arm.body, ctx, ret_ty)?;

                ctx.pop_scope();

                hir_arms.push(HirMatchArm {
                    pattern: hir_pattern,
                    body: hir_body,
                });
            }

            Ok(HirExpr::Match(HirMatch {
                expr: Box::new(scrutinee),
                arms: hir_arms,
                result_ty: hir_ty,
                span: match_expr.span,
            }))
        }
        Expr::Try(try_expr) => {
            let scrutinee = lower_expr(&try_expr.expr, ctx, ret_ty)?;
            let scrutinee_ty = scrutinee.ty().clone();
            let ok_ty = match &scrutinee_ty.ty {
                Ty::Path(name, args) if name == "Result" && args.len() == 2 => args[0].clone(),
                _ => {
                    return Err(TypeError::new(
                        "the `?` operator expects a Result value".to_string(),
                        try_expr.span,
                    ))
                }
            };

            Ok(HirExpr::Try(crate::hir::HirTry {
                expr: Box::new(scrutinee),
                ok_ty: hir_type_for(ok_ty, ctx, try_expr.span)?,
                ret_ty: hir_type_for(ret_ty.clone(), ctx, try_expr.span)?,
                span: try_expr.span,
            }))
        }
        Expr::Index(index_expr) => {
            let object = lower_expr(&index_expr.object, ctx, ret_ty)?;
            let index = lower_expr(&index_expr.index, ctx, ret_ty)?;

            Ok(HirExpr::Index(crate::hir::HirIndex {
                object: Box::new(object),
                index: Box::new(index),
                elem_ty: hir_ty,
                span: index_expr.span,
            }))
        }
    }
}

/// Lower a match-as-statement (arms can contain returns, don't need to produce values).
fn lower_match_stmt(
    match_expr: &MatchExpr,
    ctx: &mut LoweringCtx,
    ret_ty: &Ty,
) -> Result<HirExpr, TypeError> {
    let scrutinee = lower_expr(&match_expr.expr, ctx, ret_ty)?;
    let scrutinee_ty = scrutinee.ty().clone();

    let mut hir_arms = Vec::new();
    for arm in &match_expr.arms {
        ctx.push_scope();

        let hir_pattern = lower_pattern(&arm.pattern, &scrutinee_ty, ctx)?;

        let hir_body = lower_block(&arm.body, ctx, ret_ty)?;

        ctx.pop_scope();

        hir_arms.push(HirMatchArm {
            pattern: hir_pattern,
            body: hir_body,
        });
    }

    Ok(HirExpr::Match(HirMatch {
        expr: Box::new(scrutinee),
        arms: hir_arms,
        result_ty: hir_type_for(
            Ty::Builtin(super::BuiltinType::Unit),
            ctx,
            match_expr.span,
        )?,
        span: match_expr.span,
    }))
}

/// Lower an AST pattern to an HIR pattern, adding any bindings to the context.
fn lower_pattern(
    pattern: &Pattern,
    match_ty: &HirType,
    ctx: &mut LoweringCtx,
) -> Result<HirPattern, TypeError> {
    match pattern {
        Pattern::Wildcard(_) => Ok(HirPattern::Wildcard),

        Pattern::Literal(lit) => Ok(HirPattern::Literal(lit.clone())),

        Pattern::Binding(ident) => {
            let local_id = ctx.fresh_local(ident.item.clone(), match_ty.ty.clone());
            Ok(HirPattern::Binding(local_id))
        }

        Pattern::Path(path) => {
            if let Some(_enum_ty) = resolve_enum_variant(path, ctx.use_map, ctx.enums, ctx.module_name) {
                let variant_name = path.segments.last()
                    .map(|s| s.item.clone())
                    .unwrap_or_else(|| "unknown".to_string());
                Ok(HirPattern::Variant {
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
            if path.segments.len() == 1 {
                let name = &path.segments[0].item;
                if name == "Ok" || name == "Err" {
                    if let Ty::Path(ty_name, args) = &match_ty.ty {
                        if ty_name == "Result" && args.len() == 2 {
                            let variant_name = name.clone();

                            let binding_info = if let Some(bind_ident) = binding {
                                let bind_ty = if variant_name == "Ok" {
                                    args[0].clone()
                                } else {
                                    args[1].clone()
                                };
                                let local_id = ctx.fresh_local(bind_ident.item.clone(), bind_ty);
                                Some(local_id)
                            } else {
                                None
                            };

                            return Ok(HirPattern::Variant {
                                variant_name,
                                binding: binding_info,
                            });
                        }
                    }
                }
            }

            if let Some(_enum_ty) = resolve_enum_variant(path, ctx.use_map, ctx.enums, ctx.module_name) {
                let variant_name = path.segments.last()
                    .map(|s| s.item.clone())
                    .unwrap_or_else(|| "unknown".to_string());

                let binding_info = if let Some(bind_ident) = binding {
                    let bind_ty = match_ty.ty.clone();
                    let local_id = ctx.fresh_local(bind_ident.item.clone(), bind_ty);
                    Some(local_id)
                } else {
                    None
                };

                Ok(HirPattern::Variant {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use std::collections::HashMap;
    use super::super::TypeKind;

    #[test]
    fn abi_type_for_resolves_module_local_structs() {
        let mut structs = HashMap::new();
        structs.insert(
            "foo.Pair".to_string(),
            StructInfo {
                type_params: Vec::new(),
                fields: HashMap::new(),
                is_opaque: false,
                is_capability: false,
                kind: TypeKind::Unrestricted,
                module: "foo".to_string(),
            },
        );
        let functions = HashMap::new();
        let enums = HashMap::new();
        let use_map = UseMap {
            aliases: HashMap::new(),
        };
        let stdlib = StdlibIndex {
            types: HashMap::new(),
        };
        let ctx = LoweringCtx::new(
            &functions,
            &structs,
            &enums,
            &use_map,
            &stdlib,
            "foo",
            None,
            true,
        );
        let ty = Ty::Path("Pair".to_string(), Vec::new());
        let abi = abi_type_for(&ty, &ctx, Span::new(0, 0)).expect("abi");
        assert_eq!(abi, AbiType::Ptr);
    }
}
