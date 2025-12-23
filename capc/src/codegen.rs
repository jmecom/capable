use std::collections::HashMap;
use std::fs;
use std::path::Path;

use cranelift_codegen::ir::{self, AbiParam, Function, InstBuilder, Signature, Type};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{Configurable, Flags};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, Linkage, Module as ModuleTrait};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cranelift_native;
use thiserror::Error;

use crate::ast::{
    BinaryOp, Expr, Item, Literal, Module as AstModule, Path as AstPath, Stmt, Type as AstType,
    UnaryOp,
};

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("unsupported {0}")]
    Unsupported(String),
    #[error("unknown function `{0}`")]
    UnknownFunction(String),
    #[error("unknown variable `{0}`")]
    UnknownVariable(String),
    #[error("io error: {0}")]
    Io(String),
    #[error("codegen error: {0}")]
    Codegen(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TyKind {
    I32,
    Bool,
    Unit,
    String,
    Handle,
}

#[derive(Clone, Debug)]
struct FnSig {
    params: Vec<TyKind>,
    ret: TyKind,
}

#[derive(Clone, Debug)]
struct FnInfo {
    sig: FnSig,
    symbol: String,
    is_runtime: bool,
}

#[derive(Clone, Debug)]
struct UseMap {
    aliases: HashMap<String, Vec<String>>,
}

impl UseMap {
    fn new(module: &AstModule) -> Self {
        let mut aliases: HashMap<String, Vec<String>> = HashMap::new();
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

#[derive(Clone, Debug)]
struct StdlibIndex {
    types: HashMap<String, String>,
}

#[derive(Clone, Debug)]
enum ValueRepr {
    Single(ir::Value),
    Pair(ir::Value, ir::Value),
}

pub fn build_object(
    entry: &AstModule,
    user_modules: &[AstModule],
    stdlib: &[AstModule],
    out_path: &Path,
) -> Result<(), CodegenError> {
    let mut flag_builder = cranelift_codegen::settings::builder();
    flag_builder
        .set("is_pic", "true")
        .map_err(|err| CodegenError::Codegen(err.to_string()))?;
    let isa = cranelift_native::builder()
        .map_err(|err| CodegenError::Codegen(err.to_string()))?
        .finish(Flags::new(flag_builder))
        .map_err(|err| CodegenError::Codegen(err.to_string()))?;

    let mut module = ObjectModule::new(
        ObjectBuilder::new(
            isa,
            "capable",
            cranelift_module::default_libcall_names(),
        )
        .map_err(|err| CodegenError::Codegen(err.to_string()))?,
    );

    let stdlib_index = build_stdlib_index(stdlib);

    let mut fn_map = HashMap::new();
    register_runtime_intrinsics(&mut fn_map, module.isa().pointer_type());
    let all_modules = user_modules
        .iter()
        .chain(std::iter::once(entry))
        .collect::<Vec<_>>();
    for module_ref in &all_modules {
        register_user_functions(
            module_ref,
            entry,
            &stdlib_index,
            &mut fn_map,
            module.isa().pointer_type(),
        )?;
    }

    let mut data_counter = 0u32;
    for module_ref in &all_modules {
        let module_name = module_ref.name.to_string();
        let use_map = UseMap::new(module_ref);
        for item in &module_ref.items {
            if let Item::Function(func) = item {
                let key = format!("{module_name}.{}", func.name.item);
                let info = fn_map
                    .get(&key)
                    .ok_or_else(|| CodegenError::UnknownFunction(key.clone()))?
                    .clone();
                if info.is_runtime {
                    continue;
                }
                let func_id = module
                    .declare_function(&info.symbol, Linkage::Export, &sig_to_clif(&info.sig, module.isa().pointer_type()))
                    .map_err(|err| CodegenError::Codegen(err.to_string()))?;
                let mut ctx = module.make_context();
                ctx.func = Function::with_name_signature(
                    ir::UserFuncName::user(0, func_id.as_u32()),
                    sig_to_clif(&info.sig, module.isa().pointer_type()),
                );

                let mut builder_ctx = FunctionBuilderContext::new();
                let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
                let block = builder.create_block();
                builder.append_block_params_for_function_params(block);
                builder.switch_to_block(block);
                builder.seal_block(block);

                let mut locals: HashMap<String, ValueRepr> = HashMap::new();
                let mut param_index = 0;
                for param in &func.params {
                    let ty_kind = lower_ty(&param.ty, &use_map, &stdlib_index);
                    match ty_kind {
                        TyKind::String => {
                            let ptr = builder.block_params(block)[param_index];
                            let len = builder.block_params(block)[param_index + 1];
                            param_index += 2;
                            locals.insert(param.name.item.clone(), ValueRepr::Pair(ptr, len));
                        }
                        TyKind::Unit => {
                            locals.insert(param.name.item.clone(), ValueRepr::Single(builder.ins().iconst(ir::types::I32, 0)));
                        }
                        _ => {
                            let val = builder.block_params(block)[param_index];
                            param_index += 1;
                            locals.insert(param.name.item.clone(), ValueRepr::Single(val));
                        }
                    }
                }

                for stmt in &func.body.stmts {
                    emit_stmt(
                        &mut builder,
                        stmt,
                        &mut locals,
                        &fn_map,
                        &use_map,
                        &module_name,
                        &stdlib_index,
                        &mut module,
                        &mut data_counter,
                    )?;
                }

                if info.sig.ret == TyKind::Unit {
                    builder.ins().return_(&[]);
                }

                builder.finalize();
                module
                    .define_function(func_id, &mut ctx)
                    .map_err(|err| CodegenError::Codegen(err.to_string()))?;
            }
        }
    }

    let object = module.finish();
    fs::write(out_path, object.emit().map_err(|err| CodegenError::Codegen(err.to_string()))?)
        .map_err(|err| CodegenError::Io(err.to_string()))?;
    Ok(())
}

fn sig_to_clif(sig: &FnSig, ptr_ty: Type) -> Signature {
    let mut signature = Signature::new(CallConv::SystemV);
    for param in &sig.params {
        match param {
            TyKind::String => {
                signature.params.push(AbiParam::new(ptr_ty));
                signature.params.push(AbiParam::new(ir::types::I64));
            }
            TyKind::Handle => signature.params.push(AbiParam::new(ir::types::I64)),
            TyKind::I32 => signature.params.push(AbiParam::new(ir::types::I32)),
            TyKind::Bool => signature.params.push(AbiParam::new(ir::types::I8)),
            TyKind::Unit => {}
        }
    }
    match sig.ret {
        TyKind::Unit => {}
        TyKind::I32 => signature.returns.push(AbiParam::new(ir::types::I32)),
        TyKind::Bool => signature.returns.push(AbiParam::new(ir::types::I8)),
        TyKind::Handle => signature.returns.push(AbiParam::new(ir::types::I64)),
        TyKind::String => {
            signature.returns.push(AbiParam::new(ptr_ty));
            signature.returns.push(AbiParam::new(ir::types::I64));
        }
    }
    signature
}

fn register_runtime_intrinsics(map: &mut HashMap<String, FnInfo>, ptr_ty: Type) {
    let system_console = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Handle,
    };
    let console_println = FnSig {
        params: vec![TyKind::Handle, TyKind::String],
        ret: TyKind::Unit,
    };
    let console_print = FnSig {
        params: vec![TyKind::Handle, TyKind::String],
        ret: TyKind::Unit,
    };

    map.insert(
        "sys.system.console".to_string(),
        FnInfo {
            sig: system_console,
            symbol: "capable_rt_system_console".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.println".to_string(),
        FnInfo {
            sig: console_println,
            symbol: "capable_rt_console_println".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.print".to_string(),
        FnInfo {
            sig: console_print,
            symbol: "capable_rt_console_print".to_string(),
            is_runtime: true,
        },
    );

    let _ = ptr_ty;
}

fn register_user_functions(
    module: &AstModule,
    entry: &AstModule,
    stdlib: &StdlibIndex,
    map: &mut HashMap<String, FnInfo>,
    _ptr_ty: Type,
) -> Result<(), CodegenError> {
    let module_name = module.name.to_string();
    let use_map = UseMap::new(module);
    for item in &module.items {
        if let Item::Function(func) = item {
            let sig = FnSig {
                params: func
                    .params
                    .iter()
                    .map(|p| lower_ty(&p.ty, &use_map, stdlib))
                    .collect(),
                ret: lower_ty(&func.ret, &use_map, stdlib),
            };
            let key = format!("{module_name}.{}", func.name.item);
            let symbol = if module_name == entry.name.to_string() && func.name.item == "main" {
                "capable_main".to_string()
            } else {
                mangle_symbol(&module_name, &func.name.item)
            };
            map.insert(
                key,
                FnInfo {
                    sig,
                    symbol,
                    is_runtime: false,
                },
            );
        }
    }
    Ok(())
}

fn mangle_symbol(module_name: &str, func_name: &str) -> String {
    let mut out = String::from("capable_");
    out.push_str(&module_name.replace('.', "_"));
    out.push('_');
    out.push_str(func_name);
    out
}

fn emit_stmt(
    builder: &mut FunctionBuilder,
    stmt: &Stmt,
    locals: &mut HashMap<String, ValueRepr>,
    fn_map: &HashMap<String, FnInfo>,
    use_map: &UseMap,
    module_name: &str,
    stdlib: &StdlibIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<(), CodegenError> {
    match stmt {
        Stmt::Let(let_stmt) => {
            let value = emit_expr(
                builder,
                &let_stmt.expr,
                locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                module,
                data_counter,
            )?;
            locals.insert(let_stmt.name.item.clone(), value);
        }
        Stmt::Return(ret_stmt) => {
            if let Some(expr) = &ret_stmt.expr {
                let value = emit_expr(
                    builder,
                    expr,
                    locals,
                    fn_map,
                    use_map,
                    module_name,
                    stdlib,
                    module,
                    data_counter,
                )?;
                match value {
                    ValueRepr::Single(val) => builder.ins().return_(&[val]),
                    ValueRepr::Pair(_, _) => {
                        return Err(CodegenError::Unsupported(
                            "string return values".to_string(),
                        ))
                    }
                };
            } else {
                builder.ins().return_(&[]);
            }
        }
        Stmt::Expr(expr_stmt) => {
            let _ = emit_expr(
                builder,
                &expr_stmt.expr,
                locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                module,
                data_counter,
            )?;
        }
        Stmt::If(if_stmt) => {
            let then_block = builder.create_block();
            let merge_block = builder.create_block();
            let else_block = if if_stmt.else_block.is_some() {
                builder.create_block()
            } else {
                merge_block
            };
            let cond_val = emit_expr(
                builder,
                &if_stmt.cond,
                locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                module,
                data_counter,
            )?;
            let cond_b1 = to_b1(builder, cond_val)?;
            builder.ins().brif(cond_b1, then_block, &[], else_block, &[]);

            builder.switch_to_block(then_block);
            for stmt in &if_stmt.then_block.stmts {
                emit_stmt(
                    builder,
                    stmt,
                    locals,
                    fn_map,
                    use_map,
                    module_name,
                    stdlib,
                    module,
                    data_counter,
                )?;
            }
            builder.ins().jump(merge_block, &[]);
            builder.seal_block(then_block);

            if let Some(else_block_ast) = &if_stmt.else_block {
                builder.switch_to_block(else_block);
                for stmt in &else_block_ast.stmts {
                    emit_stmt(
                        builder,
                        stmt,
                        locals,
                        fn_map,
                        use_map,
                        module_name,
                        stdlib,
                        module,
                        data_counter,
                    )?;
                }
                builder.ins().jump(merge_block, &[]);
                builder.seal_block(else_block);
            }

            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);
        }
        Stmt::While(while_stmt) => {
            let header_block = builder.create_block();
            let body_block = builder.create_block();
            let exit_block = builder.create_block();

            builder.ins().jump(header_block, &[]);
            builder.switch_to_block(header_block);
            let cond_val = emit_expr(
                builder,
                &while_stmt.cond,
                locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                module,
                data_counter,
            )?;
            let cond_b1 = to_b1(builder, cond_val)?;
            builder.ins().brif(cond_b1, body_block, &[], exit_block, &[]);

            builder.switch_to_block(body_block);
            for stmt in &while_stmt.body.stmts {
                emit_stmt(
                    builder,
                    stmt,
                    locals,
                    fn_map,
                    use_map,
                    module_name,
                    stdlib,
                    module,
                    data_counter,
                )?;
            }
            builder.ins().jump(header_block, &[]);
            builder.seal_block(body_block);
            builder.seal_block(header_block);

            builder.switch_to_block(exit_block);
            builder.seal_block(exit_block);
        }
    }
    Ok(())
}

fn emit_expr(
    builder: &mut FunctionBuilder,
    expr: &Expr,
    locals: &HashMap<String, ValueRepr>,
    fn_map: &HashMap<String, FnInfo>,
    use_map: &UseMap,
    module_name: &str,
    stdlib: &StdlibIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    match expr {
        Expr::Literal(lit) => match &lit.value {
            Literal::Int(value) => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I32, *value as i64))),
            Literal::Bool(value) => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I8, *value as i64))),
            Literal::String(value) => emit_string(builder, value, module, data_counter),
            Literal::Unit => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I32, 0))),
        },
        Expr::Path(path) => {
            if path.segments.len() != 1 {
                return Err(CodegenError::Unsupported("path expression".to_string()));
            }
            locals
                .get(&path.segments[0].item)
                .cloned()
                .ok_or_else(|| CodegenError::UnknownVariable(path.to_string()))
        }
        Expr::Call(call) => {
            let callee_path = match &*call.callee {
                Expr::Path(path) => resolve_call_path(path, module_name, use_map),
                _ => {
                    return Err(CodegenError::Unsupported(
                        "call target".to_string(),
                    ))
                }
            };
            let info = fn_map
                .get(&callee_path)
                .ok_or_else(|| CodegenError::UnknownFunction(callee_path.clone()))?
                .clone();
            let mut args = Vec::new();
            for arg in &call.args {
                let value = emit_expr(
                    builder,
                    arg,
                    locals,
                    fn_map,
                    use_map,
                    module_name,
                    stdlib,
                    module,
                    data_counter,
                )?;
                match value {
                    ValueRepr::Single(val) => args.push(val),
                    ValueRepr::Pair(a, b) => {
                        args.push(a);
                        args.push(b);
                    }
                }
            }
            let sig = sig_to_clif(&info.sig, module.isa().pointer_type());
            let func_id = module
                .declare_function(&info.symbol, if info.is_runtime { Linkage::Import } else { Linkage::Export }, &sig)
                .map_err(|err| CodegenError::Codegen(err.to_string()))?;
            let local = module.declare_func_in_func(func_id, builder.func);
            let call_inst = builder.ins().call(local, &args);
            let results = builder.inst_results(call_inst);
            match info.sig.ret {
                TyKind::Unit => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I32, 0))),
                TyKind::String => {
                    if results.len() != 2 {
                        return Err(CodegenError::Codegen("string return count".to_string()));
                    }
                    Ok(ValueRepr::Pair(results[0], results[1]))
                }
                _ => Ok(ValueRepr::Single(results[0])),
            }
        }
        Expr::Binary(binary) => {
            let lhs = emit_expr(
                builder,
                &binary.left,
                locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                module,
                data_counter,
            )?;
            let rhs = emit_expr(
                builder,
                &binary.right,
                locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                module,
                data_counter,
            )?;
            match (&binary.op, lhs, rhs) {
                (BinaryOp::Add, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(builder.ins().iadd(a, b)))
                }
                (BinaryOp::Sub, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(builder.ins().isub(a, b)))
                }
                (BinaryOp::Mul, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(builder.ins().imul(a, b)))
                }
                (BinaryOp::Div, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(builder.ins().sdiv(a, b)))
                }
                (BinaryOp::Eq, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    let cmp = builder.ins().icmp(IntCC::Equal, a, b);
                    Ok(ValueRepr::Single(bool_to_i8(builder, cmp)))
                }
                (BinaryOp::Neq, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    let cmp = builder.ins().icmp(IntCC::NotEqual, a, b);
                    Ok(ValueRepr::Single(bool_to_i8(builder, cmp)))
                }
                (BinaryOp::Lt, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    let cmp = builder.ins().icmp(IntCC::SignedLessThan, a, b);
                    Ok(ValueRepr::Single(bool_to_i8(builder, cmp)))
                }
                (BinaryOp::Lte, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    let cmp = builder.ins().icmp(IntCC::SignedLessThanOrEqual, a, b);
                    Ok(ValueRepr::Single(bool_to_i8(builder, cmp)))
                }
                (BinaryOp::Gt, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    let cmp = builder.ins().icmp(IntCC::SignedGreaterThan, a, b);
                    Ok(ValueRepr::Single(bool_to_i8(builder, cmp)))
                }
                (BinaryOp::Gte, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    let cmp = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, a, b);
                    Ok(ValueRepr::Single(bool_to_i8(builder, cmp)))
                }
                (BinaryOp::And, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(builder.ins().band(a, b)))
                }
                (BinaryOp::Or, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(builder.ins().bor(a, b)))
                }
                _ => Err(CodegenError::Unsupported("binary op".to_string())),
            }
        }
        Expr::Grouping(group) => emit_expr(
            builder,
            &group.expr,
            locals,
            fn_map,
            use_map,
            module_name,
            stdlib,
            module,
            data_counter,
        ),
        Expr::Unary(unary) => {
            let value = emit_expr(
                builder,
                &unary.expr,
                locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                module,
                data_counter,
            )?;
            match (&unary.op, value) {
                (UnaryOp::Neg, ValueRepr::Single(v)) => Ok(ValueRepr::Single(builder.ins().ineg(v))),
                (UnaryOp::Not, ValueRepr::Single(v)) => {
                    let one = builder.ins().iconst(ir::types::I8, 1);
                    Ok(ValueRepr::Single(builder.ins().bxor(v, one)))
                }
                _ => Err(CodegenError::Unsupported("unary op".to_string())),
            }
        }
        Expr::Match(_) | Expr::StructLiteral(_) => Err(CodegenError::Unsupported(
            "expression".to_string(),
        )),
    }
}

fn to_b1(builder: &mut FunctionBuilder, value: ValueRepr) -> Result<ir::Value, CodegenError> {
    match value {
        ValueRepr::Single(val) => Ok(builder.ins().icmp_imm(IntCC::NotEqual, val, 0)),
        ValueRepr::Pair(_, _) => Err(CodegenError::Unsupported("string condition".to_string())),
    }
}

fn bool_to_i8(builder: &mut FunctionBuilder, value: ir::Value) -> ir::Value {
    builder.ins().uextend(ir::types::I8, value)
}

fn emit_string(
    builder: &mut FunctionBuilder,
    value: &str,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    let name = format!("__str_{}", data_counter);
    *data_counter += 1;
    let data_id = module
        .declare_data(&name, Linkage::Local, false, false)
        .map_err(|err| CodegenError::Codegen(err.to_string()))?;
    let mut data_ctx = DataDescription::new();
    data_ctx.define(value.as_bytes().to_vec().into_boxed_slice());
    module
        .define_data(data_id, &data_ctx)
        .map_err(|err| CodegenError::Codegen(err.to_string()))?;
    let global = module.declare_data_in_func(data_id, builder.func);
    let ptr = builder.ins().global_value(module.isa().pointer_type(), global);
    let len = builder.ins().iconst(ir::types::I64, value.len() as i64);
    Ok(ValueRepr::Pair(ptr, len))
}

fn resolve_call_path(path: &AstPath, module_name: &str, use_map: &UseMap) -> String {
    let resolved = resolve_path(path, use_map);
    if resolved.len() == 1 {
        format!("{module_name}.{}", resolved[0])
    } else {
        resolved.join(".")
    }
}

fn resolve_path(path: &AstPath, use_map: &UseMap) -> Vec<String> {
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

fn build_stdlib_index(stdlib: &[AstModule]) -> StdlibIndex {
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
            types.insert(name, qualified);
        }
    }
    StdlibIndex { types }
}

fn lower_ty(ty: &AstType, use_map: &UseMap, stdlib: &StdlibIndex) -> TyKind {
    let resolved = resolve_type_name(&ty.path, use_map, stdlib);
    if resolved == "i32" {
        return TyKind::I32;
    }
    if resolved == "bool" {
        return TyKind::Bool;
    }
    if resolved == "unit" {
        return TyKind::Unit;
    }
    if resolved == "string" {
        return TyKind::String;
    }
    if resolved == "sys.system.System"
        || resolved == "sys.console.Console"
        || resolved == "sys.fs.ReadFS"
    {
        return TyKind::Handle;
    }
    TyKind::Unit
}

fn resolve_type_name(path: &AstPath, use_map: &UseMap, stdlib: &StdlibIndex) -> String {
    let resolved = resolve_path(path, use_map);
    if resolved.len() == 1 {
        if let Some(full) = stdlib.types.get(&resolved[0]) {
            return full.clone();
        }
    }
    resolved.join(".")
}
