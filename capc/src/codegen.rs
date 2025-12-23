use std::collections::HashMap;
use std::fs;
use std::path::Path;

use cranelift_codegen::ir::{self, AbiParam, Function, InstBuilder, MemFlags, Signature, Type};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{Configurable, Flags};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, Linkage, Module as ModuleTrait};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cranelift_native;
use thiserror::Error;

use crate::ast::{
    BinaryOp, Expr, Item, Literal, Module as AstModule, Path as AstPath, Pattern, Stmt, Type as AstType,
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
    ResultString,
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
    ResultString {
        tag: ir::Value,
        ok_ptr: ir::Value,
        ok_len: ir::Value,
        err: ir::Value,
    },
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
                // Blocks sealed after body emission.

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

                builder.seal_all_blocks();
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
            TyKind::ResultString => {
                signature.params.push(AbiParam::new(ir::types::I64)); // out_ptr
                signature.params.push(AbiParam::new(ir::types::I64)); // out_len
                signature.params.push(AbiParam::new(ir::types::I64)); // out_err
            }
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
        TyKind::ResultString => {
            signature.returns.push(AbiParam::new(ir::types::I8)); // tag
        }
    }
    signature
}

fn register_runtime_intrinsics(map: &mut HashMap<String, FnInfo>, ptr_ty: Type) {
    let system_console = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Handle,
    };
    let system_fs_read = FnSig {
        params: vec![TyKind::Handle, TyKind::String],
        ret: TyKind::Handle,
    };
    let fs_read_to_string = FnSig {
        params: vec![TyKind::Handle, TyKind::String, TyKind::ResultString],
        ret: TyKind::ResultString,
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
        "sys.system.fs_read".to_string(),
        FnInfo {
            sig: system_fs_read,
            symbol: "capable_rt_system_fs_read".to_string(),
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
    map.insert(
        "sys.fs.read_to_string".to_string(),
        FnInfo {
            sig: fs_read_to_string,
            symbol: "capable_rt_fs_read_to_string".to_string(),
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
                    ValueRepr::Pair(_, _) | ValueRepr::ResultString { .. } => {
                        return Err(CodegenError::Unsupported(
                            "non-scalar return values".to_string(),
                        ))
                    }
                };
            } else {
                builder.ins().return_(&[]);
            }
        }
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                emit_match_stmt(
                    builder,
                    match_expr,
                    locals,
                    fn_map,
                    use_map,
                    module_name,
                    stdlib,
                    module,
                    data_counter,
                )?;
            } else {
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
                    ValueRepr::ResultString { .. } => {
                        return Err(CodegenError::Unsupported("result value arg".to_string()))
                    }
                }
            }
            let mut out_slots = None;
            if info.sig.ret == TyKind::ResultString {
                let slot_ptr = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    8,
                ));
                let slot_len = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    8,
                ));
                let slot_err = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    4,
                ));
                let ptr_ptr = builder.ins().stack_addr(module.isa().pointer_type(), slot_ptr, 0);
                let len_ptr = builder.ins().stack_addr(module.isa().pointer_type(), slot_len, 0);
                let err_ptr = builder.ins().stack_addr(module.isa().pointer_type(), slot_err, 0);
                args.push(ptr_ptr);
                args.push(len_ptr);
                args.push(err_ptr);
                out_slots = Some((slot_ptr, slot_len, slot_err));
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
                TyKind::ResultString => {
                    let tag = results[0];
                    let (slot_ptr, slot_len, slot_err) =
                        out_slots.ok_or_else(|| CodegenError::Codegen("missing slots".to_string()))?;
                    let ptr_addr =
                        builder.ins().stack_addr(module.isa().pointer_type(), slot_ptr, 0);
                    let len_addr =
                        builder.ins().stack_addr(module.isa().pointer_type(), slot_len, 0);
                    let err_addr =
                        builder.ins().stack_addr(module.isa().pointer_type(), slot_err, 0);
                    let ptr = builder.ins().load(
                        module.isa().pointer_type(),
                        MemFlags::new(),
                        ptr_addr,
                        0,
                    );
                    let len = builder.ins().load(ir::types::I64, MemFlags::new(), len_addr, 0);
                    let err = builder.ins().load(ir::types::I32, MemFlags::new(), err_addr, 0);
                    Ok(ValueRepr::ResultString {
                        tag,
                        ok_ptr: ptr,
                        ok_len: len,
                        err,
                    })
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
            if matches!(binary.op, BinaryOp::And | BinaryOp::Or) {
                let lhs_val = match lhs {
                    ValueRepr::Single(v) => v,
                    ValueRepr::Pair(_, _) | ValueRepr::ResultString { .. } => {
                        return Err(CodegenError::Unsupported("boolean op on string".to_string()))
                    }
                };
                return emit_short_circuit_expr(
                    builder,
                    lhs_val,
                    &binary.right,
                    matches!(binary.op, BinaryOp::And),
                    locals,
                    fn_map,
                    use_map,
                    module_name,
                    stdlib,
                    module,
                    data_counter,
                );
            }
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

fn emit_match_stmt(
    builder: &mut FunctionBuilder,
    match_expr: &crate::ast::MatchExpr,
    locals: &mut HashMap<String, ValueRepr>,
    fn_map: &HashMap<String, FnInfo>,
    use_map: &UseMap,
    module_name: &str,
    stdlib: &StdlibIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<(), CodegenError> {
    let value = emit_expr(
        builder,
        &match_expr.expr,
        locals,
        fn_map,
        use_map,
        module_name,
        stdlib,
        module,
        data_counter,
    )?;
    let (match_val, match_result) = match value.clone() {
        ValueRepr::Single(v) => (v, None),
        ValueRepr::ResultString { tag, ok_ptr, ok_len, err } => {
            (tag, Some((ok_ptr, ok_len, err)))
        }
        ValueRepr::Pair(_, _) => {
            return Err(CodegenError::Unsupported("match on string".to_string()))
        }
    };

    let merge_block = builder.create_block();
    let mut current_block = builder.current_block().ok_or_else(|| {
        CodegenError::Codegen("no current block for match".to_string())
    })?;

    let mut all_return = true;
    for arm in &match_expr.arms {
        if !matches!(arm.body.stmts.last(), Some(Stmt::Return(_))) {
            all_return = false;
            break;
        }
    }

    for (idx, arm) in match_expr.arms.iter().enumerate() {
        let is_last = idx + 1 == match_expr.arms.len();
        let arm_block = builder.create_block();
        let next_block = if is_last {
            merge_block
        } else {
            builder.create_block()
        };

        if idx > 0 {
            builder.switch_to_block(current_block);
        }
        let cond = match_pattern_cond(builder, &arm.pattern, match_val)?;
        builder.ins().brif(cond, arm_block, &[], next_block, &[]);

        builder.switch_to_block(arm_block);
        bind_match_pattern_value(&arm.pattern, &value, match_result, locals)?;
        for stmt in &arm.body.stmts {
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
        if !matches!(arm.body.stmts.last(), Some(Stmt::Return(_))) {
            builder.ins().jump(merge_block, &[]);
        }
        builder.seal_block(arm_block);

        if is_last {
            break;
        }
        current_block = next_block;
    }

    if all_return {
        builder.switch_to_block(merge_block);
        builder.ins().trap(ir::TrapCode::UnreachableCodeReached);
        builder.seal_block(merge_block);
    } else {
        builder.switch_to_block(merge_block);
        builder.seal_block(merge_block);
    }
    Ok(())
}

fn match_pattern_cond(
    builder: &mut FunctionBuilder,
    pattern: &Pattern,
    match_val: ir::Value,
) -> Result<ir::Value, CodegenError> {
    match pattern {
        Pattern::Wildcard(_) | Pattern::Binding(_) => {
            let one = builder.ins().iconst(ir::types::I8, 1);
            Ok(builder.ins().icmp_imm(IntCC::NotEqual, one, 0))
        }
        Pattern::Literal(lit) => match lit {
            Literal::Bool(value) => {
                let rhs = builder.ins().iconst(ir::types::I8, *value as i64);
                Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
            }
            Literal::Int(value) => {
                let rhs = builder.ins().iconst(ir::types::I32, *value);
                Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
            }
            _ => Err(CodegenError::Unsupported("literal pattern".to_string())),
        },
        Pattern::Call { path, .. } => {
            let name = path.segments.last().map(|seg| seg.item.as_str()).unwrap_or("");
            let tag_value = if name == "Ok" { 0 } else if name == "Err" { 1 } else { 2 };
            if tag_value == 2 {
                return Err(CodegenError::Unsupported("call pattern".to_string()));
            }
            let rhs = builder.ins().iconst(ir::types::I8, tag_value);
            Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
        }
        _ => Err(CodegenError::Unsupported("pattern".to_string())),
    }
}

fn bind_match_pattern_value(
    pattern: &Pattern,
    value: &ValueRepr,
    result: Option<(ir::Value, ir::Value, ir::Value)>,
    locals: &mut HashMap<String, ValueRepr>,
) -> Result<(), CodegenError> {
    match pattern {
        Pattern::Binding(ident) => match value {
            ValueRepr::ResultString { .. } => Err(CodegenError::Unsupported(
                "binding result value".to_string(),
            )),
            _ => {
                locals.insert(ident.item.clone(), value.clone());
                Ok(())
            }
        },
        Pattern::Call { path, binding, .. } => {
            let name = path.segments.last().map(|seg| seg.item.as_str()).unwrap_or("");
            let Some((ok_ptr, ok_len, err)) = result else {
                return Err(CodegenError::Unsupported("call pattern".to_string()));
            };
            if let Some(binding) = binding {
                if name == "Ok" {
                    locals.insert(binding.item.clone(), ValueRepr::Pair(ok_ptr, ok_len));
                    return Ok(());
                }
                if name == "Err" {
                    locals.insert(binding.item.clone(), ValueRepr::Single(err));
                    return Ok(());
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn to_b1(builder: &mut FunctionBuilder, value: ValueRepr) -> Result<ir::Value, CodegenError> {
    match value {
        ValueRepr::Single(val) => Ok(builder.ins().icmp_imm(IntCC::NotEqual, val, 0)),
        ValueRepr::Pair(_, _) | ValueRepr::ResultString { .. } => {
            Err(CodegenError::Unsupported("string condition".to_string()))
        }
    }
}

fn bool_to_i8(builder: &mut FunctionBuilder, value: ir::Value) -> ir::Value {
    builder.ins().uextend(ir::types::I8, value)
}

fn emit_short_circuit_expr(
    builder: &mut FunctionBuilder,
    lhs: ir::Value,
    rhs_expr: &Expr,
    is_and: bool,
    locals: &HashMap<String, ValueRepr>,
    fn_map: &HashMap<String, FnInfo>,
    use_map: &UseMap,
    module_name: &str,
    stdlib: &StdlibIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    let rhs_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, ir::types::I8);

    let lhs_cond = builder.ins().icmp_imm(IntCC::NotEqual, lhs, 0);
    if is_and {
        builder.ins().brif(lhs_cond, rhs_block, &[], merge_block, &[lhs]);
    } else {
        builder.ins().brif(lhs_cond, merge_block, &[lhs], rhs_block, &[]);
    }
    let current = builder.current_block().ok_or_else(|| {
        CodegenError::Codegen("no block for short-circuit".to_string())
    })?;
    builder.seal_block(current);

    builder.switch_to_block(rhs_block);
    let rhs_value = emit_expr(
        builder,
        rhs_expr,
        locals,
        fn_map,
        use_map,
        module_name,
        stdlib,
        module,
        data_counter,
    )?;
    let rhs_val = match rhs_value {
        ValueRepr::Single(v) => v,
        ValueRepr::Pair(_, _) | ValueRepr::ResultString { .. } => {
            return Err(CodegenError::Unsupported("boolean op on string".to_string()))
        }
    };
    builder.ins().jump(merge_block, &[rhs_val]);
    builder.seal_block(rhs_block);

    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);
    let merged = builder.block_params(merge_block)[0];
    Ok(ValueRepr::Single(merged))
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
