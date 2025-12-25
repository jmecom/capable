use std::collections::HashMap;
use std::fs;
use std::path::Path;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, AbiParam, Function, InstBuilder, MemFlags, Signature, Type};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{Configurable, Flags};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, Linkage, Module as ModuleTrait};
use cranelift_native;
use cranelift_object::{ObjectBuilder, ObjectModule};
use thiserror::Error;

use crate::ast::{
    BinaryOp, Expr, Item, Literal, Module as AstModule, Path as AstPath, Pattern, Stmt,
    Type as AstType, UnaryOp,
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

#[derive(Clone, Debug, PartialEq, Eq)]
enum TyKind {
    I32,
    U32,
    U8,
    Bool,
    Unit,
    String,
    Handle,
    Ptr,
    Result(Box<TyKind>, Box<TyKind>),
    ResultOut(Box<TyKind>, Box<TyKind>),
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
    abi_sig: Option<FnSig>,
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
struct EnumIndex {
    variants: HashMap<String, HashMap<String, i32>>,
}

#[derive(Clone, Debug)]
enum ValueRepr {
    Unit,
    Single(ir::Value),
    Pair(ir::Value, ir::Value),
    Result {
        tag: ir::Value,
        ok: Box<ValueRepr>,
        err: Box<ValueRepr>,
    },
}

#[derive(Clone, Debug)]
enum LocalValue {
    Value(ValueRepr),
    Slot(ir::StackSlot, Type),
}

#[derive(Clone, Debug)]
struct ResultShape {
    kind: ResultKind,
    slots: Vec<ir::StackSlot>,
    types: Vec<Type>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ResultKind {
    Unit,
    Single,
    Pair,
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
        ObjectBuilder::new(isa, "capable", cranelift_module::default_libcall_names())
            .map_err(|err| CodegenError::Codegen(err.to_string()))?,
    );

    let stdlib_index = build_stdlib_index(stdlib);
    let enum_index = build_enum_index(entry, user_modules, stdlib);

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
            &enum_index,
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
                    .declare_function(
                        &info.symbol,
                        Linkage::Export,
                        &sig_to_clif(&info.sig, module.isa().pointer_type()),
                    )
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

                let mut locals: HashMap<String, LocalValue> = HashMap::new();
                let params = builder.block_params(block).to_vec();
                let mut param_index = 0;
                for param in &func.params {
                    let ty_kind = lower_ty(&param.ty, &use_map, &stdlib_index, &enum_index)?;
                    let value =
                        value_from_params(&mut builder, &ty_kind, &params, &mut param_index)?;
                    let local = store_local(&mut builder, value);
                    locals.insert(param.name.item.clone(), local);
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
                        &enum_index,
                        &mut module,
                        &mut data_counter,
                    )?;
                }

                // Only add implicit unit return if function body doesn't end with return
                if info.sig.ret == TyKind::Unit && !block_ends_with_return(&func.body) {
                    builder.ins().return_(&[]);
                }

                builder.seal_all_blocks();
                builder.finalize();
                if let Err(err) = cranelift_codegen::verify_function(&ctx.func, module.isa()) {
                    return Err(CodegenError::Codegen(format!("verifier errors: {err}")));
                }
                module
                    .define_function(func_id, &mut ctx)
                    .map_err(|err| CodegenError::Codegen(err.to_string()))?;
            }
        }
    }

    let object = module.finish();
    fs::write(
        out_path,
        object
            .emit()
            .map_err(|err| CodegenError::Codegen(err.to_string()))?,
    )
    .map_err(|err| CodegenError::Io(err.to_string()))?;
    Ok(())
}

fn sig_to_clif(sig: &FnSig, ptr_ty: Type) -> Signature {
    let mut signature = Signature::new(CallConv::SystemV);
    for param in &sig.params {
        append_ty_params(&mut signature, param, ptr_ty);
    }
    append_ty_returns(&mut signature, &sig.ret, ptr_ty);
    signature
}

fn append_ty_params(signature: &mut Signature, ty: &TyKind, ptr_ty: Type) {
    match ty {
        TyKind::Unit => {}
        TyKind::String => {
            signature.params.push(AbiParam::new(ptr_ty));
            signature.params.push(AbiParam::new(ir::types::I64));
        }
        TyKind::Handle => signature.params.push(AbiParam::new(ir::types::I64)),
        TyKind::Ptr => signature.params.push(AbiParam::new(ptr_ty)),
        TyKind::I32 => signature.params.push(AbiParam::new(ir::types::I32)),
        TyKind::U32 => signature.params.push(AbiParam::new(ir::types::I32)),
        TyKind::U8 => signature.params.push(AbiParam::new(ir::types::I8)),
        TyKind::Bool => signature.params.push(AbiParam::new(ir::types::I8)),
        TyKind::Result(ok, err) => {
            signature.params.push(AbiParam::new(ir::types::I8)); // tag
            append_ty_params(signature, ok, ptr_ty);
            append_ty_params(signature, err, ptr_ty);
        }
        TyKind::ResultOut(ok, err) => {
            if **ok != TyKind::Unit {
                signature.params.push(AbiParam::new(ptr_ty));
            }
            if **err != TyKind::Unit {
                signature.params.push(AbiParam::new(ptr_ty));
            }
        }
        TyKind::ResultString => {
            signature.params.push(AbiParam::new(ptr_ty)); // out_ptr
            signature.params.push(AbiParam::new(ptr_ty)); // out_len
            signature.params.push(AbiParam::new(ptr_ty)); // out_err
        }
    }
}

fn append_ty_returns(signature: &mut Signature, ty: &TyKind, ptr_ty: Type) {
    match ty {
        TyKind::Unit => {}
        TyKind::I32 => signature.returns.push(AbiParam::new(ir::types::I32)),
        TyKind::U32 => signature.returns.push(AbiParam::new(ir::types::I32)),
        TyKind::U8 => signature.returns.push(AbiParam::new(ir::types::I8)),
        TyKind::Bool => signature.returns.push(AbiParam::new(ir::types::I8)),
        TyKind::Handle => signature.returns.push(AbiParam::new(ir::types::I64)),
        TyKind::Ptr => signature.returns.push(AbiParam::new(ptr_ty)),
        TyKind::String => {
            signature.returns.push(AbiParam::new(ptr_ty));
            signature.returns.push(AbiParam::new(ir::types::I64));
        }
        TyKind::Result(ok, err) => {
            signature.returns.push(AbiParam::new(ir::types::I8)); // tag
            append_ty_returns(signature, ok, ptr_ty);
            append_ty_returns(signature, err, ptr_ty);
        }
        TyKind::ResultOut(_, _) => {
            signature.returns.push(AbiParam::new(ir::types::I8)); // tag
        }
        TyKind::ResultString => {
            signature.returns.push(AbiParam::new(ir::types::I8)); // tag
        }
    }
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
        params: vec![TyKind::Handle, TyKind::String],
        ret: TyKind::Result(Box::new(TyKind::String), Box::new(TyKind::I32)),
    };
    let fs_read_to_string_abi = FnSig {
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
    let console_print_i32 = FnSig {
        params: vec![TyKind::Handle, TyKind::I32],
        ret: TyKind::Unit,
    };
    let mem_malloc = FnSig {
        params: vec![TyKind::I32],
        ret: TyKind::Ptr,
    };
    let mem_free = FnSig {
        params: vec![TyKind::Ptr],
        ret: TyKind::Unit,
    };
    let mem_cast = FnSig {
        params: vec![TyKind::Ptr],
        ret: TyKind::Ptr,
    };
    let mem_alloc_default = FnSig {
        params: vec![],
        ret: TyKind::Handle,
    };
    let mem_slice_from_ptr = FnSig {
        params: vec![TyKind::Ptr, TyKind::I32],
        ret: TyKind::Handle,
    };
    let mem_slice_len = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::I32,
    };
    let mem_slice_at = FnSig {
        params: vec![TyKind::Handle, TyKind::I32],
        ret: TyKind::U8,
    };
    let mem_buffer_new = FnSig {
        params: vec![TyKind::Handle, TyKind::I32],
        ret: TyKind::Result(Box::new(TyKind::Handle), Box::new(TyKind::I32)),
    };
    let mem_buffer_new_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::I32,
            TyKind::ResultOut(Box::new(TyKind::Handle), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::Handle), Box::new(TyKind::I32)),
    };
    let mem_buffer_len = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::I32,
    };
    let mem_buffer_push = FnSig {
        params: vec![TyKind::Handle, TyKind::U8],
        ret: TyKind::Result(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let mem_buffer_push_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::U8,
            TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let mem_buffer_free = FnSig {
        params: vec![TyKind::Handle, TyKind::Handle],
        ret: TyKind::Unit,
    };
    let mem_buffer_as_slice = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Handle,
    };
    let args_len = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::I32,
    };
    let args_at = FnSig {
        params: vec![TyKind::Handle, TyKind::I32],
        ret: TyKind::Result(Box::new(TyKind::String), Box::new(TyKind::I32)),
    };
    let args_at_abi = FnSig {
        params: vec![TyKind::Handle, TyKind::I32, TyKind::ResultString],
        ret: TyKind::ResultString,
    };
    let io_read_stdin = FnSig {
        params: vec![],
        ret: TyKind::Result(Box::new(TyKind::String), Box::new(TyKind::I32)),
    };
    let io_read_stdin_abi = FnSig {
        params: vec![TyKind::ResultString],
        ret: TyKind::ResultString,
    };
    let string_len = FnSig {
        params: vec![TyKind::String],
        ret: TyKind::I32,
    };
    let string_byte_at = FnSig {
        params: vec![TyKind::String, TyKind::I32],
        ret: TyKind::U8,
    };
    let string_as_slice = FnSig {
        params: vec![TyKind::String],
        ret: TyKind::Handle,
    };
    let string_split = FnSig {
        params: vec![TyKind::String],
        ret: TyKind::Handle,
    };
    let string_split_delim = FnSig {
        params: vec![TyKind::String, TyKind::U8],
        ret: TyKind::Handle,
    };
    let vec_new = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Handle,
    };
    let vec_len = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::I32,
    };
    let vec_u8_get = FnSig {
        params: vec![TyKind::Handle, TyKind::I32],
        ret: TyKind::Result(Box::new(TyKind::U8), Box::new(TyKind::I32)),
    };
    let vec_u8_get_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::I32,
            TyKind::ResultOut(Box::new(TyKind::U8), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::U8), Box::new(TyKind::I32)),
    };
    let vec_u8_set = FnSig {
        params: vec![TyKind::Handle, TyKind::I32, TyKind::U8],
        ret: TyKind::Result(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_u8_set_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::I32,
            TyKind::U8,
            TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_u8_push = FnSig {
        params: vec![TyKind::Handle, TyKind::U8],
        ret: TyKind::Result(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_u8_push_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::U8,
            TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_u8_pop = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Result(Box::new(TyKind::U8), Box::new(TyKind::I32)),
    };
    let vec_u8_pop_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::ResultOut(Box::new(TyKind::U8), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::U8), Box::new(TyKind::I32)),
    };
    let vec_u8_as_slice = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Handle,
    };
    let vec_u8_free = FnSig {
        params: vec![TyKind::Handle, TyKind::Handle],
        ret: TyKind::Unit,
    };
    let vec_i32_get = FnSig {
        params: vec![TyKind::Handle, TyKind::I32],
        ret: TyKind::Result(Box::new(TyKind::I32), Box::new(TyKind::I32)),
    };
    let vec_i32_get_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::I32,
            TyKind::ResultOut(Box::new(TyKind::I32), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::I32), Box::new(TyKind::I32)),
    };
    let vec_i32_set = FnSig {
        params: vec![TyKind::Handle, TyKind::I32, TyKind::I32],
        ret: TyKind::Result(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_i32_set_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::I32,
            TyKind::I32,
            TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_i32_push = FnSig {
        params: vec![TyKind::Handle, TyKind::I32],
        ret: TyKind::Result(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_i32_push_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::I32,
            TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_i32_pop = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Result(Box::new(TyKind::I32), Box::new(TyKind::I32)),
    };
    let vec_i32_pop_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::ResultOut(Box::new(TyKind::I32), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::I32), Box::new(TyKind::I32)),
    };
    let vec_i32_free = FnSig {
        params: vec![TyKind::Handle, TyKind::Handle],
        ret: TyKind::Unit,
    };
    let vec_string_len = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::I32,
    };
    let vec_string_get = FnSig {
        params: vec![TyKind::Handle, TyKind::I32],
        ret: TyKind::Result(Box::new(TyKind::String), Box::new(TyKind::I32)),
    };
    let vec_string_get_abi = FnSig {
        params: vec![TyKind::Handle, TyKind::I32, TyKind::ResultString],
        ret: TyKind::ResultString,
    };
    let vec_string_push = FnSig {
        params: vec![TyKind::Handle, TyKind::String],
        ret: TyKind::Result(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_string_push_abi = FnSig {
        params: vec![
            TyKind::Handle,
            TyKind::String,
            TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
        ],
        ret: TyKind::ResultOut(Box::new(TyKind::Unit), Box::new(TyKind::I32)),
    };
    let vec_string_pop = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Result(Box::new(TyKind::String), Box::new(TyKind::I32)),
    };
    let vec_string_pop_abi = FnSig {
        params: vec![TyKind::Handle, TyKind::ResultString],
        ret: TyKind::ResultString,
    };
    let vec_string_free = FnSig {
        params: vec![TyKind::Handle, TyKind::Handle],
        ret: TyKind::Unit,
    };

    map.insert(
        "sys.system.console".to_string(),
        FnInfo {
            sig: system_console,
            abi_sig: None,
            symbol: "capable_rt_system_console".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.fs_read".to_string(),
        FnInfo {
            sig: system_fs_read,
            abi_sig: None,
            symbol: "capable_rt_system_fs_read".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.println".to_string(),
        FnInfo {
            sig: console_println,
            abi_sig: None,
            symbol: "capable_rt_console_println".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.print".to_string(),
        FnInfo {
            sig: console_print,
            abi_sig: None,
            symbol: "capable_rt_console_print".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.print_i32".to_string(),
        FnInfo {
            sig: console_print_i32.clone(),
            abi_sig: None,
            symbol: "capable_rt_console_print_i32".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.println_i32".to_string(),
        FnInfo {
            sig: console_print_i32,
            abi_sig: None,
            symbol: "capable_rt_console_println_i32".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.read_to_string".to_string(),
        FnInfo {
            sig: fs_read_to_string,
            abi_sig: Some(fs_read_to_string_abi),
            symbol: "capable_rt_fs_read_to_string".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.malloc".to_string(),
        FnInfo {
            sig: mem_malloc,
            abi_sig: None,
            symbol: "capable_rt_malloc".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.free".to_string(),
        FnInfo {
            sig: mem_free,
            abi_sig: None,
            symbol: "capable_rt_free".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.cast_u8_to_u32".to_string(),
        FnInfo {
            sig: mem_cast.clone(),
            abi_sig: None,
            symbol: "capable_rt_cast_u8_to_u32".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.cast_u32_to_u8".to_string(),
        FnInfo {
            sig: mem_cast,
            abi_sig: None,
            symbol: "capable_rt_cast_u32_to_u8".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.alloc_default".to_string(),
        FnInfo {
            sig: mem_alloc_default.clone(),
            abi_sig: None,
            symbol: "capable_rt_alloc_default".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.alloc_default".to_string(),
        FnInfo {
            sig: mem_alloc_default,
            abi_sig: None,
            symbol: "capable_rt_alloc_default".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.slice_from_ptr".to_string(),
        FnInfo {
            sig: mem_slice_from_ptr.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_from_ptr".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.mut_slice_from_ptr".to_string(),
        FnInfo {
            sig: mem_slice_from_ptr,
            abi_sig: None,
            symbol: "capable_rt_mut_slice_from_ptr".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.slice_len".to_string(),
        FnInfo {
            sig: mem_slice_len.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.slice_len".to_string(),
        FnInfo {
            sig: mem_slice_len.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.bytes.len".to_string(),
        FnInfo {
            sig: mem_slice_len,
            abi_sig: None,
            symbol: "capable_rt_slice_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.slice_at".to_string(),
        FnInfo {
            sig: mem_slice_at.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_at".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.slice_at".to_string(),
        FnInfo {
            sig: mem_slice_at.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_at".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.bytes.at".to_string(),
        FnInfo {
            sig: mem_slice_at.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_at".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.bytes.is_whitespace".to_string(),
        FnInfo {
            sig: FnSig {
                params: vec![TyKind::U8],
                ret: TyKind::Bool,
            },
            abi_sig: None,
            symbol: "capable_rt_bytes_is_whitespace".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.mut_slice_at".to_string(),
        FnInfo {
            sig: mem_slice_at.clone(),
            abi_sig: None,
            symbol: "capable_rt_mut_slice_at".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.mut_slice_at".to_string(),
        FnInfo {
            sig: mem_slice_at,
            abi_sig: None,
            symbol: "capable_rt_mut_slice_at".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.buffer_new".to_string(),
        FnInfo {
            sig: mem_buffer_new.clone(),
            abi_sig: Some(mem_buffer_new_abi.clone()),
            symbol: "capable_rt_buffer_new".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.buffer_new".to_string(),
        FnInfo {
            sig: mem_buffer_new,
            abi_sig: Some(mem_buffer_new_abi),
            symbol: "capable_rt_buffer_new".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.buffer_len".to_string(),
        FnInfo {
            sig: mem_buffer_len.clone(),
            abi_sig: None,
            symbol: "capable_rt_buffer_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.buffer_len".to_string(),
        FnInfo {
            sig: mem_buffer_len,
            abi_sig: None,
            symbol: "capable_rt_buffer_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.buffer_push".to_string(),
        FnInfo {
            sig: mem_buffer_push.clone(),
            abi_sig: Some(mem_buffer_push_abi.clone()),
            symbol: "capable_rt_buffer_push".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.buffer_push".to_string(),
        FnInfo {
            sig: mem_buffer_push,
            abi_sig: Some(mem_buffer_push_abi),
            symbol: "capable_rt_buffer_push".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.buffer_free".to_string(),
        FnInfo {
            sig: mem_buffer_free.clone(),
            abi_sig: None,
            symbol: "capable_rt_buffer_free".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.buffer_free".to_string(),
        FnInfo {
            sig: mem_buffer_free,
            abi_sig: None,
            symbol: "capable_rt_buffer_free".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.buffer_as_slice".to_string(),
        FnInfo {
            sig: mem_buffer_as_slice.clone(),
            abi_sig: None,
            symbol: "capable_rt_buffer_as_slice".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.buffer_as_slice".to_string(),
        FnInfo {
            sig: mem_buffer_as_slice.clone(),
            abi_sig: None,
            symbol: "capable_rt_buffer_as_slice".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.mem.buffer_as_mut_slice".to_string(),
        FnInfo {
            sig: mem_buffer_as_slice.clone(),
            abi_sig: None,
            symbol: "capable_rt_buffer_as_mut_slice".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.buffer_as_mut_slice".to_string(),
        FnInfo {
            sig: mem_buffer_as_slice,
            abi_sig: None,
            symbol: "capable_rt_buffer_as_mut_slice".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.args.len".to_string(),
        FnInfo {
            sig: args_len,
            abi_sig: None,
            symbol: "capable_rt_args_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.args.at".to_string(),
        FnInfo {
            sig: args_at,
            abi_sig: Some(args_at_abi),
            symbol: "capable_rt_args_at".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.io.read_stdin_to_string".to_string(),
        FnInfo {
            sig: io_read_stdin,
            abi_sig: Some(io_read_stdin_abi),
            symbol: "capable_rt_read_stdin_to_string".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.len".to_string(),
        FnInfo {
            sig: string_len,
            abi_sig: None,
            symbol: "capable_rt_string_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.byte_at".to_string(),
        FnInfo {
            sig: string_byte_at,
            abi_sig: None,
            symbol: "capable_rt_string_byte_at".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.as_slice".to_string(),
        FnInfo {
            sig: string_as_slice.clone(),
            abi_sig: None,
            symbol: "capable_rt_string_as_slice".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.bytes".to_string(),
        FnInfo {
            sig: string_as_slice,
            abi_sig: None,
            symbol: "capable_rt_string_as_slice".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.split_whitespace".to_string(),
        FnInfo {
            sig: string_split,
            abi_sig: None,
            symbol: "capable_rt_string_split_whitespace".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.split".to_string(),
        FnInfo {
            sig: string_split_delim,
            abi_sig: None,
            symbol: "capable_rt_string_split".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_u8_new".to_string(),
        FnInfo {
            sig: vec_new.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_u8_new".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_u8_len".to_string(),
        FnInfo {
            sig: vec_len.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_u8_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_u8_get".to_string(),
        FnInfo {
            sig: vec_u8_get.clone(),
            abi_sig: Some(vec_u8_get_abi),
            symbol: "capable_rt_vec_u8_get".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_u8_set".to_string(),
        FnInfo {
            sig: vec_u8_set.clone(),
            abi_sig: Some(vec_u8_set_abi),
            symbol: "capable_rt_vec_u8_set".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_u8_push".to_string(),
        FnInfo {
            sig: vec_u8_push.clone(),
            abi_sig: Some(vec_u8_push_abi),
            symbol: "capable_rt_vec_u8_push".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_u8_pop".to_string(),
        FnInfo {
            sig: vec_u8_pop.clone(),
            abi_sig: Some(vec_u8_pop_abi),
            symbol: "capable_rt_vec_u8_pop".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_u8_as_slice".to_string(),
        FnInfo {
            sig: vec_u8_as_slice,
            abi_sig: None,
            symbol: "capable_rt_vec_u8_as_slice".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_u8_free".to_string(),
        FnInfo {
            sig: vec_u8_free,
            abi_sig: None,
            symbol: "capable_rt_vec_u8_free".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_i32_new".to_string(),
        FnInfo {
            sig: vec_new.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_i32_new".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_i32_len".to_string(),
        FnInfo {
            sig: vec_len.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_i32_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_i32_get".to_string(),
        FnInfo {
            sig: vec_i32_get.clone(),
            abi_sig: Some(vec_i32_get_abi),
            symbol: "capable_rt_vec_i32_get".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_i32_set".to_string(),
        FnInfo {
            sig: vec_i32_set.clone(),
            abi_sig: Some(vec_i32_set_abi),
            symbol: "capable_rt_vec_i32_set".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_i32_push".to_string(),
        FnInfo {
            sig: vec_i32_push.clone(),
            abi_sig: Some(vec_i32_push_abi),
            symbol: "capable_rt_vec_i32_push".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_i32_pop".to_string(),
        FnInfo {
            sig: vec_i32_pop.clone(),
            abi_sig: Some(vec_i32_pop_abi),
            symbol: "capable_rt_vec_i32_pop".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_i32_free".to_string(),
        FnInfo {
            sig: vec_i32_free,
            abi_sig: None,
            symbol: "capable_rt_vec_i32_free".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_string_new".to_string(),
        FnInfo {
            sig: vec_new,
            abi_sig: None,
            symbol: "capable_rt_vec_string_new".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_string_len".to_string(),
        FnInfo {
            sig: vec_string_len,
            abi_sig: None,
            symbol: "capable_rt_vec_string_len".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_string_get".to_string(),
        FnInfo {
            sig: vec_string_get,
            abi_sig: Some(vec_string_get_abi),
            symbol: "capable_rt_vec_string_get".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_string_push".to_string(),
        FnInfo {
            sig: vec_string_push,
            abi_sig: Some(vec_string_push_abi),
            symbol: "capable_rt_vec_string_push".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_string_pop".to_string(),
        FnInfo {
            sig: vec_string_pop,
            abi_sig: Some(vec_string_pop_abi),
            symbol: "capable_rt_vec_string_pop".to_string(),
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.vec_string_free".to_string(),
        FnInfo {
            sig: vec_string_free,
            abi_sig: None,
            symbol: "capable_rt_vec_string_free".to_string(),
            is_runtime: true,
        },
    );

    let _ = ptr_ty;
}

fn register_user_functions(
    module: &AstModule,
    entry: &AstModule,
    stdlib: &StdlibIndex,
    enum_index: &EnumIndex,
    map: &mut HashMap<String, FnInfo>,
    _ptr_ty: Type,
) -> Result<(), CodegenError> {
    let module_name = module.name.to_string();
    let use_map = UseMap::new(module);
    for item in &module.items {
        match item {
            Item::Function(func) => {
                let sig = FnSig {
                    params: func
                        .params
                        .iter()
                        .map(|p| lower_ty(&p.ty, &use_map, stdlib, enum_index))
                        .collect::<Result<Vec<TyKind>, CodegenError>>()?,
                    ret: lower_ty(&func.ret, &use_map, stdlib, enum_index)?,
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
                        abi_sig: None,
                        symbol,
                        is_runtime: false,
                    },
                );
            }
            Item::ExternFunction(func) => {
                let sig = FnSig {
                    params: func
                        .params
                        .iter()
                        .map(|p| lower_ty(&p.ty, &use_map, stdlib, enum_index))
                        .collect::<Result<Vec<TyKind>, CodegenError>>()?,
                    ret: lower_ty(&func.ret, &use_map, stdlib, enum_index)?,
                };
                let key = format!("{module_name}.{}", func.name.item);
                map.insert(
                    key,
                    FnInfo {
                        sig,
                        abi_sig: None,
                        symbol: func.name.item.clone(),
                        is_runtime: true,
                    },
                );
            }
            _ => {}
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
    locals: &mut HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    use_map: &UseMap,
    module_name: &str,
    stdlib: &StdlibIndex,
    enum_index: &EnumIndex,
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
                enum_index,
                module,
                data_counter,
            )?;
            let local = store_local(builder, value);
            locals.insert(let_stmt.name.item.clone(), local);
        }
        Stmt::Assign(assign) => {
            let value = emit_expr(
                builder,
                &assign.expr,
                locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                enum_index,
                module,
                data_counter,
            )?;
            let Some(local) = locals.get_mut(&assign.name.item) else {
                return Err(CodegenError::UnknownVariable(assign.name.item.clone()));
            };
            match local {
                LocalValue::Slot(slot, _ty) => {
                    let ValueRepr::Single(val) = value else {
                        return Err(CodegenError::Unsupported("assignment".to_string()));
                    };
                    builder.ins().stack_store(val, *slot, 0);
                }
                LocalValue::Value(_) => {
                    return Err(CodegenError::Unsupported("assignment".to_string()));
                }
            }
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
                    enum_index,
                    module,
                    data_counter,
                )?;
                match value {
                    ValueRepr::Unit => builder.ins().return_(&[]),
                    ValueRepr::Single(val) => builder.ins().return_(&[val]),
                    ValueRepr::Pair(_, _) | ValueRepr::Result { .. } => {
                        let values = flatten_value(&value);
                        builder.ins().return_(&values)
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
                    enum_index,
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
                    enum_index,
                    module,
                    data_counter,
                )?;
            }
        }
        Stmt::If(if_stmt) => {
            // Snapshot locals so branch-scoped lets don't leak.
            let saved_locals = locals.clone();

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
                enum_index,
                module,
                data_counter,
            )?;
            let cond_b1 = to_b1(builder, cond_val)?;
            builder
                .ins()
                .brif(cond_b1, then_block, &[], else_block, &[]);

            // THEN branch with its own locals
            builder.switch_to_block(then_block);
            let mut then_locals = saved_locals.clone();
            for stmt in &if_stmt.then_block.stmts {
                emit_stmt(
                    builder,
                    stmt,
                    &mut then_locals,
                    fn_map,
                    use_map,
                    module_name,
                    stdlib,
                    enum_index,
                    module,
                    data_counter,
                )?;
            }
            if !block_ends_with_return(&if_stmt.then_block) {
                builder.ins().jump(merge_block, &[]);
            }
            builder.seal_block(then_block);

            // ELSE branch with its own locals
            if let Some(else_block_ast) = &if_stmt.else_block {
                builder.switch_to_block(else_block);
                let mut else_locals = saved_locals.clone();
                for stmt in &else_block_ast.stmts {
                    emit_stmt(
                        builder,
                        stmt,
                        &mut else_locals,
                        fn_map,
                        use_map,
                        module_name,
                        stdlib,
                        enum_index,
                        module,
                        data_counter,
                    )?;
                }
                if !block_ends_with_return(else_block_ast) {
                    builder.ins().jump(merge_block, &[]);
                }
                builder.seal_block(else_block);
            }

            // After the if, restore the pre-if locals snapshot.
            *locals = saved_locals;

            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);
        }

        Stmt::While(while_stmt) => {
            // Snapshot locals so loop-body lets don't leak out of the loop.
            let saved_locals = locals.clone();

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
                enum_index,
                module,
                data_counter,
            )?;
            let cond_b1 = to_b1(builder, cond_val)?;
            builder
                .ins()
                .brif(cond_b1, body_block, &[], exit_block, &[]);

            builder.switch_to_block(body_block);

            // Loop body gets its own locals (starts from the pre-loop snapshot each iteration in terms of
            // compilation scope; runtime mutations of existing vars still work because Assign targets Slot).
            let mut body_locals = saved_locals.clone();
            for stmt in &while_stmt.body.stmts {
                emit_stmt(
                    builder,
                    stmt,
                    &mut body_locals,
                    fn_map,
                    use_map,
                    module_name,
                    stdlib,
                    enum_index,
                    module,
                    data_counter,
                )?;
            }

            if !block_ends_with_return(&while_stmt.body) {
                builder.ins().jump(header_block, &[]);
            }

            builder.seal_block(body_block);
            builder.seal_block(header_block);

            builder.switch_to_block(exit_block);
            builder.seal_block(exit_block);

            // After the loop, restore the pre-loop locals snapshot.
            *locals = saved_locals;
        }
    }
    Ok(())
}

fn block_ends_with_return(block: &crate::ast::Block) -> bool {
    matches!(block.stmts.last(), Some(Stmt::Return(_)))
}

fn emit_expr(
    builder: &mut FunctionBuilder,
    expr: &Expr,
    locals: &HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    use_map: &UseMap,
    module_name: &str,
    stdlib: &StdlibIndex,
    enum_index: &EnumIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    match expr {
        Expr::Literal(lit) => match &lit.value {
            Literal::Int(value) => Ok(ValueRepr::Single(
                builder.ins().iconst(ir::types::I32, *value as i64),
            )),
            Literal::U8(value) => Ok(ValueRepr::Single(
                builder.ins().iconst(ir::types::I8, *value as i64),
            )),
            Literal::Bool(value) => Ok(ValueRepr::Single(
                builder.ins().iconst(ir::types::I8, *value as i64),
            )),
            Literal::String(value) => emit_string(builder, value, module, data_counter),
            Literal::Unit => Ok(ValueRepr::Unit),
        },
        Expr::Path(path) => {
            if path.segments.len() == 1 {
                if let Some(value) = locals.get(&path.segments[0].item) {
                    return Ok(load_local(builder, value));
                }
            }
            if let Some(index) = resolve_enum_variant(path, use_map, enum_index) {
                return Ok(ValueRepr::Single(
                    builder.ins().iconst(ir::types::I32, i64::from(index)),
                ));
            }
            Err(CodegenError::UnknownVariable(path.to_string()))
        }
        Expr::Call(call) => {
            let callee_path = match &*call.callee {
                Expr::Path(path) => resolve_call_path(path, module_name, use_map),
                _ => return Err(CodegenError::Unsupported("call target".to_string())),
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
                    enum_index,
                    module,
                    data_counter,
                )?;
                args.extend(flatten_value(&value));
            }
            let mut out_slots = None;
            let mut result_out = None;
            let abi_sig = info.abi_sig.as_ref().unwrap_or(&info.sig);
            if abi_sig.ret == TyKind::ResultString {
                let ptr_ty = module.isa().pointer_type();

                let slot_ptr = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    ptr_ty.bytes() as u32,
                ));
                let slot_len = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    8,
                ));
                let slot_err = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    4,
                ));

                let ptr_ptr = builder.ins().stack_addr(ptr_ty, slot_ptr, 0);
                let len_ptr = builder.ins().stack_addr(ptr_ty, slot_len, 0);
                let err_ptr = builder.ins().stack_addr(ptr_ty, slot_err, 0);

                args.push(ptr_ptr);
                args.push(len_ptr);
                args.push(err_ptr);

                out_slots = Some((slot_ptr, slot_len, slot_err));
            }
            if let TyKind::ResultOut(ok_ty, err_ty) = &abi_sig.ret {
                let ptr_ty = module.isa().pointer_type();
                let ok_slot = if **ok_ty == TyKind::Unit {
                    None
                } else {
                    let ty = value_type_for_result_out(ok_ty, ptr_ty)?;
                    let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        ty.bytes().max(1) as u32,
                    ));
                    let addr = builder.ins().stack_addr(ptr_ty, slot, 0);
                    args.push(addr);
                    Some((slot, ty))
                };
                let err_slot = if **err_ty == TyKind::Unit {
                    None
                } else {
                    let ty = value_type_for_result_out(err_ty, ptr_ty)?;
                    let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        ty.bytes().max(1) as u32,
                    ));
                    let addr = builder.ins().stack_addr(ptr_ty, slot, 0);
                    args.push(addr);
                    Some((slot, ty))
                };
                result_out = Some((ok_slot, err_slot, ok_ty.clone(), err_ty.clone()));
            }
            let sig = sig_to_clif(abi_sig, module.isa().pointer_type());
            let func_id = module
                .declare_function(
                    &info.symbol,
                    if info.is_runtime {
                        Linkage::Import
                    } else {
                        Linkage::Export
                    },
                    &sig,
                )
                .map_err(|err| CodegenError::Codegen(err.to_string()))?;
            let local = module.declare_func_in_func(func_id, builder.func);
            let call_inst = builder.ins().call(local, &args);
            let results = builder.inst_results(call_inst).to_vec();
            if abi_sig.ret == TyKind::ResultString {
                let tag = results
                    .get(0)
                    .ok_or_else(|| CodegenError::Codegen("missing result tag".to_string()))?;
                let (slot_ptr, slot_len, slot_err) =
                    out_slots.ok_or_else(|| CodegenError::Codegen("missing slots".to_string()))?;
                let ptr_addr = builder
                    .ins()
                    .stack_addr(module.isa().pointer_type(), slot_ptr, 0);
                let len_addr = builder
                    .ins()
                    .stack_addr(module.isa().pointer_type(), slot_len, 0);
                let err_addr = builder
                    .ins()
                    .stack_addr(module.isa().pointer_type(), slot_err, 0);
                let ptr =
                    builder
                        .ins()
                        .load(module.isa().pointer_type(), MemFlags::new(), ptr_addr, 0);
                let len = builder
                    .ins()
                    .load(ir::types::I64, MemFlags::new(), len_addr, 0);
                let err = builder
                    .ins()
                    .load(ir::types::I32, MemFlags::new(), err_addr, 0);
                match &info.sig.ret {
                    TyKind::Result(ok_ty, err_ty) => {
                        if **ok_ty != TyKind::String || **err_ty != TyKind::I32 {
                            return Err(CodegenError::Unsupported("result out params".to_string()));
                        }
                        Ok(ValueRepr::Result {
                            tag: *tag,
                            ok: Box::new(ValueRepr::Pair(ptr, len)),
                            err: Box::new(ValueRepr::Single(err)),
                        })
                    }
                    _ => Err(CodegenError::Unsupported("result out params".to_string())),
                }
            } else if let TyKind::ResultOut(_, _) = &abi_sig.ret {
                let tag = results
                    .get(0)
                    .ok_or_else(|| CodegenError::Codegen("missing result tag".to_string()))?;
                let (ok_slot, err_slot, ok_ty, err_ty) = result_out
                    .ok_or_else(|| CodegenError::Codegen("missing result slots".to_string()))?;
                let ok_val = if let Some((slot, ty)) = ok_slot {
                    let addr = builder
                        .ins()
                        .stack_addr(module.isa().pointer_type(), slot, 0);
                    let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
                    ValueRepr::Single(val)
                } else {
                    // ok_ty is Unit, so return ValueRepr::Unit (zero-sized)
                    ValueRepr::Unit
                };
                let err_val = if let Some((slot, ty)) = err_slot {
                    let addr = builder
                        .ins()
                        .stack_addr(module.isa().pointer_type(), slot, 0);
                    let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
                    ValueRepr::Single(val)
                } else {
                    // err_ty is Unit, so return ValueRepr::Unit (zero-sized)
                    ValueRepr::Unit
                };
                match &info.sig.ret {
                    TyKind::Result(_, _) => Ok(ValueRepr::Result {
                        tag: *tag,
                        ok: Box::new(ok_val),
                        err: Box::new(err_val),
                    }),
                    _ => Err(CodegenError::Unsupported(format!(
                        "result out params for {ok_ty:?}/{err_ty:?}"
                    ))),
                }
            } else {
                let mut index = 0;
                value_from_results(builder, &info.sig.ret, &results, &mut index)
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
                enum_index,
                module,
                data_counter,
            )?;
            if matches!(binary.op, BinaryOp::And | BinaryOp::Or) {
                let lhs_val = match lhs {
                    ValueRepr::Single(v) => v,
                    ValueRepr::Unit => {
                        return Err(CodegenError::Unsupported("boolean op on unit".to_string()))
                    }
                    ValueRepr::Pair(_, _) | ValueRepr::Result { .. } => {
                        return Err(CodegenError::Unsupported(
                            "boolean op on string".to_string(),
                        ))
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
                    enum_index,
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
                enum_index,
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
            enum_index,
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
                enum_index,
                module,
                data_counter,
            )?;
            match (&unary.op, value) {
                (UnaryOp::Neg, ValueRepr::Single(v)) => {
                    Ok(ValueRepr::Single(builder.ins().ineg(v)))
                }
                (UnaryOp::Not, ValueRepr::Single(v)) => {
                    let one = builder.ins().iconst(ir::types::I8, 1);
                    Ok(ValueRepr::Single(builder.ins().bxor(v, one)))
                }
                _ => Err(CodegenError::Unsupported("unary op".to_string())),
            }
        }
        Expr::Match(match_expr) => emit_match_expr(
            builder,
            match_expr,
            locals,
            fn_map,
            use_map,
            module_name,
            stdlib,
            enum_index,
            module,
            data_counter,
        ),
        Expr::StructLiteral(_) => Err(CodegenError::Unsupported("expression".to_string())),
    }
}

fn emit_match_stmt(
    builder: &mut FunctionBuilder,
    match_expr: &crate::ast::MatchExpr,
    locals: &mut HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    use_map: &UseMap,
    module_name: &str,
    stdlib: &StdlibIndex,
    enum_index: &EnumIndex,
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
        enum_index,
        module,
        data_counter,
    )?;

    // Special handling for matching on Unit
    let is_unit_match = matches!(value, ValueRepr::Unit);
    if is_unit_match {
        // When matching on unit, only wildcard (_), binding, or unit literal patterns are valid.
        // The typechecker should enforce this, but we validate in codegen as a safety check.
        for arm in &match_expr.arms {
            match &arm.pattern {
                Pattern::Wildcard(_) | Pattern::Binding(_) => {
                    // These are always valid for unit
                }
                Pattern::Path(path) if path.segments.len() == 1 => {
                    // Single-segment paths are effectively bindings in the current parser
                    // (used for match arm binding patterns like `match () { x => ... }`)
                }
                Pattern::Literal(Literal::Unit) => {
                    // Unit literal pattern is valid for unit
                }
                _ => {
                    return Err(CodegenError::Codegen(format!(
                        "Invalid pattern for unit match: {:?}. Only _, binding, or () patterns are allowed.",
                        arm.pattern
                    )));
                }
            }
        }
    }

    let (match_val, match_result) = match value.clone() {
        ValueRepr::Single(v) => (v, None),
        ValueRepr::Result { tag, ok, err } => (tag, Some((*ok, *err))),
        // Unit has only one value, so we use a dummy for pattern matching.
        // Pattern validation above ensures only wildcard/binding/unit-literal patterns are used.
        // Use I32 to match enum variant dispatch type (most common case).
        ValueRepr::Unit => (builder.ins().iconst(ir::types::I32, 0), None),
        ValueRepr::Pair(_, _) => {
            return Err(CodegenError::Unsupported("match on string".to_string()))
        }
    };

    let merge_block = builder.create_block();
    let mut current_block = builder
        .current_block()
        .ok_or_else(|| CodegenError::Codegen("no current block for match".to_string()))?;

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
        let cond = match_pattern_cond(builder, &arm.pattern, match_val, use_map, enum_index)?;
        builder.ins().brif(cond, arm_block, &[], next_block, &[]);

        builder.switch_to_block(arm_block);
        bind_match_pattern_value(builder, &arm.pattern, &value, match_result.as_ref(), locals)?;
        for stmt in &arm.body.stmts {
            emit_stmt(
                builder,
                stmt,
                locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                enum_index,
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

fn emit_match_expr(
    builder: &mut FunctionBuilder,
    match_expr: &crate::ast::MatchExpr,
    locals: &HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    use_map: &UseMap,
    module_name: &str,
    stdlib: &StdlibIndex,
    enum_index: &EnumIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    let value = emit_expr(
        builder,
        &match_expr.expr,
        locals,
        fn_map,
        use_map,
        module_name,
        stdlib,
        enum_index,
        module,
        data_counter,
    )?;

    // Special handling for matching on Unit
    let is_unit_match = matches!(value, ValueRepr::Unit);
    if is_unit_match {
        // When matching on unit, only wildcard (_), binding, or unit literal patterns are valid.
        // The typechecker should enforce this, but we validate in codegen as a safety check.
        for arm in &match_expr.arms {
            match &arm.pattern {
                Pattern::Wildcard(_) | Pattern::Binding(_) => {
                    // These are always valid for unit
                }
                Pattern::Path(path) if path.segments.len() == 1 => {
                    // Single-segment paths are effectively bindings in the current parser
                    // (used for match arm binding patterns like `match () { x => ... }`)
                }
                Pattern::Literal(Literal::Unit) => {
                    // Unit literal pattern is valid for unit
                }
                _ => {
                    return Err(CodegenError::Codegen(format!(
                        "Invalid pattern for unit match: {:?}. Only _, binding, or () patterns are allowed.",
                        arm.pattern
                    )));
                }
            }
        }
    }

    let (match_val, match_result) = match value.clone() {
        ValueRepr::Single(v) => (v, None),
        ValueRepr::Result { tag, ok, err } => (tag, Some((*ok, *err))),
        // Unit has only one value, so we use a dummy for pattern matching.
        // Pattern validation above ensures only wildcard/binding/unit-literal patterns are used.
        // Use I32 to match enum variant dispatch type (most common case).
        ValueRepr::Unit => (builder.ins().iconst(ir::types::I32, 0), None),
        ValueRepr::Pair(_, _) => {
            return Err(CodegenError::Unsupported("match on string".to_string()))
        }
    };

    let merge_block = builder.create_block();
    let mut current_block = builder
        .current_block()
        .ok_or_else(|| CodegenError::Codegen("no current block for match".to_string()))?;

    let mut result_shape: Option<ResultShape> = None;

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
        let cond = match_pattern_cond(builder, &arm.pattern, match_val, use_map, enum_index)?;
        builder.ins().brif(cond, arm_block, &[], next_block, &[]);

        builder.switch_to_block(arm_block);
        let mut arm_locals = locals.clone();
        bind_match_pattern_value(
            builder,
            &arm.pattern,
            &value,
            match_result.as_ref(),
            &mut arm_locals,
        )?;
        let Some((last, prefix)) = arm.body.stmts.split_last() else {
            return Err(CodegenError::Unsupported("empty match arm".to_string()));
        };
        for stmt in prefix {
            emit_stmt(
                builder,
                stmt,
                &mut arm_locals,
                fn_map,
                use_map,
                module_name,
                stdlib,
                enum_index,
                module,
                data_counter,
            )?;
        }
        let Stmt::Expr(expr_stmt) = last else {
            return Err(CodegenError::Unsupported(
                "match arm must end with expression".to_string(),
            ));
        };
        let arm_value = emit_expr(
            builder,
            &expr_stmt.expr,
            &arm_locals,
            fn_map,
            use_map,
            module_name,
            stdlib,
            enum_index,
            module,
            data_counter,
        )?;
        let values = match &arm_value {
            ValueRepr::Single(val) => vec![*val],
            ValueRepr::Pair(a, b) => vec![*a, *b],
            ValueRepr::Unit => vec![],
            ValueRepr::Result { .. } => {
                return Err(CodegenError::Unsupported("match result value".to_string()))
            }
        };
        if result_shape.is_none() {
            let mut types = Vec::new();
            let mut slots = Vec::new();
            for val in &values {
                let ty = builder.func.dfg.value_type(*val);
                let size = ty.bytes() as u32;
                let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    size.max(1),
                ));
                types.push(ty);
                slots.push(slot);
            }
            result_shape = Some(ResultShape {
                kind: match &arm_value {
                    ValueRepr::Unit => ResultKind::Unit,
                    ValueRepr::Single(_) => ResultKind::Single,
                    ValueRepr::Pair(_, _) => ResultKind::Pair,
                    _ => ResultKind::Single,
                },
                slots,
                types,
            });
        }
        let shape = result_shape
            .as_ref()
            .ok_or_else(|| CodegenError::Codegen("missing match result shape".to_string()))?;
        if values.len() != shape.types.len() {
            return Err(CodegenError::Unsupported(
                "mismatched match arm".to_string(),
            ));
        }
        for (idx, val) in values.iter().enumerate() {
            builder.ins().stack_store(*val, shape.slots[idx], 0);
        }
        builder.ins().jump(merge_block, &[]);
        builder.seal_block(arm_block);

        if is_last {
            break;
        }
        current_block = next_block;
    }

    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);
    let shape = result_shape
        .ok_or_else(|| CodegenError::Codegen("missing match result value".to_string()))?;
    let mut loaded = Vec::new();
    for (slot, ty) in shape.slots.iter().zip(shape.types.iter()) {
        loaded.push(builder.ins().stack_load(*ty, *slot, 0));
    }
    let result = match shape.kind {
        ResultKind::Unit => ValueRepr::Unit,
        ResultKind::Single => ValueRepr::Single(loaded[0]),
        ResultKind::Pair => ValueRepr::Pair(loaded[0], loaded[1]),
    };
    Ok(result)
}

fn match_pattern_cond(
    builder: &mut FunctionBuilder,
    pattern: &Pattern,
    match_val: ir::Value,
    use_map: &UseMap,
    enum_index: &EnumIndex,
) -> Result<ir::Value, CodegenError> {
    match pattern {
        Pattern::Wildcard(_) | Pattern::Binding(_) => {
            // Wildcard and binding patterns always match - return a constant true condition
            let one = builder.ins().iconst(ir::types::I32, 1);
            Ok(builder.ins().icmp_imm(IntCC::Equal, one, 1))
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
            Literal::U8(value) => {
                let rhs = builder.ins().iconst(ir::types::I8, *value as i64);
                Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
            }
            _ => Err(CodegenError::Unsupported("literal pattern".to_string())),
        },
        Pattern::Call { path, .. } => {
            let name = path
                .segments
                .last()
                .map(|seg| seg.item.as_str())
                .unwrap_or("");
            let tag_value = if name == "Ok" {
                0
            } else if name == "Err" {
                1
            } else {
                2
            };
            if tag_value == 2 {
                return Err(CodegenError::Unsupported("call pattern".to_string()));
            }
            let rhs = builder.ins().iconst(ir::types::I8, tag_value);
            Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
        }
        Pattern::Path(path) => {
            // Single-segment paths are binding patterns (always match)
            if path.segments.len() == 1 {
                // Return a constant true condition
                let one = builder.ins().iconst(ir::types::I32, 1);
                return Ok(builder.ins().icmp_imm(IntCC::Equal, one, 1));
            }
            // Multi-segment paths are enum variant patterns
            let Some(index) = resolve_enum_variant(path, use_map, enum_index) else {
                return Err(CodegenError::Unsupported("path pattern".to_string()));
            };
            let rhs = builder.ins().iconst(ir::types::I32, i64::from(index));
            Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
        }
    }
}

fn bind_match_pattern_value(
    builder: &mut FunctionBuilder,
    pattern: &Pattern,
    value: &ValueRepr,
    result: Option<&(ValueRepr, ValueRepr)>,
    locals: &mut HashMap<String, LocalValue>,
) -> Result<(), CodegenError> {
    match pattern {
        Pattern::Binding(ident) => {
            // Bind any value type - store_local handles all ValueRepr variants
            locals.insert(ident.item.clone(), store_local(builder, value.clone()));
            Ok(())
        }
        Pattern::Path(path) if path.segments.len() == 1 => {
            // Single-segment paths are used as binding patterns by the parser
            // (e.g., `match () { x => ... }`)
            let ident = &path.segments[0];
            // Bind any value type - store_local handles all ValueRepr variants
            locals.insert(ident.item.clone(), store_local(builder, value.clone()));
            Ok(())
        }
        Pattern::Call { path, binding, .. } => {
            let name = path
                .segments
                .last()
                .map(|seg| seg.item.as_str())
                .unwrap_or("");
            let Some((ok_val, err_val)) = result else {
                return Err(CodegenError::Unsupported("call pattern".to_string()));
            };
            if let Some(binding) = binding {
                if name == "Ok" {
                    locals.insert(binding.item.clone(), store_local(builder, ok_val.clone()));
                    return Ok(());
                }
                if name == "Err" {
                    locals.insert(binding.item.clone(), store_local(builder, err_val.clone()));
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
        ValueRepr::Unit => Err(CodegenError::Unsupported("unit condition".to_string())),
        ValueRepr::Pair(_, _) | ValueRepr::Result { .. } => {
            Err(CodegenError::Unsupported("string condition".to_string()))
        }
    }
}

fn bool_to_i8(builder: &mut FunctionBuilder, value: ir::Value) -> ir::Value {
    let ty = builder.func.dfg.value_type(value);
    if ty == ir::types::I8 {
        value
    } else {
        builder.ins().uextend(ir::types::I8, value)
    }
}

fn emit_short_circuit_expr(
    builder: &mut FunctionBuilder,
    lhs: ir::Value,
    rhs_expr: &Expr,
    is_and: bool,
    locals: &HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    use_map: &UseMap,
    module_name: &str,
    stdlib: &StdlibIndex,
    enum_index: &EnumIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    let rhs_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, ir::types::I8);

    let lhs_cond = builder.ins().icmp_imm(IntCC::NotEqual, lhs, 0);
    if is_and {
        builder
            .ins()
            .brif(lhs_cond, rhs_block, &[], merge_block, &[lhs]);
    } else {
        builder
            .ins()
            .brif(lhs_cond, merge_block, &[lhs], rhs_block, &[]);
    }
    let _current = builder
        .current_block()
        .ok_or_else(|| CodegenError::Codegen("no block for short-circuit".to_string()))?;

    builder.switch_to_block(rhs_block);
    let rhs_value = emit_expr(
        builder,
        rhs_expr,
        locals,
        fn_map,
        use_map,
        module_name,
        stdlib,
        enum_index,
        module,
        data_counter,
    )?;
    let rhs_val = match rhs_value {
        ValueRepr::Single(v) => v,
        ValueRepr::Unit => return Err(CodegenError::Unsupported("boolean op on unit".to_string())),
        ValueRepr::Pair(_, _) | ValueRepr::Result { .. } => {
            return Err(CodegenError::Unsupported(
                "boolean op on string".to_string(),
            ))
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
    let ptr = builder
        .ins()
        .global_value(module.isa().pointer_type(), global);
    let len = builder.ins().iconst(ir::types::I64, value.len() as i64);
    Ok(ValueRepr::Pair(ptr, len))
}

fn flatten_value(value: &ValueRepr) -> Vec<ir::Value> {
    match value {
        ValueRepr::Unit => vec![],
        ValueRepr::Single(val) => vec![*val],
        ValueRepr::Pair(a, b) => vec![*a, *b],
        ValueRepr::Result { tag, ok, err } => {
            let mut out = vec![*tag];
            out.extend(flatten_value(ok));
            out.extend(flatten_value(err));
            out
        }
    }
}

fn store_local(builder: &mut FunctionBuilder, value: ValueRepr) -> LocalValue {
    match value {
        ValueRepr::Unit => LocalValue::Value(ValueRepr::Unit),
        ValueRepr::Single(val) => {
            let ty = builder.func.dfg.value_type(val);
            let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                ty.bytes().max(1) as u32,
            ));
            builder.ins().stack_store(val, slot, 0);
            LocalValue::Slot(slot, ty)
        }
        other => LocalValue::Value(other),
    }
}

fn load_local(builder: &mut FunctionBuilder, local: &LocalValue) -> ValueRepr {
    match local {
        LocalValue::Slot(slot, ty) => ValueRepr::Single(builder.ins().stack_load(*ty, *slot, 0)),
        LocalValue::Value(value) => value.clone(),
    }
}

fn value_type_for_result_out(ty: &TyKind, ptr_ty: Type) -> Result<ir::Type, CodegenError> {
    match ty {
        TyKind::I32 | TyKind::U32 => Ok(ir::types::I32),
        TyKind::U8 | TyKind::Bool => Ok(ir::types::I8),
        TyKind::Handle => Ok(ir::types::I64),
        TyKind::Ptr => Ok(ptr_ty),
        TyKind::Unit => Err(CodegenError::Unsupported("result out unit".to_string())),
        _ => Err(CodegenError::Unsupported("result out params".to_string())),
    }
}

fn value_from_params(
    builder: &mut FunctionBuilder,
    ty: &TyKind,
    params: &[ir::Value],
    idx: &mut usize,
) -> Result<ValueRepr, CodegenError> {
    match ty {
        TyKind::Unit => Ok(ValueRepr::Unit),
        TyKind::I32 | TyKind::U32 | TyKind::U8 | TyKind::Bool | TyKind::Handle | TyKind::Ptr => {
            let val = params[*idx];
            *idx += 1;
            Ok(ValueRepr::Single(val))
        }
        TyKind::String => {
            let ptr = params[*idx];
            let len = params[*idx + 1];
            *idx += 2;
            Ok(ValueRepr::Pair(ptr, len))
        }
        TyKind::Result(ok, err) => {
            let tag = params[*idx];
            *idx += 1;
            let ok_val = value_from_params(builder, ok, params, idx)?;
            let err_val = value_from_params(builder, err, params, idx)?;
            Ok(ValueRepr::Result {
                tag,
                ok: Box::new(ok_val),
                err: Box::new(err_val),
            })
        }
        // ResultOut and ResultString are ABI-level return types, not input types.
        // They should never appear as function parameters.
        TyKind::ResultOut(ok, err) => {
            Err(CodegenError::Codegen(format!(
                "ResultOut<{ok:?}, {err:?}> cannot be a parameter type (ABI return type only)"
            )))
        }
        TyKind::ResultString => {
            Err(CodegenError::Codegen(
                "ResultString cannot be a parameter type (ABI return type only)".to_string()
            ))
        }
    }
}

fn value_from_results(
    builder: &mut FunctionBuilder,
    ty: &TyKind,
    results: &[ir::Value],
    idx: &mut usize,
) -> Result<ValueRepr, CodegenError> {
    match ty {
        TyKind::Unit => Ok(ValueRepr::Unit),
        TyKind::I32 | TyKind::U32 | TyKind::U8 | TyKind::Bool | TyKind::Handle | TyKind::Ptr => {
            let val = results
                .get(*idx)
                .ok_or_else(|| CodegenError::Codegen("missing return value".to_string()))?;
            *idx += 1;
            Ok(ValueRepr::Single(*val))
        }
        TyKind::String => {
            if results.len() < *idx + 2 {
                return Err(CodegenError::Codegen("string return count".to_string()));
            }
            let ptr = results[*idx];
            let len = results[*idx + 1];
            *idx += 2;
            Ok(ValueRepr::Pair(ptr, len))
        }
        TyKind::Result(ok, err) => {
            let tag = results
                .get(*idx)
                .ok_or_else(|| CodegenError::Codegen("missing result tag".to_string()))?;
            *idx += 1;
            let ok_val = value_from_results(builder, ok, results, idx)?;
            let err_val = value_from_results(builder, err, results, idx)?;
            Ok(ValueRepr::Result {
                tag: *tag,
                ok: Box::new(ok_val),
                err: Box::new(err_val),
            })
        }
        TyKind::ResultOut(_, _) => Err(CodegenError::Unsupported("result out params".to_string())),
        TyKind::ResultString => Err(CodegenError::Unsupported("result abi".to_string())),
    }
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

fn resolve_enum_variant(path: &AstPath, use_map: &UseMap, enum_index: &EnumIndex) -> Option<i32> {
    let resolved = resolve_path(path, use_map);
    if resolved.len() < 2 {
        return None;
    }
    let (enum_path, variant) = resolved.split_at(resolved.len() - 1);
    let enum_name = enum_path.join(".");
    let info = enum_index.variants.get(&enum_name)?;
    info.get(&variant[0]).copied()
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

fn build_enum_index(
    entry: &AstModule,
    user_modules: &[AstModule],
    stdlib: &[AstModule],
) -> EnumIndex {
    let mut variants = HashMap::new();
    let entry_name = entry.name.to_string();
    let modules = stdlib
        .iter()
        .chain(user_modules.iter())
        .chain(std::iter::once(entry));
    for module in modules {
        let module_name = module.name.to_string();
        for item in &module.items {
            let Item::Enum(decl) = item else { continue };
            let mut map = HashMap::new();
            for (idx, variant) in decl.variants.iter().enumerate() {
                map.insert(variant.name.item.clone(), idx as i32);
            }
            let qualified = format!("{module_name}.{}", decl.name.item);
            variants.insert(qualified, map.clone());
            if module_name == entry_name {
                variants.insert(decl.name.item.clone(), map);
            }
        }
    }
    EnumIndex { variants }
}

fn lower_ty(
    ty: &AstType,
    use_map: &UseMap,
    stdlib: &StdlibIndex,
    enum_index: &EnumIndex,
) -> Result<TyKind, CodegenError> {
    let resolved = match ty {
        AstType::Ptr { .. } => {
            return Ok(TyKind::Ptr);
        }
        AstType::Path { path, .. } => resolve_type_name(path, use_map, stdlib),
    };
    if resolved == "i32" {
        return Ok(TyKind::I32);
    }
    if resolved == "u32" {
        return Ok(TyKind::U32);
    }
    if resolved == "u8" {
        return Ok(TyKind::U8);
    }
    if resolved == "bool" {
        return Ok(TyKind::Bool);
    }
    if resolved == "unit" {
        return Ok(TyKind::Unit);
    }
    if resolved == "string" {
        return Ok(TyKind::String);
    }
    if resolved == "Result" {
        if let AstType::Path { args, .. } = ty {
            if args.len() == 2 {
                let ok = lower_ty(&args[0], use_map, stdlib, enum_index)?;
                let err = lower_ty(&args[1], use_map, stdlib, enum_index)?;
                return Ok(TyKind::Result(Box::new(ok), Box::new(err)));
            }
        }
    }
    if resolved == "sys.system.System"
        || resolved == "sys.console.Console"
        || resolved == "sys.fs.ReadFS"
        || resolved == "sys.buffer.Alloc"
        || resolved == "sys.buffer.Buffer"
        || resolved == "sys.buffer.Slice"
        || resolved == "sys.buffer.MutSlice"
        || resolved == "sys.vec.VecU8"
        || resolved == "sys.vec.VecI32"
        || resolved == "sys.vec.VecString"
    {
        return Ok(TyKind::Handle);
    }
    if enum_index.variants.contains_key(&resolved) {
        return Ok(TyKind::I32);
    }
    return Err(CodegenError::Unsupported(format!(
        "unknown type `{resolved}`"
    )));
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
