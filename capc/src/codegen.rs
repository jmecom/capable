use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, AbiParam, Function, InstBuilder, MemFlags, Signature, Type, Value};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{Configurable, Flags};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, Linkage, Module as ModuleTrait};
use cranelift_native;
use cranelift_object::{ObjectBuilder, ObjectModule};
use thiserror::Error;

use crate::ast::{
    BinaryOp, CallExpr, Expr, Item, Literal, Module as AstModule, Path as AstPath, Pattern, Span,
    Stmt, Type as AstType, UnaryOp,
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

/// Tracks control flow state during code emission.
/// Used to stop emitting statements after a terminator (return/jump).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Flow {
    /// Control flow continues normally; more statements can be emitted.
    Continues,
    /// Block is terminated (return/unconditional jump); no more statements should be emitted.
    Terminated,
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
    runtime_symbol: Option<String>,
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
struct StructLayoutIndex {
    layouts: HashMap<String, StructLayout>,
}

#[derive(Clone, Debug)]
struct StructLayout {
    size: u32,
    align: u32,
    fields: HashMap<String, StructFieldLayout>,
    field_order: Vec<String>,
}

#[derive(Clone, Debug)]
struct StructFieldLayout {
    offset: u32,
    ty: crate::typeck::Ty,
}

#[derive(Clone, Copy, Debug)]
struct TypeLayout {
    size: u32,
    align: u32,
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
    StructSlot(ir::StackSlot, crate::typeck::Ty, u32),
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
    program: &crate::hir::HirProgram,
    entry_ast: &AstModule,
    user_modules_ast: &[AstModule],
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

    let stdlib_index = build_stdlib_index(&program.stdlib);
    let enum_index = build_enum_index(&program.entry, &program.user_modules, &program.stdlib);
    let struct_layouts = build_struct_layout_index(
        &program.entry,
        &program.user_modules,
        &program.stdlib,
        module.isa().pointer_type(),
    )?;

    let runtime_intrinsics = register_runtime_intrinsics(module.isa().pointer_type());
    let mut fn_map = HashMap::new();
    for module_ref in &program.stdlib {
        register_user_functions(
            module_ref,
            &program.entry,
            &stdlib_index,
            &enum_index,
            &struct_layouts,
            &mut fn_map,
            &runtime_intrinsics,
            module.isa().pointer_type(),
            true,
        )?;
    }
    let missing_intrinsics = runtime_intrinsics
        .keys()
        .filter(|key| !fn_map.contains_key(*key))
        .cloned()
        .collect::<Vec<_>>();
    if !missing_intrinsics.is_empty() {
        return Err(CodegenError::Codegen(format!(
            "runtime intrinsics missing stdlib wrappers: {}",
            missing_intrinsics.join(", ")
        )));
    }
    for module_ref in &program.user_modules {
        register_user_functions(
            module_ref,
            &program.entry,
            &stdlib_index,
            &enum_index,
            &struct_layouts,
            &mut fn_map,
            &runtime_intrinsics,
            module.isa().pointer_type(),
            false,
        )?;
    }
    register_user_functions(
        &program.entry,
        &program.entry,
        &stdlib_index,
        &enum_index,
        &struct_layouts,
        &mut fn_map,
        &runtime_intrinsics,
        module.isa().pointer_type(),
        false,
    )?;

    let all_modules = program.user_modules
        .iter()
        .chain(program.stdlib.iter())
        .chain(std::iter::once(&program.entry))
        .collect::<Vec<_>>();

    // Register extern functions from AST modules (not in HIR yet)
    let all_ast_modules = user_modules_ast
        .iter()
        .chain(std::iter::once(entry_ast))
        .collect::<Vec<_>>();
    for ast_module in &all_ast_modules {
        register_extern_functions(ast_module, &stdlib_index, &enum_index, &mut fn_map)?;
    }

    let mut data_counter = 0u32;
    for module_ref in &all_modules {
        let module_name = &module_ref.name;
        for func in &module_ref.functions {
            let key = format!("{}.{}", module_name, func.name);
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

            if info.runtime_symbol.is_some() {
                let args = builder.block_params(block).to_vec();
                let value = emit_runtime_wrapper_call(&mut builder, &mut module, &info, args)?;
                match value {
                    ValueRepr::Unit => builder.ins().return_(&[]),
                    ValueRepr::Single(val) => builder.ins().return_(&[val]),
                    ValueRepr::Pair(_, _) | ValueRepr::Result { .. } => {
                        let values = flatten_value(&value);
                        builder.ins().return_(&values)
                    }
                };
                builder.seal_all_blocks();
                builder.finalize();
                if let Err(err) = cranelift_codegen::verify_function(&ctx.func, module.isa()) {
                    eprintln!("=== IR for {} ===\n{}", func.name, ctx.func.display());
                    return Err(CodegenError::Codegen(format!("verifier errors: {err}")));
                }
                module
                    .define_function(func_id, &mut ctx)
                    .map_err(|err| CodegenError::Codegen(err.to_string()))?;
                continue;
            }

            let mut locals: HashMap<String, LocalValue> = HashMap::new();
            let params = builder.block_params(block).to_vec();
            let mut param_index = 0;
            for param in &func.params {
                // HirParam already has resolved types - use typeck_ty_to_tykind
                let ty_kind = typeck_ty_to_tykind(&param.ty, Some(&struct_layouts))?;
                let value =
                    value_from_params(&mut builder, &ty_kind, &params, &mut param_index)?;
                let local = store_local(&mut builder, value);
                // HirParam.name is String, not Spanned<String>
                locals.insert(param.name.clone(), local);
            }

            let mut terminated = false;
            for stmt in &func.body.stmts {
                // Use HIR emission functions
                let flow = emit_hir_stmt(
                    &mut builder,
                    stmt,
                    &mut locals,
                    &fn_map,
                    &enum_index,
                    &struct_layouts,
                    &mut module,
                    &mut data_counter,
                )?;
                if flow == Flow::Terminated {
                    terminated = true;
                    break;
                }
            }

            // Only add implicit unit return if function body doesn't terminate
            if info.sig.ret == TyKind::Unit && !terminated {
                builder.ins().return_(&[]);
            }

            builder.seal_all_blocks();
            builder.finalize();
            if let Err(err) = cranelift_codegen::verify_function(&ctx.func, module.isa()) {
                eprintln!("=== IR for {} ===\n{}", func.name, ctx.func.display());
                return Err(CodegenError::Codegen(format!("verifier errors: {err}")));
            }
            module
                .define_function(func_id, &mut ctx)
                .map_err(|err| CodegenError::Codegen(err.to_string()))?;
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

fn register_runtime_intrinsics(ptr_ty: Type) -> HashMap<String, FnInfo> {
    let mut map = HashMap::new();
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
        params: vec![TyKind::Handle, TyKind::I32],
        ret: TyKind::Ptr,
    };
    let mem_free = FnSig {
        params: vec![TyKind::Handle, TyKind::Ptr],
        ret: TyKind::Unit,
    };
    let mem_cast = FnSig {
        params: vec![TyKind::Handle, TyKind::Ptr],
        ret: TyKind::Ptr,
    };
    let mem_alloc_default = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Handle,
    };
    let system_mint_args = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Handle,
    };
    let system_mint_stdin = FnSig {
        params: vec![TyKind::Handle],
        ret: TyKind::Handle,
    };
    let mem_slice_from_ptr = FnSig {
        params: vec![TyKind::Handle, TyKind::Ptr, TyKind::I32],
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
        params: vec![TyKind::Handle],
        ret: TyKind::Result(Box::new(TyKind::String), Box::new(TyKind::I32)),
    };
    let io_read_stdin_abi = FnSig {
        params: vec![TyKind::Handle, TyKind::ResultString],
        ret: TyKind::ResultString,
    };
    let console_assert = FnSig {
        params: vec![TyKind::Handle, TyKind::Bool],
        ret: TyKind::Unit,
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
        "sys.system.RootCap__mint_console".to_string(),
        FnInfo {
            sig: system_console.clone(),
            abi_sig: None,
            symbol: "capable_rt_mint_console".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.RootCap__mint_readfs".to_string(),
        FnInfo {
            sig: system_fs_read.clone(),
            abi_sig: None,
            symbol: "capable_rt_mint_readfs".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.RootCap__mint_args".to_string(),
        FnInfo {
            sig: system_mint_args,
            abi_sig: None,
            symbol: "capable_rt_mint_args".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.RootCap__mint_stdin".to_string(),
        FnInfo {
            sig: system_mint_stdin,
            abi_sig: None,
            symbol: "capable_rt_mint_stdin".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.args.Args__len".to_string(),
        FnInfo {
            sig: args_len,
            abi_sig: None,
            symbol: "capable_rt_args_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.args.Args__at".to_string(),
        FnInfo {
            sig: args_at,
            abi_sig: Some(args_at_abi),
            symbol: "capable_rt_args_at".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.stdin.Stdin__read_to_string".to_string(),
        FnInfo {
            sig: io_read_stdin,
            abi_sig: Some(io_read_stdin_abi),
            symbol: "capable_rt_read_stdin_to_string".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.system.RootCap__mint_alloc_default".to_string(),
        FnInfo {
            sig: mem_alloc_default.clone(),
            abi_sig: None,
            symbol: "capable_rt_alloc_default".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.Console__println".to_string(),
        FnInfo {
            sig: console_println,
            abi_sig: None,
            symbol: "capable_rt_console_println".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.Console__print".to_string(),
        FnInfo {
            sig: console_print,
            abi_sig: None,
            symbol: "capable_rt_console_print".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.Console__print_i32".to_string(),
        FnInfo {
            sig: console_print_i32.clone(),
            abi_sig: None,
            symbol: "capable_rt_console_print_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.Console__println_i32".to_string(),
        FnInfo {
            sig: console_print_i32,
            abi_sig: None,
            symbol: "capable_rt_console_println_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.console.Console__assert".to_string(),
        FnInfo {
            sig: console_assert,
            abi_sig: None,
            symbol: "capable_rt_assert".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.ReadFS__read_to_string".to_string(),
        FnInfo {
            sig: fs_read_to_string,
            abi_sig: Some(fs_read_to_string_abi),
            symbol: "capable_rt_fs_read_to_string".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__buffer_new".to_string(),
        FnInfo {
            sig: mem_buffer_new.clone(),
            abi_sig: Some(mem_buffer_new_abi.clone()),
            symbol: "capable_rt_buffer_new".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__malloc".to_string(),
        FnInfo {
            sig: mem_malloc,
            abi_sig: None,
            symbol: "capable_rt_malloc".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__free".to_string(),
        FnInfo {
            sig: mem_free,
            abi_sig: None,
            symbol: "capable_rt_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__cast_u8_to_u32".to_string(),
        FnInfo {
            sig: mem_cast.clone(),
            abi_sig: None,
            symbol: "capable_rt_cast_u8_to_u32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__cast_u32_to_u8".to_string(),
        FnInfo {
            sig: mem_cast,
            abi_sig: None,
            symbol: "capable_rt_cast_u32_to_u8".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__slice_from_ptr".to_string(),
        FnInfo {
            sig: mem_slice_from_ptr.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_from_ptr".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__mut_slice_from_ptr".to_string(),
        FnInfo {
            sig: mem_slice_from_ptr,
            abi_sig: None,
            symbol: "capable_rt_mut_slice_from_ptr".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__buffer_free".to_string(),
        FnInfo {
            sig: mem_buffer_free,
            abi_sig: None,
            symbol: "capable_rt_buffer_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__len".to_string(),
        FnInfo {
            sig: mem_buffer_len,
            abi_sig: None,
            symbol: "capable_rt_buffer_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__push".to_string(),
        FnInfo {
            sig: mem_buffer_push,
            abi_sig: Some(mem_buffer_push_abi),
            symbol: "capable_rt_buffer_push".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__as_slice".to_string(),
        FnInfo {
            sig: mem_buffer_as_slice.clone(),
            abi_sig: None,
            symbol: "capable_rt_buffer_as_slice".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Buffer__as_mut_slice".to_string(),
        FnInfo {
            sig: mem_buffer_as_slice,
            abi_sig: None,
            symbol: "capable_rt_buffer_as_mut_slice".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Slice__len".to_string(),
        FnInfo {
            sig: mem_slice_len.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Slice__at".to_string(),
        FnInfo {
            sig: mem_slice_at.clone(),
            abi_sig: None,
            symbol: "capable_rt_slice_at".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.MutSlice__at".to_string(),
        FnInfo {
            sig: mem_slice_at,
            abi_sig: None,
            symbol: "capable_rt_mut_slice_at".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_u8_new".to_string(),
        FnInfo {
            sig: vec_new.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_u8_new".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_u8_free".to_string(),
        FnInfo {
            sig: vec_u8_free,
            abi_sig: None,
            symbol: "capable_rt_vec_u8_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_i32_new".to_string(),
        FnInfo {
            sig: vec_new.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_i32_new".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_i32_free".to_string(),
        FnInfo {
            sig: vec_i32_free,
            abi_sig: None,
            symbol: "capable_rt_vec_i32_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_string_new".to_string(),
        FnInfo {
            sig: vec_new,
            abi_sig: None,
            symbol: "capable_rt_vec_string_new".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.buffer.Alloc__vec_string_free".to_string(),
        FnInfo {
            sig: vec_string_free,
            abi_sig: None,
            symbol: "capable_rt_vec_string_free".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__len".to_string(),
        FnInfo {
            sig: vec_len.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_u8_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__get".to_string(),
        FnInfo {
            sig: vec_u8_get.clone(),
            abi_sig: Some(vec_u8_get_abi),
            symbol: "capable_rt_vec_u8_get".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__set".to_string(),
        FnInfo {
            sig: vec_u8_set.clone(),
            abi_sig: Some(vec_u8_set_abi),
            symbol: "capable_rt_vec_u8_set".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__push".to_string(),
        FnInfo {
            sig: vec_u8_push.clone(),
            abi_sig: Some(vec_u8_push_abi),
            symbol: "capable_rt_vec_u8_push".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__pop".to_string(),
        FnInfo {
            sig: vec_u8_pop.clone(),
            abi_sig: Some(vec_u8_pop_abi),
            symbol: "capable_rt_vec_u8_pop".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecU8__as_slice".to_string(),
        FnInfo {
            sig: vec_u8_as_slice,
            abi_sig: None,
            symbol: "capable_rt_vec_u8_as_slice".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__len".to_string(),
        FnInfo {
            sig: vec_len.clone(),
            abi_sig: None,
            symbol: "capable_rt_vec_i32_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__get".to_string(),
        FnInfo {
            sig: vec_i32_get.clone(),
            abi_sig: Some(vec_i32_get_abi),
            symbol: "capable_rt_vec_i32_get".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__set".to_string(),
        FnInfo {
            sig: vec_i32_set.clone(),
            abi_sig: Some(vec_i32_set_abi),
            symbol: "capable_rt_vec_i32_set".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__push".to_string(),
        FnInfo {
            sig: vec_i32_push.clone(),
            abi_sig: Some(vec_i32_push_abi),
            symbol: "capable_rt_vec_i32_push".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecI32__pop".to_string(),
        FnInfo {
            sig: vec_i32_pop.clone(),
            abi_sig: Some(vec_i32_pop_abi),
            symbol: "capable_rt_vec_i32_pop".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecString__len".to_string(),
        FnInfo {
            sig: vec_string_len,
            abi_sig: None,
            symbol: "capable_rt_vec_string_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecString__get".to_string(),
        FnInfo {
            sig: vec_string_get,
            abi_sig: Some(vec_string_get_abi),
            symbol: "capable_rt_vec_string_get".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecString__push".to_string(),
        FnInfo {
            sig: vec_string_push,
            abi_sig: Some(vec_string_push_abi),
            symbol: "capable_rt_vec_string_push".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.vec.VecString__pop".to_string(),
        FnInfo {
            sig: vec_string_pop,
            abi_sig: Some(vec_string_pop_abi),
            symbol: "capable_rt_vec_string_pop".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.string__len".to_string(),
        FnInfo {
            sig: string_len,
            abi_sig: None,
            symbol: "capable_rt_string_len".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.string__byte_at".to_string(),
        FnInfo {
            sig: string_byte_at,
            abi_sig: None,
            symbol: "capable_rt_string_byte_at".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.string__as_slice".to_string(),
        FnInfo {
            sig: string_as_slice.clone(),
            abi_sig: None,
            symbol: "capable_rt_string_as_slice".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.string__bytes".to_string(),
        FnInfo {
            // bytes() is an alias for as_slice(); both map to the same runtime symbol.
            sig: string_as_slice,
            abi_sig: None,
            symbol: "capable_rt_string_as_slice".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.string__split_whitespace".to_string(),
        FnInfo {
            sig: string_split,
            abi_sig: None,
            symbol: "capable_rt_string_split_whitespace".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.string.string__split".to_string(),
        FnInfo {
            sig: string_split_delim,
            abi_sig: None,
            symbol: "capable_rt_string_split".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.bytes.u8__is_whitespace".to_string(),
        FnInfo {
            sig: FnSig {
                params: vec![TyKind::U8],
                ret: TyKind::Bool,
            },
            abi_sig: None,
            symbol: "capable_rt_bytes_is_whitespace".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );

    let _ = ptr_ty;
    map
}

/// Convert typeck::Ty to codegen::TyKind
fn typeck_ty_to_tykind(
    ty: &crate::typeck::Ty,
    struct_layouts: Option<&StructLayoutIndex>,
) -> Result<TyKind, CodegenError> {
    use crate::typeck::{BuiltinType, Ty};
    match ty {
        Ty::Builtin(b) => match b {
            BuiltinType::I32 => Ok(TyKind::I32),
            BuiltinType::I64 => Err(CodegenError::Unsupported("i64 not yet supported".to_string())),
            BuiltinType::U32 => Ok(TyKind::U32),
            BuiltinType::U8 => Ok(TyKind::U8),
            BuiltinType::Bool => Ok(TyKind::Bool),
            BuiltinType::String => Ok(TyKind::String),
            BuiltinType::Unit => Ok(TyKind::Unit),
        },
        Ty::Path(path, args) => {
            if path == "Result" && args.len() == 2 {
                let ok = typeck_ty_to_tykind(&args[0], struct_layouts)?;
                let err = typeck_ty_to_tykind(&args[1], struct_layouts)?;
                return Ok(TyKind::Result(Box::new(ok), Box::new(err)));
            }
            if let Some(layouts) = struct_layouts {
                if resolve_struct_layout(ty, "", &layouts.layouts).is_some() {
                    return Ok(TyKind::Ptr);
                }
            }
            // Handle known types
            if path.ends_with(".Slice") {
                Ok(TyKind::Handle)
            } else if path.ends_with(".Buffer") {
                Ok(TyKind::Handle)
            } else if path.ends_with(".FsErr") || path.contains("Err") {
                Ok(TyKind::I32)  // Enums are i32
            } else if path.contains('.') {
                // Other module types are generally handles
                Ok(TyKind::Handle)
            } else {
                // Local enum
                Ok(TyKind::I32)
            }
        },
        Ty::Ptr(_inner) => Ok(TyKind::Ptr),
    }
}

fn register_user_functions(
    module: &crate::hir::HirModule,
    entry: &crate::hir::HirModule,
    _stdlib: &StdlibIndex,
    _enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    map: &mut HashMap<String, FnInfo>,
    runtime_intrinsics: &HashMap<String, FnInfo>,
    _ptr_ty: Type,
    is_stdlib: bool,
) -> Result<(), CodegenError> {
    let module_name = &module.name;
    // HIR functions already have resolved types
    for func in &module.functions {
        let sig = FnSig {
            params: func
                .params
                .iter()
                .map(|p| typeck_ty_to_tykind(&p.ty, Some(struct_layouts)))
                .collect::<Result<Vec<TyKind>, CodegenError>>()?,
            ret: typeck_ty_to_tykind(&func.ret_ty, Some(struct_layouts))?,
        };
        let key = format!("{}.{}", module_name, func.name);
        if map.contains_key(&key) {
            return Err(CodegenError::Codegen(format!(
                "duplicate function `{key}`"
            )));
        }
        let symbol = if module_name == &entry.name && func.name == "main" {
            "capable_main".to_string()
        } else {
            mangle_symbol(module_name, &func.name)
        };
        let (runtime_symbol, abi_sig) = if is_stdlib {
            runtime_intrinsics
                .get(&key)
                .map(|intrinsic| (Some(intrinsic.symbol.clone()), intrinsic.abi_sig.clone()))
                .unwrap_or((None, None))
        } else {
            (None, None)
        };
        map.insert(
            key,
            FnInfo {
                sig,
                abi_sig,
                symbol,
                runtime_symbol,
                is_runtime: false,
            },
        );
    }
    // Note: HirModule doesn't have extern functions as separate items
    // They would be in the functions list if needed
    Ok(())
}

fn register_extern_functions(
    module: &AstModule,
    stdlib: &StdlibIndex,
    enum_index: &EnumIndex,
    map: &mut HashMap<String, FnInfo>,
) -> Result<(), CodegenError> {
    let module_name = module.name.to_string();
    let use_map = UseMap::new(module);

    for item in &module.items {
        if let Item::ExternFunction(func) = item {
            let sig = FnSig {
                params: func
                    .params
                    .iter()
                    .map(|p| {
                        let ty = p.ty.as_ref().ok_or_else(|| {
                            CodegenError::Codegen(format!(
                                "extern parameter `{}` requires a type annotation",
                                p.name.item
                            ))
                        })?;
                        lower_ty(ty, &use_map, stdlib, enum_index)
                    })
                    .collect::<Result<Vec<TyKind>, CodegenError>>()?,
                ret: lower_ty(&func.ret, &use_map, stdlib, enum_index)?,
            };
            let key = format!("{}.{}", module_name, func.name.item);
            map.insert(
                key,
                FnInfo {
                    sig,
                    abi_sig: None,
                    symbol: func.name.item.clone(),
                    runtime_symbol: None,
                    is_runtime: true,
                },
            );
        }
    }
    Ok(())
}

// Removed old extern function handling - HIR doesn't distinguish them separately
fn _old_extern_handling() {
    // This is dead code kept for reference
    // In HIR, extern functions would just be in the functions list
    // with appropriate metadata if we needed to track them
    /*
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
                        runtime_symbol: None,
                        is_runtime: true,
                    },
                );
            }
            _ => {}
        }
    }
    Ok(())
    */
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
) -> Result<Flow, CodegenError> {
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
                LocalValue::StructSlot(_, _, _) => {
                    return Err(CodegenError::Unsupported("struct assignment".to_string()));
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
            return Ok(Flow::Terminated);
        }
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                return emit_match_stmt(
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
                );
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
            let mut then_terminated = false;
            for stmt in &if_stmt.then_block.stmts {
                let flow = emit_stmt(
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
                if flow == Flow::Terminated {
                    then_terminated = true;
                    break;
                }
            }
            if !then_terminated {
                builder.ins().jump(merge_block, &[]);
            }
            builder.seal_block(then_block);

            // ELSE branch with its own locals
            if let Some(else_block_ast) = &if_stmt.else_block {
                builder.switch_to_block(else_block);
                let mut else_locals = saved_locals.clone();
                let mut else_terminated = false;
                for stmt in &else_block_ast.stmts {
                    let flow = emit_stmt(
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
                    if flow == Flow::Terminated {
                        else_terminated = true;
                        break;
                    }
                }
                if !else_terminated {
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
            let mut body_terminated = false;
            for stmt in &while_stmt.body.stmts {
                let flow = emit_stmt(
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
                if flow == Flow::Terminated {
                    body_terminated = true;
                    break;
                }
            }

            if !body_terminated {
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
    Ok(Flow::Continues)
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
                    return Ok(load_local(builder, value, module.isa().pointer_type()));
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
            // Convert the callee (Path or FieldAccess chain) to a Path for function lookup
            let path = call.callee.to_path()
                .ok_or_else(|| CodegenError::Unsupported("call target".to_string()))?;
            let callee_path = resolve_call_path(&path, module_name, use_map);
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
            let call_symbol = info.runtime_symbol.as_deref().unwrap_or(&info.symbol);
            let func_id = module
                .declare_function(
                    call_symbol,
                    if info.runtime_symbol.is_some() || info.is_runtime {
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
                locals.contains_key(base_name)
            } else {
                // Not a pure a.b.c chain (e.g., call result) - treat as method on value
                true
            };

            // Try to convert to a path and resolve as a module-qualified function
            let mut path = method_call.receiver.to_path()
                .ok_or_else(|| CodegenError::Unsupported("method calls on non-path receivers".to_string()))?;
            path.segments.push(method_call.method.clone());
            path.span = Span::new(path.span.start, method_call.method.span.end);

            // Check if this resolves as a function
            let callee_path = resolve_call_path(&path, module_name, use_map);
            let is_function = fn_map.contains_key(&callee_path);

            if base_is_local && !is_function {
                // Local variable + doesn't resolve as function = real method call on value
                return Err(CodegenError::Unsupported("methods on values not yet supported".to_string()));
            }

            // Either not a local, or is a local but shadows a module name that resolves
            // Treat as module-qualified function call
            let callee = Expr::Path(path);

            // Convert to Call and reuse existing logic
            let call = CallExpr {
                callee: Box::new(callee),
                args: method_call.args.clone(),
                span: method_call.span,
            };
            emit_expr(builder, &Expr::Call(call), locals, fn_map, use_map, module_name, stdlib, enum_index, module, data_counter)
        }
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
                    if let Some(index) = resolve_enum_variant(&path, use_map, enum_index) {
                        return Ok(ValueRepr::Single(
                            builder.ins().iconst(ir::types::I32, i64::from(index)),
                        ));
                    }
                }
            }

            // If not an enum variant, we don't support actual struct field access yet
            Err(CodegenError::Unsupported("field access on struct values".to_string()))
        }
        Expr::StructLiteral(_) => Err(CodegenError::Unsupported("struct literal expression".to_string())),
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
) -> Result<Flow, CodegenError> {
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

    let mut all_terminated = true;

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
        let mut arm_terminated = false;
        for stmt in &arm.body.stmts {
            let flow = emit_stmt(
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
            if flow == Flow::Terminated {
                arm_terminated = true;
                break;
            }
        }
        if !arm_terminated {
            all_terminated = false;
            builder.ins().jump(merge_block, &[]);
        }
        builder.seal_block(arm_block);

        if is_last {
            break;
        }
        current_block = next_block;
    }

    if all_terminated {
        // All arms terminated - merge block has no predecessors (only branch targets).
        // Switch to it and emit trap to satisfy Cranelift's verifier.
        // The trap is unreachable since Flow::Terminated stops further emission.
        builder.switch_to_block(merge_block);
        builder.ins().trap(ir::TrapCode::UnreachableCodeReached);
        builder.seal_block(merge_block);
        Ok(Flow::Terminated)
    } else {
        builder.switch_to_block(merge_block);
        builder.seal_block(merge_block);
        Ok(Flow::Continues)
    }
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
        let mut prefix_terminated = false;
        for stmt in prefix {
            let flow = emit_stmt(
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
            if flow == Flow::Terminated {
                prefix_terminated = true;
                break;
            }
        }

        // If prefix terminated, we can't emit the final expression
        if prefix_terminated {
            return Err(CodegenError::Unsupported(
                "match expression arm terminated before final expression".to_string(),
            ));
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

// ============================================================================
// HIR Emission Functions
// ============================================================================

fn emit_hir_stmt(
    builder: &mut FunctionBuilder,
    stmt: &crate::hir::HirStmt,
    locals: &mut HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<Flow, CodegenError> {
    use crate::hir::HirStmt;

    match stmt {
        HirStmt::Let(let_stmt) => {
            let value = emit_hir_expr(
                builder,
                &let_stmt.expr,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                module,
                data_counter,
            )?;
            if let Some(layout) =
                resolve_struct_layout(&let_stmt.ty, "", &struct_layouts.layouts)
            {
                let align = layout.align.max(1);
                let slot_size = layout.size.max(1).saturating_add(align - 1);
                let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    slot_size,
                ));
                let base_ptr = aligned_stack_addr(
                    builder,
                    slot,
                    align,
                    module.isa().pointer_type(),
                );
                store_value_by_ty(
                    builder,
                    base_ptr,
                    0,
                    &let_stmt.ty,
                    value,
                    struct_layouts,
                    module,
                )?;
                locals.insert(
                    let_stmt.name.clone(),
                    LocalValue::StructSlot(slot, let_stmt.ty.clone(), align),
                );
            } else {
                let local = store_local(builder, value);
                locals.insert(let_stmt.name.clone(), local);
            }
        }
        HirStmt::Assign(assign) => {
            let value = emit_hir_expr(
                builder,
                &assign.expr,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                module,
                data_counter,
            )?;
            let Some(local) = locals.get_mut(&assign.name) else {
                return Err(CodegenError::UnknownVariable(assign.name.clone()));
            };
            match local {
                LocalValue::Slot(slot, _ty) => {
                    let ValueRepr::Single(val) = value else {
                        return Err(CodegenError::Unsupported("assignment".to_string()));
                    };
                    builder.ins().stack_store(val, *slot, 0);
                }
                LocalValue::StructSlot(slot, ty, align) => {
                    let base_ptr = aligned_stack_addr(
                        builder,
                        *slot,
                        *align,
                        module.isa().pointer_type(),
                    );
                    store_value_by_ty(
                        builder,
                        base_ptr,
                        0,
                        ty,
                        value,
                        struct_layouts,
                        module,
                    )?;
                }
                LocalValue::Value(_) => {
                    return Err(CodegenError::Unsupported("assignment".to_string()));
                }
            }
        }
        HirStmt::Return(ret_stmt) => {
            if let Some(expr) = &ret_stmt.expr {
                let value = emit_hir_expr(
                    builder,
                    expr,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
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
            return Ok(Flow::Terminated);
        }
        HirStmt::Expr(expr_stmt) => {
            let _ = emit_hir_expr(
                builder,
                &expr_stmt.expr,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                module,
                data_counter,
            )?;
        }
        HirStmt::If(if_stmt) => {
            // Snapshot locals so branch-scoped lets don't leak
            let saved_locals = locals.clone();

            let then_block = builder.create_block();
            let merge_block = builder.create_block();
            let else_block = if if_stmt.else_block.is_some() {
                builder.create_block()
            } else {
                merge_block
            };

            let cond_val = emit_hir_expr(
                builder,
                &if_stmt.cond,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
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
            let mut then_terminated = false;
            for stmt in &if_stmt.then_block.stmts {
                let flow = emit_hir_stmt(
                    builder,
                    stmt,
                    &mut then_locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    module,
                    data_counter,
                )?;
                if flow == Flow::Terminated {
                    then_terminated = true;
                    break;
                }
            }
            if !then_terminated {
                builder.ins().jump(merge_block, &[]);
            }
            builder.seal_block(then_block);

            // ELSE branch with its own locals
            if let Some(else_block_hir) = &if_stmt.else_block {
                builder.switch_to_block(else_block);
                let mut else_locals = saved_locals.clone();
                let mut else_terminated = false;
                for stmt in &else_block_hir.stmts {
                    let flow = emit_hir_stmt(
                        builder,
                        stmt,
                        &mut else_locals,
                        fn_map,
                        enum_index,
                        struct_layouts,
                        module,
                        data_counter,
                    )?;
                    if flow == Flow::Terminated {
                        else_terminated = true;
                        break;
                    }
                }
                if !else_terminated {
                    builder.ins().jump(merge_block, &[]);
                }
                builder.seal_block(else_block);
            }

            // After the if, restore the pre-if locals snapshot
            *locals = saved_locals;

            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);
        }
        HirStmt::While(while_stmt) => {
            // Snapshot locals so loop-body lets don't leak out of the loop
            let saved_locals = locals.clone();

            let header_block = builder.create_block();
            let body_block = builder.create_block();
            let exit_block = builder.create_block();

            builder.ins().jump(header_block, &[]);
            builder.switch_to_block(header_block);

            let cond_val = emit_hir_expr(
                builder,
                &while_stmt.cond,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                module,
                data_counter,
            )?;
            let cond_b1 = to_b1(builder, cond_val)?;
            builder
                .ins()
                .brif(cond_b1, body_block, &[], exit_block, &[]);

            builder.switch_to_block(body_block);

            // Loop body gets its own locals
            let mut body_locals = saved_locals.clone();
            let mut body_terminated = false;
            for stmt in &while_stmt.body.stmts {
                let flow = emit_hir_stmt(
                    builder,
                    stmt,
                    &mut body_locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    module,
                    data_counter,
                )?;
                if flow == Flow::Terminated {
                    body_terminated = true;
                    break;
                }
            }

            if !body_terminated {
                builder.ins().jump(header_block, &[]);
            }

            builder.seal_block(body_block);
            builder.seal_block(header_block);

            builder.switch_to_block(exit_block);
            builder.seal_block(exit_block);

            // After the loop, restore the pre-loop locals snapshot
            *locals = saved_locals;
        }
    }
    Ok(Flow::Continues)
}

fn emit_hir_expr(
    builder: &mut FunctionBuilder,
    expr: &crate::hir::HirExpr,
    locals: &HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    use crate::hir::HirExpr;

    match expr {
        HirExpr::Literal(lit) => match &lit.value {
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
        HirExpr::Local(local) => {
            // Look up the local by name (we still use String keys in the HashMap)
            if let Some(value) = locals.get(&local.name) {
                return Ok(load_local(builder, value, module.isa().pointer_type()));
            }
            Err(CodegenError::UnknownVariable(local.name.clone()))
        }
        HirExpr::EnumVariant(variant) => {
            // Check if this is a Result type with payload (Ok/Err)
            if let crate::typeck::Ty::Path(ty_name, args) = &variant.enum_ty {
                if ty_name == "Result" && args.len() == 2 {
                    let ok_ty = &args[0];
                    let err_ty = &args[1];
                    let tag = if variant.variant_name == "Ok" { 0 } else { 1 };
                    let tag_val = builder.ins().iconst(ir::types::I8, tag);
                    let ptr_ty = module.isa().pointer_type();

                    let (ok, err) = match variant.variant_name.as_str() {
                        "Ok" => {
                            let payload = if let Some(payload_expr) = &variant.payload {
                                emit_hir_expr(
                                    builder,
                                    payload_expr,
                                    locals,
                                    fn_map,
                                    enum_index,
                                    struct_layouts,
                                    module,
                                    data_counter,
                                )?
                            } else {
                                zero_value_for_ty(builder, ok_ty, ptr_ty, Some(struct_layouts))?
                            };
                            let err_zero =
                                zero_value_for_ty(builder, err_ty, ptr_ty, Some(struct_layouts))?;
                            (payload, err_zero)
                        }
                        "Err" => {
                            let ok_zero =
                                zero_value_for_ty(builder, ok_ty, ptr_ty, Some(struct_layouts))?;
                            let payload = if let Some(payload_expr) = &variant.payload {
                                emit_hir_expr(
                                    builder,
                                    payload_expr,
                                    locals,
                                    fn_map,
                                    enum_index,
                                    struct_layouts,
                                    module,
                                    data_counter,
                                )?
                            } else {
                                zero_value_for_ty(builder, err_ty, ptr_ty, Some(struct_layouts))?
                            };
                            (ok_zero, payload)
                        }
                        _ => {
                            return Err(CodegenError::Codegen(format!(
                                "unknown Result variant: {}",
                                variant.variant_name
                            )))
                        }
                    };

                    return Ok(ValueRepr::Result {
                        tag: tag_val,
                        ok: Box::new(ok),
                        err: Box::new(err),
                    });
                }
            }

            // For non-Result enums or variants without payload, emit just the discriminant
            let qualified = match &variant.enum_ty {
                crate::typeck::Ty::Path(path, _) => path.clone(),
                _ => return Err(CodegenError::Codegen(format!(
                    "enum variant has non-path type: {:?}",
                    variant.enum_ty
                ))),
            };
            if let Some(variants) = enum_index.variants.get(&qualified) {
                if let Some(&discr) = variants.get(&variant.variant_name) {
                    return Ok(ValueRepr::Single(
                        builder.ins().iconst(ir::types::I32, i64::from(discr)),
                    ));
                }
            }
            Err(CodegenError::Codegen(format!(
                "unknown enum variant: {}.{}",
                qualified, variant.variant_name
            )))
        }
        HirExpr::Call(call) => {
            // HIR calls are already fully resolved - no path resolution needed!
            let (module_path, func_name, _symbol) = match &call.callee {
                crate::hir::ResolvedCallee::Function { module, name, symbol } => {
                    (module.clone(), name.clone(), symbol.clone())
                }
                crate::hir::ResolvedCallee::Intrinsic(_) => {
                    return Err(CodegenError::Unsupported("intrinsics".to_string()));
                }
            };

            // Lookup in fn_map by module.function key
            let key = format!("{}.{}", module_path, func_name);
            let info = fn_map
                .get(&key)
                .ok_or_else(|| CodegenError::UnknownFunction(key.clone()))?
                .clone();

            // Emit arguments
            let mut args = Vec::new();
            for arg in &call.args {
                let value = emit_hir_expr(
                    builder,
                    arg,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    module,
                    data_counter,
                )?;
                args.extend(flatten_value(&value));
            }

            // Handle result out-parameters (same logic as AST version)
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

            // Emit the call
            let sig = sig_to_clif(abi_sig, module.isa().pointer_type());
            let call_symbol = info.runtime_symbol.as_deref().unwrap_or(&info.symbol);
            let func_id = module
                .declare_function(
                    call_symbol,
                    if info.runtime_symbol.is_some() || info.is_runtime {
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

            // Handle result unpacking (same logic as AST version)
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
                    ValueRepr::Unit
                };
                let err_val = if let Some((slot, ty)) = err_slot {
                    let addr = builder
                        .ins()
                        .stack_addr(module.isa().pointer_type(), slot, 0);
                    let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
                    ValueRepr::Single(val)
                } else {
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
        HirExpr::Binary(binary) => {
            let lhs = emit_hir_expr(
                builder,
                &binary.left,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
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
                return emit_hir_short_circuit_expr(
                    builder,
                    lhs_val,
                    &binary.right,
                    matches!(binary.op, BinaryOp::And),
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    module,
                    data_counter,
                );
            }

            let rhs = emit_hir_expr(
                builder,
                &binary.right,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
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
        HirExpr::Unary(unary) => {
            let value = emit_hir_expr(
                builder,
                &unary.expr,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
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
        HirExpr::Match(match_expr) => {
            // Check if this is a match-as-statement (result_ty is Unit)
            if matches!(match_expr.result_ty, crate::typeck::Ty::Builtin(crate::typeck::BuiltinType::Unit)) {
                emit_hir_match_stmt(
                    builder,
                    match_expr,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    module,
                    data_counter,
                )
            } else {
                emit_hir_match_expr(
                    builder,
                    match_expr,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    module,
                    data_counter,
                )
            }
        }
        HirExpr::FieldAccess(field_access) => emit_hir_field_access(
            builder,
            field_access,
            locals,
            fn_map,
            enum_index,
            struct_layouts,
            module,
            data_counter,
        ),
        HirExpr::StructLiteral(literal) => {
            emit_hir_struct_literal(
                builder,
                literal,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                module,
                data_counter,
            )
        }
    }
}

fn emit_hir_struct_literal(
    builder: &mut FunctionBuilder,
    literal: &crate::hir::HirStructLiteral,
    locals: &HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    let layout = resolve_struct_layout(&literal.struct_ty, "", &struct_layouts.layouts)
        .ok_or_else(|| {
            CodegenError::Unsupported(format!(
                "struct layout missing for {:?}",
                literal.struct_ty
            ))
        })?;
    let ptr_ty = module.isa().pointer_type();
    let align = layout.align.max(1);
    let slot_size = layout.size.max(1).saturating_add(align - 1);
    let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
        ir::StackSlotKind::ExplicitSlot,
        slot_size,
    ));
    let base_ptr = aligned_stack_addr(builder, slot, align, ptr_ty);

    for field in &literal.fields {
        let Some(field_layout) = layout.fields.get(&field.name) else {
            return Err(CodegenError::Codegen(format!(
                "unknown struct field `{}`",
                field.name
            )));
        };
        let value = emit_hir_expr(
            builder,
            &field.expr,
            locals,
            fn_map,
            enum_index,
            struct_layouts,
            module,
            data_counter,
        )?;
        store_value_by_ty(
            builder,
            base_ptr,
            field_layout.offset,
            &field_layout.ty,
            value,
            struct_layouts,
            module,
        )?;
    }

    Ok(ValueRepr::Single(base_ptr))
}

fn emit_hir_field_access(
    builder: &mut FunctionBuilder,
    field_access: &crate::hir::HirFieldAccess,
    locals: &HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    let layout = resolve_struct_layout(&field_access.object_ty, "", &struct_layouts.layouts)
        .ok_or_else(|| {
            CodegenError::Unsupported(format!(
                "struct layout missing for {:?}",
                field_access.object_ty
            ))
        })?;
    let Some(field_layout) = layout.fields.get(&field_access.field_name) else {
        return Err(CodegenError::Codegen(format!(
            "unknown struct field `{}`",
            field_access.field_name
        )));
    };

    let object_value = emit_hir_expr(
        builder,
        &field_access.object,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        module,
        data_counter,
    )?;
    let base_ptr = match object_value {
        ValueRepr::Single(val) => val,
        _ => {
            return Err(CodegenError::Unsupported(
                "field access on non-struct value".to_string(),
            ))
        }
    };

    load_value_by_ty(
        builder,
        base_ptr,
        field_layout.offset,
        &field_layout.ty,
        struct_layouts,
        module,
    )
}

fn store_value_by_ty(
    builder: &mut FunctionBuilder,
    base_ptr: ir::Value,
    offset: u32,
    ty: &crate::typeck::Ty,
    value: ValueRepr,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
) -> Result<(), CodegenError> {
    use crate::typeck::{BuiltinType, Ty};

    let addr = ptr_add(builder, base_ptr, offset);
    let ptr_ty = module.isa().pointer_type();
    match ty {
        Ty::Builtin(b) => match b {
            BuiltinType::Unit => Ok(()),
            BuiltinType::I32 | BuiltinType::U32 => {
                let ValueRepr::Single(val) = value else {
                    return Err(CodegenError::Unsupported("store i32".to_string()));
                };
                builder.ins().store(MemFlags::new(), val, addr, 0);
                Ok(())
            }
            BuiltinType::U8 | BuiltinType::Bool => {
                let ValueRepr::Single(val) = value else {
                    return Err(CodegenError::Unsupported("store u8".to_string()));
                };
                builder.ins().store(MemFlags::new(), val, addr, 0);
                Ok(())
            }
            BuiltinType::String => {
                let ValueRepr::Pair(ptr, len) = value else {
                    return Err(CodegenError::Unsupported("store string".to_string()));
                };
                let (ptr_off, len_off) = string_offsets(ptr_ty);
                let ptr_addr = ptr_add(builder, base_ptr, offset + ptr_off);
                let len_addr = ptr_add(builder, base_ptr, offset + len_off);
                builder.ins().store(MemFlags::new(), ptr, ptr_addr, 0);
                builder.ins().store(MemFlags::new(), len, len_addr, 0);
                Ok(())
            }
            BuiltinType::I64 => Err(CodegenError::Unsupported("i64 not yet supported".to_string())),
        },
        Ty::Ptr(_) => {
            let ValueRepr::Single(val) = value else {
                return Err(CodegenError::Unsupported("store ptr".to_string()));
            };
            builder.ins().store(MemFlags::new(), val, addr, 0);
            Ok(())
        }
        Ty::Path(name, args) => {
            if name == "Result" && args.len() == 2 {
                let ValueRepr::Result { tag, ok, err } = value else {
                    return Err(CodegenError::Unsupported("store result".to_string()));
                };
                let ok_layout = type_layout_from_index(&args[0], struct_layouts, ptr_ty)?;
                let err_layout = type_layout_from_index(&args[1], struct_layouts, ptr_ty)?;
                let (_, ok_off, err_off) = result_offsets(ok_layout, err_layout);
                builder.ins().store(MemFlags::new(), tag, addr, 0);
                store_value_by_ty(
                    builder,
                    base_ptr,
                    offset + ok_off,
                    &args[0],
                    *ok,
                    struct_layouts,
                    module,
                )?;
                store_value_by_ty(
                    builder,
                    base_ptr,
                    offset + err_off,
                    &args[1],
                    *err,
                    struct_layouts,
                    module,
                )?;
                return Ok(());
            }

            if let Some(layout) = resolve_struct_layout(ty, "", &struct_layouts.layouts) {
    let ValueRepr::Single(src_ptr) = value else {
        return Err(CodegenError::Unsupported("store struct".to_string()));
    };
    for name in &layout.field_order {
        let Some(field) = layout.fields.get(name) else {
            continue;
        };
        let field_value = load_value_by_ty(
            builder,
            src_ptr,
            field.offset,
            &field.ty,
                        struct_layouts,
                        module,
                    )?;
                    store_value_by_ty(
                        builder,
                        base_ptr,
                        offset + field.offset,
                        &field.ty,
                        field_value,
                        struct_layouts,
                        module,
                    )?;
                }
                return Ok(());
            }

            let ty_kind = typeck_ty_to_tykind(ty, Some(struct_layouts))?;
            store_value_by_tykind(builder, addr, &ty_kind, value, ptr_ty)
        }
    }
}

fn store_value_by_tykind(
    builder: &mut FunctionBuilder,
    addr: ir::Value,
    ty: &TyKind,
    value: ValueRepr,
    _ptr_ty: Type,
) -> Result<(), CodegenError> {
    let ValueRepr::Single(val) = value else {
        return Err(CodegenError::Unsupported("store value".to_string()));
    };
    match ty {
        TyKind::I32 | TyKind::U32 | TyKind::U8 | TyKind::Bool | TyKind::Handle | TyKind::Ptr => {
            builder.ins().store(MemFlags::new(), val, addr, 0);
            Ok(())
        }
        _ => Err(CodegenError::Unsupported(format!(
            "store unsupported {ty:?}"
        ))),
    }
}

fn load_value_by_ty(
    builder: &mut FunctionBuilder,
    base_ptr: ir::Value,
    offset: u32,
    ty: &crate::typeck::Ty,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
) -> Result<ValueRepr, CodegenError> {
    use crate::typeck::{BuiltinType, Ty};

    let addr = ptr_add(builder, base_ptr, offset);
    let ptr_ty = module.isa().pointer_type();
    match ty {
        Ty::Builtin(b) => match b {
            BuiltinType::Unit => Ok(ValueRepr::Unit),
            BuiltinType::I32 | BuiltinType::U32 => Ok(ValueRepr::Single(
                builder.ins().load(ir::types::I32, MemFlags::new(), addr, 0),
            )),
            BuiltinType::U8 | BuiltinType::Bool => Ok(ValueRepr::Single(
                builder.ins().load(ir::types::I8, MemFlags::new(), addr, 0),
            )),
            BuiltinType::String => {
                let (ptr_off, len_off) = string_offsets(ptr_ty);
                let ptr_addr = ptr_add(builder, base_ptr, offset + ptr_off);
                let len_addr = ptr_add(builder, base_ptr, offset + len_off);
                let ptr = builder
                    .ins()
                    .load(ptr_ty, MemFlags::new(), ptr_addr, 0);
                let len = builder
                    .ins()
                    .load(ir::types::I64, MemFlags::new(), len_addr, 0);
                Ok(ValueRepr::Pair(ptr, len))
            }
            BuiltinType::I64 => Err(CodegenError::Unsupported("i64 not yet supported".to_string())),
        },
        Ty::Ptr(_) => Ok(ValueRepr::Single(
            builder.ins().load(ptr_ty, MemFlags::new(), addr, 0),
        )),
        Ty::Path(name, args) => {
            if name == "Result" && args.len() == 2 {
                let ok_layout = type_layout_from_index(&args[0], struct_layouts, ptr_ty)?;
                let err_layout = type_layout_from_index(&args[1], struct_layouts, ptr_ty)?;
                let (_, ok_off, err_off) = result_offsets(ok_layout, err_layout);
                let tag = builder.ins().load(ir::types::I8, MemFlags::new(), addr, 0);
                let ok = load_value_by_ty(
                    builder,
                    base_ptr,
                    offset + ok_off,
                    &args[0],
                    struct_layouts,
                    module,
                )?;
                let err = load_value_by_ty(
                    builder,
                    base_ptr,
                    offset + err_off,
                    &args[1],
                    struct_layouts,
                    module,
                )?;
                return Ok(ValueRepr::Result {
                    tag,
                    ok: Box::new(ok),
                    err: Box::new(err),
                });
            }

            if resolve_struct_layout(ty, "", &struct_layouts.layouts).is_some() {
                let ptr = ptr_add(builder, base_ptr, offset);
                return Ok(ValueRepr::Single(ptr));
            }

            let ty_kind = typeck_ty_to_tykind(ty, Some(struct_layouts))?;
            load_value_by_tykind(builder, addr, &ty_kind, ptr_ty)
        }
    }
}

fn load_value_by_tykind(
    builder: &mut FunctionBuilder,
    addr: ir::Value,
    ty: &TyKind,
    ptr_ty: Type,
) -> Result<ValueRepr, CodegenError> {
    let load_ty = match ty {
        TyKind::I32 | TyKind::U32 => ir::types::I32,
        TyKind::U8 | TyKind::Bool => ir::types::I8,
        TyKind::Handle => ir::types::I64,
        TyKind::Ptr => ptr_ty,
        _ => {
            return Err(CodegenError::Unsupported(format!(
                "load unsupported {ty:?}"
            )))
        }
    };
    Ok(ValueRepr::Single(
        builder.ins().load(load_ty, MemFlags::new(), addr, 0),
    ))
}

fn ptr_add(builder: &mut FunctionBuilder, base: ir::Value, offset: u32) -> ir::Value {
    if offset == 0 {
        base
    } else {
        builder.ins().iadd_imm(base, i64::from(offset))
    }
}

fn aligned_stack_addr(
    builder: &mut FunctionBuilder,
    slot: ir::StackSlot,
    align: u32,
    ptr_ty: Type,
) -> ir::Value {
    let base = builder.ins().stack_addr(ptr_ty, slot, 0);
    if align <= 1 {
        return base;
    }
    let align_mask = !((align as i64) - 1);
    let bumped = builder.ins().iadd_imm(base, i64::from(align - 1));
    builder.ins().band_imm(bumped, align_mask)
}

fn string_offsets(ptr_ty: Type) -> (u32, u32) {
    let ptr_size = ptr_ty.bytes() as u32;
    let len_offset = align_to(ptr_size, 8);
    (0, len_offset)
}

fn result_offsets(ok: TypeLayout, err: TypeLayout) -> (u32, u32, u32) {
    let tag_offset = 0u32;
    let ok_offset = align_to(1, ok.align);
    let err_offset = align_to(ok_offset.saturating_add(ok.size), err.align);
    (tag_offset, ok_offset, err_offset)
}

fn type_layout_from_index(
    ty: &crate::typeck::Ty,
    struct_layouts: &StructLayoutIndex,
    ptr_ty: Type,
) -> Result<TypeLayout, CodegenError> {
    if let Some(layout) = resolve_struct_layout(ty, "", &struct_layouts.layouts) {
        return Ok(TypeLayout {
            size: layout.size,
            align: layout.align,
        });
    }
    let ty_kind = typeck_ty_to_tykind(ty, Some(struct_layouts))?;
    type_layout_for_tykind(&ty_kind, ptr_ty)
}

fn emit_hir_short_circuit_expr(
    builder: &mut FunctionBuilder,
    lhs_val: ir::Value,
    rhs_expr: &crate::hir::HirExpr,
    is_and: bool,
    locals: &HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    let rhs_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, ir::types::I8);

    let lhs_bool = builder.ins().icmp_imm(IntCC::NotEqual, lhs_val, 0);
    if is_and {
        builder
            .ins()
            .brif(lhs_bool, rhs_block, &[], merge_block, &[lhs_val]);
    } else {
        builder
            .ins()
            .brif(lhs_bool, merge_block, &[lhs_val], rhs_block, &[]);
    }

    builder.switch_to_block(rhs_block);
    builder.seal_block(rhs_block);
    let rhs = emit_hir_expr(
        builder,
        rhs_expr,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        module,
        data_counter,
    )?;
    let rhs_val = match rhs {
        ValueRepr::Single(v) => v,
        _ => return Err(CodegenError::Unsupported("boolean op".to_string())),
    };
    builder.ins().jump(merge_block, &[rhs_val]);

    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);
    let param = builder.block_params(merge_block)[0];
    Ok(ValueRepr::Single(param))
}

/// Emit HIR match as statement (arms can contain returns, don't produce values)
fn emit_hir_match_stmt(
    builder: &mut FunctionBuilder,
    match_expr: &crate::hir::HirMatch,
    locals: &HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    // Emit the scrutinee expression
    let value = emit_hir_expr(
        builder,
        &match_expr.expr,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        module,
        data_counter,
    )?;

    let (match_val, match_result) = match value.clone() {
        ValueRepr::Single(v) => (v, None),
        ValueRepr::Result { tag, ok, err } => (tag, Some((*ok, *err))),
        ValueRepr::Unit => (builder.ins().iconst(ir::types::I32, 0), None),
        ValueRepr::Pair(_, _) => {
            return Err(CodegenError::Unsupported("match on string".to_string()))
        }
    };

    let merge_block = builder.create_block();

    // Create all check blocks upfront so they exist before being referenced
    let num_arms = match_expr.arms.len();
    let mut check_blocks: Vec<ir::Block> = Vec::new();
    let mut arm_blocks: Vec<ir::Block> = Vec::new();
    for i in 0..num_arms {
        arm_blocks.push(builder.create_block());
        if i + 1 < num_arms {
            check_blocks.push(builder.create_block());
        }
    }
    // For the last arm, the "next" block is merge_block
    check_blocks.push(merge_block);

    let mut current_block = builder
        .current_block()
        .ok_or_else(|| CodegenError::Codegen("no current block for match".to_string()))?;

    let mut any_arm_continues = false;

    for (idx, arm) in match_expr.arms.iter().enumerate() {
        let arm_block = arm_blocks[idx];
        let next_block = check_blocks[idx];

        if idx > 0 {
            builder.switch_to_block(current_block);
        }
        let cond = hir_match_pattern_cond(builder, &arm.pattern, match_val, enum_index)?;
        builder.ins().brif(cond, arm_block, &[], next_block, &[]);

        builder.switch_to_block(arm_block);

        let mut arm_locals = locals.clone();
        hir_bind_match_pattern_value(
            builder,
            &arm.pattern,
            &value,
            match_result.as_ref(),
            &mut arm_locals,
        )?;

        // Emit all statements in the arm body
        let mut arm_terminated = false;
        for stmt in &arm.body.stmts {
            let flow = emit_hir_stmt(
                builder,
                stmt,
                &mut arm_locals,
                fn_map,
                enum_index,
                struct_layouts,
                module,
                data_counter,
            )?;
            if flow == Flow::Terminated {
                arm_terminated = true;
                break;
            }
        }

        // If the arm didn't terminate (e.g., with return), jump to merge block
        if !arm_terminated {
            builder.ins().jump(merge_block, &[]);
            any_arm_continues = true;
        }

        current_block = next_block;
    }

    // Always switch to merge_block to insert it into the layout
    builder.switch_to_block(merge_block);

    // If no arm continues to merge_block, it's unreachable - add trap
    // This also ensures the block has content so it's properly inserted
    if !any_arm_continues {
        builder.ins().trap(ir::TrapCode::UnreachableCodeReached);
    }

    Ok(ValueRepr::Unit)
}

/// Emit HIR match expression
fn emit_hir_match_expr(
    builder: &mut FunctionBuilder,
    match_expr: &crate::hir::HirMatch,
    locals: &HashMap<String, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    use crate::hir::HirStmt;

    // Emit the scrutinee expression
    let value = emit_hir_expr(
        builder,
        &match_expr.expr,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        module,
        data_counter,
    )?;

    let (match_val, match_result) = match value.clone() {
        ValueRepr::Single(v) => (v, None),
        ValueRepr::Result { tag, ok, err } => (tag, Some((*ok, *err))),
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
        let cond = hir_match_pattern_cond(builder, &arm.pattern, match_val, enum_index)?;
        builder.ins().brif(cond, arm_block, &[], next_block, &[]);

        builder.switch_to_block(arm_block);
        let mut arm_locals = locals.clone();
        hir_bind_match_pattern_value(
            builder,
            &arm.pattern,
            &value,
            match_result.as_ref(),
            &mut arm_locals,
        )?;

        // Emit the arm body statements
        let stmts = &arm.body.stmts;
        let Some((last, prefix)) = stmts.split_last() else {
            return Err(CodegenError::Unsupported("empty match arm".to_string()));
        };

        let mut prefix_terminated = false;
        for stmt in prefix {
            let flow = emit_hir_stmt(
                builder,
                stmt,
                &mut arm_locals,
                fn_map,
                enum_index,
                struct_layouts,
                module,
                data_counter,
            )?;
            if flow == Flow::Terminated {
                prefix_terminated = true;
                break;
            }
        }

        // If prefix terminated, we can't emit the final expression
        if prefix_terminated {
            return Err(CodegenError::Unsupported(
                "match expression arm terminated before final expression".to_string(),
            ));
        }

        // Last statement should be an expression
        let arm_value = match last {
            HirStmt::Expr(expr_stmt) => emit_hir_expr(
                builder,
                &expr_stmt.expr,
                &arm_locals,
                fn_map,
                enum_index,
                struct_layouts,
                module,
                data_counter,
            )?,
            _ => {
                return Err(CodegenError::Unsupported(
                    "match arm must end with expression".to_string(),
                ))
            }
        };

        let values = match &arm_value {
            ValueRepr::Single(val) => vec![*val],
            ValueRepr::Pair(a, b) => vec![*a, *b],
            ValueRepr::Unit => vec![],
            ValueRepr::Result { .. } => {
                return Err(CodegenError::Unsupported("match result value".to_string()))
            }
        };

        // Set up result shape and stack slots on first arm
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

        // Store values to stack slots
        let shape = result_shape
            .as_ref()
            .ok_or_else(|| CodegenError::Codegen("missing match result shape".to_string()))?;
        if values.len() != shape.types.len() {
            eprintln!("DEBUG: arm value mismatch - values: {:?}, expected: {:?}", values.len(), shape.types.len());
            eprintln!("DEBUG: arm_value = {:?}", arm_value);
            return Err(CodegenError::Unsupported("mismatched match arm".to_string()));
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

    // Load result from stack slots
    let shape = result_shape
        .ok_or_else(|| CodegenError::Codegen("missing match result value".to_string()))?;
    let mut loaded = Vec::new();
    for (slot, ty) in shape.slots.iter().zip(shape.types.iter()) {
        let addr = builder.ins().stack_addr(module.isa().pointer_type(), *slot, 0);
        let val = builder.ins().load(*ty, MemFlags::new(), addr, 0);
        loaded.push(val);
    }

    let result = match shape.kind {
        ResultKind::Unit => ValueRepr::Unit,
        ResultKind::Single => ValueRepr::Single(loaded[0]),
        ResultKind::Pair => ValueRepr::Pair(loaded[0], loaded[1]),
    };

    Ok(result)
}

/// Compute the condition for an HIR pattern match
fn hir_match_pattern_cond(
    builder: &mut FunctionBuilder,
    pattern: &crate::hir::HirPattern,
    match_val: ir::Value,
    enum_index: &EnumIndex,
) -> Result<ir::Value, CodegenError> {
    use crate::hir::HirPattern;

    match pattern {
        HirPattern::Wildcard | HirPattern::Binding(_, _) => {
            // Wildcard and binding patterns always match
            let one = builder.ins().iconst(ir::types::I32, 1);
            Ok(builder.ins().icmp_imm(IntCC::Equal, one, 1))
        }
        HirPattern::Literal(lit) => match lit {
            Literal::Int(n) => {
                let rhs = builder.ins().iconst(ir::types::I32, *n);
                Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
            }
            Literal::U8(n) => {
                let rhs = builder.ins().iconst(ir::types::I8, i64::from(*n));
                Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
            }
            Literal::Bool(b) => {
                let rhs = builder.ins().iconst(ir::types::I8, if *b { 1 } else { 0 });
                Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
            }
            Literal::Unit => {
                // Unit always matches unit
                let one = builder.ins().iconst(ir::types::I32, 1);
                Ok(builder.ins().icmp_imm(IntCC::Equal, one, 1))
            }
            Literal::String(_) => Err(CodegenError::Unsupported(
                "string pattern matching".to_string(),
            )),
        },
        HirPattern::Variant { enum_ty, variant_name, .. } => {
            // Get the discriminant value for this variant
            let qualified = match enum_ty {
                crate::typeck::Ty::Path(path, _) => path.clone(),
                _ => return Err(CodegenError::Codegen(format!(
                    "enum variant pattern has non-path type: {:?}",
                    enum_ty
                ))),
            };

            // Get the type of match_val to ensure consistent comparison
            let val_ty = builder.func.dfg.value_type(match_val);

            // Special handling for Result type (built-in, not in enum_index)
            if qualified == "Result" {
                let discr = match variant_name.as_str() {
                    "Ok" => 0i64,
                    "Err" => 1i64,
                    _ => return Err(CodegenError::Codegen(format!(
                        "unknown Result variant: {}", variant_name
                    ))),
                };
                let rhs = builder.ins().iconst(val_ty, discr);
                return Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs));
            }

            if let Some(variants) = enum_index.variants.get(&qualified) {
                if let Some(&discr) = variants.get(variant_name) {
                    let rhs = builder.ins().iconst(val_ty, i64::from(discr));
                    return Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs));
                }
            }
            Err(CodegenError::Codegen(format!(
                "unknown enum variant in pattern: {}.{}",
                qualified, variant_name
            )))
        }
    }
}

/// Bind pattern variables for HIR patterns
fn hir_bind_match_pattern_value(
    builder: &mut FunctionBuilder,
    pattern: &crate::hir::HirPattern,
    value: &ValueRepr,
    result: Option<&(ValueRepr, ValueRepr)>,
    locals: &mut HashMap<String, LocalValue>,
) -> Result<(), CodegenError> {
    use crate::hir::HirPattern;

    match pattern {
        HirPattern::Wildcard => Ok(()),
        HirPattern::Literal(_) => Ok(()),
        HirPattern::Binding(_id, name) => {
            // Bind the entire value to the variable
            locals.insert(name.clone(), store_local(builder, value.clone()));
            Ok(())
        }
        HirPattern::Variant { variant_name, binding, .. } => {
            if let Some((_local_id, name)) = binding {
                // Bind the inner value based on variant
                let Some((ok_val, err_val)) = result else {
                    return Err(CodegenError::Unsupported(
                        "variant binding without result value".to_string(),
                    ));
                };
                if variant_name == "Ok" {
                    locals.insert(name.clone(), store_local(builder, ok_val.clone()));
                } else if variant_name == "Err" {
                    locals.insert(name.clone(), store_local(builder, err_val.clone()));
                }
            }
            Ok(())
        }
    }
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

fn load_local(builder: &mut FunctionBuilder, local: &LocalValue, ptr_ty: Type) -> ValueRepr {
    match local {
        LocalValue::Slot(slot, ty) => ValueRepr::Single(builder.ins().stack_load(*ty, *slot, 0)),
        LocalValue::StructSlot(slot, _, align) => {
            let addr = aligned_stack_addr(builder, *slot, *align, ptr_ty);
            ValueRepr::Single(addr)
        }
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

fn zero_value_for_tykind(
    builder: &mut FunctionBuilder,
    ty: &TyKind,
    ptr_ty: Type,
) -> Result<ValueRepr, CodegenError> {
    match ty {
        TyKind::Unit => Ok(ValueRepr::Unit),
        TyKind::I32 | TyKind::U32 => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I32, 0))),
        TyKind::U8 | TyKind::Bool => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I8, 0))),
        TyKind::Handle => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I64, 0))),
        TyKind::Ptr => Ok(ValueRepr::Single(builder.ins().iconst(ptr_ty, 0))),
        TyKind::String => {
            let ptr = builder.ins().iconst(ptr_ty, 0);
            let len = builder.ins().iconst(ir::types::I64, 0);
            Ok(ValueRepr::Pair(ptr, len))
        }
        TyKind::Result(ok, err) => {
            let tag = builder.ins().iconst(ir::types::I8, 0);
            let ok_val = zero_value_for_tykind(builder, ok, ptr_ty)?;
            let err_val = zero_value_for_tykind(builder, err, ptr_ty)?;
            Ok(ValueRepr::Result {
                tag,
                ok: Box::new(ok_val),
                err: Box::new(err_val),
            })
        }
        TyKind::ResultOut(_, _) => Err(CodegenError::Unsupported("result out params".to_string())),
        TyKind::ResultString => Err(CodegenError::Unsupported("result abi".to_string())),
    }
}

fn zero_value_for_ty(
    builder: &mut FunctionBuilder,
    ty: &crate::typeck::Ty,
    ptr_ty: Type,
    struct_layouts: Option<&StructLayoutIndex>,
) -> Result<ValueRepr, CodegenError> {
    use crate::typeck::Ty;

    match ty {
        Ty::Builtin(_) => {
            zero_value_for_tykind(builder, &typeck_ty_to_tykind(ty, struct_layouts)?, ptr_ty)
        }
        Ty::Ptr(_) => zero_value_for_tykind(builder, &TyKind::Ptr, ptr_ty),
        Ty::Path(name, args) => {
            if name == "Result" && args.len() == 2 {
                let tag = builder.ins().iconst(ir::types::I8, 0);
                let ok_val = zero_value_for_ty(builder, &args[0], ptr_ty, struct_layouts)?;
                let err_val = zero_value_for_ty(builder, &args[1], ptr_ty, struct_layouts)?;
                return Ok(ValueRepr::Result {
                    tag,
                    ok: Box::new(ok_val),
                    err: Box::new(err_val),
                });
            }
            zero_value_for_tykind(builder, &typeck_ty_to_tykind(ty, struct_layouts)?, ptr_ty)
        }
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

fn build_stdlib_index(stdlib: &[crate::hir::HirModule]) -> StdlibIndex {
    let mut types = HashMap::new();
    for module in stdlib {
        let module_name = &module.name;
        // Add public structs
        for st in &module.structs {
            if st.is_pub {
                let qualified = format!("{}.{}", module_name, st.name);
                types.insert(st.name.clone(), qualified);
            }
        }
        // Add public enums
        for en in &module.enums {
            if en.is_pub {
                let qualified = format!("{}.{}", module_name, en.name);
                types.insert(en.name.clone(), qualified);
            }
        }
    }
    StdlibIndex { types }
}

fn build_enum_index(
    entry: &crate::hir::HirModule,
    user_modules: &[crate::hir::HirModule],
    stdlib: &[crate::hir::HirModule],
) -> EnumIndex {
    let mut variants = HashMap::new();
    let entry_name = &entry.name;
    let modules = stdlib
        .iter()
        .chain(user_modules.iter())
        .chain(std::iter::once(entry));
    for module in modules {
        let module_name = &module.name;
        for en in &module.enums {
            let mut map = HashMap::new();
            for (idx, variant) in en.variants.iter().enumerate() {
                map.insert(variant.name.clone(), idx as i32);
            }
            let qualified = format!("{}.{}", module_name, en.name);
            variants.insert(qualified, map.clone());
            if module_name == entry_name {
                variants.insert(en.name.clone(), map);
            }
        }
    }
    EnumIndex { variants }
}

fn build_struct_layout_index(
    entry: &crate::hir::HirModule,
    user_modules: &[crate::hir::HirModule],
    stdlib: &[crate::hir::HirModule],
    ptr_ty: Type,
) -> Result<StructLayoutIndex, CodegenError> {
    let modules = stdlib
        .iter()
        .chain(user_modules.iter())
        .chain(std::iter::once(entry))
        .collect::<Vec<_>>();

    let mut defs: HashMap<String, (&str, &crate::hir::HirStruct)> = HashMap::new();
    let mut name_counts: HashMap<String, usize> = HashMap::new();
    for module in &modules {
        for st in &module.structs {
            if st.is_opaque {
                continue;
            }
            let qualified = format!("{}.{}", module.name, st.name);
            defs.insert(qualified, (&module.name, st));
            *name_counts.entry(st.name.clone()).or_insert(0) += 1;
        }
    }

    let mut layouts = HashMap::new();
    let mut visiting = HashSet::new();
    for (qualified, (module_name, st)) in &defs {
        compute_struct_layout(
            qualified,
            module_name,
            st,
            &defs,
            &mut layouts,
            &mut visiting,
            ptr_ty,
        )?;
    }

    // Also register unqualified names when unambiguous.
    for module in &modules {
        for st in &module.structs {
            if st.is_opaque {
                continue;
            }
            let qualified = format!("{}.{}", module.name, st.name);
            if let Some(layout) = layouts.get(&qualified).cloned() {
                if name_counts.get(&st.name).copied().unwrap_or(0) == 1 {
                    layouts.entry(st.name.clone()).or_insert(layout);
                }
            }
        }
    }

    Ok(StructLayoutIndex { layouts })
}

fn compute_struct_layout(
    qualified: &str,
    module_name: &str,
    st: &crate::hir::HirStruct,
    defs: &HashMap<String, (&str, &crate::hir::HirStruct)>,
    layouts: &mut HashMap<String, StructLayout>,
    visiting: &mut HashSet<String>,
    ptr_ty: Type,
) -> Result<(), CodegenError> {
    if layouts.contains_key(qualified) {
        return Ok(());
    }
    if !visiting.insert(qualified.to_string()) {
        return Err(CodegenError::Unsupported(format!(
            "recursive struct layout for {qualified}"
        )));
    }

    let mut fields = HashMap::new();
    let mut field_order = Vec::new();
    let mut offset = 0u32;
    let mut align = 1u32;
    for field in &st.fields {
        let layout =
            type_layout_for_ty(&field.ty, module_name, defs, layouts, visiting, ptr_ty)?;
        offset = align_to(offset, layout.align);
        fields.insert(
            field.name.clone(),
            StructFieldLayout {
                offset,
                ty: field.ty.clone(),
            },
        );
        field_order.push(field.name.clone());
        offset = offset.saturating_add(layout.size);
        align = align.max(layout.align);
    }
    let size = align_to(offset, align);

    layouts.insert(
        qualified.to_string(),
        StructLayout {
            size,
            align,
            fields,
            field_order,
        },
    );
    visiting.remove(qualified);
    Ok(())
}

fn type_layout_for_ty(
    ty: &crate::typeck::Ty,
    module_name: &str,
    defs: &HashMap<String, (&str, &crate::hir::HirStruct)>,
    layouts: &mut HashMap<String, StructLayout>,
    visiting: &mut HashSet<String>,
    ptr_ty: Type,
) -> Result<TypeLayout, CodegenError> {
    use crate::typeck::Ty;

    if let Some(layout) = resolve_struct_layout(ty, module_name, layouts) {
        return Ok(TypeLayout {
            size: layout.size,
            align: layout.align,
        });
    }

    match ty {
        Ty::Path(name, args) if name == "Result" && args.len() == 2 => {
            let tag = TypeLayout { size: 1, align: 1 };
            let ok = type_layout_for_ty(&args[0], module_name, defs, layouts, visiting, ptr_ty)?;
            let err = type_layout_for_ty(&args[1], module_name, defs, layouts, visiting, ptr_ty)?;
            let mut offset = 0u32;
            let mut align = tag.align;
            offset = align_to(offset, tag.align);
            offset = offset.saturating_add(tag.size);
            offset = align_to(offset, ok.align);
            offset = offset.saturating_add(ok.size);
            offset = align_to(offset, err.align);
            offset = offset.saturating_add(err.size);
            align = align.max(ok.align).max(err.align);
            let size = align_to(offset, align);
            Ok(TypeLayout { size, align })
        }
        Ty::Path(name, _) => {
            if let Some((struct_module, struct_def)) = resolve_struct_def(name, module_name, defs)
            {
                compute_struct_layout(
                    &format!("{}.{}", struct_module, struct_def.name),
                    struct_module,
                    struct_def,
                    defs,
                    layouts,
                    visiting,
                    ptr_ty,
                )?;
                if let Some(layout) =
                    resolve_struct_layout(&Ty::Path(name.clone(), Vec::new()), module_name, layouts)
                {
                    return Ok(TypeLayout {
                        size: layout.size,
                        align: layout.align,
                    });
                }
            }
            let ty_kind = typeck_ty_to_tykind(ty, None)?;
            type_layout_for_tykind(&ty_kind, ptr_ty)
        }
        _ => {
            let ty_kind = typeck_ty_to_tykind(ty, None)?;
            type_layout_for_tykind(&ty_kind, ptr_ty)
        }
    }
}

fn emit_runtime_wrapper_call(
    builder: &mut FunctionBuilder,
    module: &mut ObjectModule,
    info: &FnInfo,
    mut args: Vec<Value>,
) -> Result<ValueRepr, CodegenError> {
    let abi_sig = info.abi_sig.as_ref().unwrap_or(&info.sig);
    let mut out_slots = None;
    let mut result_out = None;

    if abi_sig.ret == TyKind::ResultString {
        let ptr_ty = module.isa().pointer_type();
        let slot_ptr = builder.create_sized_stack_slot(ir::StackSlotData::new(
            ir::StackSlotKind::ExplicitSlot,
            ptr_ty.bytes(),
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
    let call_symbol = info
        .runtime_symbol
        .as_deref()
        .unwrap_or(&info.symbol);
    let func_id = module
        .declare_function(call_symbol, Linkage::Import, &sig)
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
        let ptr = builder
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
                return Ok(ValueRepr::Result {
                    tag: *tag,
                    ok: Box::new(ValueRepr::Pair(ptr, len)),
                    err: Box::new(ValueRepr::Single(err)),
                });
            }
            _ => return Err(CodegenError::Unsupported("result out params".to_string())),
        }
    }

    if let TyKind::ResultOut(_, _) = &abi_sig.ret {
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
            ValueRepr::Unit
        };
        let err_val = if let Some((slot, ty)) = err_slot {
            let addr = builder
                .ins()
                .stack_addr(module.isa().pointer_type(), slot, 0);
            let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
            ValueRepr::Single(val)
        } else {
            ValueRepr::Unit
        };
        match &info.sig.ret {
            TyKind::Result(_, _) => {
                return Ok(ValueRepr::Result {
                    tag: *tag,
                    ok: Box::new(ok_val),
                    err: Box::new(err_val),
                });
            }
            _ => {
                return Err(CodegenError::Unsupported(format!(
                    "result out params for {ok_ty:?}/{err_ty:?}"
                )))
            }
        }
    }

    let mut idx = 0;
    value_from_results(builder, &info.sig.ret, &results, &mut idx)
}

fn type_layout_for_tykind(ty: &TyKind, ptr_ty: Type) -> Result<TypeLayout, CodegenError> {
    match ty {
        TyKind::Unit => Ok(TypeLayout { size: 0, align: 1 }),
        TyKind::I32 | TyKind::U32 => Ok(TypeLayout { size: 4, align: 4 }),
        TyKind::U8 | TyKind::Bool => Ok(TypeLayout { size: 1, align: 1 }),
        TyKind::Handle => Ok(TypeLayout { size: 8, align: 8 }),
        TyKind::Ptr => Ok(TypeLayout {
            size: ptr_ty.bytes() as u32,
            align: ptr_ty.bytes() as u32,
        }),
        TyKind::String => {
            let ptr_size = ptr_ty.bytes() as u32;
            let len_offset = align_to(ptr_size, 8);
            Ok(TypeLayout {
                size: len_offset + 8,
                align: ptr_ty.bytes().max(8) as u32,
            })
        }
        TyKind::Result(ok, err) => {
            let tag = TypeLayout { size: 1, align: 1 };
            let ok = type_layout_for_tykind(ok, ptr_ty)?;
            let err = type_layout_for_tykind(err, ptr_ty)?;
            let mut offset = 0u32;
            let mut align = tag.align;
            offset = align_to(offset, tag.align);
            offset = offset.saturating_add(tag.size);
            offset = align_to(offset, ok.align);
            offset = offset.saturating_add(ok.size);
            offset = align_to(offset, err.align);
            offset = offset.saturating_add(err.size);
            align = align.max(ok.align).max(err.align);
            let size = align_to(offset, align);
            Ok(TypeLayout { size, align })
        }
        TyKind::ResultOut(_, _) | TyKind::ResultString => Err(CodegenError::Unsupported(
            "layout for result out params".to_string(),
        )),
    }
}

fn resolve_struct_layout<'a>(
    ty: &crate::typeck::Ty,
    module_name: &str,
    layouts: &'a HashMap<String, StructLayout>,
) -> Option<&'a StructLayout> {
    match ty {
        crate::typeck::Ty::Path(name, _) => {
            if name.contains('.') {
                layouts.get(name)
            } else {
                let qualified = format!("{module_name}.{name}");
                layouts.get(&qualified).or_else(|| layouts.get(name))
            }
        }
        _ => None,
    }
}

fn resolve_struct_def<'a>(
    name: &str,
    module_name: &str,
    defs: &'a HashMap<String, (&'a str, &'a crate::hir::HirStruct)>,
) -> Option<(&'a str, &'a crate::hir::HirStruct)> {
    if name.contains('.') {
        defs.get(name).copied()
    } else {
        let qualified = format!("{module_name}.{name}");
        defs.get(&qualified)
            .copied()
            .or_else(|| defs.get(name).copied())
    }
}

fn align_to(value: u32, align: u32) -> u32 {
    if align == 0 {
        return value;
    }
    let rem = value % align;
    if rem == 0 {
        value
    } else {
        value + (align - rem)
    }
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
    if resolved == "sys.system.RootCap"
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
