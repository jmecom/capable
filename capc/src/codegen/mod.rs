//! Code generation overview.
//!
//! This module lowers HIR into Cranelift IR and emits an object file.
//! The work is split into:
//! - `emit`: HIR -> Cranelift emission and ABI lowering helpers.
//! - `layout`: type/struct layout computation and enum indexing.

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::abi::AbiType;
use cranelift_codegen::ir::{self, AbiParam, Function, InstBuilder, Signature, Type};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{Configurable, Flags};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Linkage, Module as ModuleTrait};
use cranelift_native;
use cranelift_object::{ObjectBuilder, ObjectModule};
use thiserror::Error;

mod emit;
mod layout;

use emit::{emit_hir_stmt, emit_runtime_wrapper_call, flatten_value, store_local, value_from_params};
use layout::{build_enum_index, build_struct_layout_index};

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

/// Lowered function signature in codegen form.
#[derive(Clone, Debug, PartialEq, Eq)]
struct FnSig {
    params: Vec<AbiType>,
    ret: AbiType,
}

/// Metadata for a resolved function symbol.
#[derive(Clone, Debug)]
struct FnInfo {
    sig: FnSig,
    abi_sig: Option<FnSig>,
    symbol: String,
    runtime_symbol: Option<String>,
    is_runtime: bool,
}

/// Enum discriminant table used by codegen for matches and variants.
#[derive(Clone, Debug)]
struct EnumIndex {
    variants: HashMap<String, HashMap<String, i32>>,
}

/// Struct layout index used for field offsets and sizes.
#[derive(Clone, Debug)]
struct StructLayoutIndex {
    layouts: HashMap<String, StructLayout>,
}

/// Concrete layout for a struct value.
#[derive(Clone, Debug)]
struct StructLayout {
    size: u32,
    align: u32,
    fields: HashMap<String, StructFieldLayout>,
    field_order: Vec<String>,
}

/// Field layout metadata (offset + type).
#[derive(Clone, Debug)]
struct StructFieldLayout {
    offset: u32,
    ty: crate::hir::HirType,
}

/// Size/alignment pair used in layout computations.
#[derive(Clone, Copy, Debug)]
struct TypeLayout {
    size: u32,
    align: u32,
}

/// Lowered value representation for codegen emission.
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

/// Local storage representation used during emission.
#[derive(Clone, Debug)]
enum LocalValue {
    Value(ValueRepr),
    Slot(ir::StackSlot, Type),
    StructSlot(ir::StackSlot, crate::hir::HirType, u32),
}

/// Shape metadata for match-expression lowering.
#[derive(Clone, Debug)]
struct ResultShape {
    kind: ResultKind,
    slots: Vec<ir::StackSlot>,
    types: Vec<Type>,
}

/// Result shape kinds for match-expression lowering.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ResultKind {
    Unit,
    Single,
    Pair,
}

/// Build and write the object file for a fully-checked HIR program.
pub fn build_object(
    program: &crate::hir::HirProgram,
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
            &enum_index,
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
            &enum_index,
            &mut fn_map,
            &runtime_intrinsics,
            module.isa().pointer_type(),
            false,
        )?;
    }
    register_user_functions(
        &program.entry,
        &program.entry,
        &enum_index,
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

    for module_ref in &all_modules {
        register_extern_functions_from_hir(module_ref, &mut fn_map)?;
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
                let value =
                    value_from_params(&mut builder, &param.ty.abi, &params, &mut param_index)?;
                let local = store_local(&mut builder, value);
                locals.insert(param.name.clone(), local);
            }

            let mut terminated = false;
            for stmt in &func.body.stmts {
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

            if info.sig.ret == AbiType::Unit && !terminated {
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

fn append_ty_params(signature: &mut Signature, ty: &AbiType, ptr_ty: Type) {
    match ty {
        AbiType::Unit => {}
        AbiType::String => {
            signature.params.push(AbiParam::new(ptr_ty));
            signature.params.push(AbiParam::new(ir::types::I64));
        }
        AbiType::Handle => signature.params.push(AbiParam::new(ir::types::I64)),
        AbiType::Ptr => signature.params.push(AbiParam::new(ptr_ty)),
        AbiType::I32 => signature.params.push(AbiParam::new(ir::types::I32)),
        AbiType::U32 => signature.params.push(AbiParam::new(ir::types::I32)),
        AbiType::U8 => signature.params.push(AbiParam::new(ir::types::I8)),
        AbiType::Bool => signature.params.push(AbiParam::new(ir::types::I8)),
        AbiType::Result(ok, err) => {
            signature.params.push(AbiParam::new(ir::types::I8));
            append_ty_params(signature, ok, ptr_ty);
            append_ty_params(signature, err, ptr_ty);
        }
        AbiType::ResultOut(ok, err) => {
            if **ok != AbiType::Unit {
                signature.params.push(AbiParam::new(ptr_ty));
            }
            if **err != AbiType::Unit {
                signature.params.push(AbiParam::new(ptr_ty));
            }
        }
        AbiType::ResultString => {
            signature.params.push(AbiParam::new(ptr_ty));
            signature.params.push(AbiParam::new(ptr_ty));
            signature.params.push(AbiParam::new(ptr_ty));
        }
    }
}

fn append_ty_returns(signature: &mut Signature, ty: &AbiType, ptr_ty: Type) {
    match ty {
        AbiType::Unit => {}
        AbiType::I32 => signature.returns.push(AbiParam::new(ir::types::I32)),
        AbiType::U32 => signature.returns.push(AbiParam::new(ir::types::I32)),
        AbiType::U8 => signature.returns.push(AbiParam::new(ir::types::I8)),
        AbiType::Bool => signature.returns.push(AbiParam::new(ir::types::I8)),
        AbiType::Handle => signature.returns.push(AbiParam::new(ir::types::I64)),
        AbiType::Ptr => signature.returns.push(AbiParam::new(ptr_ty)),
        AbiType::String => {
            signature.returns.push(AbiParam::new(ptr_ty));
            signature.returns.push(AbiParam::new(ir::types::I64));
        }
        AbiType::Result(ok, err) => {
            signature.returns.push(AbiParam::new(ir::types::I8));
            append_ty_returns(signature, ok, ptr_ty);
            append_ty_returns(signature, err, ptr_ty);
        }
        AbiType::ResultOut(_, _) => {
            signature.returns.push(AbiParam::new(ir::types::I8));
        }
        AbiType::ResultString => {
            signature.returns.push(AbiParam::new(ir::types::I8));
        }
    }
}

fn register_runtime_intrinsics(ptr_ty: Type) -> HashMap<String, FnInfo> {
    let mut map = HashMap::new();
    let system_console = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let system_fs_read = FnSig {
        params: vec![AbiType::Handle, AbiType::String],
        ret: AbiType::Handle,
    };
    let system_filesystem = FnSig {
        params: vec![AbiType::Handle, AbiType::String],
        ret: AbiType::Handle,
    };
    let fs_root_dir = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let fs_subdir = FnSig {
        params: vec![AbiType::Handle, AbiType::String],
        ret: AbiType::Handle,
    };
    let fs_open_read = FnSig {
        params: vec![AbiType::Handle, AbiType::String],
        ret: AbiType::Handle,
    };
    let fs_read_to_string = FnSig {
        params: vec![AbiType::Handle, AbiType::String],
        ret: AbiType::Result(Box::new(AbiType::String), Box::new(AbiType::I32)),
    };
    let fs_read_to_string_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::String, AbiType::ResultString],
        ret: AbiType::ResultString,
    };
    let fs_file_read_to_string = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::String), Box::new(AbiType::I32)),
    };
    let fs_file_read_to_string_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::ResultString],
        ret: AbiType::ResultString,
    };
    let console_println = FnSig {
        params: vec![AbiType::Handle, AbiType::String],
        ret: AbiType::Unit,
    };
    let console_print = FnSig {
        params: vec![AbiType::Handle, AbiType::String],
        ret: AbiType::Unit,
    };
    let console_print_i32 = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Unit,
    };
    let math_i32 = FnSig {
        params: vec![AbiType::I32, AbiType::I32],
        ret: AbiType::I32,
    };
    let math_u32 = FnSig {
        params: vec![AbiType::U32, AbiType::U32],
        ret: AbiType::U32,
    };
    let math_u8 = FnSig {
        params: vec![AbiType::U8, AbiType::U8],
        ret: AbiType::U8,
    };
    let mem_malloc = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Ptr,
    };
    let mem_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Unit,
    };
    let mem_cast = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr],
        ret: AbiType::Ptr,
    };
    let mem_alloc_default = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let system_mint_args = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let system_mint_stdin = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let mem_slice_from_ptr = FnSig {
        params: vec![AbiType::Handle, AbiType::Ptr, AbiType::I32],
        ret: AbiType::Handle,
    };
    let mem_slice_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };
    let mem_slice_at = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::U8,
    };
    let mem_buffer_new = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let mem_buffer_new_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Handle), Box::new(AbiType::I32)),
    };
    let mem_buffer_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };
    let mem_buffer_push = FnSig {
        params: vec![AbiType::Handle, AbiType::U8],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let mem_buffer_push_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::U8,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let mem_buffer_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Unit,
    };
    let mem_buffer_as_slice = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let args_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };
    let args_at = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::String), Box::new(AbiType::I32)),
    };
    let args_at_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::ResultString],
        ret: AbiType::ResultString,
    };
    let io_read_stdin = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::String), Box::new(AbiType::I32)),
    };
    let io_read_stdin_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::ResultString],
        ret: AbiType::ResultString,
    };
    let console_assert = FnSig {
        params: vec![AbiType::Handle, AbiType::Bool],
        ret: AbiType::Unit,
    };
    let string_len = FnSig {
        params: vec![AbiType::String],
        ret: AbiType::I32,
    };
    let string_byte_at = FnSig {
        params: vec![AbiType::String, AbiType::I32],
        ret: AbiType::U8,
    };
    let string_as_slice = FnSig {
        params: vec![AbiType::String],
        ret: AbiType::Handle,
    };
    let string_split = FnSig {
        params: vec![AbiType::String],
        ret: AbiType::Handle,
    };
    let string_lines = FnSig {
        params: vec![AbiType::String],
        ret: AbiType::Handle,
    };
    let string_split_delim = FnSig {
        params: vec![AbiType::String, AbiType::U8],
        ret: AbiType::Handle,
    };
    let string_starts_with = FnSig {
        params: vec![AbiType::String, AbiType::String],
        ret: AbiType::Bool,
    };
    let vec_new = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let vec_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };
    let vec_u8_get = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::U8), Box::new(AbiType::I32)),
    };
    let vec_u8_get_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::U8), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::U8), Box::new(AbiType::I32)),
    };
    let vec_u8_set = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::U8],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_set_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::U8,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_push = FnSig {
        params: vec![AbiType::Handle, AbiType::U8],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_push_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::U8,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_u8_pop = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::U8), Box::new(AbiType::I32)),
    };
    let vec_u8_pop_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::U8), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::U8), Box::new(AbiType::I32)),
    };
    let vec_u8_as_slice = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Handle,
    };
    let vec_u8_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Unit,
    };
    let vec_i32_get = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::I32), Box::new(AbiType::I32)),
    };
    let vec_i32_get_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::I32), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::I32), Box::new(AbiType::I32)),
    };
    let vec_i32_set = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_set_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_push = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_push_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::I32,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_i32_pop = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::I32), Box::new(AbiType::I32)),
    };
    let vec_i32_pop_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::ResultOut(Box::new(AbiType::I32), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::I32), Box::new(AbiType::I32)),
    };
    let vec_i32_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Unit,
    };
    let vec_string_len = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::I32,
    };
    let vec_string_get = FnSig {
        params: vec![AbiType::Handle, AbiType::I32],
        ret: AbiType::Result(Box::new(AbiType::String), Box::new(AbiType::I32)),
    };
    let vec_string_get_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::I32, AbiType::ResultString],
        ret: AbiType::ResultString,
    };
    let vec_string_push = FnSig {
        params: vec![AbiType::Handle, AbiType::String],
        ret: AbiType::Result(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_string_push_abi = FnSig {
        params: vec![
            AbiType::Handle,
            AbiType::String,
            AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
        ],
        ret: AbiType::ResultOut(Box::new(AbiType::Unit), Box::new(AbiType::I32)),
    };
    let vec_string_pop = FnSig {
        params: vec![AbiType::Handle],
        ret: AbiType::Result(Box::new(AbiType::String), Box::new(AbiType::I32)),
    };
    let vec_string_pop_abi = FnSig {
        params: vec![AbiType::Handle, AbiType::ResultString],
        ret: AbiType::ResultString,
    };
    let vec_string_free = FnSig {
        params: vec![AbiType::Handle, AbiType::Handle],
        ret: AbiType::Unit,
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
        "sys.system.RootCap__mint_filesystem".to_string(),
        FnInfo {
            sig: system_filesystem,
            abi_sig: None,
            symbol: "capable_rt_mint_filesystem".to_string(),
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
        "sys.math.add_wrap_i32".to_string(),
        FnInfo {
            sig: math_i32.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_add_wrap_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.sub_wrap_i32".to_string(),
        FnInfo {
            sig: math_i32.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_sub_wrap_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.mul_wrap_i32".to_string(),
        FnInfo {
            sig: math_i32,
            abi_sig: None,
            symbol: "capable_rt_math_mul_wrap_i32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.add_wrap_u32".to_string(),
        FnInfo {
            sig: math_u32.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_add_wrap_u32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.sub_wrap_u32".to_string(),
        FnInfo {
            sig: math_u32.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_sub_wrap_u32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.mul_wrap_u32".to_string(),
        FnInfo {
            sig: math_u32,
            abi_sig: None,
            symbol: "capable_rt_math_mul_wrap_u32".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.add_wrap_u8".to_string(),
        FnInfo {
            sig: math_u8.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_add_wrap_u8".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.sub_wrap_u8".to_string(),
        FnInfo {
            sig: math_u8.clone(),
            abi_sig: None,
            symbol: "capable_rt_math_sub_wrap_u8".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.math.mul_wrap_u8".to_string(),
        FnInfo {
            sig: math_u8,
            abi_sig: None,
            symbol: "capable_rt_math_mul_wrap_u8".to_string(),
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
        "sys.fs.Filesystem__root_dir".to_string(),
        FnInfo {
            sig: fs_root_dir,
            abi_sig: None,
            symbol: "capable_rt_fs_root_dir".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Dir__subdir".to_string(),
        FnInfo {
            sig: fs_subdir,
            abi_sig: None,
            symbol: "capable_rt_fs_subdir".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.Dir__open_read".to_string(),
        FnInfo {
            sig: fs_open_read,
            abi_sig: None,
            symbol: "capable_rt_fs_open_read".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.fs.FileRead__read_to_string".to_string(),
        FnInfo {
            sig: fs_file_read_to_string,
            abi_sig: Some(fs_file_read_to_string_abi),
            symbol: "capable_rt_fs_file_read_to_string".to_string(),
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
        "sys.string.string__lines".to_string(),
        FnInfo {
            sig: string_lines,
            abi_sig: None,
            symbol: "capable_rt_string_split_lines".to_string(),
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
        "sys.string.string__starts_with".to_string(),
        FnInfo {
            sig: string_starts_with,
            abi_sig: None,
            symbol: "capable_rt_string_starts_with".to_string(),
            runtime_symbol: None,
            is_runtime: true,
        },
    );
    map.insert(
        "sys.bytes.u8__is_whitespace".to_string(),
        FnInfo {
            sig: FnSig {
                params: vec![AbiType::U8],
                ret: AbiType::Bool,
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

fn register_user_functions(
    module: &crate::hir::HirModule,
    entry: &crate::hir::HirModule,
    _enum_index: &EnumIndex,
    map: &mut HashMap<String, FnInfo>,
    runtime_intrinsics: &HashMap<String, FnInfo>,
    _ptr_ty: Type,
    is_stdlib: bool,
) -> Result<(), CodegenError> {
    let module_name = &module.name;
    for func in &module.functions {
        let sig = FnSig {
            params: func
                .params
                .iter()
                .map(|p| p.ty.abi.clone())
                .collect::<Vec<AbiType>>(),
            ret: func.ret_ty.abi.clone(),
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
    Ok(())
}

fn register_extern_functions_from_hir(
    module: &crate::hir::HirModule,
    map: &mut HashMap<String, FnInfo>,
) -> Result<(), CodegenError> {
    let module_name = &module.name;
    for func in &module.extern_functions {
        let sig = FnSig {
            params: func
                .params
                .iter()
                .map(|p| p.ty.abi.clone())
                .collect::<Vec<AbiType>>(),
            ret: func.ret_ty.abi.clone(),
        };
        let key = format!("{}.{}", module_name, func.name);
        map.insert(
            key,
            FnInfo {
                sig,
                abi_sig: None,
                symbol: func.name.clone(),
                runtime_symbol: None,
                is_runtime: true,
            },
        );
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
