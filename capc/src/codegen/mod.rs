//! Code generation overview.
//!
//! This module lowers HIR into Cranelift IR and emits an object file.
//! The work is split into:
//! - `emit`: HIR -> Cranelift emission and ABI lowering helpers.
//! - `layout`: type/struct layout computation and enum indexing.

#![allow(unused_assignments)]

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::abi::AbiType;
use crate::error::format_with_context;
use cranelift_codegen::ir::{self, AbiParam, Function, InstBuilder, Signature, Type};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{Configurable, Flags};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Linkage, Module as ModuleTrait};
use cranelift_native;
use cranelift_object::{ObjectBuilder, ObjectModule};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

mod emit;
mod abi_quirks;
mod intrinsics;
mod layout;

use emit::{
    emit_hir_stmt, emit_runtime_wrapper_call, flatten_value, store_local, value_from_params,
    DeferStack, ReturnLowering,
};
use layout::{build_enum_index, build_struct_layout_index, resolve_struct_layout};

#[derive(Debug, Error, Diagnostic)]
#[allow(unused_assignments)]
pub enum CodegenError {
    #[error("{message}")]
    #[diagnostic(code(codegen::spanned))]
    Spanned {
        #[allow(dead_code)]
        message: String,
        #[label]
        span: SourceSpan,
        #[allow(dead_code)]
        span_raw: crate::ast::Span,
    },
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

impl CodegenError {
    pub fn spanned(message: impl Into<String>, span: crate::ast::Span) -> Self {
        let source_span: SourceSpan = (span.start, span.end.saturating_sub(span.start)).into();
        CodegenError::Spanned {
            message: message.into(),
            span: source_span,
            span_raw: span,
        }
    }

    pub fn with_span(self, span: crate::ast::Span) -> Self {
        match self {
            CodegenError::Spanned { .. } => self,
            other => CodegenError::spanned(other.to_string(), span),
        }
    }

    pub fn with_context(self, context: impl AsRef<str>) -> Self {
        match self {
            CodegenError::Spanned {
                message,
                span,
                span_raw,
            } => CodegenError::Spanned {
                message: format_with_context(context, message),
                span,
                span_raw,
            },
            other => CodegenError::Codegen(format_with_context(context, other.to_string())),
        }
    }
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
    payloads: HashMap<String, HashMap<String, Option<crate::hir::HirType>>>,
    layouts: HashMap<String, EnumLayout>,
}

/// Layout metadata for enums with payloads (tag + payload).
#[derive(Clone, Debug)]
struct EnumLayout {
    payload_offset: u32,
    payload_size: u32,
    size: u32,
    align: u32,
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

    let struct_layouts = build_struct_layout_index(
        &program.entry,
        &program.user_modules,
        &program.stdlib,
        module.isa().pointer_type(),
    )?;
    let enum_index = build_enum_index(
        &program.entry,
        &program.user_modules,
        &program.stdlib,
        &struct_layouts,
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
            &struct_layouts,
            module.isa().pointer_type(),
            true,
        )?;
    }
    validate_intrinsics(&fn_map, &runtime_intrinsics)?;
    for module_ref in &program.user_modules {
        register_user_functions(
            module_ref,
            &program.entry,
            &enum_index,
            &mut fn_map,
            &runtime_intrinsics,
            &struct_layouts,
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
        &struct_layouts,
        module.isa().pointer_type(),
        false,
    )?;

    let all_modules = program.user_modules
        .iter()
        .chain(program.stdlib.iter())
        .chain(std::iter::once(&program.entry))
        .collect::<Vec<_>>();

    for module_ref in &all_modules {
        register_extern_functions_from_hir(module_ref, &mut fn_map, &enum_index, &struct_layouts)?;
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
            let abi_sig = info.abi_sig.as_ref().unwrap_or(&info.sig);
            let sig_for_codegen = if info.runtime_symbol.is_some() {
                &info.sig
            } else {
                abi_sig
            };
            let func_id = module
                .declare_function(
                    &info.symbol,
                    Linkage::Export,
                    &sig_to_clif(
                        sig_for_codegen,
                        module.isa().pointer_type(),
                        module.isa().default_call_conv(),
                    ),
                )
                .map_err(|err| CodegenError::Codegen(err.to_string()))?;
            let mut ctx = module.make_context();
            ctx.func = Function::with_name_signature(
                ir::UserFuncName::user(0, func_id.as_u32()),
                sig_to_clif(
                    sig_for_codegen,
                    module.isa().pointer_type(),
                    module.isa().default_call_conv(),
                ),
            );

            let mut builder_ctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
            let block = builder.create_block();
            builder.append_block_params_for_function_params(block);
            builder.switch_to_block(block);
            // Blocks sealed after body emission.

            if info.runtime_symbol.is_some() {
                let args = builder.block_params(block).to_vec();
                let value = emit_runtime_wrapper_call(
                    &mut builder,
                    &mut module,
                    &info,
                    args,
                    &func.ret_ty,
                    &enum_index,
                    &struct_layouts,
                )?;
                match value {
                    ValueRepr::Unit => builder.ins().return_(&[]),
                    ValueRepr::Single(val) => builder.ins().return_(&[val]),
                    ValueRepr::Result { .. } => {
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

            let mut locals: HashMap<crate::hir::LocalId, LocalValue> = HashMap::new();
            let params = builder.block_params(block).to_vec();
            let mut param_index = 0;
            let mut return_lowering = ReturnLowering::Direct;

            let is_enum_payload = match &func.ret_ty.ty {
                crate::typeck::Ty::Path(name, _) => enum_index.layouts.contains_key(name),
                _ => false,
            };
            if info.sig.ret == AbiType::Ptr
                && abi_sig.ret == AbiType::Unit
                && (resolve_struct_layout(&func.ret_ty.ty, "", &struct_layouts.layouts).is_some()
                    || is_enum_payload)
            {
                let out_ptr = params
                    .get(0)
                    .copied()
                    .ok_or_else(|| CodegenError::Codegen("missing sret param".to_string()))?;
                return_lowering = ReturnLowering::SRet {
                    out_ptr,
                    ret_ty: func.ret_ty.clone(),
                };
                param_index = 1;
            } else if let AbiType::ResultOut(ok_abi, err_abi) = &abi_sig.ret {
                let (ok_ty, err_ty) = match &func.ret_ty.ty {
                    crate::typeck::Ty::Path(name, args)
                        if name == "sys.result.Result" && args.len() == 2 =>
                    {
                        (
                            crate::hir::HirType {
                                ty: args[0].clone(),
                                abi: (**ok_abi).clone(),
                            },
                            crate::hir::HirType {
                                ty: args[1].clone(),
                                abi: (**err_abi).clone(),
                            },
                        )
                    }
                    _ => {
                        return Err(CodegenError::Codegen(
                            "result out params missing result type".to_string(),
                        ))
                    }
                };
                return_lowering = ReturnLowering::ResultOut {
                    out_ok: None,
                    out_err: None,
                    ok_ty,
                    err_ty,
                };
            }
            for param in &func.params {
                let value =
                    value_from_params(&mut builder, &param.ty.abi, &params, &mut param_index)?;
                let local = store_local(&mut builder, value);
                locals.insert(param.local_id, local);
            }
            if let ReturnLowering::ResultOut {
                out_ok,
                out_err,
                ok_ty,
                err_ty,
            } = &mut return_lowering
            {
                if ok_ty.abi != AbiType::Unit {
                    *out_ok = Some(
                        params
                            .get(param_index)
                            .copied()
                            .ok_or_else(|| CodegenError::Codegen("missing ok out param".to_string()))?,
                    );
                    param_index += 1;
                }
                if err_ty.abi != AbiType::Unit {
                    *out_err = Some(
                        params
                            .get(param_index)
                            .copied()
                            .ok_or_else(|| CodegenError::Codegen("missing err out param".to_string()))?,
                    );
                }
            }
            let mut defer_stack = DeferStack::new();
            defer_stack.push_block_scope();

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
                    None, // no loop context at function top level
                    &return_lowering,
                    &mut defer_stack,
                )?;
                if flow == Flow::Terminated {
                    terminated = true;
                    break;
                }
            }

            if info.sig.ret == AbiType::Unit && !terminated {
                defer_stack.emit_all_and_clear(
                    &mut builder,
                    &locals,
                    &fn_map,
                    &enum_index,
                    &struct_layouts,
                    &return_lowering,
                    &mut module,
                    &mut data_counter,
                )?;
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

/// Lower a codegen signature into a Cranelift signature.
fn sig_to_clif(sig: &FnSig, ptr_ty: Type, call_conv: CallConv) -> Signature {
    let mut signature = Signature::new(call_conv);
    for param in &sig.params {
        append_ty_params(&mut signature, param, ptr_ty);
    }
    append_ty_returns(&mut signature, &sig.ret, ptr_ty);
    signature
}

/// Append the ABI parameters for a single type.
fn append_ty_params(signature: &mut Signature, ty: &AbiType, ptr_ty: Type) {
    match ty {
        AbiType::Unit => {}
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
    }
}

/// Append the ABI return values for a single type.
fn append_ty_returns(signature: &mut Signature, ty: &AbiType, ptr_ty: Type) {
    match ty {
        AbiType::Unit => {}
        AbiType::I32 => signature.returns.push(AbiParam::new(ir::types::I32)),
        AbiType::U32 => signature.returns.push(AbiParam::new(ir::types::I32)),
        AbiType::U8 => signature.returns.push(AbiParam::new(ir::types::I8)),
        AbiType::Bool => signature.returns.push(AbiParam::new(ir::types::I8)),
        AbiType::Handle => signature.returns.push(AbiParam::new(ir::types::I64)),
        AbiType::Ptr => signature.returns.push(AbiParam::new(ptr_ty)),
        AbiType::Result(ok, err) => {
            signature.returns.push(AbiParam::new(ir::types::I8));
            append_ty_returns(signature, ok, ptr_ty);
            append_ty_returns(signature, err, ptr_ty);
        }
        AbiType::ResultOut(_, _) => {
            signature.returns.push(AbiParam::new(ir::types::I8));
        }
    }
}

/// Register runtime-backed intrinsics for stdlib symbols.
fn register_runtime_intrinsics(ptr_ty: Type) -> HashMap<String, FnInfo> {
    intrinsics::register_runtime_intrinsics(ptr_ty)
}

fn is_non_opaque_struct_type(
    ty: &crate::typeck::Ty,
    struct_layouts: &StructLayoutIndex,
) -> bool {
    resolve_struct_layout(ty, "", &struct_layouts.layouts).is_some()
}

fn is_payload_enum_type(ty: &crate::typeck::Ty, enum_index: &EnumIndex) -> bool {
    match ty {
        crate::typeck::Ty::Path(name, _) => enum_index.layouts.contains_key(name),
        _ => false,
    }
}

fn lowered_abi_sig_for_return(
    sig: &FnSig,
    ret_ty: &crate::hir::HirType,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
) -> Option<FnSig> {
    if let crate::typeck::Ty::Path(name, args) = &ret_ty.ty {
        if name == "sys.result.Result" && args.len() == 2 {
            let ok_is_struct = is_non_opaque_struct_type(&args[0], struct_layouts)
                || is_payload_enum_type(&args[0], enum_index);
            let err_is_struct = is_non_opaque_struct_type(&args[1], struct_layouts)
                || is_payload_enum_type(&args[1], enum_index);
            if ok_is_struct || err_is_struct {
                let AbiType::Result(ok_abi, err_abi) = &ret_ty.abi else {
                    return None;
                };
                let mut params = sig.params.clone();
                params.push(AbiType::ResultOut(ok_abi.clone(), err_abi.clone()));
                return Some(FnSig {
                    params,
                    ret: AbiType::ResultOut(ok_abi.clone(), err_abi.clone()),
                });
            }
        }
    }

    if is_non_opaque_struct_type(&ret_ty.ty, struct_layouts)
        || is_payload_enum_type(&ret_ty.ty, enum_index)
    {
        let mut params = Vec::with_capacity(sig.params.len() + 1);
        params.push(AbiType::Ptr);
        params.extend(sig.params.iter().cloned());
        return Some(FnSig {
            params,
            ret: AbiType::Unit,
        });
    }

    None
}

/// Register Capable-defined functions (stdlib or user) into the codegen map.
fn register_user_functions(
    module: &crate::hir::HirModule,
    entry: &crate::hir::HirModule,
    enum_index: &EnumIndex,
    map: &mut HashMap<String, FnInfo>,
    runtime_intrinsics: &HashMap<String, FnInfo>,
    struct_layouts: &StructLayoutIndex,
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
        let abi_sig = abi_sig.or_else(|| {
            lowered_abi_sig_for_return(&sig, &func.ret_ty, enum_index, struct_layouts)
        });
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

/// Register extern functions defined in HIR into the codegen map.
fn register_extern_functions_from_hir(
    module: &crate::hir::HirModule,
    map: &mut HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
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
        let abi_sig = lowered_abi_sig_for_return(&sig, &func.ret_ty, enum_index, struct_layouts);
        map.insert(
            key,
            FnInfo {
                sig,
                abi_sig,
                symbol: func.name.clone(),
                runtime_symbol: None,
                is_runtime: true,
            },
        );
    }
    Ok(())
}

/// Validate stdlib/runtime intrinsic coverage in both directions.
fn validate_intrinsics(
    fn_map: &HashMap<String, FnInfo>,
    runtime_intrinsics: &HashMap<String, FnInfo>,
) -> Result<(), CodegenError> {
    let unknown_wrappers = fn_map
        .iter()
        .filter(|(key, info)| info.runtime_symbol.is_some() && !runtime_intrinsics.contains_key(*key))
        .map(|(key, _)| key.clone())
        .collect::<Vec<_>>();
    if !unknown_wrappers.is_empty() {
        return Err(CodegenError::Codegen(format!(
            "stdlib wrappers missing intrinsic registration: {}",
            unknown_wrappers.join(", ")
        )));
    }
    Ok(())
}

/// Build the stable, linkable symbol name for a function.
fn mangle_symbol(module_name: &str, func_name: &str) -> String {
    let mut out = String::from("capable_");
    out.push_str(&module_name.replace('.', "_"));
    out.push('_');
    out.push_str(func_name);
    out
}
