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

use emit::{emit_hir_stmt, emit_runtime_wrapper_call, flatten_value, store_local, value_from_params};
use layout::{build_enum_index, build_struct_layout_index};

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
    validate_intrinsics(&fn_map, &runtime_intrinsics)?;
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
                    &sig_to_clif(
                        &info.sig,
                        module.isa().pointer_type(),
                        module.isa().default_call_conv(),
                    ),
                )
                .map_err(|err| CodegenError::Codegen(err.to_string()))?;
            let mut ctx = module.make_context();
            ctx.func = Function::with_name_signature(
                ir::UserFuncName::user(0, func_id.as_u32()),
                sig_to_clif(
                    &info.sig,
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

            let mut locals: HashMap<crate::hir::LocalId, LocalValue> = HashMap::new();
            let params = builder.block_params(block).to_vec();
            let mut param_index = 0;
            for param in &func.params {
                let value =
                    value_from_params(&mut builder, &param.ty.abi, &params, &mut param_index)?;
                let local = store_local(&mut builder, value);
                locals.insert(param.local_id, local);
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
                    None, // no loop context at function top level
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

/// Register runtime-backed intrinsics for stdlib symbols.
fn register_runtime_intrinsics(ptr_ty: Type) -> HashMap<String, FnInfo> {
    intrinsics::register_runtime_intrinsics(ptr_ty)
}

/// Register Capable-defined functions (stdlib or user) into the codegen map.
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

/// Register extern functions defined in HIR into the codegen map.
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

/// Validate stdlib/runtime intrinsic coverage in both directions.
fn validate_intrinsics(
    fn_map: &HashMap<String, FnInfo>,
    runtime_intrinsics: &HashMap<String, FnInfo>,
) -> Result<(), CodegenError> {
    let missing_wrappers = runtime_intrinsics
        .keys()
        .filter(|key| !fn_map.contains_key(*key))
        .cloned()
        .collect::<Vec<_>>();
    if !missing_wrappers.is_empty() {
        return Err(CodegenError::Codegen(format!(
            "runtime intrinsics missing stdlib wrappers: {}",
            missing_wrappers.join(", ")
        )));
    }

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
