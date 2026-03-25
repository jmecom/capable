use std::collections::HashMap;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, InstBuilder, MemFlags, Type, Value};
use cranelift_frontend::FunctionBuilder;
use cranelift_module::{Linkage, Module as ModuleTrait};
use cranelift_object::ObjectModule;

use crate::abi::AbiType;

use super::{
    aligned_slot_size, aligned_stack_addr, emit_hir_expr, is_non_opaque_struct_type,
    load_value_by_ty, resolve_struct_layout, sig_to_clif, store_value_by_ty,
    type_layout_from_index, value_from_results, value_type_for_result_out, ReturnLowering,
};
use super::super::{
    abi_quirks, CodegenError, EnumIndex, FnInfo, LocalValue, StructLayoutIndex, ValueRepr,
};

/// Emit a call to a runtime intrinsic with ABI adaptation when needed.
pub(crate) fn emit_runtime_wrapper_call(
    builder: &mut FunctionBuilder,
    module: &mut ObjectModule,
    info: &FnInfo,
    args: Vec<Value>,
    ret_ty: &crate::hir::HirType,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
) -> Result<ValueRepr, CodegenError> {
    ensure_abi_sig_handled(info)?;
    let abi_sig = info.abi_sig.as_ref().unwrap_or(&info.sig);
    let mut result_out = None;
    let mut sret_ptr = None;
    let mut call_args = args;

    enum ResultOutSlot {
        Scalar(ir::StackSlot, ir::Type, u32),
        Struct(ir::Value),
    }

    if info.sig.ret == AbiType::Ptr
        && abi_sig.ret == AbiType::Unit
        && (is_non_opaque_struct_type(ret_ty, struct_layouts)
            || matches!(&ret_ty.ty, crate::typeck::Ty::Path(name, _) if enum_index.layouts.contains_key(name)))
    {
        let ptr_ty = module.isa().pointer_type();
        let (size, align) =
            if let Some(layout) = resolve_struct_layout(&ret_ty.ty, "", &struct_layouts.layouts) {
                (layout.size, layout.align)
            } else if let crate::typeck::Ty::Path(name, _) = &ret_ty.ty {
                let layout = enum_index
                    .layouts
                    .get(name)
                    .ok_or_else(|| CodegenError::Unsupported("enum layout missing".to_string()))?;
                (layout.size, layout.align)
            } else {
                return Err(CodegenError::Unsupported(
                    "sret return layout missing".to_string(),
                ));
            };
        let align = align.max(1);
        let slot_size = aligned_slot_size(size, align);
        let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
            ir::StackSlotKind::ExplicitSlot,
            slot_size,
        ));
        let base_ptr = aligned_stack_addr(builder, slot, align, ptr_ty);
        call_args.insert(0, base_ptr);
        sret_ptr = Some(base_ptr);
    }

    if let AbiType::ResultOut(ok_ty, err_ty) = &abi_sig.ret {
        let ptr_ty = module.isa().pointer_type();
        let ok_slot = if **ok_ty == AbiType::Unit {
            None
        } else if **ok_ty == AbiType::Ptr {
            let align = ptr_ty.bytes().max(1) as u32;
            let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                aligned_slot_size(ptr_ty.bytes() as u32, align),
            ));
            let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
            call_args.push(addr);
            Some(ResultOutSlot::Struct(addr))
        } else {
            let ty = value_type_for_result_out(ok_ty, ptr_ty)?;
            let align = ty.bytes().max(1) as u32;
            debug_assert!(align.is_power_of_two());
            let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                aligned_slot_size(ty.bytes().max(1) as u32, align),
            ));
            let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
            call_args.push(addr);
            Some(ResultOutSlot::Scalar(slot, ty, align))
        };
        let err_slot = if **err_ty == AbiType::Unit {
            None
        } else if **err_ty == AbiType::Ptr {
            let align = ptr_ty.bytes().max(1) as u32;
            let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                aligned_slot_size(ptr_ty.bytes() as u32, align),
            ));
            let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
            call_args.push(addr);
            Some(ResultOutSlot::Struct(addr))
        } else {
            let ty = value_type_for_result_out(err_ty, ptr_ty)?;
            let align = ty.bytes().max(1) as u32;
            debug_assert!(align.is_power_of_two());
            let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                aligned_slot_size(ty.bytes().max(1) as u32, align),
            ));
            let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
            call_args.push(addr);
            Some(ResultOutSlot::Scalar(slot, ty, align))
        };
        result_out = Some((ok_slot, err_slot, ok_ty.clone(), err_ty.clone()));
    }

    let sig = sig_to_clif(
        abi_sig,
        module.isa().pointer_type(),
        module.isa().default_call_conv(),
    );
    let call_symbol = info.runtime_symbol.as_deref().unwrap_or(&info.symbol);
    let func_id = module
        .declare_function(call_symbol, Linkage::Import, &sig)
        .map_err(|err| CodegenError::Codegen(err.to_string()))?;
    let local = module.declare_func_in_func(func_id, builder.func);
    let call_inst = builder.ins().call(local, &call_args);
    let results = builder.inst_results(call_inst).to_vec();

    if abi_quirks::is_result_out(&abi_sig.ret) {
        let tag = results
            .first()
            .ok_or_else(|| CodegenError::Codegen("missing result tag".to_string()))?;
        let (ok_slot, err_slot, ok_ty, err_ty) =
            result_out.ok_or_else(|| CodegenError::Codegen("missing result slots".to_string()))?;
        let ok_val = if let Some(slot) = ok_slot {
            match slot {
                ResultOutSlot::Scalar(slot, ty, align) => {
                    let addr =
                        aligned_stack_addr(builder, slot, align, module.isa().pointer_type());
                    let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
                    ValueRepr::Single(val)
                }
                ResultOutSlot::Struct(addr) => ValueRepr::Single(addr),
            }
        } else {
            ValueRepr::Unit
        };
        let err_val = if let Some(slot) = err_slot {
            match slot {
                ResultOutSlot::Scalar(slot, ty, align) => {
                    let addr =
                        aligned_stack_addr(builder, slot, align, module.isa().pointer_type());
                    let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
                    ValueRepr::Single(val)
                }
                ResultOutSlot::Struct(addr) => ValueRepr::Single(addr),
            }
        } else {
            ValueRepr::Unit
        };
        match &info.sig.ret {
            AbiType::Result(_, _) => {
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

    if let Some(ptr) = sret_ptr {
        return Ok(ValueRepr::Single(ptr));
    }

    let mut idx = 0;
    value_from_results(builder, &info.sig.ret, &results, &mut idx)
}

pub(super) fn emit_unsafe_ptr_call(
    builder: &mut FunctionBuilder,
    module: &mut ObjectModule,
    call: &crate::hir::HirCall,
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    return_lowering: &ReturnLowering,
    data_counter: &mut u32,
) -> Result<Option<ValueRepr>, CodegenError> {
    let (module_path, func_name) = match &call.callee {
        crate::hir::ResolvedCallee::Function { module, name, .. } => (module, name),
        _ => return Ok(None),
    };
    if module_path != "sys.unsafe_ptr" {
        return Ok(None);
    }
    let base_name = func_name.split("__").next().unwrap_or(func_name);
    if call.type_args.len() != 1 {
        return Err(CodegenError::Unsupported(format!(
            "{base_name} expects one type argument"
        )));
    }
    let elem_ty = &call.type_args[0];
    let ptr_ty = module.isa().pointer_type();
    let elem_hir = hir_type_from_ty(elem_ty, enum_index, struct_layouts, ptr_ty)?;
    let layout = type_layout_from_index(&elem_hir, struct_layouts, ptr_ty)?;
    match base_name {
        "sizeof" => {
            let size = builder.ins().iconst(ir::types::I32, layout.size as i64);
            return Ok(Some(ValueRepr::Single(size)));
        }
        "alignof" => {
            let align = builder.ins().iconst(ir::types::I32, layout.align as i64);
            return Ok(Some(ValueRepr::Single(align)));
        }
        "ptr_cast" | "ptr_cast_u8" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Unsupported(format!(
                    "{base_name} expects (ptr)"
                )));
            }
            let base_ptr = match emit_hir_expr(
                builder,
                &call.args[0],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )? {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Unsupported(format!(
                        "{base_name} expects a pointer value"
                    )))
                }
            };
            return Ok(Some(ValueRepr::Single(base_ptr)));
        }
        "ptr_is_null" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Unsupported(
                    "ptr_is_null expects (ptr)".to_string(),
                ));
            }
            let base_ptr = match emit_hir_expr(
                builder,
                &call.args[0],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )? {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Unsupported(
                        "ptr_is_null expects a pointer value".to_string(),
                    ))
                }
            };
            let is_null = builder
                .ins()
                .icmp_imm(ir::condcodes::IntCC::Equal, base_ptr, 0);
            return Ok(Some(ValueRepr::Single(is_null)));
        }
        "ptr_add" => {
            if call.args.len() != 2 {
                return Err(CodegenError::Unsupported(
                    "ptr_add expects (ptr, offset)".to_string(),
                ));
            }
            let base_ptr = match emit_hir_expr(
                builder,
                &call.args[0],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )? {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Unsupported(
                        "ptr_add expects a pointer value".to_string(),
                    ))
                }
            };
            let offset_val = match emit_hir_expr(
                builder,
                &call.args[1],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )? {
                ValueRepr::Single(val) => val,
                _ => {
                    return Err(CodegenError::Unsupported(
                        "ptr_add expects an i32 offset".to_string(),
                    ))
                }
            };
            let offset = if ptr_ty != ir::types::I32 {
                builder.ins().sextend(ptr_ty, offset_val)
            } else {
                offset_val
            };
            let stride = builder.ins().iconst(ptr_ty, layout.size as i64);
            let byte_offset = if layout.size == 1 {
                offset
            } else {
                builder.ins().imul(offset, stride)
            };
            let addr = builder.ins().iadd(base_ptr, byte_offset);
            return Ok(Some(ValueRepr::Single(addr)));
        }
        "ptr_read" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Unsupported(
                    "ptr_read expects (ptr)".to_string(),
                ));
            }
            let base_ptr = match emit_hir_expr(
                builder,
                &call.args[0],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )? {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Unsupported(
                        "ptr_read expects a pointer value".to_string(),
                    ))
                }
            };
            let value = load_value_by_ty(
                builder,
                base_ptr,
                0,
                &elem_hir,
                enum_index,
                struct_layouts,
                module,
            )?;
            return Ok(Some(value));
        }
        "ptr_write" => {
            if call.args.len() != 2 {
                return Err(CodegenError::Unsupported(
                    "ptr_write expects (ptr, value)".to_string(),
                ));
            }
            let base_ptr = match emit_hir_expr(
                builder,
                &call.args[0],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )? {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Unsupported(
                        "ptr_write expects a pointer value".to_string(),
                    ))
                }
            };
            let value = emit_hir_expr(
                builder,
                &call.args[1],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            store_value_by_ty(
                builder,
                base_ptr,
                0,
                &elem_hir,
                value,
                enum_index,
                struct_layouts,
                module,
            )?;
            return Ok(Some(ValueRepr::Unit));
        }
        "memcpy" | "memmove" => {
            if call.args.len() != 3 {
                return Err(CodegenError::Unsupported(format!(
                    "{base_name} expects (dst, src, count)"
                )));
            }
            let dst_ptr = match emit_hir_expr(
                builder,
                &call.args[0],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )? {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Unsupported(format!(
                        "{base_name} expects a pointer dst"
                    )))
                }
            };
            let src_ptr = match emit_hir_expr(
                builder,
                &call.args[1],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )? {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Unsupported(format!(
                        "{base_name} expects a pointer src"
                    )))
                }
            };
            let count_val = match emit_hir_expr(
                builder,
                &call.args[2],
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )? {
                ValueRepr::Single(val) => val,
                _ => {
                    return Err(CodegenError::Unsupported(format!(
                        "{base_name} expects an i32 count"
                    )))
                }
            };

            let zero_i32 = builder.ins().iconst(ir::types::I32, 0);
            let should_copy = builder
                .ins()
                .icmp(IntCC::SignedGreaterThan, count_val, zero_i32);
            let copy_block = builder.create_block();
            let done_block = builder.create_block();
            builder
                .ins()
                .brif(should_copy, copy_block, &[], done_block, &[]);

            builder.switch_to_block(copy_block);
            builder.seal_block(copy_block);
            let count_ptr = if ptr_ty != ir::types::I32 {
                builder.ins().sextend(ptr_ty, count_val)
            } else {
                count_val
            };
            let stride = builder.ins().iconst(ptr_ty, layout.size as i64);
            let byte_count = if layout.size == 1 {
                count_ptr
            } else {
                builder.ins().imul(count_ptr, stride)
            };
            let config = module.isa().frontend_config();
            if base_name == "memcpy" {
                builder.call_memcpy(config, dst_ptr, src_ptr, byte_count);
            } else {
                builder.call_memmove(config, dst_ptr, src_ptr, byte_count);
            }
            builder.ins().jump(done_block, &[]);

            builder.switch_to_block(done_block);
            builder.seal_block(done_block);
            return Ok(Some(ValueRepr::Unit));
        }
        _ => {}
    }
    Ok(None)
}

fn hir_type_from_ty(
    ty: &crate::typeck::Ty,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    ptr_ty: Type,
) -> Result<crate::hir::HirType, CodegenError> {
    use crate::typeck::{BuiltinType, Ty};
    let abi = match ty {
        Ty::Builtin(b) => match b {
            BuiltinType::I32 => AbiType::I32,
            BuiltinType::I64 => {
                return Err(CodegenError::Unsupported(
                    "i64 is not supported by the current codegen backend".to_string(),
                ))
            }
            BuiltinType::U32 => AbiType::U32,
            BuiltinType::U8 => AbiType::U8,
            BuiltinType::Bool => AbiType::Bool,
            BuiltinType::Unit | BuiltinType::Never => AbiType::Unit,
        },
        Ty::Ptr(_) => AbiType::Ptr,
        Ty::Ref(inner) => {
            return hir_type_from_ty(inner, enum_index, struct_layouts, ptr_ty);
        }
        Ty::Param(_) => {
            return Err(CodegenError::Unsupported(
                "generic type parameters must be monomorphized before codegen".to_string(),
            ))
        }
        Ty::Path(name, _args) => {
            if resolve_struct_layout(ty, "", &struct_layouts.layouts).is_some() {
                AbiType::Ptr
            } else if enum_index.layouts.contains_key(name) {
                AbiType::Ptr
            } else if enum_index.variants.contains_key(name) {
                AbiType::I32
            } else {
                AbiType::Handle
            }
        }
    };
    Ok(crate::hir::HirType {
        ty: ty.clone(),
        abi,
    })
}

pub(super) fn ensure_abi_sig_handled(info: &FnInfo) -> Result<(), CodegenError> {
    let Some(abi_sig) = info.abi_sig.as_ref() else {
        return Ok(());
    };
    if abi_sig == &info.sig {
        return Ok(());
    }
    if abi_quirks::abi_sig_requires_lowering(abi_sig, &info.sig) {
        Ok(())
    } else {
        Err(CodegenError::Codegen(format!(
            "abi signature mismatch for {} without ResultOut lowering",
            info.symbol
        )))
    }
}
