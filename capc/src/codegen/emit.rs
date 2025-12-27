//! HIR emission into Cranelift IR.
//!
//! This module is intentionally focused on expression/statement lowering and
//! ABI-adjacent helper routines used by the main codegen entry point.

use std::collections::HashMap;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, InstBuilder, MemFlags, Type, Value};
use cranelift_frontend::FunctionBuilder;
use cranelift_module::{DataDescription, Linkage, Module as ModuleTrait};
use cranelift_object::ObjectModule;

use crate::ast::{BinaryOp, Literal, UnaryOp};

use super::{
    CodegenError, EnumIndex, Flow, FnInfo, LocalValue, ResultKind, ResultShape, StructLayoutIndex,
    TyKind, TypeLayout, ValueRepr,
};
use super::layout::{align_to, resolve_struct_layout, type_layout_for_tykind};
use super::{sig_to_clif, typeck_ty_to_tykind};


/// Emit a single HIR statement.
pub(super) fn emit_hir_stmt(
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

/// Emit a single HIR expression and return its lowered value representation.
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
                crate::hir::ResolvedCallee::Intrinsic(crate::hir::IntrinsicId::Drop) => {
                    for arg in &call.args {
                        let _ = emit_hir_expr(
                            builder,
                            arg,
                            locals,
                            fn_map,
                            enum_index,
                            struct_layouts,
                            module,
                            data_counter,
                        )?;
                    }
                    return Ok(ValueRepr::Unit);
                }
            };

            // Lookup in fn_map by module.function key
            let key = format!("{}.{}", module_path, func_name);
            let info = fn_map
                .get(&key)
                .ok_or_else(|| CodegenError::UnknownFunction(key.clone()))?
                .clone();
            ensure_abi_sig_handled(&info)?;

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
                let ptr_align = ptr_ty.bytes() as u32;
                let len_bytes = result_string_len_bytes();
                let len_align = len_bytes;
                let err_align = 4u32;

                let slot_ptr = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    aligned_slot_size(ptr_ty.bytes() as u32, ptr_align),
                ));
                let slot_len = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    aligned_slot_size(len_bytes, len_align),
                ));
                let slot_err = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    aligned_slot_size(4, err_align),
                ));

                let ptr_ptr = aligned_stack_addr(builder, slot_ptr, ptr_align, ptr_ty);
                let len_ptr = aligned_stack_addr(builder, slot_len, len_align, ptr_ty);
                let err_ptr = aligned_stack_addr(builder, slot_err, err_align, ptr_ty);

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
                    let align = ty.bytes().max(1) as u32;
                    debug_assert!(align.is_power_of_two());
                    let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        aligned_slot_size(ty.bytes().max(1) as u32, align),
                    ));
                    let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
                    args.push(addr);
                    Some((slot, ty, align))
                };
                let err_slot = if **err_ty == TyKind::Unit {
                    None
                } else {
                    let ty = value_type_for_result_out(err_ty, ptr_ty)?;
                    let align = ty.bytes().max(1) as u32;
                    debug_assert!(align.is_power_of_two());
                    let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        aligned_slot_size(ty.bytes().max(1) as u32, align),
                    ));
                    let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
                    args.push(addr);
                    Some((slot, ty, align))
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
                let ptr_ty = module.isa().pointer_type();
                let ptr_align = ptr_ty.bytes() as u32;
                let len_align = result_string_len_bytes();
                let err_align = 4u32;
                let ptr_addr = aligned_stack_addr(builder, slot_ptr, ptr_align, ptr_ty);
                let len_addr = aligned_stack_addr(builder, slot_len, len_align, ptr_ty);
                let err_addr = aligned_stack_addr(builder, slot_err, err_align, ptr_ty);
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
                let ok_val = if let Some((slot, ty, align)) = ok_slot {
                    let addr = aligned_stack_addr(
                        builder,
                        slot,
                        align,
                        module.isa().pointer_type(),
                    );
                    let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
                    ValueRepr::Single(val)
                } else {
                    ValueRepr::Unit
                };
                let err_val = if let Some((slot, ty, align)) = err_slot {
                    let addr = aligned_stack_addr(
                        builder,
                        slot,
                        align,
                        module.isa().pointer_type(),
                    );
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

/// Emit a struct literal into a stack slot and return its address value.
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

/// Emit field access by computing the field address/offset.
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

/// Store a lowered value into memory using a typeck::Ty layout.
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
        Ty::Ref(inner) => store_value_by_ty(
            builder,
            base_ptr,
            offset,
            inner,
            value,
            struct_layouts,
            module,
        ),
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

/// Store a lowered value into memory using a codegen TyKind layout.
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

/// Load a value from memory using a typeck::Ty layout.
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
        Ty::Ref(inner) => load_value_by_ty(
            builder,
            base_ptr,
            offset,
            inner,
            struct_layouts,
            module,
        ),
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

/// Load a value from memory using a codegen TyKind layout.
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

/// Pointer addition helper (byte offset).
fn ptr_add(builder: &mut FunctionBuilder, base: ir::Value, offset: u32) -> ir::Value {
    if offset == 0 {
        base
    } else {
        builder.ins().iadd_imm(base, i64::from(offset))
    }
}

/// Compute an aligned stack address for a struct slot.
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

/// Size for an explicitly-aligned stack slot (size + align padding).
fn aligned_slot_size(size: u32, align: u32) -> u32 {
    let align = align.max(1);
    size.max(1).saturating_add(align.saturating_sub(1))
}

/// ResultString ABI uses a u64 length slot across targets.
fn result_string_len_bytes() -> u32 {
    8
}

/// Compute pointer/len offsets for the string layout.
fn string_offsets(ptr_ty: Type) -> (u32, u32) {
    let ptr_size = ptr_ty.bytes() as u32;
    let len_offset = align_to(ptr_size, 8);
    (0, len_offset)
}

/// Compute offsets for Result layout (tag, ok, err).
fn result_offsets(ok: TypeLayout, err: TypeLayout) -> (u32, u32, u32) {
    let tag_offset = 0u32;
    let ok_offset = align_to(1, ok.align);
    let err_offset = align_to(ok_offset.saturating_add(ok.size), err.align);
    (tag_offset, ok_offset, err_offset)
}

/// Lookup a layout for a typeck::Ty from the struct layout index.
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

/// Emit short-circuit logic for `&&` and `||`.
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
/// Emit HIR match as statement (arms can contain returns, don't produce values).
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
/// Emit HIR match expression (arms produce a value).
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
/// Compute the condition for an HIR pattern match.
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
/// Bind pattern variables for HIR patterns.
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

/// Convert a ValueRepr into a boolean condition value.
fn to_b1(builder: &mut FunctionBuilder, value: ValueRepr) -> Result<ir::Value, CodegenError> {
    match value {
        ValueRepr::Single(val) => Ok(builder.ins().icmp_imm(IntCC::NotEqual, val, 0)),
        ValueRepr::Unit => Err(CodegenError::Unsupported("unit condition".to_string())),
        ValueRepr::Pair(_, _) | ValueRepr::Result { .. } => {
            Err(CodegenError::Unsupported("string condition".to_string()))
        }
    }
}

/// Normalize boolean values to i8 for ABI uses.
fn bool_to_i8(builder: &mut FunctionBuilder, value: ir::Value) -> ir::Value {
    let ty = builder.func.dfg.value_type(value);
    if ty == ir::types::I8 {
        value
    } else {
        builder.ins().uextend(ir::types::I8, value)
    }
}

/// Emit a string literal into the data section and return (ptr, len).
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

/// Flatten a ValueRepr into ABI-ready Cranelift values.
pub(super) fn flatten_value(value: &ValueRepr) -> Vec<ir::Value> {
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

/// Store a value into a local slot where needed.
pub(super) fn store_local(builder: &mut FunctionBuilder, value: ValueRepr) -> LocalValue {
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

/// Load a local value from its storage representation.
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

/// Compute the ABI type used for ResultOut payloads.
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

/// Construct a zero/empty value for a codegen TyKind.
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

/// Construct a zero/empty value for a typeck::Ty.
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
        Ty::Ref(inner) => zero_value_for_ty(builder, inner, ptr_ty, struct_layouts),
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

/// Reconstruct a ValueRepr from ABI parameters.
pub(super) fn value_from_params(
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

/// Reconstruct a ValueRepr from ABI return values.
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

/// Emit a call to a runtime intrinsic with ABI adaptation when needed.
pub(super) fn emit_runtime_wrapper_call(
    builder: &mut FunctionBuilder,
    module: &mut ObjectModule,
    info: &FnInfo,
    mut args: Vec<Value>,
) -> Result<ValueRepr, CodegenError> {
    ensure_abi_sig_handled(info)?;
    let abi_sig = info.abi_sig.as_ref().unwrap_or(&info.sig);
    let mut out_slots = None;
    let mut result_out = None;

    if abi_sig.ret == TyKind::ResultString {
        let ptr_ty = module.isa().pointer_type();
        let ptr_align = ptr_ty.bytes() as u32;
        let len_bytes = result_string_len_bytes();
        let len_align = len_bytes;
        let err_align = 4u32;
        let slot_ptr = builder.create_sized_stack_slot(ir::StackSlotData::new(
            ir::StackSlotKind::ExplicitSlot,
            aligned_slot_size(ptr_ty.bytes(), ptr_align),
        ));
        let slot_len = builder.create_sized_stack_slot(ir::StackSlotData::new(
            ir::StackSlotKind::ExplicitSlot,
            aligned_slot_size(len_bytes, len_align),
        ));
        let slot_err = builder.create_sized_stack_slot(ir::StackSlotData::new(
            ir::StackSlotKind::ExplicitSlot,
            aligned_slot_size(4, err_align),
        ));

        let ptr_ptr = aligned_stack_addr(builder, slot_ptr, ptr_align, ptr_ty);
        let len_ptr = aligned_stack_addr(builder, slot_len, len_align, ptr_ty);
        let err_ptr = aligned_stack_addr(builder, slot_err, err_align, ptr_ty);

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
            let align = ty.bytes().max(1) as u32;
            debug_assert!(align.is_power_of_two());
            let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                aligned_slot_size(ty.bytes().max(1) as u32, align),
            ));
            let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
            args.push(addr);
            Some((slot, ty, align))
        };
        let err_slot = if **err_ty == TyKind::Unit {
            None
        } else {
            let ty = value_type_for_result_out(err_ty, ptr_ty)?;
            let align = ty.bytes().max(1) as u32;
            debug_assert!(align.is_power_of_two());
            let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                aligned_slot_size(ty.bytes().max(1) as u32, align),
            ));
            let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
            args.push(addr);
            Some((slot, ty, align))
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
        let ptr_ty = module.isa().pointer_type();
        let ptr_align = ptr_ty.bytes() as u32;
        let len_align = result_string_len_bytes();
        let err_align = 4u32;
        let ptr_addr = aligned_stack_addr(builder, slot_ptr, ptr_align, ptr_ty);
        let len_addr = aligned_stack_addr(builder, slot_len, len_align, ptr_ty);
        let err_addr = aligned_stack_addr(builder, slot_err, err_align, ptr_ty);
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
        let ok_val = if let Some((slot, ty, align)) = ok_slot {
            let addr = aligned_stack_addr(builder, slot, align, module.isa().pointer_type());
            let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
            ValueRepr::Single(val)
        } else {
            ValueRepr::Unit
        };
        let err_val = if let Some((slot, ty, align)) = err_slot {
            let addr = aligned_stack_addr(builder, slot, align, module.isa().pointer_type());
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

fn ensure_abi_sig_handled(info: &FnInfo) -> Result<(), CodegenError> {
    let Some(abi_sig) = info.abi_sig.as_ref() else {
        return Ok(());
    };
    if abi_sig == &info.sig {
        return Ok(());
    }
    match abi_sig.ret {
        TyKind::ResultString | TyKind::ResultOut(_, _) => Ok(()),
        _ => Err(CodegenError::Codegen(format!(
            "abi signature mismatch for {} without ResultString/ResultOut lowering",
            info.symbol
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::{FnInfo, FnSig};

    #[test]
    fn aligned_slot_size_adds_padding_for_alignment() {
        assert_eq!(aligned_slot_size(1, 4), 4);
        assert_eq!(aligned_slot_size(8, 8), 15);
    }

    #[test]
    fn result_string_len_is_u64() {
        assert_eq!(result_string_len_bytes(), 8);
    }

    #[test]
    fn ensure_abi_sig_allows_result_string_lowering() {
        let sig = FnSig {
            params: Vec::new(),
            ret: TyKind::Result(Box::new(TyKind::String), Box::new(TyKind::I32)),
        };
        let abi_sig = FnSig {
            params: Vec::new(),
            ret: TyKind::ResultString,
        };
        let info = FnInfo {
            sig,
            abi_sig: Some(abi_sig),
            symbol: "test".to_string(),
            runtime_symbol: None,
            is_runtime: false,
        };
        assert!(ensure_abi_sig_handled(&info).is_ok());
    }

    #[test]
    fn ensure_abi_sig_rejects_unhandled_mismatch() {
        let sig = FnSig {
            params: vec![TyKind::I32],
            ret: TyKind::I32,
        };
        let abi_sig = FnSig {
            params: vec![TyKind::I32],
            ret: TyKind::U32,
        };
        let info = FnInfo {
            sig,
            abi_sig: Some(abi_sig),
            symbol: "test".to_string(),
            runtime_symbol: None,
            is_runtime: false,
        };
        assert!(ensure_abi_sig_handled(&info).is_err());
    }
}
