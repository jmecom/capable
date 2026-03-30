use std::collections::HashMap;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, InstBuilder, MemFlags};
use cranelift_frontend::FunctionBuilder;
use cranelift_module::Module as ModuleTrait;
use cranelift_object::ObjectModule;

use crate::ast::Literal;

use super::{
    emit_hir_expr, emit_hir_stmt, load_value_by_ty, store_local, DeferStack, LoopTarget,
    ReturnLowering,
};
use super::super::{
    CodegenError, EnumIndex, Flow, FnInfo, LocalValue, ResultKind, ResultShape, StructLayoutIndex,
    ValueRepr,
};

fn value_kind(value: &ValueRepr) -> ResultKind {
    match value {
        ValueRepr::Unit => ResultKind::Unit,
        ValueRepr::Single(_) => ResultKind::Single,
        ValueRepr::Result { ok, err, .. } => {
            ResultKind::Result(Box::new(value_kind(ok)), Box::new(value_kind(err)))
        }
    }
}

fn value_from_flat_results(
    kind: &ResultKind,
    values: &[ir::Value],
    idx: &mut usize,
) -> Result<ValueRepr, CodegenError> {
    match kind {
        ResultKind::Unit => Ok(ValueRepr::Unit),
        ResultKind::Single => {
            let value = *values
                .get(*idx)
                .ok_or_else(|| CodegenError::Codegen("missing match result value".to_string()))?;
            *idx += 1;
            Ok(ValueRepr::Single(value))
        }
        ResultKind::Result(ok_kind, err_kind) => {
            let tag = *values
                .get(*idx)
                .ok_or_else(|| CodegenError::Codegen("missing match result tag".to_string()))?;
            *idx += 1;
            let ok = value_from_flat_results(ok_kind, values, idx)?;
            let err = value_from_flat_results(err_kind, values, idx)?;
            Ok(ValueRepr::Result {
                tag,
                ok: Box::new(ok),
                err: Box::new(err),
            })
        }
    }
}

/// Emit HIR match as statement (arms can contain returns, don't produce values).
/// Returns true if all paths diverged (returned/broke/continued).
pub(super) fn emit_hir_match_stmt(
    builder: &mut FunctionBuilder,
    match_expr: &crate::hir::HirMatch,
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
    loop_target: Option<LoopTarget>,
    return_lowering: &ReturnLowering,
    defer_stack: &mut DeferStack,
) -> Result<bool, CodegenError> {
    let value = emit_hir_expr(
        builder,
        &match_expr.expr,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        return_lowering,
        module,
        data_counter,
    )?;

    let (match_val, match_result) = match value.clone() {
        ValueRepr::Single(v) => {
            let tag = match &match_expr.expr.ty().ty {
                crate::typeck::Ty::Path(name, _) if enum_index.layouts.contains_key(name) => {
                    builder.ins().load(ir::types::I32, MemFlags::new(), v, 0)
                }
                _ => v,
            };
            (tag, None)
        }
        ValueRepr::Result { tag, ok, err } => (tag, Some((*ok, *err))),
        ValueRepr::Unit => (builder.ins().iconst(ir::types::I32, 0), None),
    };

    let merge_block = builder.create_block();
    let num_arms = match_expr.arms.len();
    let mut check_blocks: Vec<ir::Block> = Vec::new();
    let mut arm_blocks: Vec<ir::Block> = Vec::new();
    for i in 0..num_arms {
        arm_blocks.push(builder.create_block());
        if i + 1 < num_arms {
            check_blocks.push(builder.create_block());
        }
    }
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
        let cond = hir_match_pattern_cond(
            builder,
            &arm.pattern,
            match_val,
            match_expr.expr.ty(),
            enum_index,
        )?;
        builder.ins().brif(cond, arm_block, &[], next_block, &[]);

        builder.switch_to_block(arm_block);

        let mut arm_locals = locals.clone();
        let mut arm_defers = defer_stack.clone();
        arm_defers.push_block_scope();
        hir_bind_match_pattern_value(
            builder,
            &arm.pattern,
            &value,
            match_result.as_ref(),
            match_expr.expr.ty(),
            enum_index,
            struct_layouts,
            module,
            &mut arm_locals,
        )?;

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
                loop_target,
                return_lowering,
                &mut arm_defers,
            )?;
            if flow == Flow::Terminated {
                arm_terminated = true;
                break;
            }
        }

        if !arm_terminated {
            arm_defers.emit_current_and_pop(
                builder,
                &arm_locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            builder.ins().jump(merge_block, &[]);
            any_arm_continues = true;
        }

        current_block = next_block;
    }

    builder.switch_to_block(merge_block);
    if !any_arm_continues {
        builder.ins().trap(ir::TrapCode::UnreachableCodeReached);
    }

    Ok(!any_arm_continues)
}

/// Emit HIR match expression (arms produce a value).
pub(super) fn emit_hir_match_expr(
    builder: &mut FunctionBuilder,
    match_expr: &crate::hir::HirMatch,
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    return_lowering: &ReturnLowering,
    module: &mut ObjectModule,
    data_counter: &mut u32,
    loop_target: Option<LoopTarget>,
) -> Result<ValueRepr, CodegenError> {
    use crate::hir::HirStmt;

    let value = emit_hir_expr(
        builder,
        &match_expr.expr,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        return_lowering,
        module,
        data_counter,
    )?;

    let (match_val, match_result) = match value.clone() {
        ValueRepr::Single(v) => {
            let tag = match &match_expr.expr.ty().ty {
                crate::typeck::Ty::Path(name, _) if enum_index.layouts.contains_key(name) => {
                    builder.ins().load(ir::types::I32, MemFlags::new(), v, 0)
                }
                _ => v,
            };
            (tag, None)
        }
        ValueRepr::Result { tag, ok, err } => (tag, Some((*ok, *err))),
        ValueRepr::Unit => (builder.ins().iconst(ir::types::I32, 0), None),
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
        let cond = hir_match_pattern_cond(
            builder,
            &arm.pattern,
            match_val,
            match_expr.expr.ty(),
            enum_index,
        )?;
        builder.ins().brif(cond, arm_block, &[], next_block, &[]);

        builder.switch_to_block(arm_block);
        let mut arm_locals = locals.clone();
        let mut arm_defers = DeferStack::new();
        arm_defers.push_block_scope();
        hir_bind_match_pattern_value(
            builder,
            &arm.pattern,
            &value,
            match_result.as_ref(),
            match_expr.expr.ty(),
            enum_index,
            struct_layouts,
            module,
            &mut arm_locals,
        )?;

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
                loop_target,
                return_lowering,
                &mut arm_defers,
            )?;
            if flow == Flow::Terminated {
                prefix_terminated = true;
                break;
            }
        }

        if prefix_terminated {
            builder.seal_block(arm_block);
            if is_last {
                break;
            }
            current_block = next_block;
            continue;
        }

        let (arm_value, arm_diverges) = match last {
            HirStmt::Expr(expr_stmt) => {
                let diverges = matches!(&expr_stmt.expr, crate::hir::HirExpr::Trap(_));
                let value = emit_hir_expr(
                    builder,
                    &expr_stmt.expr,
                    &arm_locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    return_lowering,
                    module,
                    data_counter,
                )?;
                (value, diverges)
            }
            _ => {
                return Err(CodegenError::Unsupported(
                    "match arm must end with expression".to_string(),
                ))
            }
        };

        if arm_diverges {
            builder.seal_block(arm_block);
        } else {
            let values = super::flatten_value(&arm_value);

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
                    kind: value_kind(&arm_value),
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
            arm_defers.emit_current_and_pop(
                builder,
                &arm_locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            builder.ins().jump(merge_block, &[]);
            builder.seal_block(arm_block);
        }

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
        let addr = builder
            .ins()
            .stack_addr(module.isa().pointer_type(), *slot, 0);
        let val = builder.ins().load(*ty, MemFlags::new(), addr, 0);
        loaded.push(val);
    }

    let mut idx = 0;
    let result = value_from_flat_results(&shape.kind, &loaded, &mut idx)?;

    Ok(result)
}

fn hir_match_pattern_cond(
    builder: &mut FunctionBuilder,
    pattern: &crate::hir::HirPattern,
    match_val: ir::Value,
    match_ty: &crate::hir::HirType,
    enum_index: &EnumIndex,
) -> Result<ir::Value, CodegenError> {
    use crate::hir::HirPattern;

    match pattern {
        HirPattern::Wildcard | HirPattern::Binding(_) => {
            let one = builder.ins().iconst(ir::types::I32, 1);
            Ok(builder.ins().icmp_imm(IntCC::Equal, one, 1))
        }
        HirPattern::Literal(lit) => match lit {
            Literal::Int(n) => {
                let rhs = builder.ins().iconst(ir::types::I32, *n);
                Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
            }
            Literal::I64(n) => {
                let rhs = builder.ins().iconst(ir::types::I64, *n);
                Ok(builder.ins().icmp(IntCC::Equal, match_val, rhs))
            }
            Literal::U64(n) => {
                let rhs = builder.ins().iconst(ir::types::I64, *n as i64);
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
                let one = builder.ins().iconst(ir::types::I32, 1);
                Ok(builder.ins().icmp_imm(IntCC::Equal, one, 1))
            }
            Literal::String(_) => Err(CodegenError::Unsupported(
                "string pattern matching".to_string(),
            )),
        },
        HirPattern::Variant { variant_name, .. } => {
            let qualified = match &match_ty.ty {
                crate::typeck::Ty::Path(path, _) => path.clone(),
                _ => {
                    return Err(CodegenError::Codegen(format!(
                        "enum variant pattern has non-path type: {:?}",
                        match_ty.ty
                    )))
                }
            };

            let val_ty = builder.func.dfg.value_type(match_val);

            if qualified == "sys.result.Result" {
                let discr = match variant_name.as_str() {
                    "Ok" => 0i64,
                    "Err" => 1i64,
                    _ => {
                        return Err(CodegenError::Codegen(format!(
                            "unknown Result variant: {}",
                            variant_name
                        )))
                    }
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

fn hir_bind_match_pattern_value(
    builder: &mut FunctionBuilder,
    pattern: &crate::hir::HirPattern,
    value: &ValueRepr,
    result: Option<&(ValueRepr, ValueRepr)>,
    match_ty: &crate::hir::HirType,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    locals: &mut HashMap<crate::hir::LocalId, LocalValue>,
) -> Result<(), CodegenError> {
    use crate::hir::HirPattern;

    match pattern {
        HirPattern::Wildcard => Ok(()),
        HirPattern::Literal(_) => Ok(()),
        HirPattern::Binding(local_id) => {
            locals.insert(*local_id, store_local(builder, value.clone()));
            Ok(())
        }
        HirPattern::Variant {
            variant_name,
            binding,
            ..
        } => {
            if let Some(local_id) = binding {
                if let Some((ok_val, err_val)) = result {
                    if variant_name == "Ok" {
                        locals.insert(*local_id, store_local(builder, ok_val.clone()));
                    } else if variant_name == "Err" {
                        locals.insert(*local_id, store_local(builder, err_val.clone()));
                    }
                    return Ok(());
                }
                let enum_name = match &match_ty.ty {
                    crate::typeck::Ty::Path(path, _) => path,
                    _ => {
                        return Err(CodegenError::Unsupported(
                            "variant binding on non-enum".to_string(),
                        ))
                    }
                };
                let Some(layout) = enum_index.layouts.get(enum_name) else {
                    return Err(CodegenError::Unsupported(
                        "variant binding without payload".to_string(),
                    ));
                };
                let Some(payloads) = enum_index.payloads.get(enum_name) else {
                    return Err(CodegenError::Unsupported(
                        "missing enum payload info".to_string(),
                    ));
                };
                let payload_ty =
                    payloads
                        .get(variant_name)
                        .cloned()
                        .flatten()
                        .ok_or_else(|| {
                            CodegenError::Unsupported("variant binding without payload".to_string())
                        })?;
                let ValueRepr::Single(base_ptr) = value else {
                    return Err(CodegenError::Unsupported(
                        "variant binding expects enum storage".to_string(),
                    ));
                };
                let payload_val = load_value_by_ty(
                    builder,
                    *base_ptr,
                    layout.payload_offset,
                    &payload_ty,
                    enum_index,
                    struct_layouts,
                    module,
                )?;
                locals.insert(*local_id, store_local(builder, payload_val));
            }
            Ok(())
        }
    }
}
