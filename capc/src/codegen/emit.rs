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

use crate::abi::AbiType;
use crate::ast::{BinaryOp, Literal, UnaryOp};

use super::{
    CodegenError, EnumIndex, Flow, FnInfo, LocalValue, ResultKind, ResultShape,
    StructLayout, StructLayoutIndex, TypeLayout, ValueRepr,
};
use super::abi_quirks;
use super::layout::{align_to, resolve_struct_layout, type_layout_from_index};
use super::sig_to_clif;

/// Target blocks for break/continue inside a loop.
#[derive(Copy, Clone, Debug)]
pub(super) struct LoopTarget {
    pub continue_block: ir::Block, // for while: header_block, for: increment_block
    pub exit_block: ir::Block,
}

#[derive(Clone, Debug)]
pub(super) enum ReturnLowering {
    Direct,
    SRet {
        out_ptr: ir::Value,
        ret_ty: crate::hir::HirType,
    },
    ResultOut {
        out_ok: Option<ir::Value>,
        out_err: Option<ir::Value>,
        ok_ty: crate::hir::HirType,
        err_ty: crate::hir::HirType,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum DeferScopeKind {
    Regular,
    LoopBody,
}

#[derive(Clone, Debug)]
struct DeferScope {
    kind: DeferScopeKind,
    defers: Vec<crate::hir::HirExpr>,
}

#[derive(Clone, Debug)]
pub(super) struct DeferStack {
    scopes: Vec<DeferScope>,
}

impl DeferStack {
    pub(super) fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub(super) fn push_block_scope(&mut self) {
        self.push_scope(DeferScopeKind::Regular);
    }

    pub(super) fn push_loop_scope(&mut self) {
        self.push_scope(DeferScopeKind::LoopBody);
    }

    fn push_scope(&mut self, kind: DeferScopeKind) {
        self.scopes.push(DeferScope {
            kind,
            defers: Vec::new(),
        });
    }

    pub(super) fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    pub(super) fn push_defer(&mut self, expr: crate::hir::HirExpr) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.defers.push(expr);
        }
    }

    fn emit_scope_defers(
        &self,
        scope: &DeferScope,
        builder: &mut FunctionBuilder,
        locals: &HashMap<crate::hir::LocalId, LocalValue>,
        fn_map: &HashMap<String, FnInfo>,
        enum_index: &EnumIndex,
        struct_layouts: &StructLayoutIndex,
        return_lowering: &ReturnLowering,
        module: &mut ObjectModule,
        data_counter: &mut u32,
    ) -> Result<(), CodegenError> {
        for defer_expr in scope.defers.iter().rev() {
            let _ = emit_hir_expr(
                builder,
                defer_expr,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
        }
        Ok(())
    }

    pub(super) fn emit_current_and_pop(
        &mut self,
        builder: &mut FunctionBuilder,
        locals: &HashMap<crate::hir::LocalId, LocalValue>,
        fn_map: &HashMap<String, FnInfo>,
        enum_index: &EnumIndex,
        struct_layouts: &StructLayoutIndex,
        return_lowering: &ReturnLowering,
        module: &mut ObjectModule,
        data_counter: &mut u32,
    ) -> Result<(), CodegenError> {
        if let Some(scope) = self.scopes.last() {
            self.emit_scope_defers(
                scope,
                builder,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
        }
        self.pop_scope();
        Ok(())
    }

    pub(super) fn emit_all_and_clear(
        &mut self,
        builder: &mut FunctionBuilder,
        locals: &HashMap<crate::hir::LocalId, LocalValue>,
        fn_map: &HashMap<String, FnInfo>,
        enum_index: &EnumIndex,
        struct_layouts: &StructLayoutIndex,
        return_lowering: &ReturnLowering,
        module: &mut ObjectModule,
        data_counter: &mut u32,
    ) -> Result<(), CodegenError> {
        while let Some(scope) = self.scopes.pop() {
            self.emit_scope_defers(
                &scope,
                builder,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
        }
        Ok(())
    }

    pub(super) fn emit_until_loop_and_pop(
        &mut self,
        builder: &mut FunctionBuilder,
        locals: &HashMap<crate::hir::LocalId, LocalValue>,
        fn_map: &HashMap<String, FnInfo>,
        enum_index: &EnumIndex,
        struct_layouts: &StructLayoutIndex,
        return_lowering: &ReturnLowering,
        module: &mut ObjectModule,
        data_counter: &mut u32,
    ) -> Result<(), CodegenError> {
        while let Some(scope) = self.scopes.pop() {
            self.emit_scope_defers(
                &scope,
                builder,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            if scope.kind == DeferScopeKind::LoopBody {
                break;
            }
        }
        Ok(())
    }
}

/// Emit a single HIR statement.
pub(super) fn emit_hir_stmt(
    builder: &mut FunctionBuilder,
    stmt: &crate::hir::HirStmt,
    locals: &mut HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
    loop_target: Option<LoopTarget>,
    return_lowering: &ReturnLowering,
    defer_stack: &mut DeferStack,
) -> Result<Flow, CodegenError> {
    emit_hir_stmt_inner(
        builder,
        stmt,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        module,
        data_counter,
        loop_target,
        return_lowering,
        defer_stack,
    )
    .map_err(|err| err.with_span(stmt.span()))
}

fn emit_hir_stmt_inner(
    builder: &mut FunctionBuilder,
    stmt: &crate::hir::HirStmt,
    locals: &mut HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
    data_counter: &mut u32,
    loop_target: Option<LoopTarget>,
    return_lowering: &ReturnLowering,
    defer_stack: &mut DeferStack,
) -> Result<Flow, CodegenError> {
    use crate::hir::HirStmt;

    match stmt {
        HirStmt::Let(let_stmt) => {
            if matches!(let_stmt.ty.ty, crate::typeck::Ty::Ref(_)) {
                if let crate::hir::HirExpr::Local(local) = &let_stmt.expr {
                    let Some(value) = locals.get(&local.local_id).cloned() else {
                        return Err(CodegenError::UnknownVariable(format!(
                            "local {}",
                            local.local_id.0
                        )));
                    };
                    locals.insert(let_stmt.local_id, value);
                    return Ok(Flow::Continues);
                }
            }
            let value = emit_hir_expr(
                builder,
                &let_stmt.expr,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            if let crate::typeck::Ty::Path(name, _) = &let_stmt.ty.ty {
                if let Some(layout) = enum_index.layouts.get(name) {
                    let align = layout.align.max(1);
                    let slot_size = aligned_slot_size(layout.size, align);
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
                        enum_index,
                        struct_layouts,
                        module,
                    )?;
                    locals.insert(
                        let_stmt.local_id,
                        LocalValue::StructSlot(slot, let_stmt.ty.clone(), align),
                    );
                    return Ok(Flow::Continues);
                }
            }
            if let Some(layout) =
                resolve_struct_layout(&let_stmt.ty.ty, "", &struct_layouts.layouts)
            {
                let align = layout.align.max(1);
                let slot_size = aligned_slot_size(layout.size, align);
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
                    enum_index,
                    struct_layouts,
                    module,
                )?;
                locals.insert(
                    let_stmt.local_id,
                    LocalValue::StructSlot(slot, let_stmt.ty.clone(), align),
                );
            } else {
                let local = store_local(builder, value);
                locals.insert(let_stmt.local_id, local);
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
                return_lowering,
                module,
                data_counter,
            )?;
            let Some(local) = locals.get_mut(&assign.local_id) else {
                return Err(CodegenError::UnknownVariable(format!(
                    "local {}",
                    assign.local_id.0
                )));
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
                        enum_index,
                        struct_layouts,
                        module,
                    )?;
                }
                LocalValue::Value(_) => {
                    return Err(CodegenError::Unsupported("assignment".to_string()));
                }
            }
        }
        HirStmt::Defer(defer_stmt) => {
            defer_stack.push_defer(defer_stmt.expr.clone());
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
                    return_lowering,
                    module,
                    data_counter,
                )?;
                match return_lowering {
                    ReturnLowering::Direct => {
                        defer_stack.emit_all_and_clear(
                            builder,
                            locals,
                            fn_map,
                            enum_index,
                            struct_layouts,
                            return_lowering,
                            module,
                            data_counter,
                        )?;
                        match value {
                            ValueRepr::Unit => builder.ins().return_(&[]),
                            ValueRepr::Single(val) => builder.ins().return_(&[val]),
                            ValueRepr::Result { .. } => {
                                let values = flatten_value(&value);
                                builder.ins().return_(&values)
                            }
                        };
                    }
                    ReturnLowering::SRet { out_ptr, ret_ty } => {
                        store_out_value(
                            builder,
                            *out_ptr,
                            ret_ty,
                            value,
                            enum_index,
                            struct_layouts,
                            module,
                        )?;
                        defer_stack.emit_all_and_clear(
                            builder,
                            locals,
                            fn_map,
                            enum_index,
                            struct_layouts,
                            return_lowering,
                            module,
                            data_counter,
                        )?;
                        builder.ins().return_(&[]);
                    }
                    ReturnLowering::ResultOut {
                        out_ok,
                        out_err,
                        ok_ty,
                        err_ty,
                    } => {
                        let ValueRepr::Result { tag, ok, err } = value else {
                            return Err(CodegenError::Unsupported(
                                "return expects Result value".to_string(),
                            ));
                        };
                        if let Some(out_ok) = out_ok {
                            store_out_value(
                                builder,
                                *out_ok,
                                ok_ty,
                                *ok,
                                enum_index,
                                struct_layouts,
                                module,
                            )?;
                        }
                        if let Some(out_err) = out_err {
                            store_out_value(
                                builder,
                                *out_err,
                                err_ty,
                                *err,
                                enum_index,
                                struct_layouts,
                                module,
                            )?;
                        }
                        defer_stack.emit_all_and_clear(
                            builder,
                            locals,
                            fn_map,
                            enum_index,
                            struct_layouts,
                            return_lowering,
                            module,
                            data_counter,
                        )?;
                        builder.ins().return_(&[tag]);
                    }
                }
            } else {
                defer_stack.emit_all_and_clear(
                    builder,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    return_lowering,
                    module,
                    data_counter,
                )?;
                builder.ins().return_(&[]);
            }
            return Ok(Flow::Terminated);
        }
        HirStmt::Expr(expr_stmt) => {
            if matches!(expr_stmt.expr, crate::hir::HirExpr::Trap(_)) {
                let _ = emit_hir_expr(
                    builder,
                    &expr_stmt.expr,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    return_lowering,
                    module,
                    data_counter,
                )?;
                return Ok(Flow::Terminated);
            }
            // Special handling for match expressions that might diverge
            if let crate::hir::HirExpr::Match(match_expr) = &expr_stmt.expr {
                if matches!(
                    match_expr.result_ty.ty,
                    crate::typeck::Ty::Builtin(crate::typeck::BuiltinType::Unit)
                ) {
                    let diverged = emit_hir_match_stmt(
                        builder,
                        match_expr,
                        locals,
                        fn_map,
                        enum_index,
                        struct_layouts,
                        module,
                        data_counter,
                        loop_target,
                        return_lowering,
                        defer_stack,
                    )?;
                    if diverged {
                        return Ok(Flow::Terminated);
                    }
                    // Don't fall through to emit_hir_expr - we already emitted the match
                    return Ok(Flow::Continues);
                }
            }
            let _ = emit_hir_expr(
                builder,
                &expr_stmt.expr,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
        }
        HirStmt::If(if_stmt) => {
            // Snapshot locals so branch-scoped lets don't leak
            let saved_locals = locals.clone();
            let saved_defers = defer_stack.clone();

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
                return_lowering,
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
            let mut then_defers = saved_defers.clone();
            then_defers.push_block_scope();
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
                    loop_target,
                    return_lowering,
                    &mut then_defers,
                )?;
                if flow == Flow::Terminated {
                    then_terminated = true;
                    break;
                }
            }
            if !then_terminated {
                then_defers.emit_current_and_pop(
                    builder,
                    &then_locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    return_lowering,
                    module,
                    data_counter,
                )?;
                builder.ins().jump(merge_block, &[]);
            }
            builder.seal_block(then_block);

            // ELSE branch with its own locals
            if let Some(else_block_hir) = &if_stmt.else_block {
                builder.switch_to_block(else_block);
                let mut else_locals = saved_locals.clone();
                let mut else_defers = saved_defers.clone();
                else_defers.push_block_scope();
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
                        loop_target,
                        return_lowering,
                        &mut else_defers,
                    )?;
                    if flow == Flow::Terminated {
                        else_terminated = true;
                        break;
                    }
                }
                if !else_terminated {
                    else_defers.emit_current_and_pop(
                        builder,
                        &else_locals,
                        fn_map,
                        enum_index,
                        struct_layouts,
                        return_lowering,
                        module,
                        data_counter,
                    )?;
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
            let saved_defers = defer_stack.clone();

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
                return_lowering,
                module,
                data_counter,
            )?;
            let cond_b1 = to_b1(builder, cond_val)?;
            builder
                .ins()
                .brif(cond_b1, body_block, &[], exit_block, &[]);

            builder.switch_to_block(body_block);

            // Create loop target for break/continue
            // For while loops, continue goes to header (re-evaluate condition)
            let body_loop_target = Some(LoopTarget {
                continue_block: header_block,
                exit_block,
            });

            // Loop body gets its own locals
            let mut body_locals = saved_locals.clone();
            let mut body_defers = saved_defers.clone();
            body_defers.push_loop_scope();
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
                    body_loop_target,
                    return_lowering,
                    &mut body_defers,
                )?;
                if flow == Flow::Terminated {
                    body_terminated = true;
                    break;
                }
            }

            if !body_terminated {
                body_defers.emit_current_and_pop(
                    builder,
                    &body_locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    return_lowering,
                    module,
                    data_counter,
                )?;
                builder.ins().jump(header_block, &[]);
            }

            builder.seal_block(body_block);
            builder.seal_block(header_block);

            builder.switch_to_block(exit_block);
            builder.seal_block(exit_block);

            // After the loop, restore the pre-loop locals snapshot
            *locals = saved_locals;
        }
        HirStmt::For(for_stmt) => {
            // Snapshot locals so loop-body lets don't leak out of the loop
            let saved_locals = locals.clone();
            let saved_defers = defer_stack.clone();

            // Create stack slot for loop variable
            let loop_var_slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                4, // i32 is 4 bytes
            ));

            // Evaluate start value and store in slot
            let start_val = emit_hir_expr(
                builder,
                &for_stmt.start,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            let start_i32 = match start_val {
                ValueRepr::Single(v) => v,
                _ => return Err(CodegenError::Unsupported("for loop start".to_string())),
            };
            builder.ins().stack_store(start_i32, loop_var_slot, 0);

            // Evaluate end value (once, before the loop)
            let end_val = emit_hir_expr(
                builder,
                &for_stmt.end,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            let end_i32 = match end_val {
                ValueRepr::Single(v) => v,
                _ => return Err(CodegenError::Unsupported("for loop end".to_string())),
            };

            let header_block = builder.create_block();
            let body_block = builder.create_block();
            let increment_block = builder.create_block();
            let exit_block = builder.create_block();

            builder.ins().jump(header_block, &[]);
            builder.switch_to_block(header_block);

            // Load loop variable and compare with end
            let current_val = builder.ins().stack_load(ir::types::I32, loop_var_slot, 0);
            let cond = builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, current_val, end_i32);
            builder.ins().brif(cond, body_block, &[], exit_block, &[]);

            builder.switch_to_block(body_block);

            // Create loop target for break/continue
            // For for loops, continue goes to increment block (not header)
            let body_loop_target = Some(LoopTarget {
                continue_block: increment_block,
                exit_block,
            });

            // Loop body gets its own locals with the loop variable bound
            let mut body_locals = saved_locals.clone();
            body_locals.insert(
                for_stmt.var_id,
                LocalValue::Slot(loop_var_slot, ir::types::I32),
            );
            let mut body_defers = saved_defers.clone();
            body_defers.push_loop_scope();

            let mut body_terminated = false;
            for stmt in &for_stmt.body.stmts {
                let flow = emit_hir_stmt(
                    builder,
                    stmt,
                    &mut body_locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    module,
                    data_counter,
                    body_loop_target,
                    return_lowering,
                    &mut body_defers,
                )?;
                if flow == Flow::Terminated {
                    body_terminated = true;
                    break;
                }
            }

            // Fall through to increment block (if not terminated by break/continue/return)
            if !body_terminated {
                body_defers.emit_current_and_pop(
                    builder,
                    &body_locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    return_lowering,
                    module,
                    data_counter,
                )?;
                builder.ins().jump(increment_block, &[]);
            }

            // Increment block: increment loop variable and jump back to header
            builder.switch_to_block(increment_block);
            let current = builder.ins().stack_load(ir::types::I32, loop_var_slot, 0);
            let one = builder.ins().iconst(ir::types::I32, 1);
            let next = builder.ins().iadd(current, one);
            builder.ins().stack_store(next, loop_var_slot, 0);
            builder.ins().jump(header_block, &[]);

            builder.seal_block(body_block);
            builder.seal_block(increment_block);
            builder.seal_block(header_block);

            builder.switch_to_block(exit_block);
            builder.seal_block(exit_block);

            // After the loop, restore the pre-loop locals snapshot
            *locals = saved_locals;
        }
        HirStmt::Break(_) => {
            let target = loop_target.expect("break outside of loop (should be caught by typeck)");
            defer_stack.emit_until_loop_and_pop(
                builder,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            builder.ins().jump(target.exit_block, &[]);
            return Ok(Flow::Terminated);
        }
        HirStmt::Continue(_) => {
            let target = loop_target.expect("continue outside of loop (should be caught by typeck)");
            defer_stack.emit_until_loop_and_pop(
                builder,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            builder.ins().jump(target.continue_block, &[]);
            return Ok(Flow::Terminated);
        }
    }
    Ok(Flow::Continues)
}

/// Emit a single HIR expression and return its lowered value representation.
fn emit_hir_expr(
    builder: &mut FunctionBuilder,
    expr: &crate::hir::HirExpr,
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    return_lowering: &ReturnLowering,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    emit_hir_expr_inner(
        builder,
        expr,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        return_lowering,
        module,
        data_counter,
    )
    .map_err(|err| err.with_span(expr.span()))
}

fn emit_hir_expr_inner(
    builder: &mut FunctionBuilder,
    expr: &crate::hir::HirExpr,
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    return_lowering: &ReturnLowering,
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
            Literal::String(value) => {
                emit_string(builder, value, module, data_counter, struct_layouts)
            }
            Literal::Unit => Ok(ValueRepr::Unit),
        },
        HirExpr::Local(local) => {
            if let Some(value) = locals.get(&local.local_id) {
                return Ok(load_local(builder, value, module.isa().pointer_type()));
            }
            Err(CodegenError::UnknownVariable(format!(
                "local {}",
                local.local_id.0
            )))
        }
        HirExpr::EnumVariant(variant) => {
            // Check if this is a Result type with payload (Ok/Err)
            if let crate::typeck::Ty::Path(ty_name, args) = &variant.enum_ty.ty {
                if ty_name == "sys.result.Result" && args.len() == 2 {
                    let AbiType::Result(ok_abi, err_abi) = &variant.enum_ty.abi else {
                        return Err(CodegenError::Unsupported(
                            abi_quirks::result_abi_mismatch_error().to_string(),
                        ));
                    };
                    let ok_ty = crate::hir::HirType {
                        ty: args[0].clone(),
                        abi: (**ok_abi).clone(),
                    };
                    let err_ty = crate::hir::HirType {
                        ty: args[1].clone(),
                        abi: (**err_abi).clone(),
                    };
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
                                    return_lowering,
                                    module,
                                    data_counter,
                                )?
                            } else {
                                zero_value_for_ty(
                                    builder,
                                    &ok_ty,
                                    ptr_ty,
                                    enum_index,
                                    Some(struct_layouts),
                                    module,
                                )?
                            };
                            let err_zero = zero_value_for_ty(
                                builder,
                                &err_ty,
                                ptr_ty,
                                enum_index,
                                Some(struct_layouts),
                                module,
                            )?;
                            (payload, err_zero)
                        }
                        "Err" => {
                            let ok_zero = zero_value_for_ty(
                                builder,
                                &ok_ty,
                                ptr_ty,
                                enum_index,
                                Some(struct_layouts),
                                module,
                            )?;
                            let payload = if let Some(payload_expr) = &variant.payload {
                                emit_hir_expr(
                                    builder,
                                    payload_expr,
                                    locals,
                                    fn_map,
                                    enum_index,
                                    struct_layouts,
                                    return_lowering,
                                    module,
                                    data_counter,
                                )?
                            } else {
                                zero_value_for_ty(
                                    builder,
                                    &err_ty,
                                    ptr_ty,
                                    enum_index,
                                    Some(struct_layouts),
                                    module,
                                )?
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

            if let crate::typeck::Ty::Path(ty_name, _) = &variant.enum_ty.ty {
                if let Some(layout) = enum_index.layouts.get(ty_name) {
                    let variants = enum_index
                        .variants
                        .get(ty_name)
                        .ok_or_else(|| {
                            CodegenError::Codegen(format!(
                                "unknown enum variant: {}.{}",
                                ty_name, variant.variant_name
                            ))
                        })?;
                    let discr = variants.get(&variant.variant_name).ok_or_else(|| {
                        CodegenError::Codegen(format!(
                            "unknown enum variant: {}.{}",
                            ty_name, variant.variant_name
                        ))
                    })?;
                    let tag_val = builder.ins().iconst(ir::types::I32, i64::from(*discr));
                    let ptr_ty = module.isa().pointer_type();
                    let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        aligned_slot_size(layout.size, layout.align),
                    ));
                    let base_ptr = aligned_stack_addr(builder, slot, layout.align, ptr_ty);
                    builder.ins().store(MemFlags::new(), tag_val, base_ptr, 0);
                    if layout.payload_size > 0 {
                        if let Some(payloads) = enum_index.payloads.get(ty_name) {
                            let payload_ty = payloads.get(&variant.variant_name).cloned().flatten();
                            if let Some(payload_ty) = payload_ty {
                                let payload = if let Some(payload_expr) = &variant.payload {
                                    emit_hir_expr(
                                        builder,
                                        payload_expr,
                                        locals,
                                        fn_map,
                                        enum_index,
                                        struct_layouts,
                                        return_lowering,
                                        module,
                                        data_counter,
                                    )?
                                } else {
                                    zero_value_for_ty(
                                        builder,
                                        &payload_ty,
                                        ptr_ty,
                                        enum_index,
                                        Some(struct_layouts),
                                        module,
                                    )?
                                };
                                store_value_by_ty(
                                    builder,
                                    base_ptr,
                                    layout.payload_offset,
                                    &payload_ty,
                                    payload,
                                    enum_index,
                                    struct_layouts,
                                    module,
                                )?;
                            } else {
                                zero_bytes(builder, base_ptr, layout.payload_offset, layout.payload_size);
                            }
                        }
                    }
                    return Ok(ValueRepr::Single(base_ptr));
                }
            }

            // For non-Result enums or variants without payload, emit just the discriminant
            let qualified = match &variant.enum_ty.ty {
                crate::typeck::Ty::Path(path, _) => path.clone(),
                _ => return Err(CodegenError::Codegen(format!(
                    "enum variant has non-path type: {:?}",
                    variant.enum_ty.ty
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
        HirExpr::Try(try_expr) => {
            let value = emit_hir_expr(
                builder,
                &try_expr.expr,
                locals,
                fn_map,
                enum_index,
                struct_layouts,
                return_lowering,
                module,
                data_counter,
            )?;
            let ValueRepr::Result { tag, ok, err } = value else {
                return Err(CodegenError::Unsupported(
                    "try expects a Result value".to_string(),
                ));
            };

            let ok_block = builder.create_block();
            let err_block = builder.create_block();

            let is_ok = builder.ins().icmp_imm(IntCC::Equal, tag, 0);
            builder.ins().brif(is_ok, ok_block, &[], err_block, &[]);

            builder.switch_to_block(err_block);
            let ret_value = match &try_expr.ret_ty.ty {
                crate::typeck::Ty::Path(name, args) if name == "sys.result.Result" && args.len() == 2 => {
                    let AbiType::Result(ok_abi, _err_abi) = &try_expr.ret_ty.abi else {
                        return Err(CodegenError::Unsupported(
                            abi_quirks::result_abi_mismatch_error().to_string(),
                        ));
                    };
                    let ok_ty = crate::hir::HirType {
                        ty: args[0].clone(),
                        abi: (**ok_abi).clone(),
                    };
                    let ptr_ty = module.isa().pointer_type();
                    let ok_zero = zero_value_for_ty(
                        builder,
                        &ok_ty,
                        ptr_ty,
                        enum_index,
                        Some(struct_layouts),
                        module,
                    )?;
                    let tag_val = builder.ins().iconst(ir::types::I8, 1);
                    ValueRepr::Result {
                        tag: tag_val,
                        ok: Box::new(ok_zero),
                        err,
                    }
                }
                _ => {
                    return Err(CodegenError::Unsupported(
                        "try expects a Result return type".to_string(),
                    ))
                }
            };
            match return_lowering {
                ReturnLowering::Direct => {
                    let ret_values = flatten_value(&ret_value);
                    builder.ins().return_(&ret_values);
                }
                ReturnLowering::SRet { out_ptr, ret_ty } => {
                    store_out_value(
                        builder,
                        *out_ptr,
                        ret_ty,
                        ret_value,
                        enum_index,
                        struct_layouts,
                        module,
                    )?;
                    builder.ins().return_(&[]);
                }
                ReturnLowering::ResultOut {
                    out_ok,
                    out_err,
                    ok_ty,
                    err_ty,
                } => {
                    let ValueRepr::Result { tag, ok, err } = ret_value else {
                        return Err(CodegenError::Unsupported(
                            "try expects a Result value".to_string(),
                        ));
                    };
                    if let Some(out_ok) = out_ok {
                        store_out_value(
                            builder,
                            *out_ok,
                            ok_ty,
                            *ok,
                            enum_index,
                            struct_layouts,
                            module,
                        )?;
                    }
                    if let Some(out_err) = out_err {
                        store_out_value(
                            builder,
                            *out_err,
                            err_ty,
                            *err,
                            enum_index,
                            struct_layouts,
                            module,
                        )?;
                    }
                    builder.ins().return_(&[tag]);
                }
            }
            builder.seal_block(err_block);

            builder.switch_to_block(ok_block);
            builder.seal_block(ok_block);

            Ok(*ok)
        }
        HirExpr::Trap(_trap) => {
            builder.ins().trap(ir::TrapCode::UnreachableCodeReached);
            // Return a dummy value - the block is now filled so no more
            // instructions can be added. The match handler will detect this
            // and skip value storage.
            Ok(ValueRepr::Unit)
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
                            return_lowering,
                            module,
                            data_counter,
                        )?;
                    }
                    return Ok(ValueRepr::Unit);
                }
            };

            if module_path == "sys.unsafe_ptr" {
                if let Some(value) = emit_unsafe_ptr_call(
                    builder,
                    module,
                    call,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    return_lowering,
                    data_counter,
                )? {
                    return Ok(value);
                }
            }

            // Lookup in fn_map by module.function key
            let key = format!("{}.{}", module_path, func_name);
            let info = fn_map
                .get(&key)
                .ok_or_else(|| CodegenError::UnknownFunction(key.clone()))?
                .clone();
            ensure_abi_sig_handled(&info)?;
            let abi_sig = info.abi_sig.as_ref().unwrap_or(&info.sig);

            // Emit arguments
            let mut args = Vec::new();
            let mut sret_ptr = None;
            if info.sig.ret == AbiType::Ptr
                && abi_sig.ret == AbiType::Unit
                && (is_non_opaque_struct_type(&call.ret_ty, struct_layouts)
                    || matches!(&call.ret_ty.ty, crate::typeck::Ty::Path(name, _) if enum_index.layouts.contains_key(name)))
            {
                let ptr_ty = module.isa().pointer_type();
                let (size, align) = if let Some(layout) =
                    resolve_struct_layout(&call.ret_ty.ty, "", &struct_layouts.layouts)
                {
                    (layout.size, layout.align)
                } else if let crate::typeck::Ty::Path(name, _) = &call.ret_ty.ty {
                    let layout = enum_index.layouts.get(name).ok_or_else(|| {
                        CodegenError::Unsupported("enum layout missing".to_string())
                    })?;
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
                args.push(base_ptr);
                sret_ptr = Some(base_ptr);
            }
            for arg in &call.args {
                let value = emit_hir_expr(
                    builder,
                    arg,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    return_lowering,
                    module,
                    data_counter,
                )?;
                args.extend(flatten_value(&value));
            }

            // Handle result out-parameters (same logic as AST version)
            let mut result_out = None;
            let result_payloads = match &call.ret_ty.ty {
                crate::typeck::Ty::Path(name, args)
                    if name == "sys.result.Result" && args.len() == 2 =>
                {
                    match &call.ret_ty.abi {
                        AbiType::Result(ok_abi, err_abi) => Some((
                            crate::hir::HirType {
                                ty: args[0].clone(),
                                abi: (**ok_abi).clone(),
                            },
                            crate::hir::HirType {
                                ty: args[1].clone(),
                                abi: (**err_abi).clone(),
                            },
                        )),
                        _ => None,
                    }
                }
                _ => None,
            };

            enum ResultOutSlot {
                Scalar(ir::StackSlot, ir::Type, u32),
                Struct(ir::Value),
            }

            if let AbiType::ResultOut(ok_ty, err_ty) = &abi_sig.ret {
                let ptr_ty = module.isa().pointer_type();
                let ok_slot = if **ok_ty == AbiType::Unit {
                    None
                } else {
                    if let Some((ok_hir, _)) = &result_payloads {
                        if is_non_opaque_struct_type(ok_hir, struct_layouts) {
                            let layout = resolve_struct_layout(
                                &ok_hir.ty,
                                "",
                                &struct_layouts.layouts,
                            )
                            .ok_or_else(|| {
                                CodegenError::Unsupported("struct layout missing".to_string())
                            })?;
                            let align = layout.align.max(1);
                            let slot_size = layout.size.max(1).saturating_add(align - 1);
                            let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                                ir::StackSlotKind::ExplicitSlot,
                                slot_size,
                            ));
                            let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
                            args.push(addr);
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
                            args.push(addr);
                            Some(ResultOutSlot::Scalar(slot, ty, align))
                        }
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
                        Some(ResultOutSlot::Scalar(slot, ty, align))
                    }
                };
                let err_slot = if **err_ty == AbiType::Unit {
                    None
                } else {
                    if let Some((_, err_hir)) = &result_payloads {
                        if is_non_opaque_struct_type(err_hir, struct_layouts) {
                            let layout = resolve_struct_layout(
                                &err_hir.ty,
                                "",
                                &struct_layouts.layouts,
                            )
                            .ok_or_else(|| {
                                CodegenError::Unsupported("struct layout missing".to_string())
                            })?;
                            let align = layout.align.max(1);
                            let slot_size = layout.size.max(1).saturating_add(align - 1);
                            let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                                ir::StackSlotKind::ExplicitSlot,
                                slot_size,
                            ));
                            let addr = aligned_stack_addr(builder, slot, align, ptr_ty);
                            args.push(addr);
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
                            args.push(addr);
                            Some(ResultOutSlot::Scalar(slot, ty, align))
                        }
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
                        Some(ResultOutSlot::Scalar(slot, ty, align))
                    }
                };
                result_out = Some((ok_slot, err_slot, ok_ty.clone(), err_ty.clone()));
            }

            // Emit the call
            let sig = sig_to_clif(
                abi_sig,
                module.isa().pointer_type(),
                module.isa().default_call_conv(),
            );
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
            if abi_quirks::is_result_out(&abi_sig.ret) {
                let tag = results
                    .get(0)
                    .ok_or_else(|| CodegenError::Codegen("missing result tag".to_string()))?;
                let (ok_slot, err_slot, ok_ty, err_ty) = result_out
                    .ok_or_else(|| CodegenError::Codegen("missing result slots".to_string()))?;
                let ok_val = if let Some(slot) = ok_slot {
                    match slot {
                        ResultOutSlot::Scalar(slot, ty, align) => {
                            let addr = aligned_stack_addr(
                                builder,
                                slot,
                                align,
                                module.isa().pointer_type(),
                            );
                            let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
                            ValueRepr::Single(val)
                        }
                        ResultOutSlot::Struct(ptr) => ValueRepr::Single(ptr),
                    }
                } else {
                    ValueRepr::Unit
                };
                let err_val = if let Some(slot) = err_slot {
                    match slot {
                        ResultOutSlot::Scalar(slot, ty, align) => {
                            let addr = aligned_stack_addr(
                                builder,
                                slot,
                                align,
                                module.isa().pointer_type(),
                            );
                            let val = builder.ins().load(ty, MemFlags::new(), addr, 0);
                            ValueRepr::Single(val)
                        }
                        ResultOutSlot::Struct(ptr) => ValueRepr::Single(ptr),
                    }
                } else {
                    ValueRepr::Unit
                };
                match &info.sig.ret {
                    AbiType::Result(_, _) => Ok(ValueRepr::Result {
                        tag: *tag,
                        ok: Box::new(ok_val),
                        err: Box::new(err_val),
                    }),
                    _ => Err(CodegenError::Unsupported(format!(
                        "result out params for {ok_ty:?}/{err_ty:?}"
                    ))),
                }
            } else if let Some(ptr) = sret_ptr {
                Ok(ValueRepr::Single(ptr))
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
                return_lowering,
                module,
                data_counter,
            )?;

            if matches!(binary.op, BinaryOp::And | BinaryOp::Or) {
                let lhs_val = match lhs {
                    ValueRepr::Single(v) => v,
                    ValueRepr::Unit => {
                        return Err(CodegenError::Unsupported("boolean op on unit".to_string()))
                    }
                    ValueRepr::Result { .. } => {
                        return Err(CodegenError::Unsupported(
                            "boolean op on result".to_string(),
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
                    return_lowering,
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
                return_lowering,
                module,
                data_counter,
            )?;

            match (&binary.op, lhs, rhs) {
                (BinaryOp::Add, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(emit_checked_add(builder, a, b, &binary.ty)?))
                }
                (BinaryOp::Sub, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(emit_checked_sub(builder, a, b, &binary.ty)?))
                }
                (BinaryOp::Mul, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(emit_checked_mul(builder, a, b, &binary.ty)?))
                }
                (BinaryOp::Div, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    Ok(ValueRepr::Single(emit_checked_div(builder, a, b, &binary.ty)?))
                }
                (BinaryOp::Eq, ValueRepr::Single(a), ValueRepr::Single(b))
                    if is_string_type(&binary.left.ty().ty) =>
                {
                    let result = emit_string_eq(builder, module, a, b)?;
                    Ok(ValueRepr::Single(result))
                }
                (BinaryOp::Neq, ValueRepr::Single(a), ValueRepr::Single(b))
                    if is_string_type(&binary.left.ty().ty) =>
                {
                    let eq_result = emit_string_eq(builder, module, a, b)?;
                    // Invert the result: 1 becomes 0, 0 becomes 1
                    let one = builder.ins().iconst(ir::types::I8, 1);
                    let neq_result = builder.ins().bxor(eq_result, one);
                    Ok(ValueRepr::Single(neq_result))
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
                    let cmp = builder.ins().icmp(cmp_cc(&binary.left, IntCC::SignedLessThan, IntCC::UnsignedLessThan), a, b);
                    Ok(ValueRepr::Single(bool_to_i8(builder, cmp)))
                }
                (BinaryOp::Lte, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    let cmp = builder.ins().icmp(
                        cmp_cc(&binary.left, IntCC::SignedLessThanOrEqual, IntCC::UnsignedLessThanOrEqual),
                        a,
                        b,
                    );
                    Ok(ValueRepr::Single(bool_to_i8(builder, cmp)))
                }
                (BinaryOp::Gt, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    let cmp = builder.ins().icmp(
                        cmp_cc(&binary.left, IntCC::SignedGreaterThan, IntCC::UnsignedGreaterThan),
                        a,
                        b,
                    );
                    Ok(ValueRepr::Single(bool_to_i8(builder, cmp)))
                }
                (BinaryOp::Gte, ValueRepr::Single(a), ValueRepr::Single(b)) => {
                    let cmp = builder.ins().icmp(
                        cmp_cc(
                            &binary.left,
                            IntCC::SignedGreaterThanOrEqual,
                            IntCC::UnsignedGreaterThanOrEqual,
                        ),
                        a,
                        b,
                    );
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
                return_lowering,
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
            if matches!(
                match_expr.result_ty.ty,
                crate::typeck::Ty::Builtin(crate::typeck::BuiltinType::Unit)
            ) {
                // Note: divergence handling is done at HirStmt::Expr level.
                // Here we just emit the match and return Unit.
                let mut temp_defers = DeferStack::new();
                let _diverged = emit_hir_match_stmt(
                    builder,
                    match_expr,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    module,
                    data_counter,
                    None, // break/continue not supported in expression-context matches
                    return_lowering,
                    &mut temp_defers,
                )?;
                Ok(ValueRepr::Unit)
            } else {
                emit_hir_match_expr(
                    builder,
                    match_expr,
                    locals,
                    fn_map,
                    enum_index,
                    struct_layouts,
                    return_lowering,
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
            return_lowering,
            module,
            data_counter,
        ),
        HirExpr::Index(index_expr) => emit_hir_index(
            builder,
            index_expr,
            locals,
            fn_map,
            enum_index,
            struct_layouts,
            return_lowering,
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
                return_lowering,
                module,
                data_counter,
            )
        }
    }
}

fn emit_checked_add(
    builder: &mut FunctionBuilder,
    a: Value,
    b: Value,
    ty: &crate::hir::HirType,
) -> Result<Value, CodegenError> {
    let (sum, overflow) = if crate::typeck::is_unsigned_type(&ty.ty) {
        builder.ins().uadd_overflow(a, b)
    } else {
        builder.ins().sadd_overflow(a, b)
    };
    trap_on_overflow(builder, overflow);
    Ok(sum)
}

fn emit_checked_sub(
    builder: &mut FunctionBuilder,
    a: Value,
    b: Value,
    ty: &crate::hir::HirType,
) -> Result<Value, CodegenError> {
    let (diff, overflow) = if crate::typeck::is_unsigned_type(&ty.ty) {
        builder.ins().usub_overflow(a, b)
    } else {
        builder.ins().ssub_overflow(a, b)
    };
    trap_on_overflow(builder, overflow);
    Ok(diff)
}

fn emit_checked_mul(
    builder: &mut FunctionBuilder,
    a: Value,
    b: Value,
    ty: &crate::hir::HirType,
) -> Result<Value, CodegenError> {
    let (prod, overflow) = if crate::typeck::is_unsigned_type(&ty.ty) {
        builder.ins().umul_overflow(a, b)
    } else {
        builder.ins().smul_overflow(a, b)
    };
    trap_on_overflow(builder, overflow);
    Ok(prod)
}

fn emit_checked_div(
    builder: &mut FunctionBuilder,
    a: Value,
    b: Value,
    ty: &crate::hir::HirType,
) -> Result<Value, CodegenError> {
    let b_ty = builder.func.dfg.value_type(b);
    let zero = builder.ins().iconst(b_ty, 0);
    let is_zero = builder.ins().icmp(IntCC::Equal, b, zero);
    let ok_block = builder.create_block();
    let trap_block = builder.create_block();
    builder.ins().brif(is_zero, trap_block, &[], ok_block, &[]);
    builder.switch_to_block(trap_block);
    builder.ins().trap(ir::TrapCode::IntegerDivisionByZero);
    builder.switch_to_block(ok_block);
    builder.seal_block(trap_block);
    builder.seal_block(ok_block);
    let value = if crate::typeck::is_unsigned_type(&ty.ty) {
        builder.ins().udiv(a, b)
    } else {
        builder.ins().sdiv(a, b)
    };
    Ok(value)
}

fn trap_on_overflow(builder: &mut FunctionBuilder, overflow: Value) {
    let ok_block = builder.create_block();
    let trap_block = builder.create_block();
    builder.ins().brif(overflow, trap_block, &[], ok_block, &[]);
    builder.switch_to_block(trap_block);
    builder.ins().trap(ir::TrapCode::IntegerOverflow);
    builder.switch_to_block(ok_block);
    builder.seal_block(trap_block);
    builder.seal_block(ok_block);
}

/// Emit a call to the runtime string equality function.
/// Returns an i8 value: 1 if strings are equal, 0 otherwise.
fn emit_string_eq(
    builder: &mut FunctionBuilder,
    module: &mut ObjectModule,
    lhs: Value,
    rhs: Value,
) -> Result<Value, CodegenError> {
    use cranelift_codegen::ir::{AbiParam, Signature};

    let ptr_ty = module.isa().pointer_type();

    // Build signature: (ptr, ptr) -> i8
    let mut sig = Signature::new(module.isa().default_call_conv());
    sig.params.push(AbiParam::new(ptr_ty));
    sig.params.push(AbiParam::new(ptr_ty));
    sig.returns.push(AbiParam::new(ir::types::I8));

    // Declare and import the runtime function
    let func_id = module
        .declare_function("capable_rt_string_eq", Linkage::Import, &sig)
        .map_err(|err| CodegenError::Codegen(err.to_string()))?;
    let local_func = module.declare_func_in_func(func_id, builder.func);

    // Call the function
    let call_inst = builder.ins().call(local_func, &[lhs, rhs]);
    let results = builder.inst_results(call_inst);
    Ok(results[0])
}

fn is_string_type(ty: &crate::typeck::Ty) -> bool {
    matches!(ty, crate::typeck::Ty::Path(name, _) if name == "sys.string.string" || name == "string")
}

/// Emit an index expression, calling the appropriate runtime function.
fn emit_hir_index(
    builder: &mut FunctionBuilder,
    index_expr: &crate::hir::HirIndex,
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    return_lowering: &ReturnLowering,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    // Emit the object expression
    let object = emit_hir_expr(
        builder,
        &index_expr.object,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        return_lowering,
        module,
        data_counter,
    )?;

    // Emit the index expression
    let index = emit_hir_expr(
        builder,
        &index_expr.index,
        locals,
        fn_map,
        enum_index,
        struct_layouts,
        return_lowering,
        module,
        data_counter,
    )?;

    let index_val = match index {
        ValueRepr::Single(v) => v,
        _ => {
            return Err(CodegenError::Codegen(
                "index must be a single value".to_string(),
            ))
        }
    };

    // Get the object type to determine what runtime function to call
    let object_ty = &index_expr.object.ty().ty;

    match object_ty {
        ty if is_string_type(ty) => {
            // For strings, index into the backing Slice<u8>.
            let base_ptr = match object {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Codegen(
                        "expected string to be a struct pointer".to_string(),
                    ))
                }
            };
            let layout = resolve_struct_layout(object_ty, "", &struct_layouts.layouts).ok_or_else(
                || CodegenError::Unsupported("string layout missing".to_string()),
            )?;
            let field = layout.fields.get("bytes").ok_or_else(|| {
                CodegenError::Unsupported("string.bytes field missing".to_string())
            })?;
            let slice_layout =
                resolve_struct_layout(&field.ty.ty, "", &struct_layouts.layouts).ok_or_else(
                    || CodegenError::Unsupported("Slice layout missing".to_string()),
                )?;
            let addr = ptr_add(builder, base_ptr, field.offset);
            let result = emit_slice_index(builder, module, addr, slice_layout, index_val)?;
            Ok(ValueRepr::Single(result))
        }
        crate::typeck::Ty::Path(name, _) if name == "Slice" || name == "sys.buffer.Slice" => {
            // For Slice[u8], index into the raw slice.
            let base_ptr = match object {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Codegen(
                        "expected Slice to be a struct pointer".to_string(),
                    ))
                }
            };
            let layout =
                resolve_struct_layout(object_ty, "", &struct_layouts.layouts).ok_or_else(|| {
                    CodegenError::Unsupported("Slice layout missing".to_string())
                })?;
            let result = emit_slice_index(builder, module, base_ptr, layout, index_val)?;
            Ok(ValueRepr::Single(result))
        }
        crate::typeck::Ty::Path(name, _) if name == "MutSlice" || name == "sys.buffer.MutSlice" => {
            // For MutSlice[u8], index into the raw slice.
            let base_ptr = match object {
                ValueRepr::Single(ptr) => ptr,
                _ => {
                    return Err(CodegenError::Codegen(
                        "expected MutSlice to be a struct pointer".to_string(),
                    ))
                }
            };
            let layout =
                resolve_struct_layout(object_ty, "", &struct_layouts.layouts).ok_or_else(|| {
                    CodegenError::Unsupported("MutSlice layout missing".to_string())
                })?;
            let result = emit_slice_index(builder, module, base_ptr, layout, index_val)?;
            Ok(ValueRepr::Single(result))
        }
        _ => Err(CodegenError::Codegen(format!(
            "cannot index into type {:?}",
            object_ty
        ))),
    }
}

/// Emit a bounds-checked slice index.
fn emit_slice_index(
    builder: &mut FunctionBuilder,
    module: &ObjectModule,
    slice_base: Value,
    slice_layout: &StructLayout,
    index: Value,
) -> Result<Value, CodegenError> {
    let ptr_ty = module.isa().pointer_type();
    let ptr_field = slice_layout
        .fields
        .get("ptr")
        .ok_or_else(|| CodegenError::Unsupported("Slice.ptr field missing".to_string()))?;
    let len_field = slice_layout
        .fields
        .get("len")
        .ok_or_else(|| CodegenError::Unsupported("Slice.len field missing".to_string()))?;
    let ptr_addr = ptr_add(builder, slice_base, ptr_field.offset);
    let len_addr = ptr_add(builder, slice_base, len_field.offset);
    let raw_ptr = builder.ins().load(ptr_ty, MemFlags::new(), ptr_addr, 0);
    let len_val = builder
        .ins()
        .load(ir::types::I32, MemFlags::new(), len_addr, 0);
    let zero_i32 = builder.ins().iconst(ir::types::I32, 0);
    let idx_nonneg = builder
        .ins()
        .icmp(IntCC::SignedGreaterThanOrEqual, index, zero_i32);
    let idx_lt = builder
        .ins()
        .icmp(IntCC::SignedLessThan, index, len_val);
    let ptr_nonnull = builder.ins().icmp_imm(IntCC::NotEqual, raw_ptr, 0);
    let in_bounds = builder.ins().band(idx_nonneg, idx_lt);
    let in_bounds = builder.ins().band(in_bounds, ptr_nonnull);

    let ok_block = builder.create_block();
    let err_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(done_block, ir::types::I8);
    builder.ins().brif(in_bounds, ok_block, &[], err_block, &[]);

    builder.switch_to_block(ok_block);
    builder.seal_block(ok_block);
    let idx_ptr = builder.ins().uextend(ptr_ty, index);
    let addr = builder.ins().iadd(raw_ptr, idx_ptr);
    let value = builder
        .ins()
        .load(ir::types::I8, MemFlags::new(), addr, 0);
    builder.ins().jump(done_block, &[value]);

    builder.switch_to_block(err_block);
    builder.seal_block(err_block);
    let zero_i8 = builder.ins().iconst(ir::types::I8, 0);
    builder.ins().jump(done_block, &[zero_i8]);

    builder.switch_to_block(done_block);
    builder.seal_block(done_block);
    Ok(builder.block_params(done_block)[0])
}

fn cmp_cc(expr: &crate::hir::HirExpr, signed: IntCC, unsigned: IntCC) -> IntCC {
    let ty = match expr {
        crate::hir::HirExpr::Literal(lit) => &lit.ty,
        crate::hir::HirExpr::Local(local) => &local.ty,
        crate::hir::HirExpr::EnumVariant(variant) => &variant.enum_ty,
        crate::hir::HirExpr::Call(call) => &call.ret_ty,
        crate::hir::HirExpr::FieldAccess(field) => &field.field_ty,
        crate::hir::HirExpr::Index(idx) => &idx.elem_ty,
        crate::hir::HirExpr::StructLiteral(lit) => &lit.struct_ty,
        crate::hir::HirExpr::Unary(unary) => &unary.ty,
        crate::hir::HirExpr::Binary(binary) => &binary.ty,
        crate::hir::HirExpr::Match(m) => &m.result_ty,
        crate::hir::HirExpr::Try(t) => &t.ok_ty,
        crate::hir::HirExpr::Trap(t) => &t.ty,
    };
    if crate::typeck::is_unsigned_type(&ty.ty) {
        unsigned
    } else {
        signed
    }
}

/// Emit a struct literal into a stack slot and return its address value.
fn emit_hir_struct_literal(
    builder: &mut FunctionBuilder,
    literal: &crate::hir::HirStructLiteral,
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    return_lowering: &ReturnLowering,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    if matches!(literal.struct_ty.abi, AbiType::Handle) {
        if literal.fields.len() != 1 {
            return Err(CodegenError::Unsupported(format!(
                "opaque struct literal expects 1 field, got {}",
                literal.fields.len()
            )));
        }
        let value = emit_hir_expr(
            builder,
            &literal.fields[0].expr,
            locals,
            fn_map,
            enum_index,
            struct_layouts,
            return_lowering,
            module,
            data_counter,
        )?;
        return Ok(value);
    }
    let layout = resolve_struct_layout(&literal.struct_ty.ty, "", &struct_layouts.layouts)
        .ok_or_else(|| {
            CodegenError::Unsupported(format!(
                "struct layout missing for {:?}",
                literal.struct_ty.ty
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
            return_lowering,
            module,
            data_counter,
        )?;
        store_value_by_ty(
            builder,
            base_ptr,
            field_layout.offset,
            &field_layout.ty,
            value,
            enum_index,
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
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    return_lowering: &ReturnLowering,
    module: &mut ObjectModule,
    data_counter: &mut u32,
) -> Result<ValueRepr, CodegenError> {
    let object_ty = field_access.object.ty();
    if matches!(object_ty.abi, AbiType::Handle) {
        let object_value = emit_hir_expr(
            builder,
            &field_access.object,
            locals,
            fn_map,
            enum_index,
            struct_layouts,
            return_lowering,
            module,
            data_counter,
        )?;
        if matches!(field_access.field_ty.abi, AbiType::Ptr | AbiType::Handle) {
            return Ok(object_value);
        }
        return Err(CodegenError::Unsupported(
            "field access on opaque non-pointer field".to_string(),
        ));
    }
    let layout =
        resolve_struct_layout(&object_ty.ty, "", &struct_layouts.layouts).ok_or_else(|| {
            CodegenError::Unsupported(format!(
                "struct layout missing for {:?}",
                object_ty.ty
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
        return_lowering,
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
        enum_index,
        struct_layouts,
        module,
    )
}

fn is_non_opaque_struct_type(
    ty: &crate::hir::HirType,
    struct_layouts: &StructLayoutIndex,
) -> bool {
    resolve_struct_layout(&ty.ty, "", &struct_layouts.layouts).is_some()
}

fn store_out_value(
    builder: &mut FunctionBuilder,
    out_ptr: ir::Value,
    ty: &crate::hir::HirType,
    value: ValueRepr,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
) -> Result<(), CodegenError> {
    let is_enum_payload = match &ty.ty {
        crate::typeck::Ty::Path(name, _) => enum_index.layouts.contains_key(name),
        _ => false,
    };
    if is_non_opaque_struct_type(ty, struct_layouts) || is_enum_payload {
        return store_value_by_ty(
            builder,
            out_ptr,
            0,
            ty,
            value,
            enum_index,
            struct_layouts,
            module,
        );
    }
    match ty.abi {
        AbiType::I32
        | AbiType::U32
        | AbiType::U8
        | AbiType::Bool
        | AbiType::Handle
        | AbiType::Ptr => store_value_by_tykind(
            builder,
            out_ptr,
            &ty.abi,
            value,
            module.isa().pointer_type(),
        ),
        _ => Err(CodegenError::Unsupported(
            "return out param type".to_string(),
        )),
    }
}

/// Store a lowered value into memory using a typeck::Ty layout.
fn store_value_by_ty(
    builder: &mut FunctionBuilder,
    base_ptr: ir::Value,
    offset: u32,
    ty: &crate::hir::HirType,
    value: ValueRepr,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
) -> Result<(), CodegenError> {
    use crate::typeck::{BuiltinType, Ty};

    let addr = ptr_add(builder, base_ptr, offset);
    let ptr_ty = module.isa().pointer_type();
    match &ty.ty {
        Ty::Builtin(b) => match b {
            BuiltinType::Unit | BuiltinType::Never => Ok(()),
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
            BuiltinType::I64 => Err(CodegenError::Unsupported("i64 not yet supported".to_string())),
        },
        Ty::Ptr(_) => {
            let ValueRepr::Single(val) = value else {
                return Err(CodegenError::Unsupported("store ptr".to_string()));
            };
            builder.ins().store(MemFlags::new(), val, addr, 0);
            Ok(())
        }
        Ty::Ref(inner) => {
            let inner_ty = crate::hir::HirType {
                ty: *inner.clone(),
                abi: ty.abi.clone(),
            };
            store_value_by_ty(
                builder,
                base_ptr,
                offset,
                &inner_ty,
                value,
                enum_index,
                struct_layouts,
                module,
            )
        }
        Ty::Param(_) => Err(CodegenError::Unsupported(
            "generic type parameters must be monomorphized before codegen".to_string(),
        )),
        Ty::Path(name, args) => {
            if name == "sys.result.Result" && args.len() == 2 {
                let ValueRepr::Result { tag, ok, err } = value else {
                    return Err(CodegenError::Unsupported("store result".to_string()));
                };
                let AbiType::Result(ok_abi, err_abi) = &ty.abi else {
                    return Err(CodegenError::Unsupported(
                        abi_quirks::result_abi_mismatch_error().to_string(),
                    ));
                };
                let ok_ty = crate::hir::HirType {
                    ty: args[0].clone(),
                    abi: (**ok_abi).clone(),
                };
                let err_ty = crate::hir::HirType {
                    ty: args[1].clone(),
                    abi: (**err_abi).clone(),
                };
                let ok_layout = type_layout_from_index(&ok_ty, struct_layouts, ptr_ty)?;
                let err_layout = type_layout_from_index(&err_ty, struct_layouts, ptr_ty)?;
                let (_, ok_off, err_off) = result_offsets(ok_layout, err_layout);
                builder.ins().store(MemFlags::new(), tag, addr, 0);
                store_value_by_ty(
                    builder,
                    base_ptr,
                    offset + ok_off,
                    &ok_ty,
                    *ok,
                    enum_index,
                    struct_layouts,
                    module,
                )?;
                store_value_by_ty(
                    builder,
                    base_ptr,
                    offset + err_off,
                    &err_ty,
                    *err,
                    enum_index,
                    struct_layouts,
                    module,
                )?;
                return Ok(());
            }

            if let Some(layout) = enum_index.layouts.get(name) {
                let ValueRepr::Single(src_ptr) = value else {
                    return Err(CodegenError::Unsupported("store enum".to_string()));
                };
                let dst_ptr = ptr_add(builder, base_ptr, offset);
                copy_bytes(builder, dst_ptr, src_ptr, layout.size);
                return Ok(());
            }

            if let Some(layout) = resolve_struct_layout(&ty.ty, "", &struct_layouts.layouts) {
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
                        enum_index,
                        struct_layouts,
                        module,
                    )?;
                    store_value_by_ty(
                        builder,
                        base_ptr,
                        offset + field.offset,
                        &field.ty,
                        field_value,
                        enum_index,
                        struct_layouts,
                        module,
                    )?;
                }
                return Ok(());
            }

            store_value_by_tykind(builder, addr, &ty.abi, value, ptr_ty)
        }
    }
}

/// Store a lowered value into memory using an ABI layout.
fn store_value_by_tykind(
    builder: &mut FunctionBuilder,
    addr: ir::Value,
    ty: &AbiType,
    value: ValueRepr,
    _ptr_ty: Type,
) -> Result<(), CodegenError> {
    let ValueRepr::Single(val) = value else {
        return Err(CodegenError::Unsupported("store value".to_string()));
    };
    match ty {
        AbiType::I32 | AbiType::U32 | AbiType::U8 | AbiType::Bool | AbiType::Handle | AbiType::Ptr => {
            builder.ins().store(MemFlags::new(), val, addr, 0);
            Ok(())
        }
        _ => Err(CodegenError::Unsupported(format!(
            "store unsupported {ty:?}"
        ))),
    }
}

/// Load a value from memory using a resolved HIR type layout.
fn load_value_by_ty(
    builder: &mut FunctionBuilder,
    base_ptr: ir::Value,
    offset: u32,
    ty: &crate::hir::HirType,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    module: &mut ObjectModule,
) -> Result<ValueRepr, CodegenError> {
    use crate::typeck::{BuiltinType, Ty};

    let addr = ptr_add(builder, base_ptr, offset);
    let ptr_ty = module.isa().pointer_type();
    match &ty.ty {
        Ty::Builtin(b) => match b {
            BuiltinType::Unit | BuiltinType::Never => Ok(ValueRepr::Unit),
            BuiltinType::I32 | BuiltinType::U32 => Ok(ValueRepr::Single(
                builder.ins().load(ir::types::I32, MemFlags::new(), addr, 0),
            )),
            BuiltinType::U8 | BuiltinType::Bool => Ok(ValueRepr::Single(
                builder.ins().load(ir::types::I8, MemFlags::new(), addr, 0),
            )),
            BuiltinType::I64 => Err(CodegenError::Unsupported("i64 not yet supported".to_string())),
        },
        Ty::Ptr(_) => Ok(ValueRepr::Single(
            builder.ins().load(ptr_ty, MemFlags::new(), addr, 0),
        )),
        Ty::Ref(inner) => {
            let inner_ty = crate::hir::HirType {
                ty: *inner.clone(),
                abi: ty.abi.clone(),
            };
            load_value_by_ty(
                builder,
                base_ptr,
                offset,
                &inner_ty,
                enum_index,
                struct_layouts,
                module,
            )
        }
        Ty::Param(_) => Err(CodegenError::Unsupported(
            "generic type parameters must be monomorphized before codegen".to_string(),
        )),
        Ty::Path(name, args) => {
            if name == "sys.result.Result" && args.len() == 2 {
                let AbiType::Result(ok_abi, err_abi) = &ty.abi else {
                    return Err(CodegenError::Unsupported(
                        abi_quirks::result_abi_mismatch_error().to_string(),
                    ));
                };
                let ok_ty = crate::hir::HirType {
                    ty: args[0].clone(),
                    abi: (**ok_abi).clone(),
                };
                let err_ty = crate::hir::HirType {
                    ty: args[1].clone(),
                    abi: (**err_abi).clone(),
                };
                let ok_layout = type_layout_from_index(&ok_ty, struct_layouts, ptr_ty)?;
                let err_layout = type_layout_from_index(&err_ty, struct_layouts, ptr_ty)?;
                let (_, ok_off, err_off) = result_offsets(ok_layout, err_layout);
                let tag = builder.ins().load(ir::types::I8, MemFlags::new(), addr, 0);
                let ok = load_value_by_ty(
                    builder,
                    base_ptr,
                    offset + ok_off,
                    &ok_ty,
                    enum_index,
                    struct_layouts,
                    module,
                )?;
                let err = load_value_by_ty(
                    builder,
                    base_ptr,
                    offset + err_off,
                    &err_ty,
                    enum_index,
                    struct_layouts,
                    module,
                )?;
                return Ok(ValueRepr::Result {
                    tag,
                    ok: Box::new(ok),
                    err: Box::new(err),
                });
            }

            if enum_index.layouts.contains_key(name) {
                let ptr = ptr_add(builder, base_ptr, offset);
                return Ok(ValueRepr::Single(ptr));
            }

            if resolve_struct_layout(&ty.ty, "", &struct_layouts.layouts).is_some() {
                let ptr = ptr_add(builder, base_ptr, offset);
                return Ok(ValueRepr::Single(ptr));
            }

            load_value_by_tykind(builder, addr, &ty.abi, ptr_ty)
        }
    }
}

/// Load a value from memory using an ABI layout.
fn load_value_by_tykind(
    builder: &mut FunctionBuilder,
    addr: ir::Value,
    ty: &AbiType,
    ptr_ty: Type,
) -> Result<ValueRepr, CodegenError> {
    let load_ty = match ty {
        AbiType::I32 | AbiType::U32 => ir::types::I32,
        AbiType::U8 | AbiType::Bool => ir::types::I8,
        AbiType::Handle => ir::types::I64,
        AbiType::Ptr => ptr_ty,
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

fn copy_bytes(builder: &mut FunctionBuilder, dst_ptr: ir::Value, src_ptr: ir::Value, size: u32) {
    let mut offset = 0u32;
    while offset < size {
        let byte = builder
            .ins()
            .load(ir::types::I8, MemFlags::new(), src_ptr, offset as i32);
        builder
            .ins()
            .store(MemFlags::new(), byte, dst_ptr, offset as i32);
        offset += 1;
    }
}

fn zero_bytes(builder: &mut FunctionBuilder, base_ptr: ir::Value, offset: u32, size: u32) {
    if size == 0 {
        return;
    }
    let zero = builder.ins().iconst(ir::types::I8, 0);
    let mut i = 0u32;
    while i < size {
        builder
            .ins()
            .store(MemFlags::new(), zero, base_ptr, (offset + i) as i32);
        i += 1;
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

/// Compute offsets for Result layout (tag, ok, err).
fn result_offsets(ok: TypeLayout, err: TypeLayout) -> (u32, u32, u32) {
    let tag_offset = 0u32;
    let ok_offset = align_to(1, ok.align);
    let err_offset = align_to(ok_offset.saturating_add(ok.size), err.align);
    (tag_offset, ok_offset, err_offset)
}

/// Emit short-circuit logic for `&&` and `||`.
fn emit_hir_short_circuit_expr(
    builder: &mut FunctionBuilder,
    lhs_val: ir::Value,
    rhs_expr: &crate::hir::HirExpr,
    is_and: bool,
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    return_lowering: &ReturnLowering,
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
        return_lowering,
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

/// Emit HIR match as statement (arms can contain returns, don't produce values).
/// Returns true if all paths diverged (returned/broke/continued).
fn emit_hir_match_stmt(
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
    // Emit the scrutinee expression
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
                loop_target,
                return_lowering,
                &mut arm_defers,
            )?;
            if flow == Flow::Terminated {
                arm_terminated = true;
                break;
            }
        }

        // If the arm didn't terminate (e.g., with return), jump to merge block
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

    // Always switch to merge_block to insert it into the layout
    builder.switch_to_block(merge_block);

    // If no arm continues to merge_block, it's unreachable - add trap
    // This also ensures the block has content so it's properly inserted
    if !any_arm_continues {
        builder.ins().trap(ir::TrapCode::UnreachableCodeReached);
    }

    // Return true if all paths diverged (no arm continues to merge_block)
    Ok(!any_arm_continues)
}

/// Emit HIR match expression
/// Emit HIR match expression (arms produce a value).
fn emit_hir_match_expr(
    builder: &mut FunctionBuilder,
    match_expr: &crate::hir::HirMatch,
    locals: &HashMap<crate::hir::LocalId, LocalValue>,
    fn_map: &HashMap<String, FnInfo>,
    enum_index: &EnumIndex,
    struct_layouts: &StructLayoutIndex,
    return_lowering: &ReturnLowering,
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
                None, // break/continue not allowed in value-producing match
                return_lowering,
                &mut arm_defers,
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
        let (arm_value, arm_diverges) = match last {
            HirStmt::Expr(expr_stmt) => {
                // Check if this arm ends with a Trap - if so, it diverges
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

        // If the arm diverges (e.g., with a trap), skip value storage
        if arm_diverges {
            builder.seal_block(arm_block);
        } else {
            let values = match &arm_value {
                ValueRepr::Single(val) => vec![*val],
                ValueRepr::Unit => vec![],
                ValueRepr::Result { .. } => {
                    return Err(CodegenError::Unsupported("match result value".to_string()))
                }
            };

            // Set up result shape and stack slots on first non-terminated arm
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
                return Err(CodegenError::Unsupported("mismatched match arm".to_string()));
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
    };

    Ok(result)
}

/// Compute the condition for an HIR pattern match
/// Compute the condition for an HIR pattern match.
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
        HirPattern::Variant { variant_name, .. } => {
            // Get the discriminant value for this variant
            let qualified = match &match_ty.ty {
                crate::typeck::Ty::Path(path, _) => path.clone(),
                _ => return Err(CodegenError::Codegen(format!(
                    "enum variant pattern has non-path type: {:?}",
                    match_ty.ty
                ))),
            };

            // Get the type of match_val to ensure consistent comparison
            let val_ty = builder.func.dfg.value_type(match_val);

            // Special handling for Result type (built-in, not in enum_index)
            if qualified == "sys.result.Result" {
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
            // Bind the entire value to the variable
            locals.insert(*local_id, store_local(builder, value.clone()));
            Ok(())
        }
        HirPattern::Variant { variant_name, binding, .. } => {
            if let Some(local_id) = binding {
                // Bind the inner value based on variant
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
                let payload_ty = payloads
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

/// Convert a ValueRepr into a boolean condition value.
fn to_b1(builder: &mut FunctionBuilder, value: ValueRepr) -> Result<ir::Value, CodegenError> {
    match value {
        ValueRepr::Single(val) => Ok(builder.ins().icmp_imm(IntCC::NotEqual, val, 0)),
        ValueRepr::Unit => Err(CodegenError::Unsupported("unit condition".to_string())),
        ValueRepr::Result { .. } => Err(CodegenError::Unsupported("result condition".to_string())),
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
    struct_layouts: &StructLayoutIndex,
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
    let len = builder.ins().iconst(ir::types::I32, value.len() as i64);

    let string_ty = crate::typeck::Ty::Path("sys.string.string".to_string(), Vec::new());
    let layout = resolve_struct_layout(&string_ty, "", &struct_layouts.layouts).ok_or_else(|| {
        CodegenError::Unsupported("string layout missing".to_string())
    })?;
    let field = layout.fields.get("bytes").ok_or_else(|| {
        CodegenError::Unsupported("string.bytes field missing".to_string())
    })?;
    let slice_layout =
        resolve_struct_layout(&field.ty.ty, "", &struct_layouts.layouts).ok_or_else(|| {
            CodegenError::Unsupported("Slice layout missing".to_string())
        })?;
    let slice_ptr = slice_layout.fields.get("ptr").ok_or_else(|| {
        CodegenError::Unsupported("Slice.ptr field missing".to_string())
    })?;
    let slice_len = slice_layout.fields.get("len").ok_or_else(|| {
        CodegenError::Unsupported("Slice.len field missing".to_string())
    })?;
    let ptr_ty = module.isa().pointer_type();
    let align = layout.align.max(1);
    let slot_size = layout.size.max(1).saturating_add(align - 1);
    let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
        ir::StackSlotKind::ExplicitSlot,
        slot_size,
    ));
    let base_ptr = aligned_stack_addr(builder, slot, align, ptr_ty);
    let addr = ptr_add(builder, base_ptr, field.offset);
    let ptr_addr = ptr_add(builder, addr, slice_ptr.offset);
    let len_addr = ptr_add(builder, addr, slice_len.offset);
    builder
        .ins()
        .store(MemFlags::new(), ptr, ptr_addr, 0);
    builder.ins().store(MemFlags::new(), len, len_addr, 0);
    Ok(ValueRepr::Single(base_ptr))
}

/// Flatten a ValueRepr into ABI-ready Cranelift values.
pub(super) fn flatten_value(value: &ValueRepr) -> Vec<ir::Value> {
    match value {
        ValueRepr::Unit => vec![],
        ValueRepr::Single(val) => vec![*val],
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
fn value_type_for_result_out(ty: &AbiType, ptr_ty: Type) -> Result<ir::Type, CodegenError> {
    match ty {
        AbiType::I32 | AbiType::U32 => Ok(ir::types::I32),
        AbiType::U8 | AbiType::Bool => Ok(ir::types::I8),
        AbiType::Handle => Ok(ir::types::I64),
        AbiType::Ptr => Ok(ptr_ty),
        AbiType::Unit => Err(CodegenError::Unsupported("result out unit".to_string())),
        _ => Err(CodegenError::Unsupported(
            abi_quirks::result_out_params_error().to_string(),
        )),
    }
}

/// Construct a zero/empty value for an ABI type.
fn zero_value_for_tykind(
    builder: &mut FunctionBuilder,
    ty: &AbiType,
    ptr_ty: Type,
) -> Result<ValueRepr, CodegenError> {
    match ty {
        AbiType::Unit => Ok(ValueRepr::Unit),
        AbiType::I32 | AbiType::U32 => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I32, 0))),
        AbiType::U8 | AbiType::Bool => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I8, 0))),
        AbiType::Handle => Ok(ValueRepr::Single(builder.ins().iconst(ir::types::I64, 0))),
        AbiType::Ptr => Ok(ValueRepr::Single(builder.ins().iconst(ptr_ty, 0))),
        AbiType::Result(ok, err) => {
            let tag = builder.ins().iconst(ir::types::I8, 0);
            let ok_val = zero_value_for_tykind(builder, ok, ptr_ty)?;
            let err_val = zero_value_for_tykind(builder, err, ptr_ty)?;
            Ok(ValueRepr::Result {
                tag,
                ok: Box::new(ok_val),
                err: Box::new(err_val),
            })
        }
        AbiType::ResultOut(_, _) => Err(CodegenError::Unsupported(
            abi_quirks::result_out_params_error().to_string(),
        )),
    }
}

/// Construct a zero/empty value for a resolved HIR type.
fn zero_value_for_ty(
    builder: &mut FunctionBuilder,
    ty: &crate::hir::HirType,
    ptr_ty: Type,
    enum_index: &EnumIndex,
    struct_layouts: Option<&StructLayoutIndex>,
    module: &mut ObjectModule,
) -> Result<ValueRepr, CodegenError> {
    use crate::typeck::Ty;

    match &ty.ty {
        Ty::Builtin(_) | Ty::Ptr(_) => zero_value_for_tykind(builder, &ty.abi, ptr_ty),
        Ty::Ref(inner) => {
            let inner_ty = crate::hir::HirType {
                ty: *inner.clone(),
                abi: ty.abi.clone(),
            };
            zero_value_for_ty(builder, &inner_ty, ptr_ty, enum_index, struct_layouts, module)
        }
        Ty::Param(_) => Err(CodegenError::Unsupported(
            "generic type parameters must be monomorphized before codegen".to_string(),
        )),
        Ty::Path(name, args) => {
            if name == "sys.result.Result" && args.len() == 2 {
                let AbiType::Result(ok_abi, err_abi) = &ty.abi else {
                    return Err(CodegenError::Unsupported(
                        abi_quirks::result_abi_mismatch_error().to_string(),
                    ));
                };
                let ok_ty = crate::hir::HirType {
                    ty: args[0].clone(),
                    abi: (**ok_abi).clone(),
                };
                let err_ty = crate::hir::HirType {
                    ty: args[1].clone(),
                    abi: (**err_abi).clone(),
                };
                let tag = builder.ins().iconst(ir::types::I8, 0);
                let ok_val =
                    zero_value_for_ty(builder, &ok_ty, ptr_ty, enum_index, struct_layouts, module)?;
                let err_val =
                    zero_value_for_ty(builder, &err_ty, ptr_ty, enum_index, struct_layouts, module)?;
                return Ok(ValueRepr::Result {
                    tag,
                    ok: Box::new(ok_val),
                    err: Box::new(err_val),
                });
            }
            if let Some(struct_layouts) = struct_layouts {
                if let Some(layout) = resolve_struct_layout(&ty.ty, "", &struct_layouts.layouts) {
                    let align = layout.align.max(1);
                    let slot_size = layout.size.max(1).saturating_add(align - 1);
                    let slot = builder.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        slot_size,
                    ));
                    let base_ptr = aligned_stack_addr(builder, slot, align, ptr_ty);
                    for name in &layout.field_order {
                        let Some(field) = layout.fields.get(name) else {
                            continue;
                        };
                        let field_zero = zero_value_for_ty(
                            builder,
                            &field.ty,
                            ptr_ty,
                            enum_index,
                            Some(struct_layouts),
                            module,
                        )?;
                        store_value_by_ty(
                            builder,
                            base_ptr,
                            field.offset,
                            &field.ty,
                            field_zero,
                            enum_index,
                            struct_layouts,
                            module,
                        )?;
                    }
                    return Ok(ValueRepr::Single(base_ptr));
                }
            }
            zero_value_for_tykind(builder, &ty.abi, ptr_ty)
        }
    }
}

/// Reconstruct a ValueRepr from ABI parameters.
pub(super) fn value_from_params(
    builder: &mut FunctionBuilder,
    ty: &AbiType,
    params: &[ir::Value],
    idx: &mut usize,
) -> Result<ValueRepr, CodegenError> {
    match ty {
        AbiType::Unit => Ok(ValueRepr::Unit),
        AbiType::I32 | AbiType::U32 | AbiType::U8 | AbiType::Bool | AbiType::Handle | AbiType::Ptr => {
            let val = params[*idx];
            *idx += 1;
            Ok(ValueRepr::Single(val))
        }
        AbiType::Result(ok, err) => {
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
        // ResultOut is an ABI-level return type, not an input type.
        // They should never appear as function parameters.
        AbiType::ResultOut(ok, err) => {
            Err(CodegenError::Codegen(format!(
                "ResultOut<{ok:?}, {err:?}> cannot be a parameter type (ABI return type only)"
            )))
        }
    }
}

/// Reconstruct a ValueRepr from ABI return values.
fn value_from_results(
    builder: &mut FunctionBuilder,
    ty: &AbiType,
    results: &[ir::Value],
    idx: &mut usize,
) -> Result<ValueRepr, CodegenError> {
    match ty {
        AbiType::Unit => Ok(ValueRepr::Unit),
        AbiType::I32 | AbiType::U32 | AbiType::U8 | AbiType::Bool | AbiType::Handle | AbiType::Ptr => {
            let val = results
                .get(*idx)
                .ok_or_else(|| CodegenError::Codegen("missing return value".to_string()))?;
            *idx += 1;
            Ok(ValueRepr::Single(*val))
        }
        AbiType::Result(ok, err) => {
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
        AbiType::ResultOut(_, _) => Err(CodegenError::Unsupported(
            abi_quirks::result_out_params_error().to_string(),
        )),
    }
}

// --- Runtime call emission ---
/// Emit a call to a runtime intrinsic with ABI adaptation when needed.
pub(super) fn emit_runtime_wrapper_call(
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
        let (size, align) = if let Some(layout) =
            resolve_struct_layout(&ret_ty.ty, "", &struct_layouts.layouts)
        {
            (layout.size, layout.align)
        } else if let crate::typeck::Ty::Path(name, _) = &ret_ty.ty {
            let layout = enum_index.layouts.get(name).ok_or_else(|| {
                CodegenError::Unsupported("enum layout missing".to_string())
            })?;
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
    let call_symbol = info
        .runtime_symbol
        .as_deref()
        .unwrap_or(&info.symbol);
    let func_id = module
        .declare_function(call_symbol, Linkage::Import, &sig)
        .map_err(|err| CodegenError::Codegen(err.to_string()))?;
    let local = module.declare_func_in_func(func_id, builder.func);
    let call_inst = builder.ins().call(local, &call_args);
    let results = builder.inst_results(call_inst).to_vec();

    if abi_quirks::is_result_out(&abi_sig.ret) {
        let tag = results
            .get(0)
            .ok_or_else(|| CodegenError::Codegen("missing result tag".to_string()))?;
        let (ok_slot, err_slot, ok_ty, err_ty) = result_out
            .ok_or_else(|| CodegenError::Codegen("missing result slots".to_string()))?;
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

fn emit_unsafe_ptr_call(
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
            let size = builder
                .ins()
                .iconst(ir::types::I32, layout.size as i64);
            return Ok(Some(ValueRepr::Single(size)));
        }
        "alignof" => {
            let align = builder
                .ins()
                .iconst(ir::types::I32, layout.align as i64);
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
            let is_null = builder.ins().icmp_imm(
                ir::condcodes::IntCC::Equal,
                base_ptr,
                0,
            );
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
            builder.ins().brif(should_copy, copy_block, &[], done_block, &[]);

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
    Ok(crate::hir::HirType { ty: ty.clone(), abi })
}

fn ensure_abi_sig_handled(info: &FnInfo) -> Result<(), CodegenError> {
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
    fn ensure_abi_sig_rejects_unhandled_mismatch() {
        let sig = FnSig {
            params: vec![AbiType::I32],
            ret: AbiType::I32,
        };
        let abi_sig = FnSig {
            params: vec![AbiType::I32],
            ret: AbiType::U32,
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
