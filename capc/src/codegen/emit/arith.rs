use super::*;

pub(in crate::codegen) fn emit_checked_add(
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

pub(in crate::codegen) fn emit_checked_sub(
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

pub(in crate::codegen) fn emit_checked_mul(
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

pub(in crate::codegen) fn emit_checked_div(
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

pub(in crate::codegen) fn emit_checked_mod(
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
        builder.ins().urem(a, b)
    } else {
        builder.ins().srem(a, b)
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

pub(in crate::codegen) fn emit_string_eq(
    builder: &mut FunctionBuilder,
    module: &mut ObjectModule,
    lhs: Value,
    rhs: Value,
) -> Result<Value, CodegenError> {
    use cranelift_codegen::ir::{AbiParam, Signature};

    let ptr_ty = module.isa().pointer_type();
    let mut sig = Signature::new(module.isa().default_call_conv());
    sig.params.push(AbiParam::new(ptr_ty));
    sig.params.push(AbiParam::new(ptr_ty));
    sig.returns.push(AbiParam::new(ir::types::I8));

    let func_id = module
        .declare_function("capable_rt_string_eq", Linkage::Import, &sig)
        .map_err(|err| CodegenError::Codegen(err.to_string()))?;
    let local_func = module.declare_func_in_func(func_id, builder.func);
    let call_inst = builder.ins().call(local_func, &[lhs, rhs]);
    let results = builder.inst_results(call_inst);
    Ok(results[0])
}

pub(in crate::codegen) fn is_string_type(ty: &crate::typeck::Ty) -> bool {
    matches!(ty, crate::typeck::Ty::Path(name, _) if name == "sys.string.string" || name == "string")
}
