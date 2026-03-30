use super::*;

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
pub(in crate::codegen) struct DeferStack {
    scopes: Vec<DeferScope>,
}

impl DeferStack {
    pub(in crate::codegen) fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub(in crate::codegen) fn push_block_scope(&mut self) {
        self.push_scope(DeferScopeKind::Regular);
    }

    pub(in crate::codegen) fn push_loop_scope(&mut self) {
        self.push_scope(DeferScopeKind::LoopBody);
    }

    fn push_scope(&mut self, kind: DeferScopeKind) {
        self.scopes.push(DeferScope {
            kind,
            defers: Vec::new(),
        });
    }

    pub(in crate::codegen) fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    pub(in crate::codegen) fn push_defer(&mut self, expr: crate::hir::HirExpr) {
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

    pub(in crate::codegen) fn emit_current_and_pop(
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

    pub(in crate::codegen) fn emit_all_and_clear(
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

    pub(in crate::codegen) fn emit_until_loop_and_pop(
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
