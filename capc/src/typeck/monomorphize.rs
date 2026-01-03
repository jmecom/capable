use std::collections::{HashMap, HashSet};

use crate::abi::AbiType;
use crate::error::TypeError;
use crate::hir::*;
use crate::typeck::Ty;
use crate::ast::Span;

const DUMMY_SPAN: Span = Span { start: 0, end: 0 };

#[derive(Clone)]
struct ModuleOut {
    name: String,
    functions: Vec<HirFunction>,
    extern_functions: Vec<HirExternFunction>,
    structs: Vec<HirStruct>,
    enums: Vec<HirEnum>,
}

impl ModuleOut {
    fn new(name: String) -> Self {
        Self {
            name,
            functions: Vec::new(),
            extern_functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
        }
    }
}

#[derive(Clone)]
struct FunctionInstance {
    module: String,
    base_name: String,
    type_args: Vec<Ty>,
}

struct MonoCtx {
    program: HirProgram,
    functions: HashMap<String, HirFunction>,
    externs: HashMap<String, HirExternFunction>,
    structs: HashMap<String, HirStruct>,
    enums: HashMap<String, HirEnum>,
    out_modules: HashMap<String, ModuleOut>,
    generated_functions: HashSet<String>,
    generated_externs: HashSet<String>,
    generated_structs: HashSet<String>,
    generated_enums: HashSet<String>,
    queue: Vec<FunctionInstance>,
}

pub(super) fn monomorphize_program(program: HirProgram) -> Result<HirProgram, TypeError> {
    let mut ctx = MonoCtx::new(program)?;
    ctx.seed_roots()?;
    ctx.process_queue()?;
    Ok(ctx.into_program())
}

impl MonoCtx {
    fn new(program: HirProgram) -> Result<Self, TypeError> {
        let mut out_modules = HashMap::new();
        let mut functions = HashMap::new();
        let mut externs = HashMap::new();
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();

        let mut register_module = |module: &HirModule| {
            out_modules
                .entry(module.name.clone())
                .or_insert_with(|| ModuleOut::new(module.name.clone()));
            for func in &module.functions {
                let key = qualify(&module.name, &func.name);
                functions.insert(key, func.clone());
            }
            for func in &module.extern_functions {
                let key = qualify(&module.name, &func.name);
                externs.insert(key, func.clone());
            }
            for decl in &module.structs {
                let key = qualify(&module.name, &decl.name);
                structs.insert(key, decl.clone());
            }
            for decl in &module.enums {
                let key = qualify(&module.name, &decl.name);
                enums.insert(key, decl.clone());
            }
        };

        register_module(&program.entry);
        for module in &program.user_modules {
            register_module(module);
        }
        for module in &program.stdlib {
            register_module(module);
        }

        Ok(Self {
            program,
            functions,
            externs,
            structs,
            enums,
            out_modules,
            generated_functions: HashSet::new(),
            generated_externs: HashSet::new(),
            generated_structs: HashSet::new(),
            generated_enums: HashSet::new(),
            queue: Vec::new(),
        })
    }

    fn seed_roots(&mut self) -> Result<(), TypeError> {
        let modules = self.all_modules().into_iter().cloned().collect::<Vec<_>>();
        for module in modules {
            for func in &module.functions {
                if !func.type_params.is_empty() {
                    continue;
                }
                let name = func.name.clone();
                let mono = self.mono_function(&module.name, func, &HashMap::new(), name)?;
                self.push_function(&module.name, mono);
            }
            for func in &module.extern_functions {
                if !func.type_params.is_empty() {
                    continue;
                }
                let name = func.name.clone();
                let mono = self.mono_extern(&module.name, func, &HashMap::new(), name)?;
                self.push_extern(&module.name, mono);
            }
            for decl in &module.structs {
                if !decl.type_params.is_empty() {
                    continue;
                }
                let name = decl.name.clone();
                let mono = self.mono_struct(&module.name, decl, &HashMap::new(), name)?;
                self.push_struct(&module.name, mono);
            }
            for decl in &module.enums {
                if !decl.type_params.is_empty() {
                    continue;
                }
                let name = decl.name.clone();
                let mono = self.mono_enum(&module.name, decl, &HashMap::new(), name)?;
                self.push_enum(&module.name, mono);
            }
        }
        Ok(())
    }

    fn all_modules(&self) -> Vec<&HirModule> {
        let mut modules = Vec::new();
        modules.push(&self.program.entry);
        for module in &self.program.user_modules {
            modules.push(module);
        }
        for module in &self.program.stdlib {
            modules.push(module);
        }
        modules
    }

    fn process_queue(&mut self) -> Result<(), TypeError> {
        while let Some(instance) = self.queue.pop() {
            let key = qualify(&instance.module, &instance.base_name);
            if let Some(func) = self.functions.get(&key).cloned() {
                let new_name = mangle_name(&instance.base_name, &instance.type_args);
                if self.generated_functions.contains(&qualify(&instance.module, &new_name)) {
                    continue;
                }
                let subs = build_substitution(&func.type_params, &instance.type_args, DUMMY_SPAN)?;
                let mono = self.mono_function(&instance.module, &func, &subs, new_name)?;
                self.push_function(&instance.module, mono);
                continue;
            }
            if let Some(func) = self.externs.get(&key).cloned() {
                let new_name = mangle_name(&instance.base_name, &instance.type_args);
                if self.generated_externs.contains(&qualify(&instance.module, &new_name)) {
                    continue;
                }
                let subs = build_substitution(&func.type_params, &instance.type_args, DUMMY_SPAN)?;
                let mono = self.mono_extern(&instance.module, &func, &subs, new_name)?;
                self.push_extern(&instance.module, mono);
                continue;
            }
            return Err(TypeError::new(
                format!("unknown function `{}`", key),
                DUMMY_SPAN,
            ));
        }
        Ok(())
    }

    fn into_program(self) -> HirProgram {
        let mut modules = self.out_modules.into_values().collect::<Vec<_>>();
        modules.sort_by(|a, b| a.name.cmp(&b.name));
        let mut entry = None;
        let mut user_modules = Vec::new();
        let mut stdlib = Vec::new();
        for module in modules {
            if module.name == self.program.entry.name {
                entry = Some(module);
                continue;
            }
            if self
                .program
                .stdlib
                .iter()
                .any(|m| m.name == module.name)
            {
                stdlib.push(module);
            } else {
                user_modules.push(module);
            }
        }
        let entry = entry.unwrap_or_else(|| ModuleOut::new(self.program.entry.name));
        HirProgram {
            entry: entry.into(),
            user_modules: user_modules.into_iter().map(Into::into).collect(),
            stdlib: stdlib.into_iter().map(Into::into).collect(),
        }
    }

    fn push_function(&mut self, module: &str, func: HirFunction) {
        let key = qualify(module, &func.name);
        if self.generated_functions.insert(key) {
            if let Some(out) = self.out_modules.get_mut(module) {
                out.functions.push(func);
            }
        }
    }

    fn push_extern(&mut self, module: &str, func: HirExternFunction) {
        let key = qualify(module, &func.name);
        if self.generated_externs.insert(key) {
            if let Some(out) = self.out_modules.get_mut(module) {
                out.extern_functions.push(func);
            }
        }
    }

    fn push_struct(&mut self, module: &str, decl: HirStruct) {
        let key = qualify(module, &decl.name);
        if self.generated_structs.insert(key) {
            self.structs.insert(qualify(module, &decl.name), decl.clone());
            if let Some(out) = self.out_modules.get_mut(module) {
                out.structs.push(decl);
            }
        }
    }

    fn push_enum(&mut self, module: &str, decl: HirEnum) {
        let key = qualify(module, &decl.name);
        if self.generated_enums.insert(key) {
            self.enums.insert(qualify(module, &decl.name), decl.clone());
            if let Some(out) = self.out_modules.get_mut(module) {
                out.enums.push(decl);
            }
        }
    }

    fn mono_function(
        &mut self,
        module: &str,
        func: &HirFunction,
        subs: &HashMap<String, Ty>,
        new_name: String,
    ) -> Result<HirFunction, TypeError> {
        let params: Result<Vec<HirParam>, TypeError> = func
            .params
            .iter()
            .map(|param| {
                let ty = self.mono_hir_type(module, &param.ty, subs)?;
                Ok(HirParam {
                    local_id: param.local_id,
                    ty,
                })
            })
            .collect();
        let ret_ty = self.mono_hir_type(module, &func.ret_ty, subs)?;
        let body = self.mono_block(module, &func.body, subs)?;
        Ok(HirFunction {
            name: new_name,
            type_params: Vec::new(),
            params: params?,
            ret_ty,
            body,
        })
    }

    fn mono_extern(
        &mut self,
        module: &str,
        func: &HirExternFunction,
        subs: &HashMap<String, Ty>,
        new_name: String,
    ) -> Result<HirExternFunction, TypeError> {
        let params: Result<Vec<HirParam>, TypeError> = func
            .params
            .iter()
            .map(|param| {
                let ty = self.mono_hir_type(module, &param.ty, subs)?;
                Ok(HirParam {
                    local_id: param.local_id,
                    ty,
                })
            })
            .collect();
        let ret_ty = self.mono_hir_type(module, &func.ret_ty, subs)?;
        Ok(HirExternFunction {
            name: new_name,
            type_params: Vec::new(),
            params: params?,
            ret_ty,
        })
    }

    fn mono_struct(
        &mut self,
        module: &str,
        decl: &HirStruct,
        subs: &HashMap<String, Ty>,
        new_name: String,
    ) -> Result<HirStruct, TypeError> {
        let fields: Result<Vec<HirField>, TypeError> = decl
            .fields
            .iter()
            .map(|field| {
                let ty = self.mono_hir_type(module, &field.ty, subs)?;
                Ok(HirField {
                    name: field.name.clone(),
                    ty,
                })
            })
            .collect();
        Ok(HirStruct {
            name: new_name,
            type_params: Vec::new(),
            fields: fields?,
            is_opaque: decl.is_opaque,
        })
    }

    fn mono_enum(
        &mut self,
        module: &str,
        decl: &HirEnum,
        subs: &HashMap<String, Ty>,
        new_name: String,
    ) -> Result<HirEnum, TypeError> {
        let variants: Result<Vec<HirEnumVariant>, TypeError> = decl
            .variants
            .iter()
            .map(|variant| {
                let payload = match &variant.payload {
                    Some(payload) => Some(self.mono_hir_type(module, payload, subs)?),
                    None => None,
                };
                Ok(HirEnumVariant {
                    name: variant.name.clone(),
                    payload,
                })
            })
            .collect();
        Ok(HirEnum {
            name: new_name,
            type_params: Vec::new(),
            variants: variants?,
        })
    }

    fn mono_block(
        &mut self,
        module: &str,
        block: &HirBlock,
        subs: &HashMap<String, Ty>,
    ) -> Result<HirBlock, TypeError> {
        let stmts: Result<Vec<HirStmt>, TypeError> = block
            .stmts
            .iter()
            .map(|stmt| self.mono_stmt(module, stmt, subs))
            .collect();
        Ok(HirBlock { stmts: stmts? })
    }

    fn mono_stmt(
        &mut self,
        module: &str,
        stmt: &HirStmt,
        subs: &HashMap<String, Ty>,
    ) -> Result<HirStmt, TypeError> {
        match stmt {
            HirStmt::Let(let_stmt) => {
                let expr = self.mono_expr(module, &let_stmt.expr, subs)?;
                let ty = self.mono_hir_type(module, &let_stmt.ty, subs)?;
                Ok(HirStmt::Let(HirLetStmt {
                    local_id: let_stmt.local_id,
                    ty,
                    expr,
                    span: let_stmt.span,
                }))
            }
            HirStmt::Assign(assign) => {
                let expr = self.mono_expr(module, &assign.expr, subs)?;
                Ok(HirStmt::Assign(HirAssignStmt {
                    local_id: assign.local_id,
                    expr,
                    span: assign.span,
                }))
            }
            HirStmt::Defer(defer_stmt) => {
                let expr = self.mono_expr(module, &defer_stmt.expr, subs)?;
                Ok(HirStmt::Defer(HirDeferStmt {
                    expr,
                    span: defer_stmt.span,
                }))
            }
            HirStmt::Return(ret) => {
                let expr = ret
                    .expr
                    .as_ref()
                    .map(|expr| self.mono_expr(module, expr, subs))
                    .transpose()?;
                Ok(HirStmt::Return(HirReturnStmt {
                    expr,
                    span: ret.span,
                }))
            }
            HirStmt::Break(break_stmt) => Ok(HirStmt::Break(HirBreakStmt {
                span: break_stmt.span,
            })),
            HirStmt::Continue(continue_stmt) => Ok(HirStmt::Continue(HirContinueStmt {
                span: continue_stmt.span,
            })),
            HirStmt::If(if_stmt) => {
                let cond = self.mono_expr(module, &if_stmt.cond, subs)?;
                let then_block = self.mono_block(module, &if_stmt.then_block, subs)?;
                let else_block = if_stmt
                    .else_block
                    .as_ref()
                    .map(|block| self.mono_block(module, block, subs))
                    .transpose()?;
                Ok(HirStmt::If(HirIfStmt {
                    cond,
                    then_block,
                    else_block,
                    span: if_stmt.span,
                }))
            }
            HirStmt::While(while_stmt) => {
                let cond = self.mono_expr(module, &while_stmt.cond, subs)?;
                let body = self.mono_block(module, &while_stmt.body, subs)?;
                Ok(HirStmt::While(HirWhileStmt {
                    cond,
                    body,
                    span: while_stmt.span,
                }))
            }
            HirStmt::For(for_stmt) => {
                let start = self.mono_expr(module, &for_stmt.start, subs)?;
                let end = self.mono_expr(module, &for_stmt.end, subs)?;
                let body = self.mono_block(module, &for_stmt.body, subs)?;
                Ok(HirStmt::For(HirForStmt {
                    var_id: for_stmt.var_id,
                    start,
                    end,
                    body,
                    span: for_stmt.span,
                }))
            }
            HirStmt::Expr(expr_stmt) => {
                let expr = self.mono_expr(module, &expr_stmt.expr, subs)?;
                Ok(HirStmt::Expr(HirExprStmt {
                    expr,
                    span: expr_stmt.span,
                }))
            }
        }
    }

    fn mono_expr(
        &mut self,
        module: &str,
        expr: &HirExpr,
        subs: &HashMap<String, Ty>,
    ) -> Result<HirExpr, TypeError> {
        match expr {
            HirExpr::Literal(lit) => Ok(HirExpr::Literal(HirLiteral {
                value: lit.value.clone(),
                ty: self.mono_hir_type(module, &lit.ty, subs)?,
                span: lit.span,
            })),
            HirExpr::Local(local) => Ok(HirExpr::Local(HirLocal {
                local_id: local.local_id,
                ty: self.mono_hir_type(module, &local.ty, subs)?,
                span: local.span,
            })),
            HirExpr::EnumVariant(variant) => {
                let enum_ty = self.mono_hir_type(module, &variant.enum_ty, subs)?;
                let payload = variant
                    .payload
                    .as_ref()
                    .map(|expr| self.mono_expr(module, expr, subs))
                    .transpose()?
                    .map(Box::new);
                Ok(HirExpr::EnumVariant(HirEnumVariantExpr {
                    enum_ty,
                    variant_name: variant.variant_name.clone(),
                    payload,
                    span: variant.span,
                }))
            }
            HirExpr::Call(call) => {
                let args: Result<Vec<HirExpr>, TypeError> = call
                    .args
                    .iter()
                    .map(|arg| self.mono_expr(module, arg, subs))
                    .collect();
                let args = args?;
                let ret_ty = self.mono_hir_type(module, &call.ret_ty, subs)?;
                match &call.callee {
                    ResolvedCallee::Intrinsic(id) => {
                        return Ok(HirExpr::Call(HirCall {
                            callee: ResolvedCallee::Intrinsic(*id),
                            type_args: Vec::new(),
                            args,
                            ret_ty,
                            span: call.span,
                        }));
                    }
                    ResolvedCallee::Function { module, name, .. } => {
                        let key = qualify(module, name);
                        if let Some(func) = self.functions.get(&key).cloned() {
                            let (new_name, symbol, type_args) = self.mono_callee(
                                module,
                                &func,
                                &call.args,
                                &call.type_args,
                                subs,
                            )?;
                            let callee = ResolvedCallee::Function {
                                module: module.clone(),
                                name: new_name,
                                symbol,
                            };
                            return Ok(HirExpr::Call(HirCall {
                                callee,
                                type_args,
                                args,
                                ret_ty,
                                span: call.span,
                            }));
                        }
                        if let Some(func) = self.externs.get(&key).cloned() {
                            let (new_name, symbol, type_args) = self.mono_callee(
                                module,
                                &func,
                                &call.args,
                                &call.type_args,
                                subs,
                            )?;
                            let callee = ResolvedCallee::Function {
                                module: module.clone(),
                                name: new_name,
                                symbol,
                            };
                            return Ok(HirExpr::Call(HirCall {
                                callee,
                                type_args,
                                args,
                                ret_ty,
                                span: call.span,
                            }));
                        }
                        return Err(TypeError::new(
                            format!("unknown function `{}`", key),
                            DUMMY_SPAN,
                        ));
                    }
                }
            }
            HirExpr::FieldAccess(access) => Ok(HirExpr::FieldAccess(HirFieldAccess {
                object: Box::new(self.mono_expr(module, &access.object, subs)?),
                field_name: access.field_name.clone(),
                field_ty: self.mono_hir_type(module, &access.field_ty, subs)?,
                span: access.span,
            })),
            HirExpr::StructLiteral(literal) => {
                let struct_ty = self.mono_hir_type(module, &literal.struct_ty, subs)?;
                let fields: Result<Vec<HirStructLiteralField>, TypeError> = literal
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(HirStructLiteralField {
                            name: field.name.clone(),
                            expr: self.mono_expr(module, &field.expr, subs)?,
                        })
                    })
                    .collect();
                Ok(HirExpr::StructLiteral(HirStructLiteral {
                    struct_ty,
                    fields: fields?,
                    span: literal.span,
                }))
            }
            HirExpr::Unary(unary) => Ok(HirExpr::Unary(HirUnary {
                op: unary.op.clone(),
                expr: Box::new(self.mono_expr(module, &unary.expr, subs)?),
                ty: self.mono_hir_type(module, &unary.ty, subs)?,
                span: unary.span,
            })),
            HirExpr::Binary(binary) => Ok(HirExpr::Binary(HirBinary {
                op: binary.op.clone(),
                left: Box::new(self.mono_expr(module, &binary.left, subs)?),
                right: Box::new(self.mono_expr(module, &binary.right, subs)?),
                ty: self.mono_hir_type(module, &binary.ty, subs)?,
                span: binary.span,
            })),
            HirExpr::Match(m) => {
                let expr = self.mono_expr(module, &m.expr, subs)?;
                let arms: Result<Vec<HirMatchArm>, TypeError> = m
                    .arms
                    .iter()
                    .map(|arm| {
                        Ok(HirMatchArm {
                            pattern: arm.pattern.clone(),
                            body: self.mono_block(module, &arm.body, subs)?,
                        })
                    })
                    .collect();
                Ok(HirExpr::Match(HirMatch {
                    expr: Box::new(expr),
                    arms: arms?,
                    result_ty: self.mono_hir_type(module, &m.result_ty, subs)?,
                    span: m.span,
                }))
            }
            HirExpr::Try(t) => {
                let expr = self.mono_expr(module, &t.expr, subs)?;
                Ok(HirExpr::Try(HirTry {
                    expr: Box::new(expr),
                    ok_ty: self.mono_hir_type(module, &t.ok_ty, subs)?,
                    ret_ty: self.mono_hir_type(module, &t.ret_ty, subs)?,
                    span: t.span,
                }))
            }
            HirExpr::Trap(t) => Ok(HirExpr::Trap(HirTrap {
                ty: self.mono_hir_type(module, &t.ty, subs)?,
                span: t.span,
            })),
            HirExpr::Index(idx) => {
                let object = self.mono_expr(module, &idx.object, subs)?;
                let index = self.mono_expr(module, &idx.index, subs)?;
                Ok(HirExpr::Index(crate::hir::HirIndex {
                    object: Box::new(object),
                    index: Box::new(index),
                    elem_ty: self.mono_hir_type(module, &idx.elem_ty, subs)?,
                    span: idx.span,
                }))
            }
        }
    }

    fn mono_callee(
        &mut self,
        module: &str,
        func: &impl GenericSig,
        args: &[HirExpr],
        explicit_type_args: &[Ty],
        subs: &HashMap<String, Ty>,
    ) -> Result<(String, String, Vec<Ty>), TypeError> {
        if func.type_params().is_empty() {
            if !explicit_type_args.is_empty() {
                return Err(TypeError::new(
                    "function does not accept type arguments".to_string(),
                    DUMMY_SPAN,
                ));
            }
            let name = func.name().to_string();
            let symbol = function_symbol(module, &name);
            return Ok((name, symbol, Vec::new()));
        }

        let explicit_args: Vec<Ty> = explicit_type_args
            .iter()
            .map(|arg| substitute_ty(arg, subs))
            .collect();

        let mut inferred = HashMap::new();
        if !explicit_args.is_empty() {
            for (name, arg) in func.type_params().iter().zip(explicit_args.iter()) {
                inferred.insert(name.clone(), arg.clone());
            }
        }
        for (param, arg) in func.params().iter().zip(args.iter()) {
            let actual = substitute_ty(&arg.ty().ty, subs);
            match_type_params(&param.ty.ty, &actual, &mut inferred, DUMMY_SPAN)?;
        }
        for name in func.type_params() {
            if !inferred.contains_key(name) {
                return Err(TypeError::new(
                    format!("missing type argument for `{name}`"),
                    DUMMY_SPAN,
                ));
            }
        }
        let ordered_args = func
            .type_params()
            .iter()
            .map(|name| inferred.get(name).cloned().unwrap())
            .collect::<Vec<_>>();
        let new_name = mangle_name(func.name(), &ordered_args);
        let symbol = function_symbol(module, &new_name);
        self.queue.push(FunctionInstance {
            module: module.to_string(),
            base_name: func.name().to_string(),
            type_args: ordered_args.clone(),
        });
        Ok((new_name, symbol, ordered_args))
    }

    fn mono_hir_type(
        &mut self,
        module: &str,
        ty: &HirType,
        subs: &HashMap<String, Ty>,
    ) -> Result<HirType, TypeError> {
        let mono_ty = self.mono_ty(module, &ty.ty, subs)?;
        let abi = self.abi_type_for(module, &mono_ty)?;
        Ok(HirType { ty: mono_ty, abi })
    }

    fn mono_ty(
        &mut self,
        module: &str,
        ty: &Ty,
        subs: &HashMap<String, Ty>,
    ) -> Result<Ty, TypeError> {
        match ty {
            Ty::Param(name) => subs
                .get(name)
                .cloned()
                .ok_or_else(|| TypeError::new(format!("unbound type parameter `{name}`"), DUMMY_SPAN)),
            Ty::Builtin(_) => Ok(ty.clone()),
            Ty::Ptr(inner) => Ok(Ty::Ptr(Box::new(self.mono_ty(module, inner, subs)?))),
            Ty::Ref(inner) => Ok(Ty::Ref(Box::new(self.mono_ty(module, inner, subs)?))),
            Ty::Path(name, args) => {
                if name == "sys.result.Result" {
                    let args = args
                        .iter()
                        .map(|arg| self.mono_ty(module, arg, subs))
                        .collect::<Result<Vec<_>, _>>()?;
                    return Ok(Ty::Path(name.clone(), args));
                }
                let (type_module, base_name, qualified) = split_name(module, name);
                let args = args
                    .iter()
                    .map(|arg| self.mono_ty(module, arg, subs))
                    .collect::<Result<Vec<_>, _>>()?;
                let qualified_key = qualify(&type_module, &base_name);
                if let Some(struct_def) = self.structs.get(&qualified_key).cloned() {
                    if !struct_def.type_params.is_empty() {
                        let new_name = self.ensure_struct_instance(&type_module, &struct_def, &args)?;
                        let name = if qualified {
                            qualify(&type_module, &new_name)
                        } else {
                            new_name
                        };
                        return Ok(Ty::Path(name, Vec::new()));
                    }
                }
                if let Some(enum_def) = self.enums.get(&qualified_key).cloned() {
                    if !enum_def.type_params.is_empty() {
                        let new_name = self.ensure_enum_instance(&type_module, &enum_def, &args)?;
                        let name = if qualified {
                            qualify(&type_module, &new_name)
                        } else {
                            new_name
                        };
                        return Ok(Ty::Path(name, Vec::new()));
                    }
                }
                Ok(Ty::Path(name.clone(), args))
            }
        }
    }

    fn ensure_struct_instance(
        &mut self,
        module: &str,
        decl: &HirStruct,
        args: &[Ty],
    ) -> Result<String, TypeError> {
        if decl.type_params.is_empty() {
            return Ok(decl.name.clone());
        }
        if decl.type_params.len() != args.len() {
            return Err(TypeError::new(
                format!(
                    "type `{}` expects {} type argument(s), found {}",
                    decl.name,
                    decl.type_params.len(),
                    args.len()
                ),
                DUMMY_SPAN,
            ));
        }
        let name = mangle_name(&decl.name, args);
        let qualified = qualify(module, &name);
        if self.generated_structs.contains(&qualified) {
            return Ok(name);
        }
        let subs = build_substitution(&decl.type_params, args, DUMMY_SPAN)?;
        let mono = self.mono_struct(module, decl, &subs, name.clone())?;
        self.push_struct(module, mono);
        Ok(name)
    }

    fn ensure_enum_instance(
        &mut self,
        module: &str,
        decl: &HirEnum,
        args: &[Ty],
    ) -> Result<String, TypeError> {
        if decl.type_params.is_empty() {
            return Ok(decl.name.clone());
        }
        if decl.type_params.len() != args.len() {
            return Err(TypeError::new(
                format!(
                    "type `{}` expects {} type argument(s), found {}",
                    decl.name,
                    decl.type_params.len(),
                    args.len()
                ),
                DUMMY_SPAN,
            ));
        }
        let name = mangle_name(&decl.name, args);
        let qualified = qualify(module, &name);
        if self.generated_enums.contains(&qualified) {
            return Ok(name);
        }
        let subs = build_substitution(&decl.type_params, args, DUMMY_SPAN)?;
        let mono = self.mono_enum(module, decl, &subs, name.clone())?;
        self.push_enum(module, mono);
        Ok(name)
    }

    fn abi_type_for(&self, module: &str, ty: &Ty) -> Result<AbiType, TypeError> {
        use crate::typeck::BuiltinType;
        match ty {
            Ty::Builtin(b) => match b {
                BuiltinType::I32 => Ok(AbiType::I32),
                BuiltinType::I64 => Err(TypeError::new(
                    "i64 is not supported by the current codegen backend".to_string(),
                    DUMMY_SPAN,
                )),
                BuiltinType::U32 => Ok(AbiType::U32),
                BuiltinType::U8 => Ok(AbiType::U8),
                BuiltinType::Bool => Ok(AbiType::Bool),
                BuiltinType::Unit => Ok(AbiType::Unit),
                BuiltinType::Never => Ok(AbiType::Unit),
            },
            Ty::Ptr(_) => Ok(AbiType::Ptr),
            Ty::Ref(inner) => self.abi_type_for(module, inner),
            Ty::Param(_) => Err(TypeError::new(
                "generic type parameters must be monomorphized before codegen".to_string(),
                DUMMY_SPAN,
            )),
            Ty::Path(name, args) => {
                if name == "sys.result.Result" && args.len() == 2 {
                    let ok = self.abi_type_for(module, &args[0])?;
                    let err = self.abi_type_for(module, &args[1])?;
                    return Ok(AbiType::Result(Box::new(ok), Box::new(err)));
                }
                let (type_module, base_name, _qualified) = split_name(module, name);
                let qualified = qualify(&type_module, &base_name);
                if let Some(info) = self.structs.get(&qualified) {
                    return Ok(if info.is_opaque {
                        AbiType::Handle
                    } else {
                        AbiType::Ptr
                    });
                }
                if let Some(info) = self.enums.get(&qualified) {
                    let has_payload = info.variants.iter().any(|variant| variant.payload.is_some());
                    if has_payload {
                        return Ok(AbiType::Ptr);
                    }
                    return Ok(AbiType::I32);
                }
                Err(TypeError::new(
                    format!("unknown type `{}`", name),
                    DUMMY_SPAN,
                ))
            }
        }
    }
}

impl From<ModuleOut> for HirModule {
    fn from(module: ModuleOut) -> Self {
        Self {
            name: module.name,
            functions: module.functions,
            extern_functions: module.extern_functions,
            structs: module.structs,
            enums: module.enums,
        }
    }
}

trait GenericSig {
    fn name(&self) -> &str;
    fn type_params(&self) -> &Vec<String>;
    fn params(&self) -> &Vec<HirParam>;
}

impl GenericSig for HirFunction {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_params(&self) -> &Vec<String> {
        &self.type_params
    }

    fn params(&self) -> &Vec<HirParam> {
        &self.params
    }
}

impl GenericSig for HirExternFunction {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_params(&self) -> &Vec<String> {
        &self.type_params
    }

    fn params(&self) -> &Vec<HirParam> {
        &self.params
    }
}

fn split_name(module: &str, name: &str) -> (String, String, bool) {
    if let Some((mod_part, type_part)) = name.rsplit_once('.') {
        (mod_part.to_string(), type_part.to_string(), true)
    } else {
        (module.to_string(), name.to_string(), false)
    }
}

fn qualify(module: &str, name: &str) -> String {
    format!("{module}.{name}")
}

fn function_symbol(module: &str, name: &str) -> String {
    format!("capable_{}", qualify(module, name).replace('.', "_"))
}

fn build_substitution(
    params: &[String],
    args: &[Ty],
    span: Span,
) -> Result<HashMap<String, Ty>, TypeError> {
    if params.len() != args.len() {
        return Err(TypeError::new(
            format!(
                "expected {} type argument(s), found {}",
                params.len(),
                args.len()
            ),
            span,
        ));
    }
    let mut map = HashMap::new();
    for (param, arg) in params.iter().zip(args.iter()) {
        map.insert(param.clone(), arg.clone());
    }
    Ok(map)
}

fn substitute_ty(ty: &Ty, subs: &HashMap<String, Ty>) -> Ty {
    match ty {
        Ty::Param(name) => subs.get(name).cloned().unwrap_or_else(|| ty.clone()),
        Ty::Builtin(_) => ty.clone(),
        Ty::Ptr(inner) => Ty::Ptr(Box::new(substitute_ty(inner, subs))),
        Ty::Ref(inner) => Ty::Ref(Box::new(substitute_ty(inner, subs))),
        Ty::Path(name, args) => Ty::Path(
            name.clone(),
            args.iter().map(|arg| substitute_ty(arg, subs)).collect(),
        ),
    }
}

fn match_type_params(
    expected: &Ty,
    actual: &Ty,
    subs: &mut HashMap<String, Ty>,
    span: Span,
) -> Result<(), TypeError> {
    match expected {
        Ty::Param(name) => {
            if let Some(existing) = subs.get(name) {
                if existing != actual {
                    return Err(TypeError::new(
                        format!(
                            "conflicting type arguments for `{}`: {existing:?} vs {actual:?}",
                            name
                        ),
                        span,
                    ));
                }
            } else {
                subs.insert(name.clone(), actual.clone());
            }
            Ok(())
        }
        Ty::Builtin(_) => {
            if expected != actual {
                return Err(TypeError::new(
                    format!("type mismatch: expected {expected:?}, found {actual:?}"),
                    span,
                ));
            }
            Ok(())
        }
        Ty::Ptr(inner) => match actual {
            Ty::Ptr(actual_inner) => match_type_params(inner, actual_inner, subs, span),
            _ => Err(TypeError::new(
                format!("type mismatch: expected {expected:?}, found {actual:?}"),
                span,
            )),
        },
        Ty::Ref(inner) => match actual {
            Ty::Ref(actual_inner) => match_type_params(inner, actual_inner, subs, span),
            _ => Err(TypeError::new(
                format!("type mismatch: expected {expected:?}, found {actual:?}"),
                span,
            )),
        },
        Ty::Path(name, args) => match actual {
            Ty::Path(actual_name, actual_args) => {
                if name != actual_name || args.len() != actual_args.len() {
                    return Err(TypeError::new(
                        format!("type mismatch: expected {expected:?}, found {actual:?}"),
                        span,
                    ));
                }
                for (arg, actual_arg) in args.iter().zip(actual_args.iter()) {
                    match_type_params(arg, actual_arg, subs, span)?;
                }
                Ok(())
            }
            _ => Err(TypeError::new(
                format!("type mismatch: expected {expected:?}, found {actual:?}"),
                span,
            )),
        },
    }
}

fn mangle_name(base: &str, args: &[Ty]) -> String {
    if args.is_empty() {
        return base.to_string();
    }
    let suffix = args
        .iter()
        .map(mangle_type)
        .collect::<Vec<_>>()
        .join("__");
    format!("{base}__{suffix}")
}

fn mangle_type(ty: &Ty) -> String {
    match ty {
        Ty::Builtin(b) => match b {
            crate::typeck::BuiltinType::I32 => "i32".to_string(),
            crate::typeck::BuiltinType::I64 => "i64".to_string(),
            crate::typeck::BuiltinType::U32 => "u32".to_string(),
            crate::typeck::BuiltinType::U8 => "u8".to_string(),
            crate::typeck::BuiltinType::Bool => "bool".to_string(),
            crate::typeck::BuiltinType::Unit => "unit".to_string(),
            crate::typeck::BuiltinType::Never => "never".to_string(),
        },
        Ty::Ptr(inner) => format!("ptr_{}", mangle_type(inner)),
        Ty::Ref(inner) => format!("ref_{}", mangle_type(inner)),
        Ty::Param(name) => format!("param_{name}"),
        Ty::Path(name, args) => {
            if name == "sys.string.string" || name == "string" {
                return "string".to_string();
            }
            let mut base = name.replace('.', "_");
            if !args.is_empty() {
                let suffix = args
                    .iter()
                    .map(mangle_type)
                    .collect::<Vec<_>>()
                    .join("__");
                base = format!("{base}__{suffix}");
            }
            base
        }
    }
}
