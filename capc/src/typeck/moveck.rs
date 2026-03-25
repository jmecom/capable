use std::collections::HashMap;

use crate::ast::*;
use crate::error::TypeError;

use super::{type_kind, EnumInfo, MoveState, Scopes, StructInfo, TypeKind};

/// Check if a statement is syntactically total (always returns).
/// This is a purely syntactic check, not real control-flow analysis.
pub(super) fn stmt_is_total(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Return(ret_stmt) => ret_stmt.expr.is_some(),
        Stmt::Defer(_) => false,
        Stmt::Expr(expr_stmt) => {
            if let Expr::Match(match_expr) = &expr_stmt.expr {
                match_is_total(match_expr)
            } else {
                false
            }
        }
        Stmt::If(if_stmt) => {
            if let Some(else_block) = &if_stmt.else_block {
                block_ends_with_return(&if_stmt.then_block) && block_ends_with_return(else_block)
            } else {
                false
            }
        }
        _ => false,
    }
}

fn block_ends_with_return(block: &Block) -> bool {
    block.stmts.last().is_some_and(stmt_is_total)
}

fn match_is_total(match_expr: &MatchExpr) -> bool {
    !match_expr.arms.is_empty()
        && match_expr
            .arms
            .iter()
            .all(|arm| block_ends_with_return(&arm.body))
}

pub(super) fn merge_branch_states(
    base: &mut Scopes,
    left: &Scopes,
    right: &Scopes,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    for (base_scope, (left_scope, right_scope)) in base
        .stack
        .iter_mut()
        .zip(left.stack.iter().zip(&right.stack))
    {
        for (name, info) in base_scope.iter_mut() {
            let left_info = left_scope
                .get(name)
                .ok_or_else(|| TypeError::new(format!("unknown identifier `{name}`"), span))?;
            let right_info = right_scope
                .get(name)
                .ok_or_else(|| TypeError::new(format!("unknown identifier `{name}`"), span))?;
            match type_kind(&info.ty, struct_map, enum_map) {
                TypeKind::Affine => {
                    info.state = if left_info.state == MoveState::Moved
                        || right_info.state == MoveState::Moved
                    {
                        MoveState::Moved
                    } else {
                        MoveState::Available
                    };
                }
                TypeKind::Linear => {
                    if left_info.state != right_info.state {
                        return Err(TypeError::new(
                            format!("linear value `{name}` must be consumed on all paths"),
                            span,
                        ));
                    }
                    info.state = left_info.state;
                }
                TypeKind::Unrestricted => {}
            }
        }
    }
    Ok(())
}

pub(super) fn ensure_affine_states_match(
    base: &Scopes,
    other: &Scopes,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    for (base_scope, other_scope) in base.stack.iter().zip(&other.stack) {
        for (name, info) in base_scope {
            let other_info = other_scope
                .get(name)
                .ok_or_else(|| TypeError::new(format!("unknown identifier `{name}`"), span))?;
            if type_kind(&info.ty, struct_map, enum_map) != TypeKind::Unrestricted
                && info.state != other_info.state
            {
                return Err(TypeError::new(
                    format!("move-only value `{name}` moved inside loop"),
                    span,
                ));
            }
        }
    }
    Ok(())
}

pub(super) fn ensure_linear_scope_consumed(
    scopes: &Scopes,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    if let Some(scope) = scopes.stack.last() {
        for (name, info) in scope {
            if type_kind(&info.ty, struct_map, enum_map) == TypeKind::Linear
                && info.state != MoveState::Moved
            {
                return Err(TypeError::new(
                    format!("linear value `{name}` not consumed"),
                    span,
                ));
            }
        }
    }
    Ok(())
}

pub(super) fn ensure_linear_scopes_consumed_from(
    scopes: &Scopes,
    depth: usize,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    for scope in scopes.stack.iter().skip(depth) {
        for (name, info) in scope {
            if type_kind(&info.ty, struct_map, enum_map) == TypeKind::Linear
                && info.state != MoveState::Moved
            {
                return Err(TypeError::new(
                    format!("linear value `{name}` not consumed"),
                    span,
                ));
            }
        }
    }
    Ok(())
}

pub(super) fn ensure_linear_all_consumed(
    scopes: &Scopes,
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    for scope in &scopes.stack {
        for (name, info) in scope {
            if type_kind(&info.ty, struct_map, enum_map) == TypeKind::Linear
                && info.state != MoveState::Moved
            {
                return Err(TypeError::new(
                    format!("linear value `{name}` not consumed"),
                    span,
                ));
            }
        }
    }
    Ok(())
}

pub(super) fn merge_match_states(
    base: &mut Scopes,
    arms: &[Scopes],
    struct_map: &HashMap<String, StructInfo>,
    enum_map: &HashMap<String, EnumInfo>,
    span: Span,
) -> Result<(), TypeError> {
    let Some((first, rest)) = arms.split_first() else {
        return Ok(());
    };
    for (depth, (base_scope, first_scope)) in base.stack.iter_mut().zip(&first.stack).enumerate() {
        for (name, info) in base_scope.iter_mut() {
            let first_info = first_scope
                .get(name)
                .ok_or_else(|| TypeError::new(format!("unknown identifier `{name}`"), span))?;
            match type_kind(&info.ty, struct_map, enum_map) {
                TypeKind::Affine => {
                    let mut moved = first_info.state == MoveState::Moved;
                    for arm in rest {
                        let arm_scope = arm.stack.get(depth).ok_or_else(|| {
                            TypeError::new(format!("unknown identifier `{name}`"), span)
                        })?;
                        let arm_info = arm_scope.get(name).ok_or_else(|| {
                            TypeError::new(format!("unknown identifier `{name}`"), span)
                        })?;
                        if arm_info.state == MoveState::Moved {
                            moved = true;
                        }
                    }
                    info.state = if moved {
                        MoveState::Moved
                    } else {
                        MoveState::Available
                    };
                }
                TypeKind::Linear => {
                    let state = first_info.state;
                    for arm in rest {
                        let arm_scope = arm.stack.get(depth).ok_or_else(|| {
                            TypeError::new(format!("unknown identifier `{name}`"), span)
                        })?;
                        let arm_info = arm_scope.get(name).ok_or_else(|| {
                            TypeError::new(format!("unknown identifier `{name}`"), span)
                        })?;
                        if arm_info.state != state {
                            return Err(TypeError::new(
                                format!("linear value `{name}` must be consumed on all paths"),
                                span,
                            ));
                        }
                    }
                    info.state = state;
                }
                TypeKind::Unrestricted => {}
            }
        }
    }
    Ok(())
}
