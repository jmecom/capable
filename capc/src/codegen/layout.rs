//! Layout and indexing helpers for codegen.
//!
//! These routines compute struct layouts, enum discriminants, and ABI sizes.

use std::collections::{HashMap, HashSet};

use cranelift_codegen::ir::Type;

use super::{
    abi_quirks, CodegenError, EnumIndex, StructFieldLayout, StructLayout, StructLayoutIndex,
    TypeLayout,
};
use crate::abi::AbiType;

/// Build an enum discriminant table for codegen.
pub(super) fn build_enum_index(
    entry: &crate::hir::HirModule,
    user_modules: &[crate::hir::HirModule],
    stdlib: &[crate::hir::HirModule],
) -> EnumIndex {
    let mut variants = HashMap::new();
    let entry_name = &entry.name;
    let modules = stdlib
        .iter()
        .chain(user_modules.iter())
        .chain(std::iter::once(entry));
    for module in modules {
        let module_name = &module.name;
        for en in &module.enums {
            let mut map = HashMap::new();
            for (idx, variant) in en.variants.iter().enumerate() {
                map.insert(variant.name.clone(), idx as i32);
            }
            let qualified = format!("{}.{}", module_name, en.name);
            variants.insert(qualified, map.clone());
            if module_name == entry_name {
                variants.insert(en.name.clone(), map);
            }
        }
    }
    EnumIndex { variants }
}

/// Compute struct layouts for all non-opaque structs.
pub(super) fn build_struct_layout_index(
    entry: &crate::hir::HirModule,
    user_modules: &[crate::hir::HirModule],
    stdlib: &[crate::hir::HirModule],
    ptr_ty: Type,
) -> Result<StructLayoutIndex, CodegenError> {
    let modules = stdlib
        .iter()
        .chain(user_modules.iter())
        .chain(std::iter::once(entry))
        .collect::<Vec<_>>();

    let mut defs: HashMap<String, (&str, &crate::hir::HirStruct)> = HashMap::new();
    let mut name_counts: HashMap<String, usize> = HashMap::new();
    for module in &modules {
        for st in &module.structs {
            if st.is_opaque {
                continue;
            }
            let qualified = format!("{}.{}", module.name, st.name);
            defs.insert(qualified, (&module.name, st));
            *name_counts.entry(st.name.clone()).or_insert(0) += 1;
        }
    }

    let mut layouts = HashMap::new();
    let mut visiting = HashSet::new();
    for (qualified, (module_name, st)) in &defs {
        compute_struct_layout(
            qualified,
            module_name,
            st,
            &defs,
            &mut layouts,
            &mut visiting,
            ptr_ty,
        )?;
    }

    // Also register unqualified names when unambiguous.
    for module in &modules {
        for st in &module.structs {
            if st.is_opaque {
                continue;
            }
            let qualified = format!("{}.{}", module.name, st.name);
            if let Some(layout) = layouts.get(&qualified).cloned() {
                if name_counts.get(&st.name).copied().unwrap_or(0) == 1 {
                    layouts.entry(st.name.clone()).or_insert(layout);
                }
            }
        }
    }

    Ok(StructLayoutIndex { layouts })
}

/// Compute and memoize a single struct's layout (size/align/field offsets).
fn compute_struct_layout(
    qualified: &str,
    module_name: &str,
    st: &crate::hir::HirStruct,
    defs: &HashMap<String, (&str, &crate::hir::HirStruct)>,
    layouts: &mut HashMap<String, StructLayout>,
    visiting: &mut HashSet<String>,
    ptr_ty: Type,
) -> Result<(), CodegenError> {
    if layouts.contains_key(qualified) {
        return Ok(());
    }
    if !visiting.insert(qualified.to_string()) {
        return Err(CodegenError::Unsupported(format!(
            "recursive struct layout for {qualified}"
        )));
    }

    let mut fields = HashMap::new();
    let mut field_order = Vec::new();
    let mut offset = 0u32;
    let mut align = 1u32;
    for field in &st.fields {
        let layout = type_layout_for_hir_type(
            &field.ty,
            module_name,
            defs,
            layouts,
            visiting,
            ptr_ty,
        )?;
        offset = align_to(offset, layout.align);
        fields.insert(
            field.name.clone(),
            StructFieldLayout {
                offset,
                ty: field.ty.clone(),
            },
        );
        field_order.push(field.name.clone());
        offset = offset.saturating_add(layout.size);
        align = align.max(layout.align);
    }
    let size = align_to(offset, align);

    layouts.insert(
        qualified.to_string(),
        StructLayout {
            size,
            align,
            fields,
            field_order,
        },
    );
    visiting.remove(qualified);
    Ok(())
}

/// Compute a layout for a typeck::Ty.
fn type_layout_for_hir_type(
    ty: &crate::hir::HirType,
    module_name: &str,
    defs: &HashMap<String, (&str, &crate::hir::HirStruct)>,
    layouts: &mut HashMap<String, StructLayout>,
    visiting: &mut HashSet<String>,
    ptr_ty: Type,
) -> Result<TypeLayout, CodegenError> {
    use crate::typeck::Ty;

    if let Some(layout) = resolve_struct_layout(&ty.ty, module_name, layouts) {
        return Ok(TypeLayout {
            size: layout.size,
            align: layout.align,
        });
    }

    match &ty.ty {
        Ty::Path(name, args) if name == "Result" && args.len() == 2 => {
            let AbiType::Result(ok_abi, err_abi) = &ty.abi else {
                return Err(CodegenError::Unsupported(format!(
                    "{} in layout",
                    abi_quirks::result_abi_mismatch_error()
                )));
            };
            let tag = TypeLayout { size: 1, align: 1 };
            let ok = type_layout_for_abi(ok_abi, ptr_ty)?;
            let err = type_layout_for_abi(err_abi, ptr_ty)?;
            let mut offset = 0u32;
            let mut align = tag.align;
            offset = align_to(offset, tag.align);
            offset = offset.saturating_add(tag.size);
            offset = align_to(offset, ok.align);
            offset = offset.saturating_add(ok.size);
            offset = align_to(offset, err.align);
            offset = offset.saturating_add(err.size);
            align = align.max(ok.align).max(err.align);
            let size = align_to(offset, align);
            Ok(TypeLayout { size, align })
        }
        Ty::Path(name, _) => {
            if let Some((struct_module, struct_def)) = resolve_struct_def(name, module_name, defs)
            {
                compute_struct_layout(
                    &format!("{}.{}", struct_module, struct_def.name),
                    struct_module,
                    struct_def,
                    defs,
                    layouts,
                    visiting,
                    ptr_ty,
                )?;
                if let Some(layout) =
                    resolve_struct_layout(&Ty::Path(name.clone(), Vec::new()), module_name, layouts)
                {
                    return Ok(TypeLayout {
                        size: layout.size,
                        align: layout.align,
                    });
                }
            }
            type_layout_for_abi(&ty.abi, ptr_ty)
        }
        _ => {
            type_layout_for_abi(&ty.abi, ptr_ty)
        }
    }
}

/// Compute a layout for an ABI type.
pub(super) fn type_layout_for_abi(
    ty: &AbiType,
    ptr_ty: Type,
) -> Result<TypeLayout, CodegenError> {
    match ty {
        AbiType::Unit => Ok(TypeLayout { size: 0, align: 1 }),
        AbiType::I32 | AbiType::U32 => Ok(TypeLayout { size: 4, align: 4 }),
        AbiType::U8 | AbiType::Bool => Ok(TypeLayout { size: 1, align: 1 }),
        AbiType::Handle => Ok(TypeLayout { size: 8, align: 8 }),
        AbiType::Ptr => Ok(TypeLayout {
            size: ptr_ty.bytes() as u32,
            align: ptr_ty.bytes() as u32,
        }),
        AbiType::String => {
            let ptr_size = ptr_ty.bytes() as u32;
            let len_offset = align_to(ptr_size, 8);
            Ok(TypeLayout {
                size: len_offset + 8,
                align: ptr_ty.bytes().max(8) as u32,
            })
        }
        AbiType::Result(ok, err) => {
            let tag = TypeLayout { size: 1, align: 1 };
            let ok = type_layout_for_abi(ok, ptr_ty)?;
            let err = type_layout_for_abi(err, ptr_ty)?;
            let mut offset = 0u32;
            let mut align = tag.align;
            offset = align_to(offset, tag.align);
            offset = offset.saturating_add(tag.size);
            offset = align_to(offset, ok.align);
            offset = offset.saturating_add(ok.size);
            offset = align_to(offset, err.align);
            offset = offset.saturating_add(err.size);
            align = align.max(ok.align).max(err.align);
            let size = align_to(offset, align);
            Ok(TypeLayout { size, align })
        }
        AbiType::ResultOut(_, _) | AbiType::ResultString => Err(CodegenError::Unsupported(
            abi_quirks::result_lowering_layout_error().to_string(),
        )),
    }
}

/// Resolve a struct layout by name (qualified or unqualified).
pub(super) fn resolve_struct_layout<'a>(
    ty: &crate::typeck::Ty,
    module_name: &str,
    layouts: &'a HashMap<String, StructLayout>,
) -> Option<&'a StructLayout> {
    match ty {
        crate::typeck::Ty::Path(name, _) => {
            if name.contains('.') {
                layouts.get(name)
            } else {
                let qualified = format!("{module_name}.{name}");
                layouts.get(&qualified).or_else(|| layouts.get(name))
            }
        }
        _ => None,
    }
}

/// Resolve a struct definition by name (qualified or unqualified).
fn resolve_struct_def<'a>(
    name: &str,
    module_name: &str,
    defs: &'a HashMap<String, (&'a str, &'a crate::hir::HirStruct)>,
) -> Option<(&'a str, &'a crate::hir::HirStruct)> {
    if name.contains('.') {
        defs.get(name).copied()
    } else {
        let qualified = format!("{module_name}.{name}");
        defs.get(&qualified)
            .copied()
            .or_else(|| defs.get(name).copied())
    }
}

/// Align a byte offset up to the next alignment boundary.
pub(super) fn align_to(value: u32, align: u32) -> u32 {
    if align == 0 {
        return value;
    }
    let rem = value % align;
    if rem == 0 {
        value
    } else {
        value + (align - rem)
    }
}
