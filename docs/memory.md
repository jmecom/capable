# Memory Work Summary

This document summarizes the TODO-MEMORY work that closed out the memory model
milestones.

## Allocators are explicit end-to-end
- Runtime allocation paths take an explicit Alloc handle.
- The runtime currently uses libc malloc/free behind the Alloc handle.
- The ABI keeps allocator passing explicit for future custom allocators.

## Result generics for pointers are consistent
- Enum payload matching treats type params vs unqualified paths as equivalent.
- Result<*T> now typechecks without special-case workarounds.

## Vec/Slice runtime intrinsics removed
- Vec and Slice operations are implemented in pure Cap.
- The runtime no longer carries Vec tables or intrinsic helpers.

## Owned string and slice memory model
- string is a view over Slice<u8>.
- Text is an owned Vec<u8>.
- Safe non-stdlib modules may not return or store Slice/MutSlice to reduce
  use-after-free hazards until a real lifetime model exists.
- Tests cover slice returns and struct/enum field escapes in safe modules.

## Unsafe pointer API completeness
- memcpy and memmove are available in sys::unsafe_ptr and lowered in codegen.
- ptr_is_null documentation clarifies it only checks nullness, not ownership.

## ABI and lowering docs updated
- Opaque structs lowered to handles; field access is special-cased in codegen.
- sret and ResultOut behavior documented.
- Slice/string layout and alloc-passing convention documented.
