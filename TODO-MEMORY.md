TODO-MEMORY

1) Make Alloc real in runtime
   - Runtime still uses libc::malloc directly for Vec/Slice/string buffers.
   - Alloc handles are now passed and stored in Vec headers, but not used for
     allocation/free. Add an allocator vtable/bridge or syscall hook so
     Alloc controls allocation, like Zig-style explicit allocators.

2) Fix Result<*T> typechecker bug
   - Generics with pointer payloads in Result still mismatch (Param vs Path).
   - Remove Vec workaround once typechecker + monomorphizer agree on pointer
     payload generics.

3) Remove remaining runtime Vec intrinsics
   - Done: runtime Vec tables removed; Vec/Slice operations are pure Cap.

4) Owned string and slice memory model
   - string is a view over Slice<u8>; Text is owned Vec<u8>.
   - Safe non-stdlib modules may not return or store Slice/MutSlice to reduce
     use-after-free hazards until a real lifetime model exists.

5) Unsafe pointer API completeness
   - Done: memcpy/memmove added to sys::unsafe_ptr and lowered in codegen.
   - Document expectations around ptr_is_null and ownership.

6) ABI + lowering docs update
   - Document: opaque structs lowered to handles, field access is special-cased
     in codegen, sret + ResultOut behavior, alloc-passing convention, and
     Slice/string layout.
