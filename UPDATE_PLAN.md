# Update Plan: Consolidate the Local Model

This document is the immediate roadmap for Capable.

It is intentionally limited to the local language/runtime model. Remote
capability delegation is important, but it is a separate initiative and is now
tracked in [REMOTE_CAPS_RFC.md](./REMOTE_CAPS_RFC.md).

Status:

- Phase 0 is complete: the public docs now lead with the local
  data/resource/capability model.
- Phase 1 is complete: reusable capability use-operations now borrow where the
  current checker/runtime model allows it.
- Phase 2 is complete for the current local model: the compiler behavior was
  audited, the docs were aligned to it, and reference locals remain supported
  in their existing narrow form.
- Phase 3 is complete for the current local algebra: the repo now documents
  which capabilities are reusable, which derivations consume, which child
  handles are linear, and which capabilities are intentionally copyable.

The point of this plan is not to redesign Capable from scratch. The point is
to align the docs, stdlib, and compiler around the model the codebase already
mostly implements.

## Goals

- Make the language easier to explain and teach.
- Keep the capability model intact.
- Reduce friction in ordinary code without weakening resource safety.
- Stop growing surface area until the core model is clearer.

## Non-Negotiable Invariants

- Safe code has no ambient authority.
- Capability values remain unforgeable in safe code.
- Privileged effects happen only through capability-bearing APIs.
- Runtime checks remain fail-closed.
- Unsafe code remains the only escape hatch for raw pointers, FFI, and direct
  OS access.

## What the Compiler Already Does

The review was right about the broad shape of the implementation: the compiler
is already closer to the desired model than the public docs suggest.

Today, Capable already has:

- plain data that is unrestricted by default
- kind-by-containment for structs and enums
- `opaque struct` and `capability struct` as the main move-tracked categories
- static-only traits and generics

Borrow-lite is also already much more limited than a Rust-style borrow system:

- references cannot be stored in structs/enums
- references cannot be returned
- reference locals are allowed, but only when initialized from another local
- reference locals are non-assignable once created

So the immediate work is mostly consolidation and cleanup, not a ground-up
rewrite.

## Where the Friction Still Comes From

### 1. The docs overstate the ownership story

The public docs still make Capable sound more like a general affine/linear
language than it really is in practice. That hides the simpler model:

- ordinary data is ordinary
- resources are move-tracked
- capabilities are authority-bearing resources

### 2. The stdlib conflates different capability operations

Several APIs still treat semantically different operations as if they should all
consume the receiver.

We should distinguish:

- Use operations: perform an effect with existing authority.
- Attenuation operations: derive a narrower capability.
- Child-handle operations: create a fresh child handle from an existing parent.

Examples:

- `ReadFS.read_to_string` is a use operation and should not normally consume the
  capability.
- `Dir.subdir` is attenuation and may reasonably consume the stronger path cap.
- `TcpListener.accept` is a child-handle operation and may reasonably borrow the
  listener while returning a fresh connection.

This cleanup is the highest-value change in the near term.

### 3. Borrow-lite is still visible in the surface language

Capable already avoids stored refs and returned refs, which is good.
The remaining question is not "do we add a borrow system?" The codebase already
has one. The real question is how much of it should remain part of the public
story.

The likely direction is:

- keep short-lived borrowed parameters/receivers
- de-emphasize explicit reference locals in docs and examples
- avoid making users think in lifetimes or aliasing proofs

### 4. Generic code is still more pessimistic than the public story

Generics and traits are intentionally static-only, which is good.
But generic code still tends to feel more move-sensitive than ordinary code.

Near-term conclusion:

- do not expand traits/generics further
- keep the current machinery where it already pays for itself
- revisit generic kind behavior only after the local capability model is clearer

## Immediate Phases

### Phase 0: Restate the model

- Update docs to describe Capable as a capability-secure language with a small
  resource model.
- Lead with the three categories:
  - plain data
  - resources
  - capabilities
- Stop describing the language as if general affine/linear reasoning were the
  main thing users should learn first.

### Phase 1: Clean up stdlib capability APIs

- Rework `sys::fs` and `sys::net` signatures around the three-way distinction:
  use, attenuation, child-handle.
- Make semantically reusable capabilities borrowable for ordinary use.
- Reserve `linear` for must-close child handles such as `FileRead` and
  `TcpConn`.
- Remove accidental one-shot behavior from reusable capabilities.

This phase should deliver the biggest usability gain for the least compiler
churn.

### Phase 2: Tighten the compiler around the public story

- Audit reference-local behavior and decide whether to keep it as-is or reduce
  it to receiver/parameter positions in the public language.
- Keep kind-by-containment.
- Keep move tracking focused on `opaque struct`, `capability struct`, and
  values that contain them.
- Avoid new trait/generic surface area while this cleanup is in flight.

This phase is about alignment, not reinvention.

### Phase 3: Stabilize the local capability algebra

Before any remote work, local capability behavior should be crisp:

- which capabilities are reusable
- which derivations consume
- which child handles are linear
- which capabilities are deliberately copyable

If these rules are muddy locally, they will be worse remotely.

## Out of Scope for This Plan

The following are intentionally excluded from this document:

- remote capability delegation
- broker/session/lease/proxy design
- protocol/authentication/revocation details
- async/distributed execution concerns
- expanding traits or richer generic abstraction machinery

Those topics belong in separate RFCs or later roadmaps.

## Acceptance Criteria

This plan is successful if:

- the tutorial explains Capable without leading with borrow/move rules
- ordinary examples read like simple systems code, not ownership puzzles
- capability flow remains explicit in signatures and call sites
- the local attenuation model is clearer after the stdlib cleanup
- the compiler, docs, and stdlib all tell the same story

## Tests We Should Add or Tighten

- reusable capabilities can perform multiple ordinary use operations
- attenuation operations still consume when they should
- child handles still require close/consumption on all paths
- structs/enums containing resources remain resource-like by containment
- reference locals, if kept, remain tightly restricted and non-assignable

## Bottom Line

Capable does not need a new local model. It needs a cleaner expression of the
local model it already has.

The immediate work is:

- restate the language honestly
- clean up the stdlib capability APIs
- align the compiler and docs around that smaller story
