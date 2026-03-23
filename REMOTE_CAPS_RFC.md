# Remote Capability Delegation RFC

This document describes a future initiative: delegating attenuated capabilities
to remote workers or agents over the network.

It is intentionally separate from [UPDATE_PLAN.md](./UPDATE_PLAN.md). The local
model should be stabilized first. Remote delegation builds on that model; it
should not distort the scope of the local cleanup work.

## Goal

Allow a local process to delegate explicit, attenuated authority to a remote
agent while preserving Capable's core guarantee:

> safe code can only exercise authority it was explicitly given

## Non-Goals

Remote delegation is not:

- serializing local runtime handles
- exporting `RootCap`
- turning the network into ambient authority
- pretending remote calls are local calls
- claiming hostile multi-tenant isolation by default

## Why This Is Separate Work

The local cleanup plan is mostly:

- docs and tutorial cleanup
- stdlib API cleanup
- local capability algebra clarification

Remote delegation adds a distinct class of work:

- authenticated sessions
- lease tables
- typed proxy capabilities
- revocation and expiry
- protocol design
- audit logging

That is a real runtime/protocol project, not a minor extension of the local
cleanup.

## Recommended Architecture

The first design should be explicit and typed:

- a local broker owns real local capabilities
- the broker exports only explicitly delegated capabilities
- the remote side receives typed proxy capabilities, not raw local handles
- proxy method calls are RPCs back to the broker
- the broker revalidates lease and policy on every call

This should look like authority delegation, not distributed shared memory.

## Core Concepts

- Broker: trusted local runtime that owns local capabilities.
- Session: authenticated remote relationship with the broker.
- Lease: revocable exported authority bound to a session.
- Proxy capability: typed remote handle that forwards to a lease.

## Security Rules for v1

- `RootCap` is non-exportable.
- Export must be explicit and typed.
- Every lease should carry:
  - capability kind
  - policy payload
  - session binding
  - expiry and revocation state
- Every remote call should revalidate:
  - session identity
  - lease existence
  - lease cookie/generation
  - policy constraints
- Remote proxies should never be castable to local capability types.

## Scope for v1

Start with a very small set of exportable capabilities:

- remote read-only filesystem
- remote command execution with an explicit command/profile allowlist
- remote console/log sink
- optionally remote workspace file read/write as a separate capability family

Do not start with:

- generic capability serialization
- remote `RootCap`
- arbitrary user-defined capability export
- distributed GC
- async/futures machinery

## Why Typed Proxies First

Typed proxies keep four things explicit:

- authority
- latency
- fallibility
- auditability

That matters for agent workflows. If an LLM-driven worker can call a remote
filesystem or exec capability, those calls should remain visibly remote and
explicitly delegated.

## Trust-Boundary Warning

If multiple users or agents can steer the same session, they share the
delegated authority of that session.

The broker model is an authority-delegation mechanism. It is not, by itself, a
complete hostile multi-tenant sandbox.

## Suggested Future Phases

### Phase A: Stabilize the local model

Complete the work in [UPDATE_PLAN.md](./UPDATE_PLAN.md) first.

### Phase B: Design `sys::remote`

- define the safe surface API
- decide exported capability families
- decide proxy error model
- decide session lifecycle model

### Phase C: Build a broker MVP

- implement authenticated sessions
- implement lease tables
- implement typed proxy calls for a tiny capability set
- add revocation, expiry, and audit logging

### Phase D: Dogfood on agent workflows

Use the broker to support constrained remote workers:

- a code-writing agent with only a workspace capability
- a test-running agent with a restricted exec profile
- a docs/indexing agent with read-only filesystem access

The goal is to prove the authority model on real workflows before widening the
protocol.

## Acceptance Criteria

Remote delegation is only ready to ship if:

- local capability semantics are already crisp
- delegated authority is always narrower than local root authority
- revoked or expired leases fail closed
- remote proxies cannot be forged or widened
- policy and session checks happen on every call
- the resulting system is auditable enough to reason about agent authority
