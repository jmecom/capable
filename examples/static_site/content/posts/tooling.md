---
title: Pressure-Testing the Tooling
summary: Real examples are where stdlib and compiler rough edges become obvious.
date: 2026-03-26
---

# Pressure-Testing the Tooling

The first version of the generator exposed missing pieces immediately:

- directory classification
- recursive directory creation
- rooted string writes
- path extension replacement

## The Useful Rule

When a sample program wants a weird workaround, it is usually pointing at a stdlib deficiency.

```text
sample pain -> repeated workaround -> stdlib helper
```
