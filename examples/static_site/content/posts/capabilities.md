---
title: Capability-Scoped Generation
summary: The generator's authority is limited by the capabilities it receives up front.
date: 2026-03-24
---

# Capability-Scoped Generation

The generator can only read from the delegated content root.

The generator can only write inside the delegated output root.

## Why That Matters

- content reads are rooted
- output writes are rooted
- the program cannot escape either subtree in safe code

> This is the part that makes the example more interesting than a plain scripting demo.
