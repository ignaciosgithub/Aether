Threading

Builtins recognized by the backends (no surface syntax changes):
- spawn("func_name", arg: i64) -> i64 handle
- join(handle: i64) -> i32 (worker exit/status)
- destroy(handle: i64) -> i32 (1 on success, 0 on failure)

Worker functions have signature:
pub func name(arg: i64) -> i32

Per-target notes:
- x86_64 Windows: CreateThread, WaitForSingleObject, GetExitCodeThread, CloseHandle, TerminateThread
- x86_64 Linux: raw clone(SIGCHLD) with 64KiB stacks in .bss, wait4, kill(SIGKILL)
- AArch64 Linux: same behavior via svc syscalls

Examples:
let h: i64 = spawn("worker", 101);
let r: i32 = join(h);
let ok: i32 = destroy(h);

# Aether Language (current subset)

This document describes the features currently supported by the compiler. The language is evolving; syntax and semantics may change.

- Source files: .ae
- Entry point: pub func main() -> i32

## Types

- Integers: i32, i64
- Floats: f64
- String: UTF‑8 string literal type (lowered as (ptr,len) in backends)
- Structs: user‑defined types with named fields; single inheritance

## Functions

- Definition:
  pub func name(p1: i64, p2: String) -> i64 { ... }
- Calls: zero or multiple args (per calling convention for each backend)
- Return: single value; main returns i32

## Control flow

- if/else with runtime conditions:
  let x: i64 = if (a < b) { 1 } else { 2 };
- while with break/continue:
  while (cond) { ... }

Parentheses around if conditions are allowed.

## Println

- println(String expr)
  Works across x86_64 Linux/Windows and AArch64 Linux.
  Strings are lowered to (ptr,len); println writes the buffer then a newline.

- println(i64) [Linux x86_64]
  For benchmarks and checksum verification. Converts i64 to decimal and writes via write syscall.

## Structs and inheritance

- Struct definition:
  pub struct Parent { s: String }
  pub struct Child : Parent { n: i32 }

- Statics:
  static C: Child = Child { s: "Hi", n: 7 };

- Field access:
  println(C.s);

Layout:
- Parent‑first, then child fields
- String fields are two 64‑bit words: pointer then length
- Field offsets are computed with 8‑byte alignment

## Examples

See examples/*.ae:
- println.ae
- calls.ae
- factorial.ae
- runtime_ifelse.ae
- locals_mutation.ae
- nested_structs.ae
- inheritance.ae

## Targets and calling conventions

- x86_64 Linux: System V AMD64
- x86_64 Windows: Microsoft x64
- AArch64 Linux: AAPCS64

- Integers: integer registers per ABI
- Floats: f64 return in xmm0 (x86_64) / v0 (AArch64)
- Strings: passed/returned as (ptr,len) pairs

## Notes

- No GC; heap APIs not yet exposed
- No modules/imports yet
- Error handling not yet implemented
- Subject to change pre‑1.0
