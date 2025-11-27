Aether Language

Variables and assignment
- Declare with explicit type:
  let x: i64 = 1;
  let y: i32 = 5;
  let z: f64 = 3.14;
  let s: String = "Hello";
- Reassign (type must match):
  x = x + 1;
  s = "Bye";
- Assign to struct fields:
  pub struct Inner { y: i32 }
  pub struct Point { x: i32, inner: Inner, name: String }
  let p: Point = Point { x: 1, inner: Inner { y: 2 }, name: "P0" };
  p.x = 5;
  p.inner.y = 9;
  p.name = "P1";
- All statements end with a semicolon.
- Functions return with:
  return expr;


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
- Floats: f32, f64
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

## Standard Library Functions

The following built-in functions are available on x86_64 Linux:

Math functions:
- abs_i64(x: i64) -> i64: absolute value of 64-bit integer
- abs_i32(x: i32) -> i32: absolute value of 32-bit integer
- abs_f64(x: f64) -> f64: absolute value of 64-bit float
- abs_f32(x: f32) -> f32: absolute value of 32-bit float
- min_i64(a: i64, b: i64) -> i64: minimum of two 64-bit integers
- min_i32(a: i32, b: i32) -> i32: minimum of two 32-bit integers
- min_f64(a: f64, b: f64) -> f64: minimum of two 64-bit floats
- min_f32(a: f32, b: f32) -> f32: minimum of two 32-bit floats
- max_i64(a: i64, b: i64) -> i64: maximum of two 64-bit integers
- max_i32(a: i32, b: i32) -> i32: maximum of two 32-bit integers
- max_f64(a: f64, b: f64) -> f64: maximum of two 64-bit floats
- max_f32(a: f32, b: f32) -> f32: maximum of two 32-bit floats
- sqrt_f64(x: f64) -> f64: square root of 64-bit float
- sqrt_f32(x: f32) -> f32: square root of 32-bit float

String functions:
- str_len(s: String) -> i64: length of string in bytes

## HList (Heterogeneous List)

HList is a dynamic collection that can store values of different types. Each element is tagged with its type.

Tag values:
- 0: i64
- 1: f64
- 2: String (pointer)
- 3: i32
- 4: f32

Operations:
- hlist_new(cap: i64) -> HList: allocate a new HList with initial capacity
- hlist_push(h: HList, tag: i64, value: i64): push a tagged value onto the list
- hlist_len(h: HList) -> i64: get the number of elements in the list
- hlist_free(h: HList) -> i64: free the list memory (returns 1 if freed, 0 if already null)

Example:
```
let h: HList = hlist_new(4);
hlist_push(h, 0, 42);      // push i64 value 42
hlist_push(h, 3, 100);     // push i32 value 100
println(hlist_len(h));     // prints 2
hlist_free(h);
```

Memory layout:
- Header: 24 bytes (ptr at offset 0, len at offset -8, cap at offset -16)
- Each element: 16 bytes (8 byte tag + 8 byte value)
- Memory is allocated via mmap and freed via munmap

## Notes

- No GC; heap APIs not yet exposed
- Imports supported via import "path/to/file.ae";
- Error handling not yet implemented
- Subject to change pre‑1.0
