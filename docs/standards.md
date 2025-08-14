Aether Language Standards (Draft)

Principles
- Performance: predictable, near-C/Zig performance with explicit control and low-level access.
- Compatibility: portable outputs and toolchains; stable calling conventions and FFI model.
- Readability: clear syntax, explicit constructs (no hidden control flow or implicit allocations).

Core Language
- Paradigm: object-oriented with value types by default; heap allocation explicit.
- Types (initial): void, bool, i32, i64, f32, f64, Any, List[T]. List[Any] supports heterogeneous lists.
- Functions: pub/private, return types, parameters with explicit types.
- Memory: arenas (scoped), manual allocation APIs for long-lived objects; no GC.
- Threading: threaded functions, basic primitives in std.concurrent; OS abstraction for threads.
- Error handling: result-like types (Result[T, E]) initially; later: error sets and defer.
- Control flow: if, while; for and match later.
- Modules/imports: hierarchical modules, std.*; import and importc for C headers.
- FFI: C ABI stable; importc "header.h" to generate bindings; extern "C" calling conventions.
- Comptime: staged compile-time execution for constants, generics metadata, limited codegen metaprogramming (future).
- Attributes: annotations for inlining, noalias, packed, repr(C), calling convention.

Numeric Types
- Integers: i32, i64 (unsigned variants to come).
- Floats: f32, f64 with IEEE-754 semantics; backend codegen maps to native FP regs/instructions.

Collections
- Lists: List[T] with contiguous storage. List[Any] enables heterogeneous lists via tagged values.
- Value model: values carry a small tag (Int, Float32, Float64, Bool, List, etc.) when in an Any context.

Standard Library (initial sketch)
- std.io: basic stdout/stderr printing, file IO.
- std.mem: arenas, allocators, memcpy/memset.
- std.concurrent: threads, mutex (wrappers around OS primitives).
- std.os: OS-specific syscalls/handles abstracted behind stable API.
- std.fmt: formatting utilities.

Targeting and Portability
- Targets: x86_64-linux-gnu, x86_64-windows-gnu, aarch64-linux-gnu; default to x86_64-windows-gnu when unknown.
- Calling conventions:
  - x86_64 Linux: System V AMD64
  - x86_64 Windows: Microsoft x64 (shadow space)
  - AArch64 Linux: AAPCS64
- ABI stability: stable C ABI interop; Aether ABI subject to change pre-1.0.

Compiler Architecture
- Frontend: Lexer -> Parser -> AST.
- IR (planned): Aether IR (high-level, SSA-capable) for analysis and optimizations.
- Backends: Implement CodeGenerator for each Target; emit GAS/NASM compatible assembly or object files.
- Assembler/Linking: Use platform toolchains (GAS+ld/clang/lld for Linux; MinGW for Windows; cross-binutils for AArch64).

Roadmap
- Milestone 1: Bootstrap compiler pipeline, minimal AST (with floats and List[Any]), codegen stubs for exit-only programs; CLI, build scripts, docs.
- Milestone 2: Expressions (ints/floats, + - * /), function calls, return; minimal std.io::writeLine via syscall/printf.
- Milestone 3: Variables, control flow (if/while), arrays/lists, simple arena API with List allocation.
- Milestone 4: FFI importc subset; headers -> bindings; call c.printf on all targets.
- Milestone 5: Threads: threaded functions, basic OS thread wrapper.
- Milestone 6: IR, basic optimizations (const fold, dead code), improved register allocation including FP regs.
- Milestone 7: Device interface modules: memory-mapped IO, ports, safe wrappers.

Security and Safety
- No hidden allocations; unsafe blocks for direct memory/ports.
- Bounds checks configurable (compile-time flags).
- No runtime reflection; comptime only.

Versioning and Stability
- Pre-1.0: breaking changes allowed behind feature flags.
- Semantic versioning post-1.0; standard library stability with deprecation policy.
