# Aether Compiler Codebase Documentation

This document provides a high-level overview of the Aether compiler repository, focusing on architecture, key crates, backend code generation patterns, ABI rules, centralized emitters for I/O and parsing, testing strategy, and developer workflows.

## Repository Layout

- crates/
  - aether-frontend: Lexer, parser, AST, and frontend logic.
  - aether-backend-x86_64: Code generation for x86_64 (Linux and Windows).
  - aether-backend-aarch64: Code generation for aarch64 (in progress).
  - aether-codegen: Shared traits and infrastructure for code generation.
  - aetherc: CLI driver that reads aether source and emits assembly.
- examples/: Example Aether programs.
- scripts/: Helper scripts for assembling and linking generated assembly using toolchains from README.
- build/: Output folder for generated assembly (not committed).
- README.md: Quickstart and build instructions.

## Frontend

- crates/aether-frontend/src/ast.rs: Core AST types: Module, Function, Stmt, Expr, Type, etc.
- crates/aether-frontend/src/parser.rs: Pratt-style parsing with support for literals, function calls, if/else, while, unary +/- and casts.

Key AST notes:
- String model in codegen: returning strings typically uses rax=ptr and rdx=len calling convention within our generated assembly (internal ABI per backend).
- to_int(String) is a builtin recognized in backend codegen.

## Backend: x86_64 Overview

Primary file:
- crates/aether-backend-x86_64/src/lib.rs

Targets supported:
- Linux (SysV ABI, syscall-based I/O)
- Windows (Win64 ABI, kernel32: GetStdHandle/ReadFile/WriteFile)

Design principles:
- Centralized emitters for repeated patterns (printing integers, reading stdin, to_int parsing) to avoid ad-hoc sequences and ensure ABI correctness.
- Label policy for Windows: no dot-prefixed labels in site labels to avoid MASM/NASM “unknown pseudo-op” errors. Section directives (.text/.data/.bss) are allowed.

### Centralized Emitters (x86_64)

Windows helpers (module-scoped in lib.rs):
- win_emit_print_i64(out)
  - Converts integer in rax to text into a stack buffer.
  - Zero fast-path; division loop otherwise.
  - Preserves rcx via r11 around WriteFile.
  - Uses r12 for stdout handle and appends newline (LSNL).
  - Allocates 32-byte shadow space; maintains 16-byte alignment.
- win_emit_readln_to_rsi_rdx(out, func_label_suffix) / win_emit_readln_to_rbx_rcx(out, func_label_suffix)
  - Lazy-initializes stdin handle into r13 via GetStdHandle(-10).
  - Allocates per-function input buffer and bytes_read.
  - Calls ReadFile with rcx=r13, rdx=buf, r8d=cap, r9=&bytes_read.
  - Trims LF/CRLF and returns rax=ptr and rdx=len (or rbx/rcx variant).
  - No dot-prefixed site labels; use suffixes with %= for uniqueness.
- win_emit_to_int_from_mem(out, ptr_expr, len_expr)
  - Strict parsing: optional +/- then only digits; errors print "to_int error" and ExitProcess(1).
  - On success, result in rax; then printable via win_emit_print_i64.

Linux helpers:
- linux_emit_print_i64(out)
  - Converts integer in rax to decimal; writes via write syscall; appends newline (LSNL in rodata).
  - Handles zero fast-path; no divide-by-zero.
- linux_emit_readln_into(out, func_label_suffix)
  - Declares a .bss buffer per function (e.g., LINUXINBUF_<fn>); read syscall into buffer; trims LF; sets rax=ptr and rdx=len.
- linux_emit_to_int_from_rsi_rdx(out)
  - Strict parsing of memory at rsi,len in rdx; result in rax; error prints and exits with code 1 (via syscall).

These helpers are now the single source of truth for:
- println of integer values (literals, variables, results).
- readln() statement/expr emission.
- to_int(String) builtin codegen.

### Windows ABI and Conventions

- Callee-saved registers:
  - r12 = stdout handle (GetStdHandle(-11) once).
  - r13 = stdin handle (GetStdHandle(-10) once).
- rcx must be preserved around WriteFile because it is the first argument register. We save rcx into r11 before setting arguments and restore rcx after the call.
- WriteFile argument order:
  - rcx = handle (r12)
  - rdx = buffer pointer
  - r8d = bytes to write (length)
  - r9 and [rsp+32] for lpOverlapped / lpNumberOfBytesWritten as needed.
- Shadow space: sub rsp, 40 before calls; add rsp, 40 after.
- Maintain 16-byte stack alignment at call sites.
- Label policy:
  - No dot-prefixed labels for site labels (use names like LSNL, LS0, LREAD_*, WTOI_* with %= for uniqueness).
  - Section directives like .text/.data/.bss are allowed.

### Linux ABI and Conventions

- Use syscall for I/O (read on stdin fd=0; write on stdout fd=1).
- String printing expects rax=ptr and rdx=len.
- readln():
  - Reads up to capacity (default 1024) into a static .bss buffer per function.
  - Trims trailing LF (and CR if present when porting patterns).
  - Returns rax=ptr and rdx=len.

### Strict to_int(String) Semantics

- Accepts: optional '+' or '-' followed by one or more decimal digits.
- Rejects: any non-digit or stray characters (e.g., "12&3").
- On error:
  - Windows: prints "to_int error" via WriteFile and calls ExitProcess(1).
  - Linux: prints "to_int error" and exits(1) via syscall.
- On success: leaves i64 result in rax. Printing then uses the platform print_i64 helper.

### Tests

Backend tests under crates/aether-backend-x86_64/tests/ include:
- windows_labels_clean.rs: Guards that Windows labels do not start with '.'.
- stdin_to_int_linux_clean.rs: Ensures no Rust injection in Linux assembly and read syscall presence.
- stdin_to_int_windows_no_call.rs: Confirms to_int is inlined and uses WriteFile.
- win_rcx_preserve.rs: Ensures rcx is saved to r11 and restored around WriteFile; checks shadow space.
- win_writefile_args.rs: Verifies WriteFile arg order and shadow space usage.
- while_* tests and recursion_* tests: Validate control flow on Windows and Linux respects loops/returns and ABI.

Frontend tests:
- unary_minus.rs: Parser correctness for unary +/- and precedence.

aarch64 backend has initial tests for float codegen patterns (WIP).

### Examples

- examples/stdin_echo.ae: Demonstrates readln() and println(String).
- examples/stdin_to_int.ae: Demonstrates readln(), to_int(String)->i64, and integer printing.
- examples/factorial.ae, examples/benchmark.ae, and others: Exercises control flow, recursion, arithmetic.

Generate assemblies:
- Linux:
  - cargo run -p aetherc -- examples/stdin_echo.ae --arch x86_64 --os linux -o build/stdin_echo_linux.s
  - cargo run -p aetherc -- examples/stdin_to_int.ae --arch x86_64 --os linux -o build/stdin_to_int_linux.s
- Windows:
  - cargo run -p aetherc -- examples/stdin_echo.ae --arch x86_64 --os windows -o build/stdin_echo_win.s
  - cargo run -p aetherc -- examples/stdin_to_int.ae --arch x86_64 --os windows -o build/stdin_to_int_win.s

Assemble and link (see scripts/assemble_link.sh and README for toolchain details):
- ./scripts/assemble_link.sh x86_64-linux build/stdin_to_int_linux.s build/stdin_to_int_linux
- ./scripts/assemble_link.sh x86_64-windows build/stdin_to_int_win.s build/stdin_to_int_win

### Developer Guidelines

- Always route readln(), to_int, and integer printing through centralized helpers in the backend; avoid reintroducing ad-hoc emission.
- Windows label policy: never start labels with '.' except section directives; use %= for uniqueness.
- Preserve Win64 ABI:
  - r12=stdout, r13=stdin, rcx preserved around WriteFile, 32-byte shadow space, 16-byte alignment.
- Keep strict to_int semantics: optional sign + digits only; error otherwise.
- When adding features (e.g., float printing), follow the same centralized pattern to avoid regressions.
- Tests:
  - Prefer adding guard tests that assert assembly patterns for ABI correctness and label policies.
  - Keep tests OS-gated so they run portably in CI.
- Code comments:
  - The codebase avoids inline comments in emitted assembly for cleanliness. Use this docs.md for architectural explanations.

### Common Pitfalls and How We Avoid Them

- Duplicate labels (e.g., LS0 defined twice): Centralized label allocation and reuse; emit string/newline data once.
- “unknown pseudo-op” on Windows: Caused by site labels starting with '.'; fixed by enforcing non-dot label naming.
- “bad expression” or “div-by-zero”: Avoid ad-hoc integer printing; zero fast-path and safe division loop in helpers.
- rcx clobbered during recursion printing: Save rcx to r11 before setting WriteFile args; restore after.

### Roadmap

- Extend centralized emitters to additional types and conversions (floats).
- Apply the same architectural centralization to aarch64 backend.
- Expand tests for mixed data types and more complex control-flow patterns.
- Documentation updates for Windows 10 and Ubuntu quickstart, including toolchain setup.

### Build and Test

- cargo build --workspace
- cargo test --workspace
- Use the CLI (aetherc) to generate assembly as shown above.
- Assemble/link via scripts/assemble_link.sh.

### Contributing

- Prefer small, focused PRs.
- Don’t force-push branches; keep PRs updated incrementally.
- For duplicated functions, verify usage contexts before removal (see repository owner’s note).
- Follow the Win64/SysV ABI and label policies strictly.
