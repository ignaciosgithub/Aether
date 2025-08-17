Threading (portable MVP)

- spawn("func", i64) -> i64 handle
- join(i64) -> i32
- destroy(i64) -> i32 (1 success, 0 failure)

Targets:
- Windows x86_64: CreateThread + WaitForSingleObject + GetExitCodeThread + CloseHandle + TerminateThread
- Linux x86_64/AArch64: clone(SIGCHLD) + wait4 + kill(SIGKILL), 64KiB stacks in .bss

Examples: examples/threads_simple.ae, examples/threads_map_reduce.ae

See docs/BUILD.md for per-target run steps.

# Aether

Aether is a small, portable systems language and compiler with a clear, readable syntax and a focus on performance and cross‑platform code generation.

Targets today:
- x86_64 Linux
- x86_64 Windows (PE/COFF)
- AArch64 Linux

What you can do now:
- Define functions and call them (including recursion)
- Integer and float expressions and returns (f64 in proper FP return regs)
- Control flow: if/else, while with break/continue
- Println of Strings across all targets; println of i64 on Linux
- Basic object‑oriented data: structs, statics, field access; single inheritance (Child : Parent) with parent‑first layout
- Generate assembly per target and assemble/link via scripts

See docs:
- docs/BUILD.md — toolchain setup, generating assembly, assembling and linking per target
- docs/language.md — current syntax and features
- docs/benchmarks.md — fair microbenchmark methodology and how to run the harness

## Quickstart

Choose your OS and follow the steps to be ready to write and run Aether code in minutes.

### Ubuntu (22.04/24.04)

1) Install prerequisites
- Rust toolchain:
  - curl https://sh.rustup.rs -sSf | sh -s -- -y
  - Restart your terminal so cargo is on PATH
- Toolchain packages:
  - sudo apt-get update
  - sudo apt-get install -y build-essential clang lld nasm
- Note: NASM is optional; only needed if you plan to assemble hand‑written .asm files.
- Optional cross toolchains:
  - Windows: sudo apt-get install -y mingw-w64
  - AArch64: sudo apt-get install -y gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu

2) Clone and build
- git clone https://github.com/ignaciosgithub/Aether.git
- cd Aether
- cargo build --workspace

3) Your first program
Use the included example:
- cargo run -p aetherc -- examples/println.ae --arch x86_64 --os linux -o out/linux/println.s
- ./scripts/assemble_link.sh x86_64-linux out/linux/println.s out/linux/println
- ./out/linux/println

To cross‑generate Windows PE:
- cargo run -p aetherc -- examples/println.ae --arch x86_64 --os windows -o out/windows/println.s
- ./scripts/assemble_link.sh x86_64-windows out/windows/println.s out/windows/println.exe

To generate AArch64 Linux:
- cargo run -p aetherc -- examples/println.ae --arch aarch64 --os linux -o out/aarch64/println.s
- ./scripts/assemble_link.sh aarch64-linux out/aarch64/println.s out/aarch64/println

4) Write your own
Create hello.ae:
```
pub func main() -> i32 {
    println("Hello from Aether!");
    return 0;
}
```
Then:
- cargo run -p aetherc -- hello.ae --arch x86_64 --os linux -o out/linux/hello.s
- ./scripts/assemble_link.sh x86_64-linux out/linux/hello.s out/linux/hello
- ./out/linux/hello

### Windows 10 (Native, via MSYS2/MinGW64)

1) Install MSYS2
- Download and install from https://www.msys2.org/
- Launch the “MSYS2 MINGW64” terminal (important)

2) Install packages (in MINGW64 terminal)
- pacman -Syu
- pacman -S --needed git mingw-w64-x86_64-toolchain mingw-w64-x86_64-clang mingw-w64-x86_64-lld mingw-w64-x86_64-nasm make

- Note: NASM is optional; only needed if you plan to assemble hand‑written .asm files.
3) Install Rust (GNU toolchain) in MINGW64
- curl https://sh.rustup.rs -sSf | sh -s -- -y
- Close and reopen the MINGW64 terminal so cargo is on PATH
- Optional: rustup default stable-x86_64-pc-windows-gnu

4) Clone and build
- git clone https://github.com/ignaciosgithub/Aether.git
- cd Aether
- cargo build --workspace

5) Your first program (Windows target)
- cargo run -p aetherc -- examples/println.ae --arch x86_64 --os windows -o out/windows/println.s
- If you see “The system cannot find the path specified”: ensure the output directory exists and that you are generating .s (assembly), not .o. For example: mkdir -p out/windows; cargo run -p aetherc -- examples/println.ae --arch x86_64 --os windows -o out/windows/println.s; bash ./scripts/assemble_link.sh x86_64-windows out/windows/println.s out/windows/println.exe
- bash ./scripts/assemble_link.sh x86_64-windows out/windows/println.s out/windows/println.exe
- ./out/windows/println.exe

To produce Linux/AArch64 artifacts from Windows, use WSL (below) or a Linux VM.

### Windows 10 (Alternative: WSL Ubuntu)

1) Enable WSL and install Ubuntu from the Microsoft Store.
2) Open “Ubuntu” and follow the Ubuntu steps above verbatim.
3) To run Windows PE built in WSL on Windows, copy out the .exe and run it from Windows.
- Using the wrong MSYS2 shell (MSYS or UCRT64) instead of MINGW64\n  - Close the current terminal and open “MSYS2 MINGW64”. The prompt should include MINGW64.\n- Link errors about kernel32 or unresolved Windows symbols\n  - Ensure you ran: bash ./scripts/assemble_link.sh x86_64-windows ...\n  - Verify lld/clang are installed (see package list above); the script links -lkernel32 automatically.\n- Missing lld/clang/make\n  - Re-run: pacman -S --needed mingw-w64-x86_64-clang mingw-w64-x86_64-lld make\n- “The system cannot find the path specified. (os error 3)” when running aetherc\n  - Ensure the output directory exists and that aetherc writes a .s file, not .o. Example:\n    - mkdir -p out/windows\n    - cargo run -p aetherc -- examples/println.ae --arch x86_64 --os windows -o out/windows/println.s\n    - bash ./scripts/assemble_link.sh x86_64-windows out/windows/println.s out/windows/println.exe

### Troubleshooting

- “command not found: cargo”
  - Open a new terminal after installing rustup, or ensure cargo is on PATH.
- “permission denied: scripts/*.sh”
  - chmod +x scripts/*.sh or prefix calls with bash, e.g., bash ./scripts/assemble_link.sh ...
- “clang/lld/nasm not found”
  - Revisit prerequisites for your OS and install the listed packages.
- “unknown target in assemble_link.sh”
  - The first arg must be one of: x86_64-linux, x86_64-windows, aarch64-linux.

## Language overview

Examples in examples/*.ae:
- println.ae — println String
- calls.ae — multiple functions and calls
- factorial.ae — recursion returning integer
- runtime_ifelse.ae — runtime branching on conditions
- inheritance.ae — single inheritance printing a parent field from a Child
- locals_mutation.ae, nested_structs.ae — struct fields and mutation

See docs/language.md for syntax, types, println, and OO layout details.

## Printing

- println(String) works across Linux x86_64, Windows x86_64, and AArch64 Linux.
- println(i64) is implemented on Linux x86_64 to make integer work observable (used by benchmarks).

## Benchmarks
## Stdin (readln)
- readln() reads up to 1024 bytes from standard input, trims the trailing newline (LF on Linux, CRLF on Windows), and returns a String-compatible value for println (rax=ptr, rdx=len on x86_64).
- Example: examples/stdin_echo.ae
  - Linux assembly: cargo run -p aetherc -- examples/stdin_echo.ae --arch x86_64 --os linux -o out/linux/stdin_echo.s
  - Windows assembly: cargo run -p aetherc -- examples/stdin_echo.ae --arch x86_64 --os windows -o out/windows/stdin_echo.s
- Quick test on Linux after assembling/linking:
  - echo "hello" | ./out/linux/stdin_echo

### to_int(String) -> i64
- Strict parsing: optional leading + or -, followed by only decimal digits. The entire string must be digits after the optional sign.
- Invalid input (e.g., "", "+", "-", "12&3", "123abc") triggers a runtime error message and the program exits with code 1.
- Example:
  - println(to_int(readln()));
- Note on Windows assemblers: if your toolchain errors on unknown pseudo-ops for zero-fill, we emit .space for zeroed buffers to match the README toolchain.


We include simple Aether vs C/Rust/Python microbenchmarks. The harness:
- Verifies correctness via a checksum print (one line) in a separate phase
- Times identical no‑output workloads using nanosecond timing with ITER repeats

Run:
- ITER=10 N=200000000 FREPEAT=3000000 PCOUNT=100000 bash scripts/bench.sh
- See benchmarks/checksums.txt and benchmarks/results.txt

See docs/benchmarks.md for details and methodology.

## CI

Minimal GitHub Actions build/test runs on pushes and PRs to main.

## Contributing

PRs welcome. Please:
- Keep changes portable across the current targets
- Avoid introducing secrets or platform‑specific paths
- Update docs/examples if you change user‑visible behavior

License: MIT
