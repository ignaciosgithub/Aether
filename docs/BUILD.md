Threading examples

Linux (x86_64):
- cargo run -p aetherc -- examples/threads_simple.ae --arch x86_64 --os linux -o out/linux/threads_simple.s
- bash ./scripts/assemble_link.sh x86_64-linux out/linux/threads_simple.s out/linux/threads_simple
- ./out/linux/threads_simple

Windows (x86_64) in MSYS2 MINGW64:
- mkdir -p out/windows
- cargo run -p aetherc -- examples/threads_simple.ae --arch x86_64 --os windows -o out/windows/threads_simple.s
- bash ./scripts/assemble_link.sh x86_64-windows out/windows/threads_simple.s out/windows/threads_simple.exe
- ./out/windows/threads_simple.exe

AArch64 Linux:
- cargo run -p aetherc -- examples/threads_simple.ae --arch aarch64 --os linux -o out/aarch64/threads_simple.s
- bash ./scripts/assemble_link.sh aarch64-linux out/aarch64/threads_simple.s out/aarch64/threads_simple
- (optional) run with qemu-aarch64-static

destroy(handle) returns 1 on success and 0 on failure.
Build and Targeting Guide

Prerequisites (Ubuntu host)
- Rust toolchain (rustup): curl https://sh.rustup.rs -sSf | sh -s -- -y
- Build tools: sudo apt-get update && sudo apt-get install -y build-essential clang lld nasm
- Optional cross toolchains:
  - Windows x86_64 (MinGW): sudo apt-get install -y mingw-w64
  - AArch64 Linux: sudo apt-get install -y gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu

Windows 10 (Native via MSYS2/MinGW64)
- Install MSYS2 from https://www.msys2.org/, then open “MSYS2 MINGW64”.
- Install packages:
  - pacman -Syu
  - pacman -S --needed git mingw-w64-x86_64-toolchain mingw-w64-x86_64-clang mingw-w64-x86_64-lld mingw-w64-x86_64-nasm make
- Install Rust in MINGW64:
  - curl https://sh.rustup.rs -sSf | sh -s -- -y
  - Reopen the MINGW64 terminal so cargo is on PATH
  - Optional: rustup default stable-x86_64-pc-windows-gnu

Workspace Build
- Build all crates:
  cargo build --workspace

Cross-compile CLI aetherc (optional)
- Using rustup target and cargo:
  ./scripts/cross_build.sh x86_64-unknown-linux-gnu
  ./scripts/cross_build.sh x86_64-pc-windows-gnu
  ./scripts/cross_build.sh aarch64-unknown-linux-gnu

Using the Compiler
- Generate assembly:
  cargo run -p aetherc -- examples/hello.ae --arch x86_64 --os linux -o out/linux/hello.s

Assemble and link
- Linux x86_64:
  ./scripts/assemble_link.sh x86_64-linux out/linux/hello.s out/linux/hello
- Windows x86_64 (cross, produces PE):
  ./scripts/assemble_link.sh x86_64-windows out/windows/hello.s out/windows/hello.exe
- If you previously saw “Undefined symbol: MainCRTStartup”, the script now passes /ENTRY:main and /SUBSYSTEM:CONSOLE to the linker when using -nostartfiles. You still need MinGW import libs (mingw-w64) available for kernel32 symbols.
- If you see “x86_64-w64-mingw32-clang: command not found”, the script will try: clang --target=x86_64-w64-mingw32 -fuse-ld=lld. If that also fails or you get unresolved Windows symbols, install mingw-w64 to provide the import libraries.
- AArch64 Linux (cross):
  ./scripts/assemble_link.sh aarch64-linux out/aarch64/hello.s out/aarch64/hello

Notes
- If target cannot be determined, default behavior in codegen is to x86_64-windows-gnu.
- For running PE on Linux, use wine. For AArch64 binaries, use qemu-aarch64-static if needed.

Float example (generate and link)

x86_64 Linux
- cargo run -p aetherc -- examples/float_return.ae --arch x86_64 --os linux -o out/linux/float.s
- ./scripts/assemble_link.sh x86_64-linux out/linux/float.s out/linux/float

x86_64 Windows (cross-link)
- cargo run -p aetherc -- examples/float_return.ae --arch x86_64 --os windows -o out/windows/float.s
- ./scripts/assemble_link.sh x86_64-windows out/windows/float.s out/windows/float.exe

AArch64 Linux (cross-link)
- cargo run -p aetherc -- examples/float_return.ae --arch aarch64 --os linux -o out/aarch64/float.s
- ./scripts/assemble_link.sh aarch64-linux out/aarch64/float.s out/aarch64/float

Print example (generate, link, run)

x86_64 Linux
- cargo run -p aetherc -- examples/println.ae --arch x86_64 --os linux -o out/linux/println.s
- ./scripts/assemble_link.sh x86_64-linux out/linux/println.s out/linux/println
- ./out/linux/println

x86_64 Windows (native or cross-link)
- cargo run -p aetherc -- examples/println.ae --arch x86_64 --os windows -o out/windows/println.s
- ./scripts/assemble_link.sh x86_64-windows out/windows/println.s out/windows/println.exe

AArch64 Linux (cross-link)
- cargo run -p aetherc -- examples/println.ae --arch aarch64 --os linux -o out/aarch64/println.s
- ./scripts/assemble_link.sh aarch64-linux out/aarch64/println.s out/aarch64/println
