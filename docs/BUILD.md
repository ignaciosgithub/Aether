Build and Targeting Guide

Prerequisites (Linux host)
- Rust toolchain (rustup): curl https://sh.rustup.rs -sSf | sh -s -- -y
- Build tools: sudo apt-get update && sudo apt-get install -y build-essential clang lld nasm
- Cross toolchains:
  - Windows x86_64 (MinGW): sudo apt-get install -y mingw-w64
  - AArch64 Linux: sudo apt-get install -y gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu

Workspace Build
- Build all crates:
  ./scripts/build_linux.sh

Cross-compile CLI aetherc
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
- AArch64 Linux (cross):
  ./scripts/assemble_link.sh aarch64-linux out/aarch64/hello.s out/aarch64/hello

Notes
- If target cannot be determined, default behavior in codegen is to x86_64-windows-gnu.
- For running PE on Linux, use wine. For AArch64 binaries, use qemu-aarch64-static if needed.
