Benchmarks for Aether vs C, Rust, and Python

This folder contains simple, reproducible microbenchmarks to compare Aether’s current codegen against C, Rust, and Python. The goals:
- Use identical algorithms and comparable optimization flags.
- Focus on tight arithmetic, recursion, and string I/O.

Benchmarks
- loop_sum: Sum 1..N as i64
- factorial: Recursive factorial for a small n (to avoid TLE in Python)
- print_concat: Print N short strings

How to run
- Ensure Rust toolchain and GCC are installed.
- Ensure Python3 is installed.
- Build Aether compiler (cargo build --workspace).
- Run the bench script:

  ./scripts/bench.sh

It will:
- Compile C with -O3
- Compile Rust with -C opt-level=3
- Use CPython for Python
- Compile Aether programs to assembly (out/linux/*.s), assemble and run on Linux using provided assemble_link.sh
- Time each program and write results to benchmarks/results.txt

Notes
- The Aether compiler currently targets Linux x86_64 for running benchmarks locally. Windows/AArch64 assembly can be generated but not run in this script.
- Adjust N and repetitions in scripts/bench.sh to match your environment.
