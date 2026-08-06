# Benchmarks for Aether vs C, Rust, and Python

This folder contains simple, reproducible microbenchmarks to compare Aether’s current codegen against C, Rust, and Python.

Workloads
- loop_sum: sum 0..N-1 as i64
- factorial: accumulate factorial(12) FREPEAT times
- print_concat: print three short strings PCOUNT times
- inc_loop: increment i from 0 up to (2^31 - 2), with no output in timed runs (per user’s spec)
- fib_bench: recursive fib(40)
- sum_squares: sum of i*i for i in 0..n-1, repeated 10 times

Language sources
- Aether:
  - benchmarks/*.ae (loop_sum.ae, factorial.ae, print_concat.ae, inc_loop.ae)
- C:
  - benchmarks/c/*.c (loop_sum.c, factorial.c, print_concat.c, inc_loop.c)
- Rust:
  - benchmarks/rust/*.rs (loop_sum.rs, factorial.rs, print_concat.rs, inc_loop.rs)
- Python:
  - benchmarks/python/*.py (loop_sum.py, factorial.py, print_concat.py, inc_loop.py)

Fairness/flags
- C: gcc -O3 -march=native -mtune=native
- Rust: rustc -C opt-level=3
- Python: CPython 3
- Aether: compiled to assembly with aetherc, then assembled/linked

Anti-cheating measures
- gcc -O3 constant-folds fact(12) and deletes empty loops entirely, so the C/Rust factorial and inc_loop benchmarks use empty asm barriers (C) / std::hint::black_box (Rust) inside the loop to force the same per-iteration work Aether performs. Without these, C/Rust "win" by doing zero work at runtime.
- N and FREPEAT defaults match the fixed constants compiled into the Aether sources (N=500000000, FREPEAT=5000000) so every language does the same amount of work.
- Every workload prints a checksum that is verified to match across all languages before timing.

Methodology
- Verification phase: each workload prints a single checksum once to stdout; checks are recorded in benchmarks/checksums.txt.
- Timing phase: identical algorithms run without producing output; timings use nanosecond resolution and ITER repeats; results are recorded in benchmarks/results.txt.
- Inputs and repeats can be adjusted via environment variables.

Run
- Defaults are chosen for stable, non-zero timings:
  - ITER=10 N=500000000 FREPEAT=5000000 PCOUNT=100000
- Override via env and run (note: N/FREPEAT overrides change C/Rust/Python only; Aether sources use fixed constants):
  ITER=10 PCOUNT=100000 bash scripts/bench.sh

Outputs
- benchmarks/results.txt: min/avg across ITER runs per workload
- benchmarks/checksums.txt: verification that outputs match
  - loop_sum checksum equals N*(N-1)/2
  - factorial checksum equals FREPEAT * 12!
  - inc_loop final i is (2^31 - 2) + 1

Notes
- This script runs and times x86_64 Linux binaries locally. Windows/AArch64 assemblies are still generated elsewhere in the repo.
