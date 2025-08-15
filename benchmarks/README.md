# Benchmarks for Aether vs C, Rust, and Python

This folder contains simple, reproducible microbenchmarks to compare Aether’s current codegen against C, Rust, and Python.

Workloads
- loop_sum: sum 0..N-1 as i64
- factorial: accumulate factorial(12) FREPEAT times
- print_concat: print three short strings PCOUNT times
- inc_loop: increment i from 0 up to (2^31 - 2), with no output in timed runs (per user’s spec)

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

Methodology
- Verification phase: each workload prints a single checksum once to stdout; checks are recorded in benchmarks/checksums.txt.
- Timing phase: identical algorithms run without producing output; timings use nanosecond resolution and ITER repeats; results are recorded in benchmarks/results.txt.
- Inputs and repeats can be adjusted via environment variables.

Run
- Defaults are chosen for stable, non-zero timings:
  - ITER=10 N=200000000 FREPEAT=3000000 PCOUNT=100000
- Override via env and run:
  ITER=10 N=200000000 FREPEAT=3000000 PCOUNT=100000 bash scripts/bench.sh

Outputs
- benchmarks/results.txt: min/avg across ITER runs per workload
- benchmarks/checksums.txt: verification that outputs match
  - loop_sum checksum equals N*(N-1)/2
  - factorial checksum equals FREPEAT * 12!
  - inc_loop final i is (2^31 - 2) + 1

Notes
- This script runs and times x86_64 Linux binaries locally. Windows/AArch64 assemblies are still generated elsewhere in the repo.
