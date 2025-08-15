# Benchmarks

We compare Aether with C, Rust, and Python using microbenchmarks that perform identical work.

Goals:
- Parity of algorithms across languages
- Verify correctness via a checksum, printed only in a verification phase
- Time no‑output binaries to avoid I/O skew
- Use repeated runs with nanosecond timing

## Workloads

- loop_sum: sum i from 0..N-1 as i64
- factorial: accumulate factorial(12) FREPEAT times
- print_concat: print 3 short strings PCOUNT times
- inc_loop: increment i from 0 up to 2^31 − 2 (no prints in timed runs)
  - Separate verification program prints final i once

Aether sources live in benchmarks/*.ae; C/Rust/Python in benchmarks/{c,rust,python}.

## Running

Defaults:
- ITER=10 N=200000000 FREPEAT=3000000 PCOUNT=100000

Command:
- ITER=10 N=200000000 FREPEAT=3000000 PCOUNT=100000 bash scripts/bench.sh

Outputs:
- benchmarks/checksums.txt — each workload’s expected and actual checksums across languages
- benchmarks/results.txt — per workload min/avg across ITER runs for each language

## Fairness

- Same algorithms and inputs across languages
- No prints during the timing phase
- C: gcc -O3 -march=native -mtune=native
- Rust: -C opt-level=3
- Python: CPython 3.x
- Aether: integer work retained via println(i64) in verification phase

Notes:
- Integer println is currently implemented for Linux x86_64; verification runs on Linux.
- Timed runs redirect stdout to /dev/null where applicable to avoid I/O cost.
