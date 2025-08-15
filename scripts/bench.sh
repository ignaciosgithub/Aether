#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." &amp;&amp; pwd)"
OUT="$ROOT/out/linux"
RESULTS="$ROOT/benchmarks/results.txt"

mkdir -p "$OUT"
: &gt; "$RESULTS"

echo "Building Aether compiler..." | tee -a "$RESULTS"
cargo build --workspace --quiet

ae() {
  local src="$1"
  local out_s="$2"
  cargo run -q -p aetherc -- "$src" --arch x86_64 --os linux -o "$out_s"
  bash "$ROOT/scripts/assemble_link.sh" "$out_s" "$out_s.bin"
}

bench_cmd() {
  local label="$1"; shift
  echo "=== $label ===" | tee -a "$RESULTS"
  /usr/bin/time -f "real %e\nuser %U\nsys %S\n" "$@" &gt; /dev/null
}

echo "Compiling and timing loop_sum..." | tee -a "$RESULTS"
ae "$ROOT/benchmarks/loop_sum.ae" "$OUT/loop_sum.s"
bench_cmd "Aether loop_sum" "$OUT/loop_sum.s.bin"

gcc -O3 -march=native -mtune=native "$ROOT/benchmarks/c/loop_sum.c" -o "$OUT/loop_sum_c"
bench_cmd "C loop_sum" "$OUT/loop_sum_c"

rustc -C opt-level=3 "$ROOT/benchmarks/rust/loop_sum.rs" -o "$OUT/loop_sum_rust"
bench_cmd "Rust loop_sum" "$OUT/loop_sum_rust"

bench_cmd "Python loop_sum" python3 "$ROOT/benchmarks/python/loop_sum.py"

echo "Compiling and timing factorial..." | tee -a "$RESULTS"
ae "$ROOT/benchmarks/factorial.ae" "$OUT/factorial.s"
bench_cmd "Aether factorial" "$OUT/factorial.s.bin"

gcc -O3 -march=native -mtune=native "$ROOT/benchmarks/c/factorial.c" -o "$OUT/factorial_c"
bench_cmd "C factorial" "$OUT/factorial_c"

rustc -C opt-level=3 "$ROOT/benchmarks/rust/factorial.rs" -o "$OUT/factorial_rust"
bench_cmd "Rust factorial" "$OUT/factorial_rust"

bench_cmd "Python factorial" python3 "$ROOT/benchmarks/python/factorial.py"

echo "Compiling and timing print_concat..." | tee -a "$RESULTS"
ae "$ROOT/benchmarks/print_concat.ae" "$OUT/print_concat.s"
bench_cmd "Aether print_concat" "$OUT/print_concat.s.bin"

gcc -O3 -march=native -mtune=native "$ROOT/benchmarks/c/print_concat.c" -o "$OUT/print_concat_c"
bench_cmd "C print_concat" "$OUT/print_concat_c"

rustc -C opt-level=3 "$ROOT/benchmarks/rust/print_concat.rs" -o "$OUT/print_concat_rust"
bench_cmd "Rust print_concat" "$OUT/print_concat_rust"

bench_cmd "Python print_concat" python3 "$ROOT/benchmarks/python/print_concat.py"

echo "Done. Results written to $RESULTS" | tee -a "$RESULTS"
