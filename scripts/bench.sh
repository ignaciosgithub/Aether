#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT="$ROOT/out/linux"
RESULTS="$ROOT/benchmarks/results.txt"
CHECKSUMS="$ROOT/benchmarks/checksums.txt"

ITER="${ITER:-10}"
N="${N:-200000000}"
FREPEAT="${FREPEAT:-3000000}"
PCOUNT="${PCOUNT:-100000}"

mkdir -p "$OUT"
: > "$RESULTS"
: > "$CHECKSUMS"

echo "Building Aether compiler..." | tee -a "$RESULTS"
cargo build --workspace --quiet

ae() {
  local src="$1"
  local out_s="$2"
  cargo run -q -p aetherc -- "$src" --arch x86_64 --os linux -o "$out_s"
  bash "$ROOT/scripts/assemble_link.sh" x86_64-linux "$out_s" "$out_s.bin"
}

hr_now_ns() {
  date +%s%N
}

bench_cmd_repeat() {
  local label="$1"; shift
  echo "=== $label (x$ITER) ===" | tee -a "$RESULTS"
  local times_file="$OUT/${label// /_}_times.txt"
  : > "$times_file"
  for i in $(seq 1 "$ITER"); do
    local start_ns end_ns
    start_ns="$(hr_now_ns)"
    "$@" >/dev/null 2>&1
    end_ns="$(hr_now_ns)"
    awk -v s="$start_ns" -v e="$end_ns" 'BEGIN{printf "%.9f\n", (e - s)/1e9}' >> "$times_file"
  done
  awk '{sum+=$1; if(min==""||$1<min)min=$1} END{printf "min %.6f\navg %.6f\n", min, sum/NR}' "$times_file" | tee -a "$RESULTS"
}

echo "Compiling benchmarks..." | tee -a "$RESULTS"
ae "$ROOT/benchmarks/loop_sum.ae" "$OUT/loop_sum.s"
ae "$ROOT/benchmarks/factorial.ae" "$OUT/factorial.s"
ae "$ROOT/benchmarks/print_concat.ae" "$OUT/print_concat.s"
ae "$ROOT/benchmarks/inc_loop.ae" "$OUT/inc_loop.s"
ae "$ROOT/benchmarks/inc_loop_verify.ae" "$OUT/inc_loop_verify.s"

gcc -O3 -march=native -mtune=native "$ROOT/benchmarks/c/loop_sum.c" -o "$OUT/loop_sum_c"
rustc -C opt-level=3 "$ROOT/benchmarks/rust/loop_sum.rs" -o "$OUT/loop_sum_rust"
gcc -O3 -march=native -mtune=native "$ROOT/benchmarks/c/factorial.c" -o "$OUT/factorial_c"
rustc -C opt-level=3 "$ROOT/benchmarks/rust/factorial.rs" -o "$OUT/factorial_rust"
gcc -O3 -march=native -mtune=native "$ROOT/benchmarks/c/print_concat.c" -o "$OUT/print_concat_c"
rustc -C opt-level=3 "$ROOT/benchmarks/rust/print_concat.rs" -o "$OUT/print_concat_rust"
gcc -O3 -march=native -mtune=native "$ROOT/benchmarks/c/inc_loop.c" -o "$OUT/inc_loop_c"
rustc -C opt-level=3 "$ROOT/benchmarks/rust/inc_loop.rs" -o "$OUT/inc_loop_rust"
gcc -O3 -march=native -mtune=native "$ROOT/benchmarks/c/inc_loop_verify.c" -o "$OUT/inc_loop_verify_c"
rustc -C opt-level=3 "$ROOT/benchmarks/rust/inc_loop_verify.rs" -o "$OUT/inc_loop_verify_rust"

echo "=== CHECKSUMS ===" | tee -a "$CHECKSUMS"
exp_loop_sum=$(awk -v n="$N" 'BEGIN{printf "%.0f\n", (n*(n-1))/2}')
echo "Expected loop_sum: $exp_loop_sum" | tee -a "$CHECKSUMS"
echo "Aether loop_sum: $("$OUT/loop_sum.s.bin")" | tee -a "$CHECKSUMS"
echo "C loop_sum: $(env N="$N" "$OUT/loop_sum_c")" | tee -a "$CHECKSUMS"
echo "Rust loop_sum: $(env N="$N" "$OUT/loop_sum_rust")" | tee -a "$CHECKSUMS"
echo "Python loop_sum: $(env N="$N" python3 "$ROOT/benchmarks/python/loop_sum.py")" | tee -a "$CHECKSUMS"

exp_fact=479001600
exp_fact_total=$(awk -v f="$FREPEAT" -v v="$exp_fact" 'BEGIN{printf "%.0f\n", f*v}')
echo "Expected factorial acc: $exp_fact_total" | tee -a "$CHECKSUMS"
if [ -x "$OUT/factorial.s.bin" ]; then
  echo "Aether factorial: $("$OUT/factorial.s.bin")" | tee -a "$CHECKSUMS"
fi
echo "C factorial: $(env FREPEAT="$FREPEAT" "$OUT/factorial_c")" | tee -a "$CHECKSUMS"
echo "Rust factorial: $(env FREPEAT="$FREPEAT" "$OUT/factorial_rust")" | tee -a "$CHECKSUMS"
echo "Python factorial: $(env FREPEAT="$FREPEAT" python3 "$ROOT/benchmarks/python/factorial.py")" | tee -a "$CHECKSUMS"

exp_inc=$((2147483647))
echo "Expected inc_loop final i: $exp_inc" | tee -a "$CHECKSUMS"
echo "Aether inc_loop: $("$OUT/inc_loop_verify.s.bin")" | tee -a "$CHECKSUMS"
echo "C inc_loop: $("$OUT/inc_loop_verify_c")" | tee -a "$CHECKSUMS"
echo "Rust inc_loop: $("$OUT/inc_loop_verify_rust")" | tee -a "$CHECKSUMS"
echo "Python inc_loop: $(python3 "$ROOT/benchmarks/python/inc_loop_verify.py")" | tee -a "$CHECKSUMS"

bench_cmd_repeat "Aether loop_sum" "$OUT/loop_sum.s.bin"
bench_cmd_repeat "C loop_sum" env N="$N" "$OUT/loop_sum_c"
bench_cmd_repeat "Rust loop_sum" env N="$N" "$OUT/loop_sum_rust"
bench_cmd_repeat "Python loop_sum" env N="$N" python3 "$ROOT/benchmarks/python/loop_sum.py"

bench_cmd_repeat "Aether factorial" "$OUT/factorial.s.bin"
bench_cmd_repeat "C factorial" env FREPEAT="$FREPEAT" "$OUT/factorial_c"
bench_cmd_repeat "Rust factorial" env FREPEAT="$FREPEAT" "$OUT/factorial_rust"
bench_cmd_repeat "Python factorial" env FREPEAT="$FREPEAT" python3 "$ROOT/benchmarks/python/factorial.py"

bench_cmd_repeat "Aether inc_loop (no output)" "$OUT/inc_loop.s.bin"
bench_cmd_repeat "C inc_loop (no output)" "$OUT/inc_loop_c"
bench_cmd_repeat "Rust inc_loop (no output)" "$OUT/inc_loop_rust"
bench_cmd_repeat "Python inc_loop (no output)" python3 "$ROOT/benchmarks/python/inc_loop.py"

bench_cmd_repeat "Aether print_concat" "$OUT/print_concat.s.bin"
bench_cmd_repeat "C print_concat" env PCOUNT="$PCOUNT" "$OUT/print_concat_c"
bench_cmd_repeat "Rust print_concat" env PCOUNT="$PCOUNT" "$OUT/print_concat_rust"
bench_cmd_repeat "Python print_concat" env PCOUNT="$PCOUNT" python3 "$ROOT/benchmarks/python/print_concat.py"

echo "Done. Results written to $RESULTS; checks to $CHECKSUMS" | tee -a "$RESULTS"
