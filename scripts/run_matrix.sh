#!/usr/bin/env bash
# Run every example on x86_64-linux and x86_64-windows (via wine), reporting pass/fail.
set -u
cd "$(dirname "$0")/.."
AETHERC=target/release/aetherc
mkdir -p out/linux out/windows
for ae in examples/*.ae; do
  name=$(basename "$ae" .ae)
  stdin_data=""
  case "$name" in
    stdin_echo) stdin_data="hello" ;;
    stdin_to_int) stdin_data="42" ;;
    greet) stdin_data="Devin" ;;
  esac
  for os in linux windows; do
    s=out/$os/$name.s
    bin=out/$os/$name
    run=$bin
    [ "$os" = windows ] && run=$bin.exe
    if ! "$AETHERC" "$ae" --arch x86_64 --os $os -o "$s" >/dev/null 2>"out/$os/$name.compile.err"; then
      echo "FAIL-COMPILE $os $name: $( head -3 out/$os/$name.compile.err)"
      continue
    fi
    if ! ./scripts/assemble_link.sh x86_64-$os "$s" "$bin" >/dev/null 2>"out/$os/$name.link.err"; then
      echo "FAIL-LINK    $os $name: $(grep -m1 -i 'error\|undefined' out/$os/$name.link.err | head -c 120)"
      continue
    fi
    if [ "$os" = linux ]; then
      out=$(echo "$stdin_data" | timeout 10 "$run" 2>&1); rc=$?
    else
      out=$(echo "$stdin_data" | timeout 20 wine "$run" 2>/dev/null); rc=$?
    fi
    if [ $rc -eq 124 ]; then
      echo "FAIL-RUN     $os $name: timeout/crash rc=$rc"
    else
      echo "OK           $os $name rc=$rc: $(echo "$out" | tr '\n' '|' | head -c 110)"
    fi
  done
done
