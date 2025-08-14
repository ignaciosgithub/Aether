#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 3 ]; then
  echo "Usage: $0 <target> <input.s> <output>"
  echo "Targets: x86_64-linux | x86_64-windows | aarch64-linux"
  exit 1
fi

TARGET="$1"
ASM="$2"
OUT="$3"

case "$TARGET" in
  x86_64-linux)
    clang -nostartfiles -static -o "$OUT" "$ASM" -fuse-ld=lld
    ;;
  x86_64-windows)
    x86_64-w64-mingw32-gcc -nostartfiles -o "$OUT" "$ASM"
    ;;
  aarch64-linux)
    aarch64-linux-gnu-gcc -nostartfiles -static -o "$OUT" "$ASM"
    ;;
  *)
    echo "Unknown target: $TARGET"
    exit 2
    ;;
esac

echo "Linked -> $OUT"
