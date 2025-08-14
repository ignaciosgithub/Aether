#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

TARGET="${1:-x86_64-unknown-linux-gnu}"

rustup target add "$TARGET" || true
cargo build -p aetherc --release --target "$TARGET"
echo "Built aetherc for target: $TARGET"
