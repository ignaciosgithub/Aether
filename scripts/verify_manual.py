#!/usr/bin/env python3
"""Verify that the code examples in docs/manual.md actually execute.

Extracts every ```aether fenced block containing a main function, compiles
it for x86_64 Linux, links it, runs it, and reports the exit code and
output so the manual's listings can be checked against reality.
"""
import os
import re
import subprocess
import sys
import tempfile

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
AETHERC = os.path.join(REPO, "target", "release", "aetherc")
LINK = os.path.join(REPO, "scripts", "assemble_link.sh")


def main():
    md = open(os.path.join(REPO, "docs", "manual.md")).read()
    blocks = re.findall(r"```aether\n(.*?)```", md, re.S)
    workdir = tempfile.mkdtemp(prefix="aether-manual-")
    # Library snippet needed by the imports example.
    open(os.path.join(workdir, "lib.ae"), "w").write(
        "pub func inc(x: i32) -> i32 { return x + 1; }\n"
    )
    failures = 0
    for i, block in enumerate(blocks):
        if "func main" not in block:
            print(f"[{i}] skipped (library snippet)")
            continue
        src = os.path.join(workdir, f"block{i}.ae")
        open(src, "w").write(block)
        asm = src + ".s"
        binp = src + ".bin"
        r = subprocess.run(
            [AETHERC, src, "--arch", "x86_64", "--os", "linux", "-o", asm],
            capture_output=True, text=True,
        )
        if r.returncode != 0:
            print(f"[{i}] COMPILE FAIL: {r.stderr.strip()[:120]}")
            failures += 1
            continue
        r = subprocess.run(
            ["bash", LINK, "x86_64-linux", asm, binp],
            capture_output=True, text=True,
        )
        if r.returncode != 0:
            print(f"[{i}] LINK FAIL: {r.stderr.strip()[:120]}")
            failures += 1
            continue
        stdin = "42\n" if "readln" in block else ""
        r = subprocess.run(
            [binp], input=stdin, capture_output=True, text=True, timeout=30
        )
        print(f"[{i}] rc={r.returncode} out={r.stdout!r}")
    print(f"{failures} failure(s)" if failures else "all blocks compiled and ran")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
