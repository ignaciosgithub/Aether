import os
import sys

def sum_to_iter(n: int) -> int:
    s = 0
    i = 0
    while i < n:
        s += i
        i += 1
    return s

def main():
    N = int(os.getenv("N", "500000000"))
    x = sum_to_iter(N)
    print(x)
    _ = x

if __name__ == "__main__":
    main()
