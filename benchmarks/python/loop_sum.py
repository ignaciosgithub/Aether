import sys

sys.setrecursionlimit(1_000_000)

def sum_to(n: int) -> int:
    if n <= 1:
        return 1
    return n + sum_to(n - 1)

def main():
    N = 20000
    x = sum_to(N)
    _ = x

if __name__ == "__main__":
    main()
