import sys

sys.setrecursionlimit(1_000_000)

def fact(n: int) -> int:
    if n <= 1:
        return 1
    return n * fact(n - 1)

def main():
    n = 12
    x = fact(n)
    _ = x

if __name__ == "__main__":
    main()
