import os
import sys

def fact(n: int) -> int:
    if n <= 1:
        return 1
    return n * fact(n - 1)

def main():
    reps = int(os.getenv("FREPEAT", "5000000"))
    acc = 0
    i = 0
    while i < reps:
        acc += fact(12)
        i += 1
    print(acc)
    _ = acc

if __name__ == "__main__":
    main()
