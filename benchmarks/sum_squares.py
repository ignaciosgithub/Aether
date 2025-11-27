# Benchmark: Sum of squares
# Computes sum of i*i for i from 0 to n-1, repeated R times

def work(n):
    s = 0
    i = 0
    while i < n:
        s = s + i * i
        i = i + 1
    return s

def main():
    total = 0
    k = 0
    n = 10000000
    r = 10
    while k < r:
        total = total + work(n)
        k = k + 1
    print(total)

if __name__ == "__main__":
    main()
