// Benchmark: Fibonacci
// Computes fib(n) using recursion

#include <stdio.h>
#include <stdint.h>

int64_t fib(int64_t n) {
    if (n < 2) return n;
    return fib(n - 1) + fib(n - 2);
}

int main() {
    printf("%ld\n", fib(40));
    return 0;
}
