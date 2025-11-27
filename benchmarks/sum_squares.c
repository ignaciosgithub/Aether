// Benchmark: Sum of squares
// Computes sum of i*i for i from 0 to n-1, repeated R times

#include <stdio.h>
#include <stdint.h>

int64_t work(int64_t n) {
    int64_t s = 0;
    int64_t i = 0;
    while (i < n) {
        s = s + i * i;
        i = i + 1;
    }
    return s;
}

int main() {
    int64_t total = 0;
    int64_t k = 0;
    int64_t n = 10000000;
    int64_t r = 10;
    while (k < r) {
        total = total + work(n);
        k = k + 1;
    }
    printf("%ld\n", total);
    return 0;
}
