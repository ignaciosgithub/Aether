#include <stdint.h>
#include <stdio.h>

static int64_t sum_to(int64_t n){
    if(n <= 1) return 1;
    return n + sum_to(n - 1);
}

int main(){
    int64_t N = 20000;
    volatile int64_t x = sum_to(N);
    (void)x;
    return 0;
}
