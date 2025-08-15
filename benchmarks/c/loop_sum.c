#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static int64_t sum_to_iter(int64_t n){
    int64_t s = 0;
    for (int64_t i = 0; i < n; i++) {
        s += i;
    }
    return s;
}

int main(){
    const char* env = getenv("N");
    int64_t N = env ? atoll(env) : 500000000;
    int64_t x = sum_to_iter(N);
    printf("%lld\n", (long long)x);
    return 0;
}
