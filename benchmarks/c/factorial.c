#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

static int64_t fact(int64_t n){
    if(n <= 1) return 1;
    return n * fact(n - 1);
}

int main(){
    const char* env = getenv("FREPEAT");
    int64_t reps = env ? atoll(env) : 5000000;
    int64_t acc = 0;
    for (int64_t i = 0; i < reps; i++) {
        acc += fact(12);
    }
    printf("%lld\n", (long long)acc);
    return 0;
}
