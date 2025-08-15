#include <stdint.h>

static int64_t fact(int64_t n){
    if(n <= 1) return 1;
    return n * fact(n - 1);
}

int main(){
    int64_t n = 12;
    volatile int64_t x = fact(n);
    (void)x;
    return 0;
}
