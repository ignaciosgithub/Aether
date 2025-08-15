#include <stdio.h>
#include <stdlib.h>

int main(){
    const char* env = getenv("PCOUNT");
    long count = env ? atol(env) : 200000;
    for (long i = 0; i < count; i++) {
        puts("hello");
        puts("world");
        puts("aether");
    }
    return 0;
}
