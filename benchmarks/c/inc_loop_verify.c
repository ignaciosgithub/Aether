#include <stdint.h>
#include <stdio.h>
int main() {
    int64_t end_value = 2147483647LL - 1;
    int64_t i = 0;
    while (i <= end_value) {
        i += 1;
    }
    printf("%lld\n", (long long)i);
    return 0;
}
