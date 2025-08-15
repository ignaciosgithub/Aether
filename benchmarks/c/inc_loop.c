#include <stdint.h>
int main() {
    int64_t end_value = 2147483647LL - 1;
    int64_t i = 0;
    while (i <= end_value) {
        i += 1;
    }
    return 0;
}
