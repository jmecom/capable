#include <stdio.h>

void demo_log(const char *ptr, unsigned long len) {
    fwrite(ptr, 1, len, stdout);
    fwrite("\n", 1, 1, stdout);
    fflush(stdout);
}
