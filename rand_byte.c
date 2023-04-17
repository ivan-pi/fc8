#include <stdint.h>
#include <stdlib.h>

// Returns a random byte.
uint8_t rand_byte() {
    // The int returned by rand() will be automatically reduced 
    // (modulo UINT8_MAX) due to truncation. Alternatively the return
    // value could be specified as rand() % 256.
    return rand();
}

// Returns a random byte, combined with the integer kk using a bitwise ADD.
// This function can be used to limit the range of the returned results.
// E.g. to toss a coin use rand_byte_masked(0x1).
uint8_t rand_byte_masked(int8_t kk) {
    return ((uint8_t) rand()) & kk;
}