#include <stdint.h>

uint64_t bit_and(uint64_t a, uint64_t b) {
    return a & b;
}

uint64_t bit_or(uint64_t a, uint64_t b) {
    return a | b;
}

uint64_t bit_shift_left(uint64_t x, uint64_t amount) {
    return x << amount;
}

uint64_t bit_shift_right(uint64_t x, uint64_t amount) {
    return x >> amount;
}
