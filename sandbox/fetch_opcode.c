#include <stdint.h>

uint16_t fetch_opcode(uint8_t *memory, int pc) {
    pc--; // Fortran uses 1-based indexing
    return memory[pc] << 8 | memory[pc + 1];
}