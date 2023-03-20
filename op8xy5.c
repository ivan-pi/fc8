
#include <stdint.h>

void op8xy5(uint8_t *Vx, const uint8_t *Vy, uint8_t *Vf) {
    *Vf = (*Vx > *Vy) ? 1 : 0;
    *Vx = *Vx - *Vy;
} 
