
#include <stdint.h>
#include <stdio.h>

int main(int argc, char const *argv[])
{

    uint8_t Vx = 15;
    uint8_t Vf = 1;

    printf("Before = %d, %d\n", Vx, Vf);

    Vf = Vx & 0x1;
    Vx = Vx >> 1;
    printf("After  = %d, %d\n", Vx, Vf);

    /* code */
    return 0;
}