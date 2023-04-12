#include "chip8.asm"

main:
    LD V1, 0x0    ; The value to draw
    LD V3, -3    ; x- and y- coordinate
    LD V4, 4
.loop:
    ADD V3, 5
    CALL sss    ; Show letter
    ADD V1, 1
    SNE V1, 10
    JP .double
    JP .loop
.double:
    LD V1, 99
    LD V3, 2
    LD V4, 13
    CALL sds
.end:
    JP .end

; Show single-digit score (BCD)
sss:
    LD I, 0x2A0         ; SSS:      3 byte work area
    LD B, V1            ;
    LD I, 0x2A2         ;           Least significant digit
    LD V0, [I]          ;
    LD F, V0            ;
    DRW V3, V4, 5       ;
    RET   



; Show double-digit score (BCD)
; The value to draw should be placed in V0
; The coordinates should be set in V3, V4
sds:
    LD I, 0x2A0         ; SSS:      3 byte work area
    LD B, V1            ;
    LD I, 0x2A1         ;           Least significant digit
    LD V0, [I]          ;
    LD F, V0            ;
    DRW V3, V4, 5       ;
    LD I, 0x2A2
    LD V0, [I]
    LD F, V0
    ADD V3, 5
    DRW V3, V4, 5
    ADD V3, -5
    RET   