#include "chip8.asm"

main:
    LD V6, 1            ; Set y := 1
.a:
    CALL draw           ; Draw value in V3
    LD V4, K            ; Wait for key-press
    SNE V4, 2           ; Skip if not K != 2 (up)
    ADD V3, 1           ; V3 := V3 + 1, if K == 2, increase timer
    SNE V4, 8           ; Skip it not K != 8 (down)
    SUB V3, V6          ; V3 := V3 - V6 (1)
    SE V4, 5            ; Skip if K == 5
    JP .a               ; Cycle and wait for new input
    LD DT, V3           ; K == 5, set delay timer to selected value
.b:
    LD V3, DT           ; Load delay timer in to V3
    CALL draw           ; Draw
    SE V3, 0            ; Skip if V3 (delay-timer) equals zero
    JP .b               ; Loop back to b (until DT reaches zero)
    JP .a               ; Loop back to a

; Draw the value stored in V3 at the location V5, V6
;   The registers V0, V1, V2 are used as workspace
draw:
    CLS                 ; Clear the screen
    LD V5, 0            ; Set x := 0
    LD I, end           ; Point I to free memory past the end of the program
    LD B, V3            ; Load binary-decimal for V3
    LD V2, [I]          ; Load registers V(0:2) = memory(I:I+2)
    LD F, V0            ; Load sprite for 1st character
    DRW V5, V6, 5       ; Draw character
    LD F, V1            ; Load sprite for 2nd character
    LD V5, 5            ; Set x := 5
    DRW V5, V6, 5       ; Draw character
    LD F, V2            ; Load sprite for 3rd character
    LD V5, 10           ; Set x := 10
    DRW V5, V6, 5       ; Draw character
    RET                 ; Return
end:                    ; Label to empty area past the end of the program