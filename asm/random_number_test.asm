#include "chip8.asm"

; Random Number Test
;
; Displays a random number in the range 0-255 on the screen
; With each key-press, a new number is drawn
;

main:
    CLS                 ; Clear screen
    RND V0, 0xFF        ; Random byte
    LD I, .end          ; I := 0x224
    LD B, V0            ; Binary-coded decimal of V0 at memory(I:I+2)
    LD V2, [I]          ; Register load, V(0:2) := memory(I:I+2)
    LD F, V0            ; Load font sprites
    LD V0, 0            ; x := 0, Set x,y coordinates
    LD V3, 0            ; y := 0
    DRW V0, V3, 5       ; Draw first character
    LD F, V1            ; Load second character
    LD V0, 5            ; x := 5       
    DRW V0, V3, 5       ; Draw second character 
    LD F, V2            ; Load third character 
    LD V0, 10           ; x := 10 
    DRW V0, V3, 5       ; Draw third character  
    LD V0, K            ; Wait for keypress 
    JP main             ; Jump to beginning

#res 2                  ; Reserve 2 bytes of empty space
.end:                   ; Empty label used to 
                        ; set memory pointer after program