#include "chip8.asm"

; Jumping X and O Program
;
; The original hexadecimal listing of this program can be 
; found in the article,
;
;   H. Kleinberg. Programming in CHIP-8.
;   RCA Corporation, 1978.

main:
    LD I, block_pattern ; Set I to block pattern
    LD V5, 0x30         ; Coordinates (x,y) of block
    LD V6, 0x04         
    DRW V5, V6, 6       ; Show block at V5, V6
.setx:
    LD I, x_pattern     ; Set I to X pattern
    CALL jump           ; Do subroutine "jump"
.seto:
    LD I, o_pattern     ; Set I to O pattern
    CALL jump           ; Do subroutine "jump"
    JP .setx            ; Go to "setx" (Return to X pattern)
    
jump:
    LD V1, 30       ; Set V1, V2 to center coordinates
    LD V2, 13
.show1:
    DRW V1, V2, 5   ; Show the pattern
    LD V3, 0x0C     ; Set V3 to 0C (= 1/5 second)
    LD DT, V3       ; Set timer from V3
.timer:
    LD V4, DT       ; Timer -> V4
    SE V4, 0        ; Skip if V4 (timer) = 0
    JP .timer       ; Return to "timer" if V4 /= 0
    SNE VF, 1       ; Skip if VF = 1 (checking for overlap)
    JP .show2       ; Go to "show2" if VF /= 01 (overlap, switch patterns)
    DRW V1, V2, 5   ; If no overlap, show old pattern to erase
    RND V1, 0x3F    ; Random number (6-bit) to V1 (new x-coordinate)
    RND V2, 0x1F    ; Random number (5-bit) to V2 (new y-coordinate)
    JP .show1       ; Go back to "show1" to show pattern in new location
.show2:
    DRW V1, V2, 5   ; Show old pattern to erase
    RET             ; Return from subroutine
    
    
; Reserve some space for future changes directly
; in the hexadecimal listing
; (the value 14 is chosen to match the locations in the original listing)
#res 14

;
; Sprites
;
x_pattern:
    #d8 0b10001000
    #d8 0b01010000
    #d8 0b00100000
    #d8 0b01010000
    #d8 0b10001000

#align 16
o_pattern:
    #d8 0b11111000
    #d8 0b10001000
    #d8 0b10001000
    #d8 0b10001000
    #d8 0b11111000
    
#align 16
block_pattern:
    #d8 0b11111100
    #d8 0b11111100 
    #d8 0b11111100 
    #d8 0b11111100 
    #d8 0b11111100
    #d8 0b11111100