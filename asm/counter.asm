#include "chip8.asm"

; Example of implementing a counter using 
; a static variable
;
; The counter counts how many time the routine
; count is called. 
; Finally,the value is printed to the display.
;
; The example served for a discussion at Fortran-Lang:
; https://fortran-lang.discourse.group/t/save-attribute-scope/8509/11?u=ivanpribec

main:
    LD VA, 0        ; VA = 0
    CALL count_sm
    CALL count_sm
    CALL count_sm
    LD VA, 1        ; VA = 1, retrieve count,
    CALL count_sm   ; result is found in VB 
    LD V3, 2
    LD V4, 2
    LD V5, VB       
    call write      ; Write count to screen
.fin:
    JP .fin

; Count number of function calls using a 
; static variable
;
; Arguments:
;   VA ... if 1, write output value
;   VB ... output value
;
count:
    JP .body        ; Jump to executable part 
.counter:           ; Counter variable
    #d8 0x0         ; reserve 1 byte of memory
.body:
    LD I, .counter  ; Load address of counter
    LD V0, [I]      ; Load counter into register
    ADD V0, 1       ; Increment
    LD I, .counter  ; Re-load address
    LD [I], V0      ; Store counter
    SNE VA, 1       ; Skip if VA != 1
    LD VB, V0       ; VB = V0
    RET

; Increment counter (uses self-modifying code) 
; Set VA = 1 beforehand to retrieve it. Output is in VB.
; The counter is stored as an immediate value in the 
; instructions that loads it
count_sm:
    LD I, .load ; Load address
    LD V1, [I]  ; Load V0 and V1
    ADD V1, 1   ; Add 1 to counter
    LD I, .load ; Re-load address
    LD [I], V1  ; Store V0 and V1
    SNE VA, 1   ; Skip if VA != 1
.load:
    LD VB, 0x0  ; Retrieve counter
    RET

; Write value to screen
;
; Arguments:
;   V3 ... x-coordinate
;   V4 ... y-coordinate
;   V5 ... the value we want to output
;
write:
    LD V0, 6            ; 6 characters
    LD V1, 5            ; address increment
    LD I, ascii_c - 5   ; initial address, offset by 5
.begin:
    ADD I, V1       ; Load character address.
    DRW V3, V4, 5   ; Draw character.
    ADD V3, 6       ; Increment x-coordinate.
    ADD V0, -1      ; Decrement counter.
    SE V0, 0        ; If characters remain,
    JP .begin       ; return to draw next character.
    LD I, digits
    LD B, V5        ; Load 
    LD V2, [I]      ; Load decimals into V0-V2
    LD F, V0        ; Digit 1
    DRW V3, V4, 5
    ADD V3, 5
    LD F, V1        ; Digit 2
    DRW V3, V4, 5
    ADD V3, 5   
    LD F, V2        ; Digit 3
    DRW V3, V4, 5
    RET

;
; Alphabetic characters
;
ascii_c:
    #d8 0b11111000
    #d8 0b10000000
    #d8 0b10000000
    #d8 0b10000000
    #d8 0b11111000
ascii_o:
    #d8 0b11111000
    #d8 0b10001000
    #d8 0b10001000
    #d8 0b10001000
    #d8 0b11111000
ascii_u:
    #d8 0b10001000
    #d8 0b10001000
    #d8 0b10001000
    #d8 0b10001000
    #d8 0b11111000
ascii_n:
    #d8 0b10001000
    #d8 0b11001000
    #d8 0b10101000
    #d8 0b10011000
    #d8 0b10001000
ascii_t:
    #d8 0b11111000
    #d8 0b00100000
    #d8 0b00100000
    #d8 0b00100000
    #d8 0b00100000
ascii_eq:
    #d8 0b00000000
    #d8 0b11111000
    #d8 0b00000000
    #d8 0b11111000
    #d8 0b00000000
digits:
    #res 3

