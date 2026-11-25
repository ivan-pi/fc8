#include "chip8.asm"

;
; Evaluate the factorial of n
;
; Example motivated by the Control Structures lecture of Prof. Xavier Leroy
; at Collège de France: https://xavierleroy.org/CdF/2023-2024/


n = 1

main:
    LD V3, 2
    LD V4, 2
    LD V5, n
    call write_n
    LD V1, n
    call factorial
    LD V3, 2
    LD V4, 10
    LD V5, V0
    call write_n_fact
.fin:
    JP .fin


; Evaluate factorial, n!
;
; Arguments:
;   V0 ... output
;   V1 ... input n, is destroyed
;
factorial:
    LD V0, 1        ; 0! = 1! = 1
.next:
    SNE V1, 0       ; done when n has counted down to 0 ...
    RET
    SNE V1, 1       ; ... or 1
    RET
    call multiply   ; V0 *= V1
    ADD V1, -1
    JP .next

; Multiply function, V0 *= V1
multiply:
    LD V2, V1
    LD V3, V0
.repeat:
    ADD V0, V3
    ADD V2, -1
    SE V2, 1
    JP .repeat
    RET

    LD I, .load ; Re-load address
    LD [I], V1  ; Store V0 and V1
    SNE VA, 1   ; Skip if VA != 1
.load:
    LD VB, 0x0  ; Retrieve counter
.return:
    RET


; Write value to screen
;
; Arguments:
;   V3 ... x-coordinate
;   V4 ... y-coordinate
;   V5 ... the value we want to output
;
write_n:
    LD V0, 2            ; 6 characters
    LD V1, 5            ; address increment
    LD I, n_equals - 5   ; initial address, offset by 5
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

n_equals:
    #d8 0b10001000 ; N
    #d8 0b11001000
    #d8 0b10101000
    #d8 0b10011000
    #d8 0b10001000
    #d8 0b00000000 ; =
    #d8 0b11111000
    #d8 0b00000000
    #d8 0b11111000
    #d8 0b00000000

; Arguments:
;   V3 ... x-coordinate
;   V4 ... y-coordinate
;   V5 ... the value we want to output
write_n_fact:
    LD V0, 2            ; 6 characters
    LD V1, 5            ; address increment
    LD I, n_fact_equals - 5   ; initial address, offset by 5
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

n_fact_equals:
    #d8 0b10001000 ; N
    #d8 0b11001000
    #d8 0b10101000
    #d8 0b10011000
    #d8 0b10001000
    #d8 0b10000000 ; !=
    #d8 0b10111000
    #d8 0b10000000
    #d8 0b00111000
    #d8 0b10000000

; Place to store the decimals
digits:
    #res 3

