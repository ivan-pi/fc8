#include "chip8.asm"

; VIP Kaleidoscope
;
; Four spots appear in the centre of the screen. Keys 2, 4, 6 and 8 move
; them up, left, right and down; every move is mirrored into the four
; quadrants. Key 0 ends the entry, after which the recorded key sequence
; is replayed over and over. The sequence "44444442220" is the one
; suggested in the manual.
;
; The original hexadecimal listing is by Joseph Weisbecker and can be
; found in the
;
;   RCA COSMAC VIP CDP18S711 Instruction Manual.
;   RCA Corporation, 1978.
;
; Registers:
;   V0 ... key
;   V1 ... x in the upper-left quadrant (0-31)
;   V2 ... y in the upper-left quadrant (0-15)
;   V3 ... index into the key list, counting from 0x80 up to 0xFF
;   VA, VB ... scratch
;
; The key list is not part of the program image: it lives at
; main + V3, i.e. in the 128 bytes from 0x280 to 0x2FF.

main:
    LD V0, 0            ; 6000      No key yet
    LD V3, 0x80         ; 6380      Start of the key list
    LD V1, 0x1F         ; 611F      x := 31  (centre of the screen)
    LD V2, 0x0F         ; 620F      y := 15

; Record: apply the last key, then wait for the next one and store it
.record:
    CALL move           ; 2232      Move and draw the four spots
    LD I, main          ; A200
    ADD I, V3           ; F31E      I := address of the list entry
    LD V0, K            ; F00A      Wait for a key
    LD [I], V0          ; F055      Store it
    SNE V0, 0           ; 4000      Key 0 ends the entry
    JP .replay          ; 121C
    ADD V3, 1           ; 7301      Next entry
    SE V3, 0            ; 3300      Fall through when the list is full
    JP .record          ; 1208

; Replay: step through the list; restart at a 0 or at the end of the list
.replay:
    LD V3, 0x80         ; 6380      Back to the start of the key list
.next:
    LD I, main          ; A200
    ADD I, V3           ; F31E
    LD V0, [I]          ; F065      Fetch the recorded key
    SNE V0, 0           ; 4000      A 0 marks the end of the sequence
    JP .replay          ; 121C
    ADD V3, 1           ; 7301
    SNE V3, 0           ; 4300      End of the list
    JP .replay          ; 121C
    CALL move           ; 2232
    JP .next            ; 121E

; Move the spot according to the key in V0 and draw it together with
; its three mirror images
move:
    SNE V0, 2           ; 4002      Key 2: up
    ADD V2, 0xFF        ; 72FF          y := y - 1
    SNE V0, 4           ; 4004      Key 4: left
    ADD V1, 0xFF        ; 71FF          x := x - 1
    SNE V0, 6           ; 4006      Key 6: right
    ADD V1, 1           ; 7101          x := x + 1
    SNE V0, 8           ; 4008      Key 8: down
    ADD V2, 1           ; 7201          y := y + 1
    LD I, spot          ; A277      A single pixel

    ; Keep (x,y) inside the 32-by-16 quadrant. Leaving it on one axis
    ; also bumps the other coordinate, which keeps patterns from
    ; simply retracing themselves.
    LD VA, 0xE0         ; 6AE0
    AND VA, V1          ; 8A12      VA /= 0 if x left 0-31
    LD VB, 0x1F         ; 6B1F
    AND V1, VB          ; 81B2      x := x mod 32
    SE VA, 0            ; 3A00
    ADD V2, 1           ; 7201      y := y + 1
    LD VA, 0xF0         ; 6AF0
    AND VA, V2          ; 8A22      VA /= 0 if y left 0-15
    LD VB, 0x0F         ; 6B0F
    AND V2, VB          ; 82B2      y := y mod 16
    SE VA, 0            ; 3A00
    ADD V1, 1           ; 7101      x := x + 1
    LD VB, 0x1F         ; 6B1F
    AND V1, VB          ; 81B2      x := x mod 32

    DRW V1, V2, 1       ; D121      Upper left:  (x, y)
    LD VA, V1           ; 8A10
    LD VB, 0x1F         ; 6B1F
    SUB VB, V2          ; 8B25
    DRW VA, VB, 1       ; DAB1      Lower left:  (x, 31 - y)
    LD VA, 0x3F         ; 6A3F
    SUB VA, V1          ; 8A15
    DRW VA, VB, 1       ; DAB1      Lower right: (63 - x, 31 - y)
    LD VB, V2           ; 8B20
    DRW VA, VB, 1       ; DAB1      Upper right: (63 - x, y)
    RET                 ; 00EE

; The spot sprite is the second of these four bytes; the others are
; kept to match the original listing.
    #d8 0x01
spot:
    #d8 0b10000000
    #d8 0x00, 0x00
