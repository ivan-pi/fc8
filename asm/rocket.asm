#include "chip8.asm"

; Rocket program
;
; The original hexadecimal listing of this program can be 
; found in the article,
;
;   Joseph Weisbecker. An Easy Programming System. BYTE, December 1978.
;
main:
.step1:
    LD V1, 0x00         ; Step 1:   Score
    LD V2, 0x00         ;           Rocket count
    LD V3, 0x38         ;           Score X
    LD V4, 0x1B         ;           Score Y
    LD V5, 0x00         ;           UFO X
    LD V6, 0x08         ;           UFO Y
    LD I, ufo           ;           UFO pattern
    DRW V5, V6, 3       ;           UFO
    
.step2:
    CALL sss            ; Step 2:   Show score
    SNE V2, 0x09        ;
.step3:                 ; Step 3:   End loop
    JP .step3           ;
.step4:
    ADD V2, 1           ; Step 4:
    LD V8, 0x1A         ;           Rocket Y
    LD VA, 0x00         ;
    RND V7, 0x1F        ;
    ADD V7, 15          ;           Rocket X
    LD V9, 0            ;
    LD I, rock          ;           Rocket pattern
    DRW V7, V8, 6       ;
    
.step5:
    LD I, ufo           ; Step 5:   UFO pattern
    DRW V5, V6, 3       ;           Erase UFO
    RND V0, 0x03        ;
    ADD V5, V0          ;           Set VF
    DRW V5, V6, 3       ;
    SE VF, 0x00         ;
    JP .step12          ;           Step 12 if hit
    
.step6:
    LD V0, 0x0F         ; Step 6:
    SKNP V0             ;
    LD V9, 0x01         ;
    
.step7:
    SE V9, 0x01         ; Step 7:
    JP .step5           ;           Step 5
    
.step8:
    LD V0, DT           ; Step 8:
    SE V0, 0x00         ;
    JP .step5           ;           Step 5
    
.step9:
    LD I, rock          ; Step 9:   Rocket pattern  
    DRW V7, V8, 6       ;           Erase rocket
    ADD V8, 0xFF        ;
    DRW V7, V8, 6       ;
    SE VF, 0x00         ;
    JP .step12          ;           Step 12
    
.step10:
    LD V0, 0x03         ; Step 10:
    LD DT, V0           ;
    SE V8, 0x00         ;
    JP .step12          ;           Step 12
    
.step11:
    CALL sss            ; Step 11:  Erase score
    LD I, rock          ;           Rocket pattern
    DRW V7, V8, 6       ;           Erase rocket
    ADD V1, VA          ;           Score + VA
    JP .step2           ;           Step 2
    
.step12:
    LD VA, 0x01         ; Step 12:
    LD V0, 0x03         ;
    LD ST, V0           ;
    JP .step11          ;           Step 11
    
sss:
    LD I, 0x2A0         ; SSS:      3 byte work area
    LD B, V1            ;
    LD I, 0x2A2         ;           Least significant digit
    LD V0, [I]          ;
    LD F, V0            ;
    DRW V3, V4, 5       ;
    RET                 ;
    
rock:                           ; Rocket pattern
    #d16 0x2070, 0x70F8, 0xD888

ufo:                            ; UFO Pattern
    #d16 0x7CD6, 0x7C00
