#include "chip8.asm"

main:
    CLS                 ; 00E0      Clear screen
    LD I, fish          ; A220      Load Fish pictogram
    LD V2, 8            ; 6208      Sprite-size in bytes
    LD V0, -8           ; 60F8      y := -8
.yloop:                 ;       Outer y-loop    
    ADD V0, 8           ; 7008      y := y + 8
    LD V1, 16           ; 6110      x := 16
    SNE V0, 32          ; 4020      Exit y-loop after 32/8=4 iterations
.end:                   ;       End
    JP .end             ; 120E  
.xloop:                 ;       Inner x-loop
    DRW V1, V0, 8       ; D108      Draw tile at (x,y)
    ADD I, V2           ; F21E      Set I to next tile, I := I + 8
    ADD V1, 8           ; 7108      x := x + 8
    SNE V1, 48          ; 4130      Exit x-loop after (48-16)/8=4 iterations
    JP .yloop           ; 1208      Continue y-loop
    JP .xloop           ; 1210      Continue x-loop

; Padding to match original ROM
#res 4

; Fish pictogram 
; (stored as sequential 8-by-8 square tiles)
fish:
    #d16 0x0000, 0x0000, 0x0018, 0x3c3c
    #d16 0x0000, 0x0000, 0x0000, 0x0000
    #d16 0x0000, 0x0000, 0x0000, 0x0000
    #d16 0x0000, 0x0000, 0x0000, 0x0000
    #d16 0x3E3F, 0x3F3B, 0x3938, 0x3838
    #d16 0x0000, 0x80C1, 0xE7FF, 0x7E3C
    #d16 0x001F, 0xFFF9, 0xC080, 0x0303
    #d16 0x0080, 0xE0F0, 0x7838, 0x1C1C
    #d16 0x3838, 0x393B, 0x3F3F, 0x3E3C
    #d16 0x78FC, 0xFECF, 0x8703, 0x0100
    #d16 0x0000, 0x0000, 0x80E3, 0xFF7F
    #d16 0x1C38, 0x3870, 0xF0E0, 0xC000
    #d16 0x3C18, 0x0000, 0x0000, 0x0000
    #d16 0x0000, 0x0000, 0x0000, 0x0000
    #d16 0x0000, 0x0000, 0x0000, 0x0000
    #d16 0x0000, 0x0000, 0x0000, 0x0000