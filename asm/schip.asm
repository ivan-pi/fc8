#once
#include "chip8.asm"

; Super Chip-48 additions, as listed in Cowgod's reference (section 3.2)
#ruledef schip
{
    SCD {n: u4} => 0x00C @ n`4          ; 00Cn  scroll down n lines
    SCR  => 0x00FB                      ; 00FB  scroll right 4 pixels
    SCL  => 0x00FC                      ; 00FC  scroll left 4 pixels
    EXIT => 0x00FD                      ; 00FD  exit the interpreter
    LOW  => 0x00FE                      ; 00FE  64x32 mode
    HIGH => 0x00FF                      ; 00FF  128x64 mode

    ; Dxy0 (16x16 sprite) needs no rule: DRW Vx,Vy,0 already encodes it

    LD HF,{vx: chip8_reg} => 0xF @ vx @ 0x30     ; Fx30  I = 10-byte font digit
    LD R,{vx: chip8_reg}  =>                     ; Fx75  save V0..Vx to RPL flags
    {
        assert(vx <= 7)
        0xF @ vx @ 0x75
    }
    LD {vx: chip8_reg},R  =>                     ; Fx85  load V0..Vx from RPL flags
    {
        assert(vx <= 7)
        0xF @ vx @ 0x85
    }
}
