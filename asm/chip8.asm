#once

; CHIP8 Instruction as defined in
; Cowgod's Chip-8 Technical Reference v.1
; http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#Dxyn
;
#subruledef chip8_reg
{
    V{n: u4} => n`4
    VA => 0xA
    VB => 0xB
    VC => 0xC
    VD => 0xD
    VE => 0xE
    VF => 0xF
}

#subruledef chip8_addr
{
    {addr: u12} =>
    {
        absaddr = addr
        assert(absaddr >= 0x200)
        assert(absaddr <= 0xFFF)
        absaddr`12
    }
}

#bankdef chip8_bank
{
; CHIP-8 was designed for the COSMAC VIP which had 4 kb of addressable memory.
; The first 512 bytes were reserved for the CHIP-8 interpreter.
; We can address bytes 512-4096, or the locations 0x200 to 0xFFF
; In practice even less memory would be available, because the interpreter
; would also require some dynamic memory.

    #addr     0x0200    ; Starting logical address (512 bytes reserved by Chip-8 interpreter)
    #addr_end 0x1000    ; Ending logical address (exclusive)
    #outp 0             ; Output location
}

#ruledef chip8
{
    CLS => 0x00E0
    RET => 0x00EE
    SYS {addr: chip8_addr} => 0x0 @ addr
    JP {addr: chip8_addr} => 0x1 @ addr
    CALL {addr: chip8_addr} => 0x2 @ addr
    SE  {vx: chip8_reg}, {byte: i8} => 0x3 @ vx @ byte`8
    SNE {vx: chip8_reg}, {byte: i8} => 0x4 @ vx @ byte`8
    SE {vx: chip8_reg}, {vy: chip8_reg} => 0x5 @ vx @ vy @ 0`4
    LD  {vx: chip8_reg}, {byte: i8} => 0x6 @ vx @ byte`8
    ADD {vx: chip8_reg}, {byte: i8} => 0x7 @ vx @ byte`8
    
; Arithmetic commands (originally these were machine commands)
    LD   {vx: chip8_reg}, {vy: chip8_reg} => 0x8 @ vx @ vy @0`4
    OR   {vx: chip8_reg}, {vy: chip8_reg} => 0x8 @ vx @ vy @1`4
    AND  {vx: chip8_reg}, {vy: chip8_reg} => 0x8 @ vx @ vy @2`4
    XOR  {vx: chip8_reg}, {vy: chip8_reg} => 0x8 @ vx @ vy @3`4
    ADD  {vx: chip8_reg}, {vy: chip8_reg} => 0x8 @ vx @ vy @4`4
    SUB  {vx: chip8_reg}, {vy: chip8_reg} => 0x8 @ vx @ vy @5`4
    SUBN {vx: chip8_reg}, {vy: chip8_reg} => 0x8 @ vx @ vy @7`4
    
    SNE {vx: chip8_reg}, {vy: chip8_reg} => 0x9 @ vx @ vy @0`4
    
    LD I, {addr: chip8_addr} => 0xA @ addr
    JP V0, {addr: chip8_addr} => 0xB @ addr
    
    RND  {vx: chip8_reg}, {byte: i8} => 0xC @ vx @ byte`8
    
    DRW {vx: chip8_reg}, {vy: chip8_reg}, {n: u4} => 0xD @ vx @ vy @ n`4
    
    SKP  {vx: chip8_reg} => 0xE @ vx @ 0x9E
    SKNP {vx: chip8_reg} => 0xE @ vx @ 0xA1
    
    LD {vx: chip8_reg}, DT => 0xF @ vx @ 0x07
    LD {vx: chip8_reg}, K  => 0xF @ vx @ 0x0A
    LD DT, {vx: chip8_reg} => 0xF @ vx @ 0x15
    LD ST, {vx: chip8_reg} => 0xF @ vx @ 0x18
    
    ADD I, {vx: chip8_reg} => 0xF @ vx @ 0x1E
    
    LD F, {vx: chip8_reg} => 0xF @ vx @ 0x29
    LD B, {vx: chip8_reg} => 0xF @ vx @ 0x33
    
    LD [I], {vx: chip8_reg}  => 0xF @ vx @ 0x55
    LD {vx: chip8_reg}, [I]  => 0xF @ vx @ 0x65
    
}