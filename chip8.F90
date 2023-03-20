
#define  I8(byte) int(byte,int8)
#define I16(byte) int(byte,int16)

!
! chip8-test-rom: https://github.com/corax89/chip8-test-rom
! chip8 ROM's: https://github.com/loktar00/chip8/tree/master/roms
! chip8 Program Pack: https://github.com/dmatlack/chip8/tree/master/roms
!
module chip8

use procall, only: clear_window => gclr, pset, copylayer, msleep

use, intrinsic :: iso_fortran_env, only: int8, int16, int32
implicit none
private

public :: int8, int16
public :: loadgame, memory
public :: initmem, vexec

integer(int8) :: memory(0:4095) = 0
integer(int8) :: V(0:15) = 0
integer :: I  = 0
integer :: PC = 512   ! z'200'

integer :: sp = 1
integer(int16) :: stack(16) = 0

integer :: delay_timer = 0
integer :: sound_timer = 0

logical :: keypad(16)

interface
   function rand() bind(c,name="randint8")
      use, intrinsic :: iso_c_binding, only: c_int8_t
      integer(c_int8_t) :: rand
   end function
end interface

!> Screen buffer (black and white pixels)
!> TODO: Should this be flipped 32 rows by 64 columns?
logical :: screen(0:63,0:31) = .false.

!> Lookup table for the hexadecimal character sprites.
integer :: fontidx(0:15)

!> The height of the font; corresponds to the number of bytes to be drawn.
integer, parameter :: FONT_HEIGHT = 5

contains

    subroutine initmem()


        !> Seed random number generatr

        ! Initialize font (all characters are 5 bytes)
        ! TODO: factor this out as an include file for easy
        !       compile time font replacement

        fontidx( 0) = 43
        fontidx( 1) = 13
        fontidx( 2) = 29
        fontidx( 3) = 37
        fontidx( 4) = 45
        fontidx( 5) = 27
        fontidx( 6) = 31
        fontidx( 7) = 8
        fontidx( 8) = 33
        fontidx( 9) = 35
        fontidx(10) = 41
        fontidx(11) = 18
        fontidx(12) = 4
        fontidx(13) = 22
        fontidx(14) = 0
        fontidx(15) = 2

        memory( 0) = I8(b'11110000') ! E
        memory( 1) = I8(b'10000000')
        memory( 2) = I8(b'11110000') ! F
        memory( 3) = I8(b'10000000')
        memory( 4) = I8(b'11110000') ! C
        memory( 5) = I8(b'10000000')
        memory( 6) = I8(b'10000000')
        memory( 7) = I8(b'10000000')
        memory( 8) = I8(b'11110000') ! 7 
        memory( 9) = I8(b'00010000')
        memory(10) = I8(b'00100000')
        memory(11) = I8(b'01000000')
        memory(12) = I8(b'01000000')
        memory(13) = I8(b'00100000') ! 1
        memory(14) = I8(b'01100000')
        memory(15) = I8(b'00100000')
        memory(16) = I8(b'00100000')
        memory(17) = I8(b'01110000')
        memory(18) = I8(b'11100000') ! B
        memory(19) = I8(b'10010000')
        memory(20) = I8(b'11100000')
        memory(21) = I8(b'10010000')
        memory(22) = I8(b'11100000') ! D
        memory(23) = I8(b'10010000')
        memory(24) = I8(b'10010000')
        memory(25) = I8(b'10010000')
        memory(26) = I8(b'11100000')
        memory(27) = I8(b'11110000') ! 5
        memory(28) = I8(b'10000000')
        memory(29) = I8(b'11110000') ! 2
        memory(30) = I8(b'00010000')
        memory(31) = I8(b'11110000') ! 6
        memory(32) = I8(b'10000000')
        memory(33) = I8(b'11110000') ! 8
        memory(34) = I8(b'10010000')
        memory(35) = I8(b'11110000') ! 9
        memory(36) = I8(b'10010000')
        memory(37) = I8(b'11110000') ! 3
        memory(38) = I8(b'00010000')
        memory(39) = I8(b'11110000')
        memory(40) = I8(b'00010000')
        memory(41) = I8(b'11110000') ! A
        memory(42) = I8(b'10010000')
        memory(43) = I8(b'11110000') ! 0
        memory(44) = I8(b'10010000')
        memory(45) = I8(b'10010000') ! 4
        memory(46) = I8(b'10010000')
        memory(47) = I8(b'11110000')
        memory(48) = I8(b'00010000')
        memory(49) = I8(b'00010000')

    end subroutine

    subroutine loadgame(game)
        character(len=*), intent(in) :: game

        integer :: i, stat, game_unit
        integer(int8) :: b1, b2

        open(newunit=game_unit, &
             file=game, &
             form="unformatted", &
             access="stream", &
             action="read", &
             status="old")

        ! The first 512 bytes, from z'000' to z'1ff', were used for the 
        ! original interpreter, and hence should not be used by programs.
        ! Most programs start at location z'200'

        i = 512  ! z'200'
        do
            read(game_unit,iostat=stat) memory(i:i+1)
            if (is_iostat_end(stat)) exit
            print '(I4,2X,Z4,2X,2Z2)', i - 512, i, memory(i:i+1)
            i = i + 2

            if (i > 4095) then
                error stop "FATAL ERROR: Cartridge exceeds available memory."
            end if

        end do

        close(game_unit)

    end subroutine

    subroutine print_state()

        character(len=*), parameter :: fmt = '(A,Z2,A,Z2,A,Z2,A,Z2)'

        write(*,*)
        write(*,'(A)') "---------------- Registers ---------------"
        write(*,fmt) 'V0: ', V(0), ' | V4: ', V(4), ' | V8: ', V(8),  ' | VC:', V(12)
        write(*,fmt) 'V1: ', V(1), ' | V5: ', V(5), ' | V9: ', V(9),  ' | VD:', V(13)
        write(*,fmt) 'V2: ', V(2), ' | V6: ', V(6), ' | VA: ', V(10), ' | VE:', V(14)
        write(*,fmt) 'V3: ', V(3), ' | V7: ', V(7), ' | VB: ', V(11), ' | VF:', V(15)

        write(*,*)
        write(*,'(A,2X,I0,2X,Z4)') "PC:", pc, pc
        write(*,*)

    end subroutine

   function fetch_opcode() result(opcode)
      integer(int16) :: opcode
      opcode = transfer( &
         achar(memory(pc+1)) // achar(memory(pc)), &
         opcode)
   end function

   integer(int8) function readx(op)
      integer(int16), intent(in), target :: op
      integer(int8) :: op8(2)
      op8 = transfer(op,op8)
      readx = iand(op8(2),I8(b'00001111'))
   end function

   integer(int8) function ready(op)
      integer(int16), intent(in), target :: op
      integer(int8) :: op8(2)
      op8 = transfer(shiftr(op,4),op8)
      ready = iand(op8(1),I8(b'00001111'))
   end function

   subroutine vexec(win)
      integer, intent(inout) :: win

      integer(int16) :: opcode

      integer(int8) :: x, y, n
      integer(int16) :: nnn, kk

      integer(int16), parameter :: CLS_ = int(z'00E0')
      integer(int16), parameter :: RET_ = int(z'00EE')

      character(len=4) :: vxstr

      opcode = fetch_opcode()

      print '("PC: ",Z4," opcode: ",Z4)', pc, opcode

      x = readx(opcode)
      y = ready(opcode)

      n   = iand(opcode,int(z'000F',int16)) ! the lowest 4 bits
      kk  = iand(opcode,int(z'00FF',int16)) ! the lowest 8 bits
      nnn = iand(opcode,int(z'0FFF',int16)) ! the lowest 12 bits

      select case(iand(opcode,int(z'F000',int16)))
      case(int(z'0000',int16))
         select case(kk)
         case (CLS_)
            print *, "Clear the screen"
            call clear_window(win)
            screen = .false.
            pc = pc + 2
         case (RET_)
            print *, "Return from subroutine"
            sp = sp - 1
            pc = stack(sp)
         case default
            call unknown_opcode(opcode)
         end select
      case(int(z'1000',int16)) ! 1nnn: jump to address nnn

         print '(A,Z4)', "Jump to address ", nnn
         pc = nnn
         !print '(A,Z4,A,I0)', "jumping to address (hex) ", nnn, " (dec) ", nnn

      case(int(z'2000',int16)) ! 2nnn: call address nnn
            stack(sp) = int(pc + 2, int16)
            sp = sp + 1
            pc = nnn
      case(int(z'3000',int16)) ! 3xkk: skip next instr if V(x) == kk
            if (V(x) == kk) then
                pc = pc + 4
            else
                pc = pc + 2
            end if
      case(int(z'4000',int16)) ! 4xkk: skip next instr if V(x) /= kk
            if (V(x) /= kk) then
                pc = pc + 4
            else
                pc = pc + 2
            end if
      case(int(z'5000',int16)) ! 5xy0: skip next instr if V(x) == V(y)
            if (V(x) == V(y)) then
               pc = pc + 4
            else
               pc = pc + 2
            end if
      case(int(z'6000',int16)) ! 6xkk; set V(x) = kk
            print '("Set v[",Z4,"] = ",Z4)', x, kk
            v(x) = I8(kk)
            pc = pc + 2
      case(int(z'7000',int16)) ! 7xkk; set V(x) = V(x) + kk
            print '("Set v[",Z4,"] += ",Z4)', x, kk
            v(x) = v(x) + I8(kk)
            pc = pc + 2
      case(I16(z'8000'))
         call op8switch(n,V(x),V(y),V(15))
         pc = pc + 2
      case(int(z'9000',int16)) ! 9xy0: skip the next instruction is V(x) does not equal V(y)
         if (V(x) /= V(y)) then
            pc = pc + 4
         else
            pc = pc + 2
         end if
      case(int(z'A000',int16)) ! Annn: set I to address nnn
         write(*,'(A,Z4)') "Set I to ", nnn
         I = nnn
         pc = pc + 2
      case(int(z'B000',int16)) ! Bnnn: jump to location nnn + V(0)
         pc = nnn + V(0)
      case(int(z'C000',int16)) ! Cxkk: V(x) = random byte AND kk
         V(x) = iand(rand(),I8(kk))
         pc = pc + 2
      case(int(z'D000',int16)) ! Dxyn: display an n-byte sprite starting at memory
                                 !       location I at (Vx, Vy) on the screen, VF = collision
         print '(A,Z4,2X,Z4,A,I2)', "Draw sprite at ",v(x),v(y)," of height ", n

         call draw_sprite(v(x),v(y),n,win)
         pc = pc + 2
      case(I16(z'E000'))
         call unknown_opcode(opcode)
      case(I16(z'F000')) ! F...
         print '(A,I0,2X,Z4)', "Opcode F... ", opcode, opcode
         select case(kk)
         case(I16(z'07'))
            V(x) = delay_timer
            pc = pc + 2
         case(I16(z'11'))
            call wait_for_keypress(keypad)
            pc = pc + 2
         case(I16(z'15'))
            delay_timer = V(x)
            pc = pc + 2
         case(I16(z'18'))
            sound_timer = V(x)
            pc = pc + 2
         case(I16(z'1E'))
            I = I + V(x)
            pc = pc + 2
         case(I16(z'29'))
            ! FX29: Set I to location of sprite for
            !       the charater in V(X)
            I = fontidx(V(x))
            pc = pc + 2
         case(I16(z'33'))
            ! FX33: Decode VX into binary-coded decimal
            write(vxstr,'(I0.4)') transfer(V(x),1_Int16)
            memory(I)   = index('0123456789ABCDEF',vxstr(2:2)) - 1
            memory(I+1) = index('0123456789ABCDEF',vxstr(3:3)) - 1
            memory(I+2) = index('0123456789ABCDEF',vxstr(4:4)) - 1
            pc = pc + 2
         case(I16(z'55'))
            ! FX55: Register dump
            memory(I:I+x) = V(0:x)
            I = I + x + 1
            pc = pc + 2
         case(I16(z'65'))
            ! FX65: Register load
            V(0:x) = memory(I:I+x)
            I = I + x + 1
            pc = pc + 2
         case default
            call unknown_opcode(opcode)
         end select
      case default
         !> Should not be here
         call unknown_opcode(opcode)
      end select

      call flush_screen(win)
      !call print_state()

   end subroutine

   subroutine op8switch(n,Vx,Vy,VF)
      integer(Int8), intent(in) :: n
      integer(Int8), intent(inout) :: Vx, VF
      integer(Int8), intent(in) :: Vy

      interface
         subroutine op8xy5(Vx,Vy,Vf) bind(c,name="op8xy5")
            use, intrinsic :: iso_c_binding, only: c_int8_t
            integer(c_int8_t) :: Vx, Vy
            integer(c_int8_t) :: Vf
         end subroutine
      end interface

      select case(n)
      case(I8(z'0'))
         ! 8xy0
         Vx = Vy
      case(I8(z'1'))
         ! 8xy1
         Vx = ior(Vx,Vy)
      case(I8(z'2'))
         ! 8xy2
         Vx = iand(Vx,Vy)
      case(I8(z'3'))
         ! 8xy3
         Vx = ieor(Vx,Vy)
      case(I8(z'4'))
         ! 8xy4
         block
            integer(Int16) :: sxy, bx, by
            bx = transfer(vx,bx)
            by = transfer(vy,by)
            bx = iand(bx,I16(z'00FF'))
            by = iand(by,I16(z'00FF'))
            sxy = bx + by
            if (sxy > 255) then
               VF = 1
            else
               VF = 0
            end if
            Vx = Vx + Vy
         end block
      case(I8(z'5'))
         !8XY5: SUB Vx Vy
         print '(A,2X,B0.16,2X,B0.16)', "before = ", int(Vx,1_Int16), int(Vy,1_Int16)


         block
            integer(Int16) :: bx, by
            bx = transfer(vx,bx)
            by = transfer(vy,by)

            bx = iand(bx,I16(z'00FF'))
            by = iand(by,I16(z'00FF'))

            !bx = shiftr(bx,24)
            !by = shiftr(by,24)

            write(*,'(A,B0.16,2X,B0.8)') "bx      = ", bx, vx
            write(*,'(A,B0.16,2X,B0.8)') "by      = ", by, vy

            if (bx > by) then
               vf = 1
            else
               vf = 0
            end if

            write(*,'(A,B0.16,2X,B0.8)') "bx - by = ", bx - by, vx - vy
            vx = vx + (-vy)

         end block

         print '(A,2X,I0,2X,I0)', "after = ", Vx, Vf
      case(I8(z'6'))
         
         write(11,'(A,I0,2X,I0)') "Before = ", Vx, Vf
         ! 8XY6: SHR Vx
         VF = iand(Vx,I8(b'00000001'))
         Vx = shiftr(Vx,1_Int8)

         write(11,'(A,I0,2X,I0)') "After  = ", Vx, Vf
         flush(11)

      case(I8(z'7'))
         ! 8XY7: SUBN Vx Vy
         if (Vy > Vx) then
            VF = 1
         else
            VF = 0
         end if
         Vx = Vy - Vx
      case(I8(z'E'))
         ! 8XYE
         VF = iand(shiftr(Vx,7_Int8),I8(z'1'))
         Vx = shiftl(Vx,1_Int8)
      case default
         call unknown_opcode(8_Int16)
      end select

   end subroutine

   pure function iadd(a,b) result(ia)
      integer(Int8), intent(in) :: a, b
      integer(Int8) :: carry, ib, ia
      ia = a
      ib = b
      do while(ib /= 0_Int8)
         carry = iand(ia,ib)
         ia = ieor(ia,ib)
         ib = shiftl(carry,1_Int8)
      end do
   end function

   pure function iadd16(a,b) result(ia)
      integer(Int16), intent(in) :: a, b
      integer(Int16) :: carry, ib, ia
      ia = a
      ib = b
      do while(ib /= 0_Int16)
         carry = iand(ia,ib)
         ia = ieor(ia,ib)
         ib = shiftl(carry,1_Int16)
      end do
   end function

   subroutine wait_for_keypress(keypad)
      logical, intent(inout) :: keypad(0:15)

      error stop
   end subroutine

    ! subroutine tick()
    !     if (delay_timer > 0) then
    !         delay_timer = delay_timer - 1
    !     end if
    !     if (sound_timer > 0) then
    !         sound_timer = sound_timer - 1
    !         if (sound_timer == 0) then
    !             print *, "BEEP"
    !         end if
    !     end if
    ! end subroutine

   subroutine unknown_opcode(opcode)
      integer(int16), intent(in) :: opcode
      write(*,'(A,Z4)') "FATAL ERROR: Unknown opcode = ", opcode
      pause
      stop
   end subroutine

   ! Draws a sprite at coordinate (x,y) that has a width of 8 pixels
   ! and a height of N pixels. Each row of 8 pixels is read as bit-coded
   ! starting from the memory location I; the I value doesn't change after
   ! execution of this instruction. V(z'F') is set to 1 if any screen pixels
   ! are flipped from set to unset when the sprite is drawn, and to 0 if
   ! that doesn't happen.
   !
   subroutine draw_sprite(vx,vy,n,win)
      integer(int8), intent(in) :: vx, vy, n
      integer, intent(in) :: win

      logical :: sprite(0:n-1,0:7), collide
      integer :: row, col, sx, sy, zx, zy 

      integer, parameter :: coll_reg = int(z'F')

      v(coll_reg) = 0 

      do col = 0, 7
         do row = 0, n-1
            sprite(row,7 - col) = btest(memory(I + row),col)
         end do
      end do

      print *, "Drawn sprite at address I = ", I

      do row = 0, n-1
         do col = 0, 7
            zx = vx + col
            zy = vy + row

            collide = sprite(row,col) .eqv. screen(zx,zy)
            if (collide) v(coll_reg) = 1
            screen(zx,zy) = .not. collide
         end do
      end do
   end subroutine

   !> Flush the Display Pixels
   !>
   subroutine flush_screen(win)

      integer, intent(in) :: win
      integer :: zx, zy
      real :: wx, wy
      integer, parameter :: MF = 8

      call clear_window(win)

      do zy = 0, 31
         do zx = 0, 63
            if (screen(zx,zy)) then
               wx = MF*(zx)
               wy = MF*(31 - zy)
               call fillrect(win,wx,wy,real(MF),real(MF))
            end if
         end do
      end do

     call copylayer(win,1,0)
     call msleep(10)

   end subroutine

end module




program main
    
    use, intrinsic :: iso_c_binding, only: c_null_char
    
    use procall
    use chip8
    
    implicit none

    character(len=*), parameter :: WIN_TITLE  = 'CHIP-8 Interpreter' // c_null_char
    character(len=*), parameter :: BG_COLOR  = 'black' // c_null_char

    integer, parameter :: MF = 8

    integer,          parameter :: WIN_WIDTH  = 64 * MF
    integer,          parameter :: WIN_HEIGHT = 32 * MF
    integer,          parameter :: MAX_ITER   = 256

    integer :: key, win

    character(len=256) :: filename
    integer :: nargs, rom_unit, byte
    integer(int16) :: inst
    logical :: rom_exists

    call initmem()

    nargs = command_argument_count()
    if (nargs /= 1) then
        print *, "Wrong number of arguments."
        print *, "Usage:"
        print *, "   chip8 <filename>"
        stop
    end if

    call get_command_argument(1,filename)
    print *, filename

    inquire(file=filename,exist=rom_exists)
    if (.not. rom_exists) then
        print *, "Cartridge " // trim(filename) // " doesn't exist"
        stop
    end if

    call loadgame(filename)

    call gopen(WIN_WIDTH, WIN_HEIGHT, win)  ! Open X11 window.
    call winname(win, WIN_TITLE)            ! Set window title.

    call newpencolor(win,1)
    
    call layer(win,0,1)
    call gsetbgcolor(win, BG_COLOR)         ! Set background colour.

    open(11,file="debug.txt")

    do
        call vexec(win)
    end do

    close(11)

    call ggetch(key)
    call gcloseall()

end program main