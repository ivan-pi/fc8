
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
public :: timers

integer(int8) :: memory(0:4095) = 0
integer(int8) :: V(0:15) = 0

integer :: I  = 0
integer :: PC = 512   ! z'200'

integer :: sp = 1
integer(int16) :: stack(16) = 0

integer :: delay_timer = 0
integer :: sound_timer = 0

logical :: keypad(0:15) = .false.

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

      logical :: collision
      character(len=4) :: vxstr

      opcode = fetch_opcode()

      print '("PC: ",Z4," opcode: ",Z0.4)', pc, opcode

      x = readx(opcode) ! A value from 0 to F
      y = ready(opcode) ! A value from 0 to F

      n   = I8(iand(opcode,int(z'000F',int16))) ! the lowest 4 bits
      kk  = iand(opcode,int(z'00FF',int16)) ! the lowest 8 bits
      nnn = iand(opcode,int(z'0FFF',int16)) ! the lowest 12 bits

      select case(iand(opcode,int(z'F000',int16)))
      case(int(z'0000',int16))
         select case(kk)
         case (CLS_)
            print *, "Clear the screen"
            screen = .false.
            call clear_window(win)
            pc = pc + 2
         case (RET_)
            print *, "Return from subroutine"
            sp = sp - 1
            pc = stack(sp)
         case default
            ! The 0NNN opcode (machine language subroutine) lands here
            call unknown_opcode(opcode)
         end select
      case(int(z'1000',int16)) ! 1nnn: jump to address nnn

         print '(A,Z4)', "Jump to address ", nnn
         pc = nnn
         !print '(A,Z4,A,I0)', "jumping to address (hex) ", nnn, " (dec) ", nnn

      case(int(z'2000',int16)) ! 2nnn: call address nnn
            stack(sp) = I16(pc + 2)
            sp = sp + 1
            pc = nnn
      case(int(z'3000',int16)) ! 3xkk: skip next instr if V(x) == kk
            if (as_uint(V(x)) == kk) then
                pc = pc + 4
            else
                pc = pc + 2
            end if
      case(int(z'4000',int16)) ! 4xkk: skip next instr if V(x) /= kk
            print '("Skip next instruction if v[",I0,"] = ",Z4," /= ",Z4)', x, V(x), kk
            if (as_uint(V(x)) /= kk) then
               print '(B16)', V(x)
               print '(B16)', kk
               print *, "Result = .true. (V(x) /= kk)"
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
         !if (x == 15_int8) then
         !   error stop "Register collision"
         !end if

         ! We need to define what happens first, the register update
         ! or the status flag

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
         pc = nnn + as_uint(V(0))
         !pc = nnn + V(0)
      case(int(z'C000',int16)) ! Cxkk: V(x) = random byte AND kk
         V(x) = iand(rand(),I8(kk))
         pc = pc + 2
      case(int(z'D000',int16)) ! Dxyn: display an n-byte sprite starting at memory
                                 !       location I at (Vx, Vy) on the screen, VF = collision
         print '(A,Z4,2X,Z4,A,I2)', "Draw sprite at ",v(x),v(y)," of height ", n
         call draw_sprite(v(x),v(y),n,win,collision)
         if (collision) then
            v(15) = 1
         else
            v(15) = 0
         end if

         pc = pc + 2
      case(I16(z'E000'))
         select case(kk)
         case(I16(z'009E'))
           ! TODO: Check VX range in 0-15
           if (keypad(V(x))) then
              pc = pc + 4
           else
              pc = pc + 2
           end if
         case(I16(z'00A1'))
           ! TODO: Check VX range in 0-15
           if (.not. keypad(V(x))) then
              pc = pc + 4
           else
              pc = pc + 2
           end if
         case default
          print *, "IMPOSSIBLE"
           call unknown_opcode(opcode)
         end select
      case(I16(z'F000')) ! F...
         select case(kk)
         case(I16(z'07'))
            V(x) = I8(delay_timer)
            pc = pc + 2
         case(I16(z'0A'))
            call wait_for_keypress(keypad)
            V(x) = findloc(keypad,.true.,1) - 1
            print *, "FX0A V(x) = ", V(x)
            pc = pc + 2
         case(I16(z'15'))
            delay_timer = as_uint(V(x))
            pc = pc + 2
         case(I16(z'18'))
            sound_timer = as_uint(V(x))
            pc = pc + 2
         case(I16(z'1E'))
            !I = I + V(x)
            I = I + as_uint(V(x))
            pc = pc + 2
         case(I16(z'29'))
            ! FX29: Set I to location of sprite for
            !       the character in V(X)
            ! TODO: Check V(x) in range 0 - 15
            I = fontidx(V(x))
            pc = pc + 2
         case(I16(z'33'))
            ! FX33: Decode VX into binary-coded decimal
            ! TODO: MAke 
            write(vxstr,'(I0.4)') as_uint(V(x))
            memory(I)   = I8(index('0123456789ABCDEF',vxstr(2:2)) - 1)
            memory(I+1) = I8(index('0123456789ABCDEF',vxstr(3:3)) - 1)
            memory(I+2) = I8(index('0123456789ABCDEF',vxstr(4:4)) - 1)
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

   end subroutine

   function as_uint(x) result(y)
      integer(int8), intent(in), value :: x
      integer(int16) :: y
      y = iand(transfer(x,y),I16(z'00FF'))
   end function

   subroutine op8switch(N,VX,VY,VF)
      integer(Int8), intent(in) :: N
      integer(Int8), intent(inout) :: VX, VF
      integer(Int8), intent(in), value :: VY

      ! VF can potentially alias Vy, which can be problematic...
      ! Perhaps best to set a status flag, and then update 
      ! the register outside of this subroutine
      !
      ! We use "value" to prevent this
      !
      ! V(x) shouldn't be equal to VF

      interface
         subroutine op8xy5(Vx,Vy,Vf) bind(c,name="op8xy5")
            use, intrinsic :: iso_c_binding, only: c_int8_t
            integer(c_int8_t) :: Vx, Vy
            integer(c_int8_t) :: Vf
         end subroutine
      end interface

      logical :: borrow

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
         borrow = (as_uint(vx) + as_uint(vy)) > I16(z'FF')
         Vx = Vx + Vy
         if (borrow) then
            VF = 1
         else
            VF = 0
         end if
      case(I8(z'5'))
         borrow = as_uint(Vx) >= as_uint(Vy)
         Vx = Vx - Vy
         if (borrow) then
            VF = 1
         else
            VF = 0
         end if
      case(I8(z'6'))
         ! 8XY6: SHR Vx, Vy
         !Vx = Vy
         Vx = shiftr(Vy,1_Int8)
         VF = iand(Vy,I8(b'00000001'))
      case(I8(z'7'))
         ! 8XY7: SUBN Vx Vy
         borrow = as_uint(Vy) >= as_uint(Vx)
         Vx = Vy - Vx
         if (borrow) then
            VF = 1
         else
            VF = 0
         end if
      case(I8(z'E'))
         ! 8XYE: SHL Vx, Vy
         !Vx = Vy
         Vx = shiftl(Vy,1_Int8)
         VF = iand(shiftr(Vy,7_Int8),I8(b'00000001'))
      case default
         call unknown_opcode(8_Int16)
      end select

   end subroutine

   subroutine wait_for_keypress(keypad)
      logical, intent(inout) :: keypad(0:15)
      integer :: key
                                            !   ProCall          Hex  Keyboard
      integer, parameter :: keymap(0:15) = [  INT(z'78'), &    !  0      x
                                              INT(z'31'), &    !  1      1
                                              INT(z'32'), &    !  2      2
                                              INT(z'33'), &    !  3      3
                                              INT(z'71'), &    !  4      q
                                              INT(z'77'), &    !  5      w
                                              INT(z'65'), &    !  6      e
                                              INT(z'61'), &    !  7      a
                                              INT(z'73'), &    !  8      s
                                              INT(z'64'), &    !  9      d
                                              INT(z'79'), &    !  A      y
                                              INT(z'63'), &    !  B      c
                                              INT(z'34'), &    !  C      4
                                              INT(z'72'), &    !  D      r
                                              INT(z'66'), &    !  E      f
                                              INT(z'76')]      !  F      v
      do
        call ggetch(key)
        if (any(key == keymap)) then
          where(key == keymap) keypad = .true.
          return
        endif
      end do

   end subroutine

   subroutine timers()
      if (delay_timer > 0) delay_timer = delay_timer - 1
    !     if (sound_timer > 0) then
    !         sound_timer = sound_timer - 1
    !         if (sound_timer == 0) then
    !             print *, "BEEP"
    !         end if
    !     end if
   end subroutine


   subroutine unknown_opcode(opcode)
      integer(int16), intent(in) :: opcode
      write(*,'(A,Z4)') "FATAL ERROR: Unknown opcode = ", opcode
      stop
   end subroutine

   ! Draws a sprite at coordinate (x,y) that has a width of 8 pixels
   ! and a height of N pixels. Each row of 8 pixels is read as bit-coded
   ! starting from the memory location I; the I value doesn't change after
   ! execution of this instruction. V(z'F') is set to 1 if any screen pixels
   ! are flipped from set to unset when the sprite is drawn, and to 0 if
   ! that doesn't happen.
   !
   subroutine draw_sprite(vx,vy,n,win,collision)
      integer(int8), intent(in) :: vx, vy, n
      integer, intent(in) :: win
      logical, intent(out) :: collision

      logical :: sprite(0:n-1,0:7), collide
      integer :: row, col, sx, sy, zx, zy 

      collision = .false.

      do col = 0, 7
         do row = 0, n-1
            sprite(row,7 - col) = btest(memory(I + row),col)
         end do
      end do

#if DEBUG
      print *, "Drawn sprite at address I = ", I
#endif

      do row = 0, n-1
         do col = 0, 7
            zx = vx + col
            zy = vy + row

            ! TODO: handle negative vx and vy
            zx = mod(zx,64)
            zy = mod(zy,32)

            collide = sprite(row,col) .eqv. screen(zx,zy)
            if (screen(zx,zy)) then
              if (collide) collision = .true.
            end if
            screen(zx,zy) = .not. collide
         end do
      end do

      call flush_screen(win)

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
     call msleep(5)

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

    integer(8) :: delta, tprev, tcurr, trate, tstep

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

    call gsetnonblock(0)

   call system_clock(tprev,trate)
   tstep = nint(trate/60.d0)
   !print *, "tstep = ", tstep

   do
      call vexec(win)

      call system_clock(tcurr)
      delta = tcurr - tprev
      if (delta > tstep) then
         call timers()
         tprev = tcurr
      end if

   end do

   call gcloseall()

end program main