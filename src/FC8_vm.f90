!> FC8 virtual machine module
!>
!> This module contains the internals of the CHIP-8 interpreter or
!> virtual machine. Currently there is some overlap with the graphics
!> subroutines, which should be isolated/separated better in a future
!> version.
!>
module FC8_vm

use procall, only: clear_window => gclr, copylayer, msleep

use, intrinsic :: iso_fortran_env, only: int8, int16
implicit none
private

public :: int8, int16
public :: loadgame, memory
public :: initmem, vexec
public :: timers

!> Kind parameter for the memory and variables/registers
integer, parameter :: byte = int8

!> Kind parameter for instructions (twice the storage size of byte)
integer, parameter :: instr = int16

! The use of int8 (albeit a signed integer) gives us the greatest
! ease of addressing memory. If `int8` is unavailable, e.g. in a
! processor preceding Fortran 2008, a workaround is needed (perhaps
! using shifting).

!> CHIP-8 Random Access Memory (RAM)
integer(byte) :: memory(0:4095) = 0

! The first 512 bytes or locations z'000'-z'1FF' (inclusive) were reserved 
! or use by the original interpreter. Hence CHIP-8 games must be loaded 
! starting at address z'200'.
!
! Originally, the last 352 bytes of on-card RAM were used for variables
! and display refresh. In a 4096-byte system, this would leave the 
! locations z'200'-z'E8F' for the user. This area is equal to 1608 CHIP-8
! instructions (3216 bytes)

!> CHIP-8 Variables/Registers
integer(byte) :: V(0:15) = 0

!> Kind parameter used to represent addresses
integer, parameter :: addr = kind(0)

! The address kind should have sufficient range to reference the RAM.
! Since Fortran 2008, the default integer kind (`kind(0)`) shall have 
! a decimal exponent range of at least 5, i.e. -10^5 (exclusive) to 10^5 
! (exclusive), meaning it would suffice for up to 100 kilobytes of memory.
!
! Alternative choices for the address kind are:
!     int16:                -32768 to 32767
!     selected_int_kind(4): -10^4 (exclusive) to 10^4 (exclusive)
!     c_int:                atleast -32768 to 32767 according to the C standard
! 
! For ease of use we pick the default integer type.

!> Memory pointer
integer(addr) :: I  = 0

! The memory pointer is used to specify any location in RAM.
! It can be manipulated by certain CHIP-8 instructions.

!> Program counter (PC)
integer(addr) :: pc = 512   ! z'200'

!> Stack pointer
integer :: sp = 1
!> Function return stack
integer(addr) :: stack(12) = 0

! In the original implementation, the return stack would occupy 48-bytes
! allowing for a maximum of 12 levels of subroutine nesting. The stack
! would be located in the penultimate RAM page.
!
! An integer type with the same range as a 12-bit unsigned integer,
! z'000' to z'FFF' (inclusive) would be sufficient to capture the return 
! addresses of the return stack. For simplicity we use the default
! integer type.

!> CHIP-8 Delay Timer (DT)
integer(byte) :: delay_timer = 0

!> CHIP-8 Sound Timer / Tone Duration
integer(byte) :: sound_timer = 0

! The timers share the same type as the variables V(:)
! as the only means of manipulating the timers is using
! the following instructions:
! 
!   FX07 ... Let VX = current delay timer value
!   FX15 ... Set delay timer = VX
!   FX18 ... Set tone duration = VX

logical :: keypad(0:15) = .false.


!> Screen buffer (black and white pixels)
!> TODO: Should this be flipped 32 rows by 64 columns?
logical :: screen(0:63,0:31) = .false.

!
! Font
! 

!> The height of the font sprites (the number of bytes to be drawn)
integer, parameter :: FONT_HEIGHT = 5
!> Lookup table for the hexadecimal character sprites.
integer :: fontidx(0:15)

! The value at element i, points to the sprite data for
! the hex digit represented by i, which in our case is stored 
! n the area of memory, reserved by the original interpreter.
! The font-map is initizialized by the `initmem` routine.

interface
   !> Return a random byte (implemented in C for simplicity)
   function rand_byte() bind(c,name="randint8")
      ! The return integer type must be storage-compatible with 
      ! the `byte` of the CHIP-8 variables.
      use, intrinsic :: iso_c_binding, only: c_int8_t
      integer(c_int8_t) :: rand_byte
   end function

   ! TODO: the rand_byte function should be replaced with an integer
   !       PRNG in Fortran to avoid use of the C run-time library.
end interface

interface asuint
   module procedure asuint_int16
   module procedure asuint_default
end interface

contains

   pure function asuint_int16(x,res) result(y)
      integer(byte), intent(in), value :: x
      integer(int16), intent(in), value :: res
      integer(kind(res)) :: y
      y = iand(transfer(x,y),int(z'FF',kind(res)))
   end function
   pure function asuint_default(x,res) result(y)
      integer(byte), intent(in), value :: x
      integer(kind(0)), intent(in), value :: res
      integer(kind(res)) :: y
      y = iand(transfer(x,y),int(z'FF',kind(res)))
   end function

   pure function asaddr(x) result(y)
      integer(byte), intent(in), value :: x
      integer(addr) :: y
      y = iand(transfer(x,y),int(z'FF',addr))
   end function

   !> Return the lower hex digit of a byte, by masking the upper bits.
   !>
   !> This function in used to access the key-pressesö
   pure function ashexdigit(x) result(y)
      integer(byte), intent(in), value :: x
      integer :: y
      y = iand(transfer(x,y),int(z'0F',addr))
   end function

   !> Initializes the CHIP-8 interpreter memory
   subroutine initmem()

      ! TODO: think about calling this within `load_game`

      ! TODO: optionally, seed random number generator
      !       to provide determinism when debugging

      ! TODO: factor this out as an include file for easy
      !       compile time font replacement

      !
      ! Initialize font lookup table 
      !
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

      !
      ! Initialize font; each character is 5 bytes
      !
      memory( 0) = int( b'11110000', byte ) ! E
      memory( 1) = int( b'10000000', byte )
      memory( 2) = int( b'11110000', byte ) ! F
      memory( 3) = int( b'10000000', byte )
      memory( 4) = int( b'11110000', byte ) ! C
      memory( 5) = int( b'10000000', byte )
      memory( 6) = int( b'10000000', byte )
      memory( 7) = int( b'10000000', byte )
      memory( 8) = int( b'11110000', byte ) ! 7 
      memory( 9) = int( b'00010000', byte )
      memory(10) = int( b'00100000', byte )
      memory(11) = int( b'01000000', byte )
      memory(12) = int( b'01000000', byte )
      memory(13) = int( b'00100000', byte ) ! 1
      memory(14) = int( b'01100000', byte )
      memory(15) = int( b'00100000', byte )
      memory(16) = int( b'00100000', byte )
      memory(17) = int( b'01110000', byte )
      memory(18) = int( b'11100000', byte ) ! B
      memory(19) = int( b'10010000', byte )
      memory(20) = int( b'11100000', byte )
      memory(21) = int( b'10010000', byte )
      memory(22) = int( b'11100000', byte ) ! D
      memory(23) = int( b'10010000', byte )
      memory(24) = int( b'10010000', byte )
      memory(25) = int( b'10010000', byte )
      memory(26) = int( b'11100000', byte )
      memory(27) = int( b'11110000', byte ) ! 5
      memory(28) = int( b'10000000', byte )
      memory(29) = int( b'11110000', byte ) ! 2
      memory(30) = int( b'00010000', byte )
      memory(31) = int( b'11110000', byte ) ! 6
      memory(32) = int( b'10000000', byte )
      memory(33) = int( b'11110000', byte ) ! 8
      memory(34) = int( b'10010000', byte )
      memory(35) = int( b'11110000', byte ) ! 9
      memory(36) = int( b'10010000', byte )
      memory(37) = int( b'11110000', byte ) ! 3
      memory(38) = int( b'00010000', byte )
      memory(39) = int( b'11110000', byte )
      memory(40) = int( b'00010000', byte )
      memory(41) = int( b'11110000', byte ) ! A
      memory(42) = int( b'10010000', byte )
      memory(43) = int( b'11110000', byte ) ! 0
      memory(44) = int( b'10010000', byte )
      memory(45) = int( b'10010000', byte ) ! 4
      memory(46) = int( b'10010000', byte )
      memory(47) = int( b'11110000', byte )
      memory(48) = int( b'00010000', byte )
      memory(49) = int( b'00010000', byte )

   end subroutine

   !> Load a user-provided game cartridge or "ROM"
   subroutine loadgame(game)
      character(len=*), intent(in) :: game

      integer :: i, stat, game_unit

      open(newunit=game_unit, &
           file=game, &
           form="unformatted", &
           access="stream", &
           action="read", &
           status="old")

      ! The first 512 bytes, from z'000' to z'1FF', were used for the 
      ! original interpreter, and hence should not be used by programs.
      ! Programs start at z'200'.

      i = 512  ! z'200'
      do
         ! We read two bytes per loop iteration.

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

   !> Fetch an instruction/opcode from the memory, depending on the
   !> location of the program counter.
   !>
   !> The instructions are big-endian encodes, meaning
   !> a byte-swap is required.
   !>
   pure function fetch_opcode() result(opcode)
      integer(instr) :: opcode
      opcode = transfer([memory(pc+1),memory(pc)],opcode)
   end function

   !> Read x-variable index 
   !> (guaranteed to be in the range 0-15 due to masking)
   pure function readx(op)
      integer(instr), intent(in), target :: op
      integer(byte) :: b(2), readx
      b = transfer(op,b)
      readx = iand(b(2),int(b'00001111',byte))
   end function
   !> Read y-variable index 
   !> (guaranteed to be in the range 0-15 due to masking)
   pure function ready(op)
      integer(instr), intent(in), target :: op
      integer(byte) :: b(2), ready
      b = transfer(shiftr(op,4),b)
      ready = iand(b(1),int(b'00001111',byte))
   end function

   !> Helper function to skip an instruction by incrementing the
   !> program counter
   subroutine skip_if(expr)
      logical, intent(in) :: expr
      if (expr) then
         pc = pc + 4
      else
         pc = pc + 2
      end if
   end subroutine

   !> Virtual machine execution
   subroutine vexec(win)
      integer, intent(inout) :: win

      integer(instr) :: opcode

      integer(byte) :: x, y, n, kk
      integer(instr) :: nnn

      ! Select instruction
      integer(instr) :: D

      ! Select instruction masks 
      integer(instr), parameter :: D0 = int(z'0000',instr)
      integer(instr), parameter :: D1 = int(z'1000',instr)
      integer(instr), parameter :: D2 = int(z'2000',instr)
      integer(instr), parameter :: D3 = int(z'3000',instr)
      integer(instr), parameter :: D4 = int(z'4000',instr)
      integer(instr), parameter :: D5 = int(z'5000',instr)
      integer(instr), parameter :: D6 = int(z'6000',instr)
      integer(instr), parameter :: D7 = int(z'7000',instr)
      integer(instr), parameter :: D8 = int(z'8000',instr)
      integer(instr), parameter :: D9 = int(z'9000',instr)
      integer(instr), parameter :: DA = int(z'A000',instr)
      integer(instr), parameter :: DB = int(z'B000',instr)
      integer(instr), parameter :: DC = int(z'C000',instr)
      integer(instr), parameter :: DD = int(z'D000',instr)
      integer(instr), parameter :: DE = int(z'E000',instr)
      integer(instr), parameter :: DF = int(z'F000',instr)

      ! 00__ instruction masks; same kind as `kk`
      integer(byte), parameter :: D00E0 = int(z'E0',byte)
      integer(byte), parameter :: D00EE = int(z'EE',byte)

      ! EX__ instruction masks; same kind as `kk`
      integer(byte), parameter :: DEX9E = int(z'9E',byte)
      integer(byte), parameter :: DEXA1 = int(z'A1',byte)

      ! FX__ instruction masks; same kind as `kk`
      integer(byte), parameter :: DFX07 = int(z'07',byte)
      integer(byte), parameter :: DFX0A = int(z'0A',byte)
      integer(byte), parameter :: DFX15 = int(z'15',byte)
      integer(byte), parameter :: DFX18 = int(z'18',byte)
      integer(byte), parameter :: DFX1E = int(z'1E',byte)
      integer(byte), parameter :: DFX29 = int(z'29',byte)
      integer(byte), parameter :: DFX33 = int(z'33',byte)
      integer(byte), parameter :: DFX55 = int(z'55',byte)
      integer(byte), parameter :: DFX65 = int(z'65',byte)

      logical :: collision

      opcode = fetch_opcode()

      print '("PC: ",Z4," opcode: ",Z0.4)', pc, opcode

      x = readx(opcode) ! A value from 0 to F
      y = ready(opcode) ! A value from 0 to F

      n   = iand(opcode,int(z'00F',instr)) ! the lowest 4 bits  (0 - 15, index)
      kk  = iand(opcode,int(z'0FF',instr)) ! the lowest 8 bits  (0 - 255, 1-byte value)

      nnn = iand(opcode,int(z'FFF',instr)) ! the lowest 12 bits (0 - 4095, address)

      print '("n: ",Z2," kk: ",Z2)', n, kk

      D = iand(opcode, DF)

      select case ( D )
      case( D0 )
         select case(kk)
         case ( D00E0 )
            print *, "Clear the screen"
            screen = .false.
            call clear_window(win)
            pc = pc + 2
         case ( D00EE )
            print *, "Return from subroutine"
            sp = sp - 1
            pc = stack(sp)
         case default
            ! The 0NNN opcode (machine language subroutine) lands here
            call unknown_opcode(opcode)
         end select
      case( D1 ) ! 1nnn: jump to address nnn

         print '(A,Z4)', "Jump to address ", nnn
         pc = nnn
         !print '(A,Z4,A,I0)', "jumping to address (hex) ", nnn, " (dec) ", nnn

      case( D2 ) ! 2nnn: call address nnn
         stack(sp) = pc + 2
         sp = sp + 1
         pc = nnn
      case( D3 ) ! 3xkk: skip next instr if V(x) == kk
         !if (V(x) == kk) then
         !   pc = pc + 4
         !else
         !   pc = pc + 2
         !end if
         call skip_if( V(x) == kk )
      case( D4 ) ! 4xkk: skip next instr if V(x) /= kk
         !if (V(x) /= kk) then
         !   pc = pc + 4
         !else
         !   pc = pc + 2
         !end if
         call skip_if( V(x) /= kk )
      case( D5 ) ! 5xy0: skip next instr if V(x) == V(y)
         call skip_if( V(x) == V(y) )
      case( D6 ) ! 6xkk; set V(x) = kk
         print '("Set v[",Z4,"] = ",Z4)', x, kk
         v(x) = kk
         pc = pc + 2
      case( D7 ) ! 7xkk; set V(x) = V(x) + kk
         print '("Set v[",Z4,"] += ",Z4)', x, kk
         v(x) = v(x) + kk
         pc = pc + 2
      case( D8 )
         block
            integer(byte) :: VF
            call D8XYN(n,V(x),V(y),VF)
            V(15) = VF
            pc = pc + 2
         end block
      case( D9 ) ! 9xy0: skip the next instruction is V(x) does not equal V(y)
         call skip_if(V(x) /= V(y))
      case( DA ) ! Annn: set I to address nnn
         write(*,'(A,Z4)') "Set I to ", nnn
         I = nnn
         pc = pc + 2
      case( DB ) ! Bnnn: jump to location nnn + V(0)
         pc = nnn + asaddr(V(0))
         !pc = nnn + V(0)
      case( DC ) ! Cxkk: V(x) = random byte AND kk
         V(x) = iand(rand_byte(), kk)
         pc = pc + 2
      case( DD ) ! Dxyn: display an n-byte sprite starting at memory
                                 !       location I at (Vx, Vy) on the screen, VF = collision
         print '(A,Z4,2X,Z4,A,I2)', "Draw sprite at ",v(x),v(y)," of height ", n
         call draw_sprite(v(x),v(y),n,win,collision)
         if (collision) then
            v(15) = 1
         else
            v(15) = 0
         end if
         pc = pc + 2
      case( DE )
         select case(kk)
         case( DEX9E )
            block
               integer :: hd
               hd = ashexdigit(V(x))
               call skip_if( keypad(hd) )
            end block
         case( DEXA1 )
            block
               integer :: hd
               hd = ashexdigit(V(x))
               call skip_if ( .not. keypad(hd) )
            end block
         case default
          print *, "IMPOSSIBLE"
           call unknown_opcode(opcode)
         end select
      case( DF ) ! F...
         select case(kk)
         case(DFX07)
            V(x) = delay_timer
            pc = pc + 2
         case(DFX0A)
            call wait_for_keypress(keypad)
            V(x) = findloc(keypad,.true.,1,kind=byte) - 1_byte
            print *, "FX0A V(x) = ", V(x)
            pc = pc + 2
         case(DFX15)
            delay_timer = V(x)
            pc = pc + 2
         case(DFX18)
            sound_timer = V(x)
            pc = pc + 2
         case(DFX1E)
            !I = I + V(x)
            I = I + asaddr(V(x))
            pc = pc + 2
         case(DFX29)
            ! FX29: Set I to location of sprite for
            !       the character in V(X)
            block
               integer :: hd
               hd = ashexdigit(V(x))
               I = fontidx(hd)
            end block
            pc = pc + 2
         case(DFX33)
            ! FX33: Decode VX into binary-coded decimal
            ! TODO: MAke 
            call bcd(V(x))
            pc = pc + 2
         case(DFX55)
            ! FX55: Register dump
            memory(I:I+x) = V(0:x)
            I = I + x + 1
            pc = pc + 2
         case(DFX65)
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

   !> Binary-coded decimal; decode the value in V(x)
   !> to it's decimal digits and store those at address I
   !>
   !> The BCD instruction is useful to display a score.
   subroutine bcd(Vx)
      integer(byte), intent(in) :: Vx
      integer :: u, d

      ! Convert to unsigned integer
      u = iand(transfer(Vx,u),int(z'FF',kind(u)))

      ! The purpose of this function is to set:
      !
      !      memory(I)   = u / 100
      !      memory(I+1) = mod(u,100) / 10
      !      memory(I+2) = mod(u,10)
      !
      ! Here we take the more explicit approach instead.

      d = u / 100   ! Divisor  
      u = u - d*100 ! Remainder

      memory(I)   = int(d,byte)
      
      d = u / 10    ! Divisor
      u = u - d*10  ! Remainder

      memory(I+1) = int(d,byte)
      memory(I+2) = int(u,byte)

   end subroutine



   !> Helper routine for 8XY_ instruction subset
   pure subroutine D8XYN(N,VX,VY,VF)
      integer(byte), intent(in) :: N
      integer(byte), intent(inout) :: VX
      integer(byte), intent(in), value :: VY

      integer(byte), intent(out) :: VF
         ! TODO: Should VF be inout?

      ! It's the responsibility of the caller to make sure the input
      ! arguments don't alias. VY is passed by value; for VF a temporary
      ! variable must be used. After calling this instruction, set 
      ! the status register, V(F).

      integer(byte), parameter :: D8XY0 = int(z'0',byte)
      integer(byte), parameter :: D8XY1 = int(z'1',byte)
      integer(byte), parameter :: D8XY2 = int(z'2',byte)
      integer(byte), parameter :: D8XY3 = int(z'3',byte)
      integer(byte), parameter :: D8XY4 = int(z'4',byte)
      integer(byte), parameter :: D8XY5 = int(z'5',byte)
      integer(byte), parameter :: D8XY6 = int(z'6',byte)
      integer(byte), parameter :: D8XY7 = int(z'7',byte)
      integer(byte), parameter :: D8XY8 = int(z'8',byte)
      integer(byte), parameter :: D8XYE = int(z'E',byte)

      logical :: borrow
      integer(byte), parameter :: BIT0 = int(b'00000001',byte)

      select case ( int(n) )
      case( D8XY0 )
         Vx = Vy
      case( D8XY1 )
         Vx = ior(Vx,Vy)
      case( D8XY2 )
         Vx = iand(Vx,Vy)
      case( D8XY3 )
         Vx = ieor(Vx,Vy)
      case( D8XY4 )
         borrow = (asuint(vx,0) + asuint(vy,0)) > 255
         Vx = Vx + Vy
         if (borrow) then
            VF = 1
         else
            VF = 0
         end if
      case( D8XY5 )
         borrow = asuint(Vx,0) >= asuint(Vy,0)
         Vx = Vx - Vy
         if (borrow) then
            VF = 1
         else
            VF = 0
         end if
      case(  D8XY6 )
         ! 8XY6: SHR Vx, Vy
         !Vx = Vy
         Vx = shiftr(Vy,1)
         VF = iand(Vy,BIT0)
      case( D8XY7 )
         ! 8XY7: SUBN Vx Vy
         borrow = asuint(Vy,0) >= asuint(Vx,0)
         Vx = Vy - Vx
         if (borrow) then
            VF = 1
         else
            VF = 0
         end if
      case( D8XYE )
         ! 8XYE: SHL Vx, Vy
         !Vx = Vy
         Vx = shiftl(Vy,1)
         VF = iand(shiftr(Vy,7),BIT0)
      case default
         block
            integer(instr) :: op
            op = fetch_opcode()
            call unknown_opcode(op)
         end block
      end select

   end subroutine

! Keypad             Keyboard
! +-+-+-+-+          +-+-+-+-+
! |1|2|3|C|          |1|2|3|4|
! +-+-+-+-+          +-+-+-+-+
! |4|5|6|D|          |Q|W|E|R|
! +-+-+-+-+    =>    +-+-+-+-+
! |7|8|9|E|          |A|S|D|F|
! +-+-+-+-+          +-+-+-+-+
! |A|0|B|F|          |Z|X|C|V|
! +-+-+-+-+          +-+-+-+-+
!
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
      if (delay_timer > 0) delay_timer = delay_timer - 1_byte
      if (sound_timer > 0) then
         sound_timer = sound_timer - 1_byte
         ! TODO: make beep
      end if
   end subroutine


   pure subroutine unknown_opcode(opcode)
      integer(int16), intent(in) :: opcode
      character(len=128) :: msg
      write(msg,'(A,Z4)') "FATAL ERROR: Unknown instruction = ", opcode
      error stop msg
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
      integer :: row, col, zx, zy 

      collision = .false.

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

            ! TODO: handle negative vx and vy and clipping
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
      external :: fillrect

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
