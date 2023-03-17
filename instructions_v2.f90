! Installing 
! $ sudo apt-get install libsdl2-dev libsdl2-doc
! $ sudo apt-get install portaudio19-dev portaudio19-doc



! 00E0: Clear the display

! 00EE: Return from subroutine

! 1NNN: Jump to address nnn

! 2NNN: Call subroutine at nnn

! 3xkk: Skip next instruction
if (V(x) == kk) then
   pc = pc + 4
else
   pc = pc + 2
end if

! 4xkk: Skip next instruction
if (V(x) /= kk) then
   pc = pc + 4
else
   pc = pc + 2
end if

! 5xy0: Skip next instruction
if (V(x) == V(y)) then
   pc = pc + 4
else
   pc = pc + 2
endif

! 6xkk
V(x) = kk
pc = pc + 2

! 7xkk
V(x) = V(x) + kk
pc = pc + 2


subroutine op8_math(k,V,x,y)
   integer, intent(in) :: k, x, y
   integer, intent(inout) :: V(0:15)

   select case(k)
   case(0)
      ! 8xy0
      V(x) = V(y)
   case(1)
      ! 8xy1
      V(x) = ior(V(x),V(y))
   case(2)
      ! 8xy2
      V(x) = iand(V(x),V(y))
   case(3)
      ! 8xy3
      V(x) = ieor(V(x),V(y))
   case(4)
      ! 8xy4
      sxy = V(x) + V(y)
      if (sxy > 255) then
         V(15) = 1
      else
         V(15) = 0
      end if
      V(x) = iand(sxy,int(z'FF',int16))
   case(5)
      !8XY5: SUB Vx Vy
      if (V(x) > V(y)) then
         V(15) = 1
      else
         V(15) = 0
      end if
      V(x) = V(x) - V(y)
   case(6)
      ! 8XY6: SHR Vx
      if (mod(V(x),2) == 1) then
         V(15) = 1
      else
         V(15) = 0
      end if
      V(x) = shiftr(V(x),1)
   case(7)
      ! 8XY7: SUBN Vx Vy
      if (V(y) > V(x)) then
         V(15) = 1
      else
         V(15) = 0
      end if
      V(x) = V(y) - V(x)
   case(8)
      if (iand(V(x),10000000) == 1) then
         V(15) = 1
      else
         V(15) = 0
      end if
      V(x) = shiftl(V(x),1)
   end select
end subroutine


! 9XY0: Skip next instruction
if (V(x) /= V(y)) then
   pc = pc + 4
else
   pc = pc + 2
end if

! ANNN
I = nnn

! BNNN: Jump + V(0)
pc = nnn + V(0)

! CXKK: Generate random

! DXYN: Display N-byte sprite

! EX9E: Skip next instruction if key pressed
if (V(x)

! EXA1: Skip next instruction if key not pressed


! FX07: Load Vx DT
V(x) = delay_timer
pc = pc + 2

! FX0A: Load Vx, K
!
! Wait for a key press
!
pressed = .false.
do
   pressed = peekcharqq()
   if (pressed) then
      key = getcharqq()                   ! <--- insert keymap here
      i = index('0123456789ABCDEF',key)
      if (i > 0) then
         i = i - 1
         exit
      end if
   end if
end do
V(x) = i
pc = pc + 2




! FX15: Load DT, Vx
delay_timer = V(x)
pc = pc + 2

! FX18: Load ST, Vx
sound_timer = V(x)
pc = pc + 2

! FX1E: ADD I Vx
I = I + V(x)
pc = pc + 2

! FX29: LD F, Vx
I = V(x) * 5      ! ???

! FX33: LD B Vx
mem(I)   = mod(V(x)    ,100)
mem(I+1) = mod(V(x)/10 ,10)
mem(I+2) = mod(V(x)/100,10)
pc = pc + 2


! FX55: LD [I], Vx
mem(I:I + x) = V(0:x)
I = I + x + 1         ! ???

! FX65: LD Vx, [I]
V(0:x) = mem(I:I+x) 
I = I + x + 1         ! ???


