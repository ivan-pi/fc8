

program test_shift
use, intrinsic :: iso_fortran_env, only: int8, int16
implicit none
integer(int8) :: Vx, Vy, Vf

Vx = 224_int8
!Vx = as_sint(224)
Vx = shiftl(Vx,1_int8)

print *, "Actual   = ", as_uint(Vx)
print *, "Expected = ", 192_int16

vx = 120
vy = 140
vx = as_uint(vx) - as_uint(vy)

print *, "Actual   = ", as_uint(vx)
print *, "Expected = ", 236_int16


vx = int(z'8C')
vy = int(z'78')
vx = vx - vy
print *, "Actual   = ", as_uint(vx)
print *, "Expected = ", int(z'EC',int16)

print *, "v7 = ", int(z'8C')
print *, "v6 = ", int(z'78')

contains
    function as_uint(x) result(y)
        integer(int8), intent(in), value :: x
        integer(int16) :: y
        y = iand(transfer(x,y),int(z'00FF',int16))
    end function

    function as_sint(x) result(y)
        integer, intent(in), value :: x
        integer(int8) :: y
        y = transfer(iand(x,int(z'000F')),y)
    end function
end program