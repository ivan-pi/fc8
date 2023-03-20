use iso_fortran_env, only: int8

print '(b0.8)', iadd(int(b'01110000',int8),int(b'00001111',int8))

contains

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
end