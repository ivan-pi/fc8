use, intrinsic :: iso_fortran_env, only: int8
interface
   function rand() bind(c,name="randint8")
      use, intrinsic :: iso_c_binding, only: c_int8_t
      integer(c_int8_t) :: rand
   end function
end interface

do i = 1, 20
    print *, iand(rand(),int(b'00000111',int8))
end do
end