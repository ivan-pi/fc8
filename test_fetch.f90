program test_fetch
use, intrinsic :: iso_fortran_env, only: int8, int16
use, intrinsic :: iso_c_binding, only: c_loc
implicit none

    interface
        integer(c_int16_t) function fetch_opcode(memory,pc) bind(c)
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: memory
            integer(c_int), value :: pc
        end function
    end interface

    integer(int8), target :: memory(5) = [ &   
        int(z'f0',int8), &  ! 11110000
        int(z'90',int8), &  ! 10010000
        int(z'90',int8), &  ! 10010000
        int(z'90',int8), &  ! 10010000
        int(z'f0',int8) &   ! 11110000
    ]

    integer :: pc
    integer(int8) :: x, y

    pc = 1

    print '(Z4,X,Z4)', fetch_int16(), fetch_int16_bit()
    print '(Z4)', fetch_opcode(c_loc(memory),pc)

    print '(Z4)', shiftl(int(memory(pc),int16),8)
    print '(Z4)', memory(pc+1)

    print *, "T .and. T = ", .true. .and. .true.
    print *, "T .and. F = ", .true. .and. .false.
    print *, "F .and. T = ", .false. .and. .true.
    print *, "F .and. F = ", .false. .and. .false.
    print *, " ------------- "
    print *, "T .neqv. T = ", .true. .neqv. .true.
    print *, "T .neqv. F = ", .true. .neqv. .false.
    print *, "F .neqv. T = ", .false. .neqv. .true.
    print *, "F .neqv. F = ", .false. .neqv. .false.


    x = readx(int(z'D01F',int16))
    y = ready(int(z'D01F',int16))
    
    print *, "x = ", x
    print *, "y = ", y
    print *, "n = ", iand(int(z'D01F',int16),int(z'000F',int16)) ! the lowest 4 bits
contains

    function fetch_int16() result(ptr)
        use, intrinsic :: iso_c_binding
        integer(int16), pointer :: ptr
        call c_f_pointer(cptr=c_loc(memory(pc)),fptr=ptr)
    end function

    function fetch_int16_bit() result(opcode)
        integer(int16) :: opcode

        opcode = transfer(achar(memory(pc+1)) // &
                          achar(memory(pc)), opcode)
!        opcode = ior(shiftl(int(memory(pc),int16),8), &
!                     int(memory(pc+1),int16))
        
    end function


    integer(int8) function readx(op)
        integer(int16), intent(in), target :: op

        integer(int8) :: op8(2)

        op8 = transfer(op,op8)
        readx = iand(op8(2),int(b'00001111',int8))

    end function

    integer(int8) function ready(op)
        integer(int16), intent(in), target :: op
        integer(int8) :: op8(2)

        op8 = transfer(shiftr(op,4),op8)
        ready = iand(op8(1),int(b'00001111',int8))

    end function
end program