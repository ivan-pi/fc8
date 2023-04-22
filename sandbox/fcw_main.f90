
module fcw
    use, intrinsic :: iso_c_binding
    implicit none
    interface
        subroutine fcw_open_display(mf, title) bind(c,name="fcw_open_display")
            import c_int, c_char
            integer(c_int), intent(in), optional :: mf
            character(kind=c_char), intent(in), optional :: title(*)
        end subroutine
        subroutine fcw_close_display() bind(c,name="fcw_close_display")
        end subroutine
        subroutine fcw_clear_display() bind(c,name="fcw_clear_display")
        end subroutine
        subroutine fcw_draw_display(screen) bind(c,name="fcw_draw_display")
            import c_int32_t
            integer(c_int32_t), intent(in) :: screen(64)
        end subroutine
        subroutine fcw_get_event(irep, ikey) bind(c,name="fcw_get_event")
            import c_int
            integer(c_int), intent(out) :: irep, ikey
        end subroutine
    end interface

    type, bind(c) :: keypad
        logical(c_bool) :: key(0:15)
    end type

    type(keypad), bind(c,name="pad") :: pad

end module

program fcw_main

    use fcw
    implicit none

    integer :: buffer(64), res, ikey

    integer, parameter :: ESC = -2

    ! Open a window
    call fcw_open_display(mf=15,title="CHIP-8")

    ! Fill a fixed buffer of 2048 pixels, 
    ! or 64 default (32-bit) integer, with random bits
    buffer = 0
    call fill_buffer(buffer)

    call fcw_draw_display(screen=buffer)

    do
        call fcw_get_event(res,ikey)
        !if (res >= 0) 
        !print *, res, ikey
        if (res == ESC) exit

        ! Is 1 pressed?
        if (pad%key(1)) then
            call fill_buffer(buffer)
            call fcw_draw_display(screen=buffer)
        end if

        if (pad%key(3)) then
            call fcw_clear_display()
        end if

        if (any(pad%key)) then
            write(*,'(*(L1))') pad%key
        end if

    end do

    ! Close window connection
    !call fcw_close()


contains

    subroutine fill_buffer(buffer)
        integer, intent(inout) :: buffer(64)
        integer :: i, j
        real :: r

        ! This is a really inefficient way of generating
        ! a random bit pattern

        do i = 1, 64
            buffer(i) = 0
            do j = 1, 32
                call random_number(r)
                if (r > 0.5) buffer(i) = ibset(buffer(i),j-1)
            end do
        end do
    end subroutine

end program