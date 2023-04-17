
! Wrapper produce, just to get null string termination
subroutine fcw_open(title)
    use, intrinsic :: iso_c_binding, only: c_null_char
    character(*), intent(in) :: title
    call fcw_open_(title // c_null_char)
end subroutine

program fcw_main

    implicit none (type, external)

    external fcw_open, fcw_clear_screen, fcw_draw_screen, fcw_flush, fcw_close

    external fcw_keypress, fcw_keyrelease
    integer :: fcw_keypress, fcw_keyrelease

    external :: fcw_event, fcw_getkey
    integer :: fcw_getkey

    integer :: buffer(64), res, ievent, istat
    logical :: keys(0:15)

    ! Open a window
    call fcw_open("My XCB Window")

    ! Fill a fixed buffer of 2048 pixels, 
    ! or 64 default (32-bit) integer, with random bits
    buffer = 0
    call fill_buffer(buffer)

    do
        ! Poll events
        ! (only those of relevance for a CHIP-8 engine)
        call fcw_event(ievent,istat)

        ! Check if Escape key was pressed
        if (istat == -2) exit

        ! Is 1 pressed?
        if (fcw_getkey(1) == 1) then
            call fill_buffer(buffer)
            call fcw_draw_screen(buffer)
        end if

        ! Is 3 pressed?
        if (fcw_getkey(3) == 1) then
            call fcw_clear_screen()
        end if

    end do

    ! Close window connection
    call fcw_close()

contains

    function keypad()
        logical :: keypad(0:15)
        integer :: i
        do i = 0, 15
            keypad(i) = fcw_keypress(i) == 1
        end do
    end function

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