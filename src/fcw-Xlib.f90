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

    type(keypad), bind(c,name="pad"), protected :: fcw_pad

    type, bind(c) :: drawinfo
        integer(c_int) :: ix, iy, n
    end type

    type(drawinfo), bind(c,name="last") :: last

end module