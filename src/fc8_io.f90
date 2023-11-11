module fc8_io

use, intrinsic :: iso_c_binding
implicit none

private

! Display management
public :: display_open
public :: display_close
public :: display_clear
public :: display_draw

! Keyboard event management
public :: event_get
public :: keypad

interface

    subroutine display_open(title,n,zoom) bind(c,name="fc8_display_open")
        import c_int, c_char, c_float
        integer(c_int), intent(in), value :: n
        character(kind=c_char), intent(in) :: title(n)
        real(c_float), intent(in), optional :: zoom
    end subroutine

    subroutine display_close() bind(c,name="fc8_display_close")
    end subroutine

    subroutine display_clear() bind(c,name="fc8_display_clear")
    end subroutine

    subroutine display_draw(screen) bind(c,name="fc8_display_draw")
        import c_int32_t
        integer(c_int32_t), intent(in) :: screen(64)
    end subroutine

    subroutine event_get(irep, ikey) bind(c,name="fc8_event_get")
        import c_int
        integer(c_int), intent(out) :: irep, ikey
    end subroutine

end interface

type, bind(c) :: fc8_keypad
    logical(c_bool) :: key(0:15) = .false.
end type

type(fc8_keypad), bind(c,name="keypad"), protected :: keypad

end module