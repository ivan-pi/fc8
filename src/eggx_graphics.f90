module eggx_graphics

    ! The abstract graphics device interface
    use graphics, only: device

    ! The ProCALL graphics library
    use procall

    implicit none
    private

    public :: eggx_device

    type, extends(device) :: eggx_device
        integer :: win
    end type

    integer :: num_eggx_devices = 0

    character(len=*), parameter :: WIN_TITLE  = 'CHIP-8 Interpreter' // c_null_char
    character(len=*), parameter :: BG_COLOR  = 'black' // c_null_char

    ! Magnification factor
    integer, parameter :: MF = 8

    integer, parameter :: WIN_WIDTH  = 64 * MF
    integer, parameter :: WIN_HEIGHT = 32 * MF

contains

    function new_eggx_device() result(dev)

        type(eggx_device) :: dev

        call gopen(WIDTH,HEIGHT,dev%win)
        call winname(dev%win,TITLE)

        call newpencolor(dev%win,1)
    
        call layer(dev%win,0,1)
        call gsetbgcolor(dev%win, BG_COLOR)         ! Set background colour.

        ! Count how many EGGX windows are open
        num_eggx_devices = num_eggx_devices + 1

    end function

    subroutine eggx_device_clear(dev)
        class(eggx_device), intent(inout) :: dev
        call clear_window(dev%win)
    end subroutine

    subroutine eggx_device_display(dev,screen)

        class(eggx_device), intent(in) :: dev
        logical, intent(in) :: screen(0:31,0:63)

        integer :: zx, zy
        real :: wx, wy
        INTEGER, PARAMETER :: MF = 8

        call clear_window(dev%win)

        do zy = 0, 31
            do zx = 0, 63
                if (screen(zx,zy)) then

                    wx = mf*(zx)
                    wy = mf*(31 - zy)

                    call fillrect(dev%win,wx,wy,real(mf),real(mf))

                end if
            end do
        end do

        call copylayer(dev%win,1,0)
        call msleep(10)


    subroutine eggx_close_all()
        call gcloseall()
    end subroutine

end module