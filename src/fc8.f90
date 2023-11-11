program fc8
    
use, intrinsic :: iso_c_binding, only: c_null_char

use fc8_cmd, only: process_cmdline, filename, zoom
use fc8_io
use fc8_vm, only: vexec, loadgame, timers, screen => pixelbuf
use sleep_std, only: sleep_ms

implicit none

character(len=*), parameter :: wtitle  = 'CHIP-8 Interpreter'

integer :: nargs
logical :: rom_exists

integer, parameter :: prec = selected_int_kind(18)
integer(prec) :: delta, tprev, tcurr, trate, tstep

integer :: ievent, ikey, ireq

call process_cmdline()

call loadgame(filename)

call display_open(wtitle,len(wtitle),zoom)

call system_clock(tprev,trate)
tstep = nint(trate/60.d0)
!print *, "tstep = ", tstep

ireq = -1  ! No request


! Enter the CHIP8 instruction cycle
execute: do

    !
    ! Handle events
    !
    call event_get(ievent,ikey)

    select case(ievent)
    case( -2 )
        ! Escape (ESC) was pressed
        exit execute
    case( -3 )
        ! Reload game (key 0)
        call loadgame(filename)
    end select

    !
    ! Fetch-decode-execute
    !
    call vexec(ireq,keypad%key)

    !
    ! Handle interpreter requests
    !
    select case( ireq )
    case( 1 )
        ! 00E0: Clear display
        call display_clear()
        ireq = -1
    case( 2 )
        ! DXYN: Update display
        call display_draw(screen)
        call sleep_ms(17)
        ireq = -1
    case( 3 )
        ! FX0A: Wait for key press (we actually wait for release)
        if (ievent == 3) then
            if (ikey >= 0) ireq = ikey
        else
            ! Block until valid key is pressed
            ! The program counter is not incremented
            ireq = -1
        end if
    end select

    ! Timers
    call system_clock(tcurr)
    delta = tcurr - tprev
    if (delta > tstep) then
        call timers()
        tprev = tcurr
    end if

end do execute

call display_close()

end program
