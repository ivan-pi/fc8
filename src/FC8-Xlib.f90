program FC8
    
  use, intrinsic :: iso_c_binding, only: c_null_char
  
  use fcw
  use FC8_vm, only: vexec, initmem, loadgame, timers, screen => pixelbuf
  use sleep_std, only: sleep_ms

  implicit none

  character(len=*), parameter :: WIN_TITLE  = 'CHIP-8 Interpreter' // c_null_char
  character(len=*), parameter :: BG_COLOR  = 'black' // c_null_char

  integer, parameter :: MF = 8

  character(len=256) :: filename
  integer :: nargs
  logical :: rom_exists

  integer, parameter :: prec = selected_int_kind(18)
  integer(prec) :: delta, tprev, tcurr, trate, tstep

  integer :: ievent, ikey, ireq

  call initmem()

  nargs = command_argument_count()
  if (nargs /= 1) then
      print *, "Wrong number of arguments."
      print *, "Usage:"
      print *, "   chip8 <filename>"
      stop
  end if

  call get_command_argument(1,filename)
  print *, filename

  inquire(file=filename,exist=rom_exists)
  if (.not. rom_exists) then
      print *, "Cartridge " // trim(filename) // " doesn't exist"
      stop
  end if

  call loadgame(filename)

  call fcw_open_display(MF,title="CHIP-8")

  call system_clock(tprev,trate)
  tstep = nint(trate/60.)
  !print *, "tstep = ", tstep

  ireq = -1  ! No request

  do

    call fcw_get_event(ievent,ikey)

    select case(ievent)
    case( -2 )
      ! Escape (ESC) was pressed
      exit
    case( -3 )
      ! Reload game (key 0)
      call loadgame(filename)
    end select

    ! Interpreter step
    call vexec(ireq,fcw_pad%key)

    ! Handle interpreter requests
    select case( ireq )
    case( 1 )
      ! 00E0: Clear display
      call fcw_clear_display()
      ireq = -1
    case( 2 )
      ! DXYN: Update display
      call fcw_draw_display(screen)
      call sleep_ms(15)
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

  end do

  call fcw_close_display()

end program
