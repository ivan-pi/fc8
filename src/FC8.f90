program FC8
    
    use, intrinsic :: iso_c_binding, only: c_null_char
    
    use procall
    use FC8_vm
    
    implicit none

    character(len=*), parameter :: WIN_TITLE  = 'CHIP-8 Interpreter' // c_null_char
    character(len=*), parameter :: BG_COLOR  = 'black' // c_null_char

    integer, parameter :: MF = 8

    integer,          parameter :: WIN_WIDTH  = 64 * MF
    integer,          parameter :: WIN_HEIGHT = 32 * MF
    integer,          parameter :: MAX_ITER   = 256

    integer :: win

    character(len=256) :: filename
    integer :: nargs
    logical :: rom_exists

    integer, parameter :: prec = selected_int_kind(18)
    integer(prec) :: delta, tprev, tcurr, trate, tstep

    external :: winname

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

    call gopen(WIN_WIDTH, WIN_HEIGHT, win)  ! Open X11 window.
    call winname(win, WIN_TITLE)            ! Set window title.

    call newpencolor(win,1)
    
    call layer(win,0,1)
    call gsetbgcolor(win, BG_COLOR)         ! Set background colour.

    call gsetnonblock(0)

   call system_clock(tprev,trate)
   tstep = nint(trate/60.d0)
   !print *, "tstep = ", tstep

   do
      call vexec(win)

      call system_clock(tcurr)
      delta = tcurr - tprev
      if (delta > tstep) then
         call timers()
         tprev = tcurr
      end if

   end do

   call gcloseall()

end program
