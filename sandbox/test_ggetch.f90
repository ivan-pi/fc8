program main
    
    use, intrinsic :: iso_c_binding, only: c_null_char
    use procall, only: ggetch
    implicit none

    character(len=*), parameter :: WIN_TITLE  = 'CHIP-8 Interpreter' // c_null_char
    character(len=*), parameter :: BG_COLOR  = 'black' // c_null_char

    integer, parameter :: MF = 8

    integer,          parameter :: WIN_WIDTH  = 64 * MF
    integer,          parameter :: WIN_HEIGHT = 32 * MF
    integer,          parameter :: MAX_ITER   = 256

    integer :: win
    integer :: key
    logical :: keypad(0:15) = .false.
    logical :: esc = .false.
    integer :: nnone

    call gopen(WIN_WIDTH, WIN_HEIGHT, win)  ! Open X11 window.
    call winname(win, WIN_TITLE)            ! Set window title.
    call gsetbgcolor(win, BG_COLOR)         ! Set background colour.

    call gsetnonblock(1)
    nnone = 0
    do
        call ggetch(key)
        if (key >= 0) then
            print *, "Got key = ", key, nnone
            nnone = 0
        else
            nnone = nnone + 1
        end if
        !call get_keypad(esc)
        !if (any(keypad)) then
        !    write(*,'(*(L1))') keypad
        !end if

        if (esc) exit
    end do

    call gcloseall()


contains
   subroutine get_keypad(esc)
      logical, intent(out) :: esc
                                      !   ProCall          Hex  Keyboard
integer, parameter :: keymap(0:15) = [  INT(z'78'), &    !  0      x
                                        INT(z'31'), &    !  1      1
                                        INT(z'32'), &    !  2      2
                                        INT(z'33'), &    !  3      3
                                        INT(z'71'), &    !  4      q
                                        INT(z'77'), &    !  5      w
                                        INT(z'65'), &    !  6      e
                                        INT(z'61'), &    !  7      a
                                        INT(z'73'), &    !  8      s
                                        INT(z'64'), &    !  9      d
                                        INT(z'79'), &    !  A      y
                                        INT(z'63'), &    !  B      c
                                        INT(z'34'), &    !  C      4
                                        INT(z'72'), &    !  D      r
                                        INT(z'66'), &    !  E      f
                                        INT(z'76')]      !  F      v
      integer :: key
      integer :: i, which, nnone

      esc = .false.
      keypad = .false.
      nnone = 0


      do i = 1, 16
         call ggetch(key)
         if (key == int(z'1b')) then
            esc = .true.
            return
        end if
         which = findloc(keymap,key,dim=1)
         if (which > 0) then
            keypad(which-1) = .true.
         end if
         call msleep(5)
      end do
   end subroutine
end program