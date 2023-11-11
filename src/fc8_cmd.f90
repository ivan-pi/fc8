module fc8_cmd

implicit none

logical :: silent = .false.
real :: zoom = 100

character(len=32) :: displaynumber = "unknown"

character(len=128) :: filename = ""
character(len=128) :: config_file = "~/.fc8.nml"

character(len=*), parameter :: version = "0.1.0"

contains

subroutine print_help_message_and_stop

   write(*,'(A)') "Usage: fc8 [-h] [-s] [-zoom=Z] [--config=FILE] <path-to-ch8>"
   write(*,'(A)')
   write(*,'(A)') "  Fortran CHIP-8 interpreter"
   write(*,'(A)')
   write(*,'(A)') "required arguments:"
   write(*,'(A)') "  <path-to-ch8>           a CHIP8 ROM file"
   write(*,'(A)') 
   write(*,'(A)') "options:"
   write(*,'(A)') "  -h, --help              show this help text and exit"
   write(*,'(A)') "  -v, --version           display version and exit"
   write(*,'(A)') "  -s, --silent            silent mode (disable audio)"
   write(*,'(A)') "  --zoom=Z                zoom percentage, 30 <= Z <= 200"
   write(*,'(A)') "  --display=:N            server display number; the default is 0"
   write(*,'(A)') "  --config=FILE           configuration file: a Fortran namelist"
   write(*,'(A)')

   stop

end subroutine


subroutine process_cmdline()

   use, intrinsic :: iso_fortran_env, only: error_unit, compiler_version

   integer :: argc

   character(len=64) :: value
   integer :: lv, stat, i, k

   logical :: file_exists, option_exists

   argc = command_argument_count()

   if (argc < 1) then
      call print_help_message_and_stop
   end if

   ! Process options
   do i = 1, argc-1

      call get_command_argument(i,value,lv,status=stat)
!      print *, "LOG: ",i, value

      if (stat /= 0) then
         write(error_unit,'(A)') "Failed to retrieve command argument."
         error stop
      end if

      ! Assume the option exists
      option_exists = .true.

      ! Check if stand-along or key-value option
      k = index(value(1:lv),'=')

      if (k == 0) then
         ! Standalone options
         select case(value(1:lv))
         case("-h","--help")
            call print_help_message_and_stop
         case("-v","--version")
            write(*,'(A)') "fc8 version "//version
            write(*,'(A)') "  built using "//compiler_version()
            stop
         case("-s","--silent")
            ! Turn off the sound
            silent = .true.
         case default
            option_exists = .false.
            if (i < argc) then
               write(error_unit,'(A)') &
                  "Error: '"//trim(value)//"' does not match any of the expected optional arguments."
               error stop
            end if
         end select

      else
         ! Key-value options
         select case(value(1:k-1))
         case("--zoom")
            read(value(k+1:),*,iostat=stat) zoom
            if (stat /= 0 .or. zoom < 30. .or. zoom > 200) then
               write(error_unit,'(A)') &
                  "Invalid zoom value; it should be in the range [30,200]."
               error stop
            end if
            print *, "LOG: zoom = ", zoom
         case("--display")
            error stop "--display is not implemented"
         case("--config")
            read(value(k+1:),*,iostat=stat) config_file
            if (stat /= 0) then
               write(error_unit,'(A)') "Warning: failed to read the config file option,"
               write(error_unit,'(A)') "         using the internal default settings instead"
            end if
            print *, "LOG: config = ", trim(config_file)
         case default
            option_exists = .false.
         end select
      end if

      if (.not. option_exists .and. i < argc) then
         write(error_unit,'(A)') &
            "Error: '"//value(1:k)//"' does not match any of the expected optional arguments."
         error stop
      end if

   end do
   
   ! Assume the last argument, is the ROM file

   call get_command_argument(argc,filename,lv,status=stat)
   print *, "LOG: ", filename

   if (stat /= 0) then
      write(error_unit,'(A,G0,A)') &
         "Failed to retrieve command argument #", argc, " (the ROM file)."
      write(error_unit,'(A,I0)') "Status flag: ", stat
      error stop
   end if

   inquire(file=filename(1:lv),exist=file_exists)
   if (.not. file_exists) then
       write(error_unit,'(A)') &
         "The ROM (cartridge) '"//filename(1:lv)//"' does not exist"
       error stop
   end if

end subroutine

end module