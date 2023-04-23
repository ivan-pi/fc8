module sleep_std

use, intrinsic :: iso_c_binding, only : C_INT

implicit none (type, external)

private
public :: sleep_ms

interface
subroutine c_sleep(milliseconds) bind(C, name="c_sleep")
import C_INT
integer(C_INT), intent(in) :: milliseconds
end subroutine
end interface

contains

subroutine sleep_ms(milliseconds)
integer(C_INT), intent(in) :: milliseconds
call c_sleep(milliseconds)
end subroutine sleep_ms

end module sleep_std