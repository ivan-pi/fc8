module portaudio_binding

use, intrinsic :: iso_c_binding
implicit none
private

public :: Pa_GetVersion, Pa_GetVersionText, Pa_GetErrorText, &
          Pa_Initialize, Pa_Terminate, Pa_GetHostApiCount, &
          Pa_GetDefaultHostApi, Pa_OpenStream, Pa_OpenDefaultStream, &
          Pa_CloseStream, Pa_StartStream, Pa_StopStream, Pa_AbortStream, &
          Pa_IsStreamStopped, Pa_IsStreamActive, Pa_Sleep

public :: checkerr

public :: paFloat32, paInt32, paInt24, paInt16, paInt8, paUInt8, &
          paCustomFormat, paNonInterleaved

integer(c_long), parameter :: paFloat32        = int(z'00000001',c_long)
integer(c_long), parameter :: paInt32          = int(z'00000002',c_long)
integer(c_long), parameter :: paInt24          = int(z'00000004',c_long)
integer(c_long), parameter :: paInt16          = int(z'00000008',c_long)
integer(c_long), parameter :: paInt8           = int(z'00000010',c_long)
integer(c_long), parameter :: paUInt8          = int(z'00000020',c_long)
integer(c_long), parameter :: paCustomFormat   = int(z'00010000',c_long)
integer(c_long), parameter :: paNonInterleaved = int(z'80000000',c_long)

interface

function Pa_GetVersion() bind(c,name="Pa_GetVersion")
  import c_int
  integer(c_int) :: Pa_GetVersion
end function

function Pa_GetVersionText() bind(c,name="Pa_GetVersionText")
  import c_ptr
  type(c_ptr) :: Pa_GetVersionText
end function


function Pa_GetErrorText(errorCode) bind(c,name="Pa_GetErrorText")
  import c_int, c_ptr
  integer(c_int) :: errorCode
  type(c_ptr) :: Pa_GetErrorText
end function

function Pa_Initialize() bind(c,name="Pa_Initialize")
  import c_int
  integer(c_int) :: Pa_Initialize
end function

function Pa_Terminate() bind(c,name="Pa_Terminate")
  import c_int
  integer(c_int) :: Pa_Terminate
end function

function Pa_GetHostApiCount() bind(c,name="Pa_GetHostApiCount")
  import c_int
  integer(c_int) :: Pa_GetHostApiCount
end function
function Pa_GetDefaultHostApi() bind(c,name="Pa_GetDefaultHostApi")
  import c_int
  integer(c_int) :: Pa_GetDefaultHostApi
end function

!
! Streaming Types and Functions
!

function Pa_OpenStream( &
    stream, &
    inputParameter, &
    outputParameter, &
    sampleRate, &
    framesPerBuffer, &
    streamFlags, &
    streamCallback, &
    userData) &
  bind(c,name="Pa_OpenStream")
  import :: c_ptr, c_double, c_long, c_funptr, c_int

    type(c_ptr) :: stream
    type(c_ptr), value :: inputParameter
    type(c_ptr), value :: outputParameter
    real(c_double), value :: sampleRate
    integer(c_long), value :: framesPerBuffer
    integer(c_long), value :: streamFlags
    type(c_funptr), value :: streamCallback
    type(c_ptr), value :: userData

    integer(c_int) :: Pa_OpenStream
end function

function Pa_OpenDefaultStream( &
    stream, &
    numInputChannels, &
    numOutputChannels, &
    sampleFormat, &
    sampleRate, &
    framesPerBuffer, &
    streamCallback, &
    userData) &
  bind(c,name="Pa_OpenDefaultStream")
  import :: c_ptr, c_int, c_long, c_double, c_funptr
    type(c_ptr), intent(out) :: stream
    integer(c_int), value :: numInputChannels
    integer(c_int), value :: numOutputChannels
    integer(c_long), value :: sampleFormat
    real(c_double), value :: sampleRate
    integer(c_long), value :: framesPerBuffer
    type(c_funptr), value :: streamCallback
    type(c_ptr), value :: userData
    integer(c_int) :: Pa_OpenDefaultStream
end function

function Pa_CloseStream(stream) bind(c,name="Pa_CloseStream")
  import :: c_ptr, c_int
  type(c_ptr), value :: stream
  integer(c_int) :: Pa_CloseStream
end function

function Pa_StartStream(stream) bind(c,name="Pa_StartStream")
  import :: c_ptr, c_int
  type(c_ptr), value :: stream
  integer(c_int) :: Pa_StartStream
end function

function Pa_StopStream(stream) bind(c,name="Pa_StopStream")
  import :: c_ptr, c_int
  type(c_ptr), value :: stream
  integer(c_int) :: Pa_StopStream
end function

function Pa_AbortStream(stream) bind(c,name="Pa_AbortStream")
  import :: c_ptr, c_int
  type(c_ptr), value :: stream
  integer(c_int) :: Pa_AbortStream
end function

function Pa_IsStreamStopped(stream) bind(c,name="Pa_IsStreamStopped")
  import :: c_ptr, c_int 
  type(c_ptr), value :: stream
  integer(c_int) :: Pa_IsStreamStopped
end function

function Pa_IsStreamActive(stream) bind(c,name="Pa_IsStreamActive")
  import :: c_ptr, c_int 
  type(c_ptr), value :: stream
  integer(c_int) :: Pa_IsStreamActive
end function

function Pa_GetStreamInfo(stream) bind(c,name="Pa_GetStreamInfo")
  import :: c_ptr
  type(c_ptr), value :: stream
  type(c_ptr) :: Pa_GetStreamInfo
end function

function Pa_GetStreamTime(stream) bind(c,name="Pa_GetStreamTime")
  import :: c_ptr, c_double
  type(c_ptr), value :: stream
  real(c_double) :: Pa_GetStreamTime
end function

function Pa_GetStreamCpuLoad(stream) bind(c,name="Pa_GetStreamCpuLoad")
  import :: c_ptr, c_double
  type(c_ptr), value :: stream
  real(c_double) :: Pa_GetStreamCpuLoad
end function

function Pa_ReadStream(stream,buffer,frames) bind(c,name="Pa_ReadStream")
  import :: c_ptr, c_long, c_int 
  type(c_ptr), value :: stream
  type(c_ptr), value :: buffer
  integer(c_long), value :: frames
  integer(c_int) :: Pa_ReadStream
end function

function Pa_WriteStream(stream,buffer,frames) bind(c,name="Pa_WriteStream")
  import :: c_ptr, c_long, c_int
  type(c_ptr), value :: stream
  type(c_ptr), value :: buffer
  integer(c_long), value :: frames
  integer(c_int) :: Pa_WriteStream
end function

function Pa_GetStreamReadAvailable(stream) bind(c,name="Pa_GetStreamReadAvailable")
  import :: c_ptr, c_long
  type(c_ptr), value :: stream
  integer(c_long) :: Pa_GetStreamReadAvailable
end function

function Pa_GetStreamWriteAvailable(stream) bind(c,name="Pa_GetStreamWriteAvailable")
  import :: c_ptr, c_long
  type(c_ptr), value :: stream
  integer(c_long) :: Pa_GetStreamWriteAvailable
end function

!
! Miscellaneous utilities
!
function Pa_GetSampleSize(format) bind(c,name="Pa_GetSampleSize")
  import :: c_long, c_int
  integer(c_long), value :: format
  integer(c_int) :: Pa_GetSampleSize
end function

subroutine Pa_Sleep(msec) bind(c,name="Pa_Sleep")
  import :: c_long
  integer(c_long), intent(in), value :: msec
end subroutine

end interface

contains

subroutine checkerr(ierr)
  use, intrinsic :: iso_fortran_env, only: error_unit
  integer(c_int), intent(in), value :: ierr
  
  character(len=:), pointer :: str => null()
  integer :: unit
  integer(c_size_t) :: n

  interface 
    function c_strlen(str) bind(c,name="strlen")
      import c_ptr, c_size_t
      type(c_ptr), intent(in), value :: str
      integer(c_size_t) :: c_strlen
    end function
  end interface


  if (ierr == 0) return
  unit = error_unit

  n = c_strlen(Pa_GetErrorText(ierr))

  allocate(character(n) :: str)
  call c_f_pointer(Pa_GetErrorText(ierr), str)

  write(unit,'(A)') "PA Error: " // str

end subroutine

end module


module envelopes
    ! Subroutines generating envelopes
    ! https://en.wikipedia.org/wiki/Envelope_(music)

    implicit none
    private

    integer, parameter :: dp = kind(1.0d0)
    real(dp), parameter :: PI = 4*atan(1.0_dp)
    integer, parameter :: RATE = 44100

    ! Parameters of the ADSR envelope:
    ! A   D S   R
    !    /\
    !   /  \____
    !  /        \
    ! /          \
    real(dp) :: attack  = 30.0_dp      ! duration %
    real(dp) :: decay   = 20.0_dp      ! duration %
    real(dp) :: sustain = 80.0_dp      ! max level %
    real(dp) :: release = 30.0_dp      ! duration %

    public :: ADSR_enveloppe, attack, decay, sustain, release

contains

    real(dp) function ADSR_enveloppe(t, t1, t2)
        ! Returns the level in [0, 1] of an ADSR envelope at time t1 < t < t2

        real(dp), intent(in) :: t, t1, t2
        integer :: i, i1, i2, i3, i4, i5

        i = nint(t * RATE)

        ! First part (Attack):
        i1 = nint(t1 * RATE)
        i2 = nint((t1 + (t2-t1) * attack / 100.0_dp) * RATE)

        if ((i >= i1) .and. (i < i2)) then
            ADSR_enveloppe = (i-i1) / real(i2-i1, dp)
        else
            i3 = nint((t1 + (t2-t1) * (attack+decay) / 100.0_dp) * RATE)
            if ((i >= i2) .and. (i < i3)) then
                ADSR_enveloppe = (100.0_dp - (i-i2)/real(i3-i2, dp) * &
                               & (100.0_dp-sustain)) / 100.0_dp
            else
                i4 = nint((t2 - (t2-t1) * release / 100.0_dp) * RATE)
                if ((i >= i3) .and. (i < i4)) then
                    ADSR_enveloppe = (sustain / 100.0_dp)
                else
                    i5 = nint(t2 * RATE)
                    if ((i >= i4) .and. (i <= i5)) then
                        ADSR_enveloppe = (sustain - (i-i4)/real(i5-i4, dp) * &
                                       & sustain) / 100.0_dp
                    else
                        print *, "ERROR ADSR_envelope: t outside [t1, t2] !"
                        ADSR_enveloppe = 1.0_dp
                    end if
                end if
            end if
        end if
    end function

end module envelopes

program main

  use, intrinsic :: iso_c_binding
  use portaudio_binding

  implicit none

  type(c_ptr) :: stream

  real(c_float), parameter :: PI = 4*atan(1.0)

  call checkerr( Pa_Initialize() )

  call checkerr( &
    Pa_OpenDefaultStream( &
            stream = stream, &
            numInputChannels = 0, &
            numOutputChannels = 2, &
            sampleFormat = paFloat32, &
            sampleRate = 44100.0_c_double, &
            framesPerBuffer = 256_c_long, &
            streamCallback = c_funloc(sinewave), &
            userData = c_null_ptr) )

  call checkerr( Pa_StartStream(stream) )

  call Pa_Sleep(5000_c_long)
  
  call checkerr( Pa_StopStream(stream) )

  call checkerr( Pa_Terminate() )

contains

  function sinewave(input,output,frameCount,timeInfo,statusFlags,userData) bind(c)
    
    use envelopes, only: ADSR_enveloppe

    type(c_ptr), value :: input
    type(c_ptr), value :: output
    integer(c_long), value :: frameCount
    type(c_ptr), value :: timeInfo
    integer(c_long), value :: statusFlags
    type(c_ptr), value :: userData

    integer(c_int) :: sinewave

    real(c_float), pointer :: out(:,:) => null()
    
    real(c_double), pointer :: time(:)
    real(c_double), parameter :: dt = 1.0_c_double/44100
    real(c_double) :: t

    real(c_double) :: amplitude, omega, phi

    integer(c_long) :: i

    real(c_double) :: env, sig

    call c_f_pointer(output,out,[2_c_long,frameCount])
    !call c_f_pointer(timeInfo,time,[3])

    amplitude = 4.0
    omega     = 0.5
    phi       = 0.5

    t = 0
    do i = 1, frameCount

      env = ADSR_enveloppe(t,0.0_c_double,frameCount*dt)

      out(1,i) = amplitude*sin(omega*t + phi) * env
      out(2,i) = amplitude*sin(omega*t + phi) * env

      t = t + dt
    end do

    sinewave = 0

  end function

end program


  ! ...

  ierr = Pa_Initialize()
  ierr = Pa_OpenDefaultStream( ..., chip8beep, c_null_ptr)

  execute: do 

    ! ... decode instructions ...


    select case(opcode)
    ! ...
    case(opcode_FX18)
      sound_timer = V(x)   ! <--- set sound timer to value in register V(x)
      pc = pc + 2          ! <--- increase program counter
    ! ...
    end select

    call beep(sound_timer,stream)

  end do execute

  ierr = Pa_Terminate()

contains

  !> Play a beeping sound if sound timer is non-zero.
  !> The sound timer must be decremented at a rate of 60 Hz.
  subroutine beep(sound_timer,stream)
    integer, intent(in) :: sound_timer
    type(c_ptr), value :: stream

    logical, save :: active = .false.
    integer :: ierr

    if (sound_timer > 0) then
      if (.not. active) then
        active = Pa_StartStream(stream) == paNoError
      end if
      sound_timer = sound_timer - 1
    else    
      if (active) then
        active = Pa_StopStream(stream) == paNoError
      end if
    end if

  end subroutine
